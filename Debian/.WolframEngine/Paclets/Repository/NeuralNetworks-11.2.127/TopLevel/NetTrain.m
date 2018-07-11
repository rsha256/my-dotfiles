Package["NeuralNetworks`"]



PackageExport["NetTrain"]

Clear[NetTrain]

Options[NetTrain] = {
	BatchSize -> Automatic,
	MaxTrainingRounds -> Automatic,
	Method -> Automatic,
	TargetDevice -> "CPU",
	ValidationSet -> None,
	LearningRateMultipliers -> Automatic,
	TrainingProgressCheckpointing -> None,
	TrainingProgressReporting -> Automatic,
	TrainingProgressFunction -> None
};

NetTrain::invnet = "First argument to NetTrain should be a fully specified net.";
NetTrain::unspecloss = "Provided loss layer is not fully specified.";
NetTrain::netnoparams = "Net does not contain any trainable parameters: returning net unchanged."
NetTrain::netfrozen = "All trainable parameters have been frozen: returning net unchanged."
General::optpi = "The value of `` -> `` should be a positive machine-sized integer."

$now := N[SessionTime[]]; (* %KERNEL otherwise it is a bignum *)

$SymbolRule = Rule[_Symbol, _] | RuleDelayed[_Symbol, _];
$NotOption = Except[$SymbolRule | {$SymbolRule..}];

$OptionLikeLossSpec = {Rule[_String, _]..} | Rule[_String, _]; (* 329437 *)
NetTrain[net_, data_, lossSpec:$OptionLikeLossSpec, opts:OptionsPattern[]] :=
	 NetTrain[net, data, lossSpec, "TrainedNet", opts];

NetTrain[net_, data_, opts:OptionsPattern[]] :=
	NetTrain[net, data, Automatic, "TrainedNet", opts];

NetTrain[net_, data_, lossSpec:$NotOption, opts:OptionsPattern[]] :=
	If[MatchQ[ToList[lossSpec], $validReturnSpec], 
		(* because people will make this mistake... *)
		NetTrain[net, data, Automatic, lossSpec, opts],
		NetTrain[net, data, lossSpec, "TrainedNet", opts]
	];

$LastAbortedLine = 0;
$LastAbortedTime = 0;
$TrainingCounter = 0;

SetAttributes[NetTrainBlock, HoldAllComplete];
NetTrainBlock[expr_] := WithLocalSettings[
	$CleanupQueue = {}; (* this has to be global, as the Scope needs to happen inside the WithLocalSettings *)
	, 
	CatchFailureAsMessage[NetTrain, CatchEncoderFailure @ expr]
	,
	AllowExceptions[
		ReleaseHold[$CleanupQueue];
		$CleanupQueue = {};
	]
] // Replace[{$Aborted :> Abort[], $SoftAborted :> $Aborted}];

prePack[a_ -> b_] := prePack[a] -> prePack[b];
prePack[assoc_Association] := Map[prePack, assoc];
prePack[e:{__Rule}] := prePack[Keys[e]] -> prePack[Values[e]];
prePack[e_List] := ToPackedArray[e];
prePack[e_] := e;

NetTrain[net_, idata_, lossSpec:$NotOption, returnSpec:$NotOption, OptionsPattern[]] := Timed @ NetTrainBlock @ Scope[

	$CleanupQueue ^= {};

	(****************************************************************)
	(* get options and do basic validation                          *)
	(****************************************************************)

	TimeLabel["Setup"];
	If[!MatchQ[ToList @ returnSpec, $validReturnSpec],
		NetTrain::invntrspec = "Property specification should be one or more of ``.";
		ThrowFailure["invntrspec", DiscardStrings["$*"] @ DeepCases[$validReturnSpec, _String]];
	];

	(* it is much more expensive to do things like get array dimensions if the array isn't
	packed, so we take a hit now in exchange for other things being cheaper later *)
	data = prePack[idata];

	If[!ValidNetQ[net], ThrowFailure["invnet"]];
	If[!ConcreteNetQ[net], 
		net2 = JITInferTrainNet[net, lossSpec, data];
		If[FailureQ[net2] || !ConcreteNetQ[net2], ThrowNotSpecifiedFailure[net, "train"]];
		net = net2;
	];

	CheckForTuplePorts[Inputs[net], NetTrain];
	CheckForTuplePorts[Outputs[net], NetTrain];
	
	UnpackOptions[
		method, batchSize, maxTrainingRounds, targetDevice, validationSet,
		learningRateMultipliers, trainingProgressCheckpointing, trainingProgressFunction, trainingProgressReporting
	];

	If[!PositiveMachineIntegerQ[batchSize] && batchSize =!= Automatic,
		ThrowFailure["optpi", BatchSize, batchSize];
	];

	$targetTime = None;
	maxTrainingRounds = Replace[maxTrainingRounds,
		HoldPattern[q:Quantity[_ ? Positive, "Seconds"|"Minutes"|"Hours"]] :> (
			$targetTime = N@QuantityMagnitude[q, "Seconds"];
			Automatic
		)
	];
	If[!PositiveMachineIntegerQ[maxTrainingRounds] && maxTrainingRounds =!= Automatic,
		ThrowFailure["optpi", MaxTrainingRounds, maxTrainingRounds];
	];
	If[IntegerQ[maxTrainingRounds] && maxTrainingRounds < 5, 
		$doPreEncoding = False,
		$doPreEncoding = True
	];

	(* TODO: is DefaultContext needed anymore? *)
	$DefaultContext = ParseContext[targetDevice];

	(****************************************************************)
	(* initialize the net and set up an MXTrainer                   *)
	(****************************************************************)
	
	$AbnormalArrayCallback = throwNetDiverge;

	(* free up as much RAM as possible *)
	ClearCache[];
	ClearCachedNDArrays[];

	$lossSpec = lossSpec; (* for "TrainingNet" property *)
	$net = NetInitialize[net];
	If[FailureQ[$net], ThrowRawFailure[$net]];

	(* pick a method *)
	If[method === Automatic,
		numberParams = Total @ DeepCases[NData @ $net, ra_RawArray :> (Times @@ Dimensions[ra])];
		method = If[numberParams < 128, "StochasticGradientDescent", "ADAM"];
	,
		If[!FreeQ[method, "SequenceBucketingPartitions"],
			bpart = Null;
			method = method /. ("SequenceBucketingPartitions" -> c_) :> (bpart = c; Nothing);
			If[bpart =!= Null,
				If[!IntegerQ[bpart] && bpart =!= None, ThrowRawFailure[$Failed]];
				$SequenceBucketingPartitions ^= bpart;
			]
		];
	];

	(* this is to make sure our first trainer doesn't have a batch size
	that exceeds the data length, so we can avoid reshaping the first trainer.
	mainly an optimizer for small example NetTrain usage *)
	trainingLength = GuessDataLength[data];
	If[trainingLength === 0, ThrowFailure["invtdata"]];

	(* for ValidationSet -> Scaled[..], pick a maxBatchSize that will allow 
	us to allocate an automatic ValidationSet of the right size. Only kicks 
	in for VERY small training data set sizes *)
	maxBatchSize = trainingLength;
	If[MatchQ[validationSet, Scaled[n_ /; 0 <= n <= 1]] && batchSize === Automatic,
		maxBatchSize = Ceiling[First[validationSet] * trainingLength];
	];

	maxBatches = trainingLength; (* dummy value until fixed later *)
	$trainer = ToMXTrainer[
		$net, Replace[r_Rule :> {r}] @ $lossSpec, 
		"BatchSize" -> batchSize, "Context" -> $DefaultContext,
		"Optimizer" -> method, "MaxBatchSize" -> maxBatchSize,
		"TotalBatches" -> Hold[maxBatches],
		"LearningRates" -> learningRateMultipliers
	];
	
	If[$trainer === None,
		If[learningRateMultipliers =!= Automatic,
			Message[NetTrain::netfrozen],
			Message[NetTrain::netnoparams]
		];
		Return[net];
	];

	inputs = $trainer["Inputs"];
	batchSize = $trainer["BatchSize"];

	(****************************************************************)
	(* create the training data generator                           *)
	(****************************************************************)

	(* this is an assoc containing misc info that gets passed to any user-defined
	generators. Set up by ParseTrainingData. *)
	$generatorInput = None;

	(* normalize the training data, check it matches the inputs of the training net,
	create a generator, and get an exact length *)
	{trainingGenerator, trainingLength} = ParseTrainingData[data, inputs, batchSize, ConstructTrainingGenerator];

	(* if we guessed the training length wrong and the batch size is actaully bigger than
	the training length, we have to recreate the mxtrainer *)
	If[batchSize > trainingLength,
		maxBatches = trainingLength;
		(* TODO: actually implement MXTrainerReshape and use here instead *)
		$trainer = Null;
		$trainer = ToMXTrainer[
			$net, $lossSpec, 
			"BatchSize" -> batchSize, "Context" -> $DefaultContext,
			"Optimizer" -> method, "MaxBatchSize" -> trainingLength,
			"TotalBatches" -> Hold[maxBatches], "LearningRates" -> learningRateMultipliers
		];
	];

	$batchesPerRound = Ceiling[trainingLength / batchSize];

	(****************************************************************)
	(* process the ValidationSet option                             *)
	(****************************************************************)

	If[Length[validationSet] == 2 && MatchQ[validationSet, {_, "Interval" -> _}],
		{validationSet, validationInterval} = validationSet;
		validationInterval = {validationInterval, "MinimumInterval" -> 0};
	,
		validationInterval = {"Interval" -> 1, "MinimumInterval" -> 2}; (* default interval is 1 round *)
	];

	Switch[validationSet
	,	None,
		$doValidation = False;

	,	Scaled[_ ? NumericQ],
		(* for this option, we re-use the training generator, reserving some fraction 
		of the batches at the end to be devoted to validation. *)
		$doValidation = True;
		$scaledValidation = True;
		vfraction = N @ First[validationSet];
		NetTrain::invvsfrac = "Setting ValidationSet->Scaled[n] should use n between 0 and 1 such that at training data retains at least one batch.";
		If[!(0 < fraction < 1), ThrowFailure["invvsfrac"]];
		newBPR = Floor[$batchesPerRound * (1 - vfraction)];
		If[newBPR < 1 || newBPR == $batchesPerRound, ThrowFailure["invvsfrac"]];
		$validationLength = ($batchesPerRound - newBPR) * batchSize;
		$batchesPerRound = newBPR; trainingLength = newBPR * batchSize;
		$validationGenerator = Function[n, trainingGenerator[n + $batchesPerRound]];
	,	_,
		$doValidation = True;
		$scaledValidation = False;
		validationSet = prePack[validationSet];
		{$validationGenerator, $validationLength} = ParseTrainingData[validationSet, inputs, batchSize, ConstructValidationGenerator];
	];

	batchperm = Range[$batchesPerRound];

	If[$doValidation,
		$validationDir = CreateDirectory[];
		AppendTo[$CleanupQueue, Hold[DeleteDirectory][$validationDir, DeleteContents -> True]];
		validationTimer = makeTimer[ValidationSet, validationFunction, validationInterval];
	,
		validationTimer = Hold;
	];

	(****************************************************************)
	(* pick a max training round by timing an update                *)
	(****************************************************************)

	If[maxTrainingRounds === Automatic, 
		{genTime, nextDataBatch} = AbsoluteTiming @ trainingGenerator[1];
		execTime = timeTrainer[];
		execTime = Which[ (* choose an repetition time adaptively *)
			execTime < 0.001, Min @ Table[timeTrainer[], 50], 
			execTime < 0.01, Min @ Table[timeTrainer[], 10],
			execTime < 0.05, Min @ Table[timeTrainer[], 3],
			execTime < 0.25, Min[execTime, timeTrainer[]],
			True, execTime
		];
		(* async data gen: we will be generating the next batch while
		training this batch, so we do Max rather than add them *)
		timeForBatch = Max[genTime, execTime, 0.0000001];
		timeForRound = timeForBatch * $batchesPerRound;
		maxTrainingRounds = If[$targetTime === None,
			Clip[Ceiling[20. / timeForRound], {10, 10000}],
			Ceiling[1.5 * $targetTime / timeForRound]
		];
	];

	maxBatches = maxTrainingRounds * $batchesPerRound;

	(****************************************************************)
	(* parse specs that take intervals                              *)
	(****************************************************************)

	$callbackInfo = None; (* this is set by parseTrainingProgressFunction *)
	trainingProgressTimer = parseTrainingProgressFunction[trainingProgressFunction];
	checkpointTimer = parseCheckpointSpec[trainingProgressCheckpointing];

	(****************************************************************)
	(* initialize various dynamic-scoped 'globals'                  *)
	(****************************************************************)

	If[$collectGradEnergy = !FreeQ[returnSpec, "RMSGradientLists" | "RMSGradientSeries" | "RMSGradientEvolutionPlot"], 
		$gradEnergyHistory = Bag[];
		$gradEnergyCollector := StuffBag[$gradEnergyHistory, 
			Map[NDArrayGet /* RMSEnergy, Values[$ndgrads]]];
	];

	If[$collectWeightEnergy = !FreeQ[returnSpec, "RMSWeightLists" | "RMSWeightSeries" | "RMSWeightEvolutionPlot"], 
		$weightEnergyHistory = Bag[];
		$weightEnergyCollector := StuffBag[$weightEnergyHistory, 
			Map[NDArrayGet /* RMSEnergy, Values[$ndarrays]]];
	];

	$currentNet := TrainerCurrentNet[$trainer];

	$batchRate = $batch = $absoluteBatch = $round = 0;
	$progressFraction = $timeRemaining = $timeElapsed = 0.;
	$roundLoss = $batchLoss = $validationLoss = None;
	$lowestValidationFile = None; $lowestValidationLoss = Infinity;
	$lastValidationLossTime = None; $validationFileCount = 0;

	$lossIsPositive = True;
	$validationLossHistory = Bag[];
	$batchLossHistory = Bag[];
	$roundLossHistory = Bag[];
	$checkpointFiles = Bag[];
	
	$ndarrays = $trainer["Arrays"];
	$ndgrads = $trainer["ArrayGradients"];

	With[{$ndarrays = $ndarrays, $ndgrads = $ndgrads}, 
		(* substitute ndarrays into lazy-getting ownvalues *)
		$currentArrays := Map[NDArrayGet, $ndarrays];
		$currentArrayGradients := Map[NDArrayGet, $ndgrads];
	];

	$clock := {$absoluteBatch, $round-1, $now}; 
	$timeElapsed := $now - $startTime;
	If[$targetTime === None,
		$timeRemaining := (1.0 - $progressFraction) / (Max[$progressFraction, $MachineEpsilon] / $timeElapsed);
		rawProgressFraction := N[$absoluteBatch / maxBatches];
	,
		$timeRemaining := Max[$targetTime - $timeElapsed, 0.];
		rawProgressFraction := Clip[N[$timeElapsed / $targetTime], {0, 1}];
	];

	(****************************************************************)
	(* spin up the progress reporting window / print callback       *)
	(****************************************************************)

	$startTime = $now;

	$doPlotting = $doPrinting = False; $lastmin ^= Infinity; $lastmax ^= -Infinity;
	$progCell = None; $progressType = None; $progressVar = 0;
	If[trainingProgressReporting =!= None,
		reportingTimer = processTrainingProgressReportingSpec[],
		reportingTimer = Hold;
	];
	
	$startTime = $now;
	$plotBatchFactor = 1;
	$plotIsStale = True;
	endTime = $startTime + Replace[$targetTime, None -> Infinity];
	hardAbort = softAbort = stop = manualStop = False;
	
	(****************************************************************
    Explanation of abort flags:
	
	* stop is always True by the end. it's just used to exit the training loops.

	* manualStop says if either the Stop button was pressed or a callback returned "StopTraining".
	when this is true, a final validation round will not be performed.

	* softAbort says if a callback function said to abort training, either through
	returning "AbortTraining" or through calling Abort[]. This returns $Aborted, rather than
	produce a top-level Abort[], so its a form of 'soft abort'.

	* abortedQ says whether either a hard abort (the user did CMD+.) or a soft abort 
	(callback said to abort) occurred. If the user did CMD+., the current net is returned the first
	time. Subsequent hard aborts within a short time window produce a top-level Abort. 
	A soft abort by contrast always causes $Aborted to be returned.
	*)

	(****************************************************************)
	(* main training loop                                           *)
	(****************************************************************)

	TimeLabel["TrainingLoop"];
	(* init first data batch *)
	nextIndex = batchperm[[1]];
	nextDataBatch = trainingGenerator[nextIndex];

	{$totalTrainingTime, hardAbort} = AbsoluteTiming @ 
CheckAbort[
	Do[
		attainedRound = $round;
		partialRoundLoss = 0; (* accumulates round loss *)
		Do[ 
			batchStartTime = $now;
			If[batchStartTime > endTime, stop = True];
			
			$absoluteBatch++; 
			$progressFraction = rawProgressFraction;

			(* do a trainer update *)
			If[stop, Break[]];
			TrainerUpdate[$trainer, nextDataBatch];
			If[stop, Break[]];
			If[$batch === $batchesPerRound, 
				(* at the end of the training round, randomize the visit order of batches *)
				batchperm = RandomSample[batchperm];
			];
			(* obtain the next data batch while the net is still busy with the current batch *)
			nextIndex = batchperm[[Mod[$batch + 1, $batchesPerRound, 1]]];
			nextDataBatch = trainingGenerator @ nextIndex;
			
			(* loss measurement *)
			loss = TrainerCurrentLoss[$trainer];
			$gradEnergyCollector; $weightEnergyCollector;

			$batchLoss = Total[loss] / batchSize;
			partialRoundLoss += $batchLoss;
			If[$lossIsPositive && $batchLoss <= 0, switchToNegativeLoss[]];
			StuffBag[$batchLossHistory, $batchLoss];

			(* call the TrainingProgressFunction, if any *)
			trainingProgressTimer[$clock, $callbackInfo];

			$batchRate = UnsafeQuietCheck[
				1.0 / ($now - batchStartTime),
				$absoluteBatch / ($now - $startTime + $MachineEpsilon) (* <- see 321439 *)
			];

			(* do progress reporting (Print, Dynamic, etc) *)
			If[$doPlotting && Mod[$absoluteBatch, $plotBatchFactor] == 0, 
				$plotIsStale = True; 
				$plotBatchFactor = If[$round > 125, $batchesPerRound, Ceiling[$absoluteBatch / 125.]];
			];
			reportingTimer[$clock];

			If[stop, Break[]];
			validationTimer[$clock];
			checkpointTimer[$clock];
		,
			{$batch, $batchesPerRound}
		];

		If[stop, Break[]];

		$roundLoss = partialRoundLoss / $batchesPerRound;
		StuffBag[$roundLossHistory, $roundLoss];
	,
		{$round, maxTrainingRounds}
	];
	NDArrayWaitForAll[];
	False
, 
	(* CheckAbort *)
	If[$progCell =!= None, NotebookDelete[$progCell]; $progCell = None];
	True
];

	TimeLabel["Cleanup"];
	abortedQ = hardAbort || softAbort;

	$meanBatchesPerSecond = $absoluteBatch / $totalTrainingTime;

	If[$collectGradEnergy,
		$gradEnergyHistory = AssociationThread[Keys[$ndgrads], Transpose @ BagPart[$gradEnergyHistory, All]];
	];
	If[$collectWeightEnergy,
		$weightEnergyHistory = AssociationThread[Keys[$ndarrays], Transpose @ BagPart[$weightEnergyHistory, All]];
	];
	
	(* ensure that checkpointing and validation happens one final time (though
	it won't happen if it already happened for this batch) *)
	$round = attainedRound;
	$futureClock = {$absoluteBatch, Infinity, Infinity};

	If[!abortedQ && !manualStop,
		validationTimer[$futureClock];
		checkpointTimer[$futureClock];
	];
	
	If[$progCell =!= None, NotebookDelete[$progCell]];
	If[$doPrinting, 
		If[abortedQ, 
			Print["Training aborted."], 
			Print["Training complete."]
		]
	];

	If[abortedQ && !softAbort,
		If[$LastAbortedLine =!= $Line && SessionTime[] > $LastAbortedTime + 1.0, abortedQ = False];
		$LastAbortedLine ^= $Line;
		$LastAbortedTime ^= SessionTime[];
	];

	$lastmin = $lastmax = None; 
	$LastBatchLossHistory ^= BagPart[$batchLossHistory, All];
	$LastRoundLossHistory ^= BagPart[$roundLossHistory, All];	
	$LastValidationLossHistory = If[$doValidation, $linearValidationLossHistory, None];

	TimeLabel["CalculateResult"];
	If[abortedQ, 
		If[softAbort, $SoftAborted, $Aborted]
	,
		$finalNet = MemberQ[ToList @ returnSpec, "FinalNet"];
		Lookup[$returnSpecFunctions, returnSpec]
	]
];


PackageScope["$LastDivergentArray"]

NetTrain::arrdiv = "Training was aborted because one or more trainable parameters of the net diverged. To avoid this, ensure that the training data has been normalized to have zero mean and unit variance. You can also try specifying a lower \"InitialLearningRate\" to Method; the value used for this training session was ``. Alternatively, you can use the \"GradientClipping\" option to Method to bound the magnitude of gradients during training."

throwNetDiverge[arr_] := (
	$LastDivergentArray = arr;
	ThrowFailure[NetTrain::arrdiv, $LastInitialLearningRate]
);

timeTrainer[] := 
	First @ AbsoluteTiming[
		TrainerUpdate[$trainer, nextDataBatch];
		TrainerCurrentLoss[$trainer];
		NDArrayWaitForAll[];
	];

getBestNet[] := (
	getFinalNet[]; (* in case we are asked for the best net AND THEN final net *)
	If[$doValidation && StringQ[$lowestValidationFile], TrainerLoadCheckpoint[$trainer, $lowestValidationFile]];
	TrainerCurrentNet[$trainer]
);

getFinalNet[] := (
	If[TrueQ[$finalNet], $finalNet = TrainerCurrentNet[$trainer]];
	$finalNet
);

$returnSpecFunctions = Association[
	"FinalNet" :> getFinalNet[],
	"TrainedNet" :> getBestNet[], 
	"BatchLossList" :> $LastBatchLossHistory,
	"RoundLossList" :> $LastRoundLossHistory,
	"ValidationLossList" :> $LastValidationLossHistory,
	"ValidationLossSeries" :> 
		If[$LastValidationLossHistory === None, None, TimeSeries[$LastValidationLossHistory]],
	"LossEvolutionPlot" :> makeFinalLossPlot[],
	"RMSGradientLists" :> $gradEnergyHistory,
	"RMSGradientSeries" :> Map[TimeSeries, $gradEnergyHistory],
	"RMSGradientEvolutionPlot" :> energyPlot[$gradEnergyHistory],
	"RMSWeightLists" :> $weightEnergyHistory,
	"RMSWeightSeries" :> Map[TimeSeries, $weightEnergyHistory],
	"RMSWeightEvolutionPlot" :> energyPlot[$weightEnergyHistory],
	"LastBatchLoss" :> $batchLoss,
	"LastRoundLoss" :> $roundLoss,
	"LastValidationLoss" :> $validationLoss,
	"LowestValidationLoss" :> $lowestValidationLoss,
	"TotalTrainingTime" :> $totalTrainingTime,
	"MeanBatchesPerSecond" :> $meanBatchesPerSecond,
	"MeanInputsPerSecond" :> $meanBatchesPerSecond * batchSize,
	"FinalLearningRate" :> MXNetLink`$LastGlobalLearningRate,
	"InitialLearningRate" :> MXNetLink`$LastInitialLearningRate,
	"BatchSize" :> batchSize,
	"LossNet" :> NetAttachLoss[$net, $lossSpec],
    "CheckpointFiles" :> BagPart[$checkpointFiles, All],
	"$Trainer" :> $trainer,
	"$TrainingGenerator" :> {toClosure @ trainingGenerator, $batchesPerRound},
	"$ValidationGenerator" :> {toClosure @ $validationGenerator, Ceiling[$validationLength / batchSize]}
];

toClosure[e_] := Block[{varsets = {}, e2, newvar},
	e2 = ReplaceAll[e, t_Symbol ? System`Private`HasImmediateValueQ :> RuleCondition[
		newvar = Unique[SymbolName[Unevaluated[t]]]; SetAttributes[newvar, Temporary];
		AppendTo[varsets, Hold[Set][newvar, t]];
		newvar
	]];
	ReleaseHold[varsets];
	e2
];

energyPlot[e_] := ListLogLogPlot[
	condense @ Values @ e, PlotLabels -> Map[Text, Keys[e]],
	PlotRange -> All, Joined -> True,
	ImageSize -> 450
]

condense[table_List] := Scope[
	len = Length[First[table]];
	If[len < 500, table,
		factor = Floor[len / 200];
		BlockMap[Mean, #, factor]& /@ table
	]
];

$validReturnSpec = {Repeated @ Apply[Alternatives, Keys[$returnSpecFunctions]]};

makeFinalLossPlot[] := Scope[
	$LossPlotImageSize = {500, 200};
	vloss = Replace[$LastValidationLossHistory, None -> {}];
	If[vloss =!= {} && $lossIsPositive, vloss = MapAt[Log10, $LastValidationLossHistory, {All, 2}]];
	(* because LossEvolutionPlot always does log of vlossif $lossIsPositive is True, so we have to compensate *)
	plot = If[$round > 250,
		LossEvolutionPlot[$LastRoundLossHistory, -$batchesPerRound, vloss, Ceiling[$round / 500.]],
		LossEvolutionPlot[$LastBatchLossHistory, $batchesPerRound, vloss]
	];	
	If[vloss =!= {}, 
		plot = Legended[plot, LineLegend[{$ValidationLossLineStyle, $LossLineStyle}, {"validation", "training"}]]
	];
	plot
];

SetHoldFirst[CatchEncoderFailure];
CatchEncoderFailure[body_] := 
	Catch[body, EncodeFail, $Failed&];

(* aux functions *)

$defaultProgType = If[!$Notebooks, "Print", If[!$CloudOrPlayer, "Panel", None]];
$allowedProgType := $allowedProgType = EnumT @ Flatten @ {If[$Notebooks, {"Panel", "ProgressIndicator"}, Nothing], "Print", Automatic, None};

processTrainingProgressReportingSpec[] := Block[
	{allowedProgInf, progRepSpec, progRepOpts, reportingTimer, secint, pffunc, method},

	progRepSpec = ToList[trainingProgressReporting];

	method = First @ progRepSpec;
	progRepSpec = Rest @ progRepSpec;
	progRepOpts = Sequence @@ Append[progRepSpec, $defTimerUnit -> 3 (* seconds *)];

	SetAutomatic[method, $defaultProgType];
	Switch[method,
	"Panel" /; $Notebooks,
		reportingTimer = makeTimer[TrainingProgressReporting, updateDynamicProgress, progRepOpts, 
			"Interval" -> {0.4, "Seconds"}, "MinimumInterval" -> 0.1];
		$lastLossEvolutionPlot = ToBoxes @ LossEvolutionPlot[{}, 1, {}, 1];
		secint = reportingTimer[[1, 2, 1, 1]][[3]];
		$doPlotting = True;
		$progCell = PrintTemporary @ makeTrainingBox[secint],
	"ProgressIndicator" /; $Notebooks,
		reportingTimer = makeTimer[TrainingProgressReporting, Function[$progressVar = $progressFraction], progRepOpts,
			"Interval" -> {0.1, "Seconds"}, "MinimumInterval" -> 0];
		$progCell = PrintTemporary @ makeSimpleProgressBox[],
	"Print",
		$doPrinting = True; $firstPrint = True;
		$colSizes = {3, colSize[maxTrainingRounds], colSize[$batchesPerRound], 10, 10, 10, 10, 10, 10};
		reportingTimer = makeTimer[TrainingProgressReporting, progressPrintFunction, progRepOpts,  
			"Interval" -> {2, "Seconds"}, "MinimumInterval" -> 0.2];
		Print["Starting training."],
	_Function /; $Notebooks, 
		setupCallbackInfo[];
		pffunc = customProgressFunction @ optimizePF @ method;
		(* ensure we don't prevent PerformanceGoal being set manually *)
		If[FreeQ[If[SymbolQ[method], DownValues[method], method], PerformanceGoal], 
			pffunc = withPerfGoal[pffunc]
		];
		$customProg = Spacer[{5,5}]; $customProgError = None;
		$progCell = PrintTemporary @ Dynamic[$customProg, TrackedSymbols :> {$customProg}, ShrinkingDelay -> 5.0];
		reportingTimer = makeTimer[TrainingProgressReporting, pffunc, progRepOpts,  
			"Interval" -> {1, "Rounds"}, "MinimumInterval" -> 0.05],
	None, 
		reportingTimer = Hold,
	_,
		ThrowFailure["netinvopt", TrainingProgressReporting, TypeString[$allowedProgType]];
	];

	reportingTimer
];

withPerfGoal[f_][] := Block[{$PerformanceGoal = $ProgressReportingFunctionPerformanceGoal}, f[]];

PackageExport["$ProgressReportingFunctionPerformanceGoal"]
$ProgressReportingFunctionPerformanceGoal = "Speed";

PackageExport["$ProgressReportingFunctionChecking"]
$ProgressReportingFunctionChecking = True;

$pfchecker := If[TrueQ[$ProgressReportingFunctionChecking], EvaluateChecked, Identity];
customProgressFunction[func_][] := Set[$customProg, Deploy @ $pfchecker @ FormatResult @ func @ $callbackInfo];


PackageExport["FormatResult"]

FormatResult[e_] := fmtRes[e];

fmtRes[l_List ? MachineArrayQ] := 
	Style[MatrixForm[
		l /. r_Real | r_Rational :> SciString[r, 6], 
		TableAlignments -> Right,
		TableDirections -> If[ArrayDepth[l] === 1, Row, Automatic]
	], FontSize -> 10];

$fmtd = 0;
fmtRes[l_List] := Block[{$fmtd = $fmtd + 1}, Which[
	VectorQ[l], If[$fmtd == 1, Multicolumn[fmtRes /@ l], Row[fmtRes /@ l, " "]],
	MatrixQ[l], Grid[Map[fmtRes, l, 2]],
	True, Column[fmtRes /@ l, Alignment -> Left]
]];

fmtRes[a_Association] := Grid[
	KeyValueMap[{Style[#1, "Label", 10], fmtRes[#2]}&, a], 
	Alignment -> Left, Dividers -> {False, Center}, 
	FrameStyle -> LightGray
];

fmtRes[e_] := e;

NetTrain::netdiverge = "Parameters of net diverged during training, try specifying a lower InitialLearningRate."

validationFunction[] := (
	If[$doPrinting && $lastValidationLossTime > 5.0, 
		Print["Computing validation loss."];
	];
	$validationLoss = "computing...";
	$lastValidationLossTime = First @ AbsoluteTiming[
		$validationLoss = Total @ TrainerComputeValidationLoss[$trainer, $validationGenerator, $validationLength];
	];
	If[$scaledValidation,
		(* if we are doing Scaled, the nextDataBatch might have been corrupted by reusing the generator,
		so we have to regenerate it here *)
		nextDataBatch = trainingGenerator[nextIndex];
	];
	If[$lossIsPositive && $validationLoss <= 0, switchToNegativeLoss[]];
	StuffBag[$validationLossHistory, {$absoluteBatch, If[$lossIsPositive, Log10[$validationLoss], $validationLoss]}];
	If[$validationLoss < $lowestValidationLoss, 
		Block[{newFile, oldFile},
			newFile = FileNameJoin[{$validationDir, IntegerString[$validationFileCount++]}];
			oldFile = $lowestValidationFile;
			TrainerSaveCheckpoint[$trainer, newFile];
			$lowestValidationLoss = $validationLoss;
			$lowestValidationFile = newFile;
			If[StringQ[oldFile], Quiet @ DeleteFile[oldFile]];
		]
	];
);

$linearValidationLossHistory := Scope[
	history = BagPart[$validationLossHistory, All];
	If[$lossIsPositive && history =!= {},
		history = MapAt[Power[10, #]&, history, {All, 2}];
	];
	ToPackedArray @ history
];

switchToNegativeLoss[] := Scope[
	If[$doValidation,
		$validationLossHistory ^= Bag[$linearValidationLossHistory];
	];
	$lossIsPositive ^= False;
];

updateDynamicProgress[] := 
	If[$plotIsStale, updateLossPlotBoxes[]; $plotIsStale = False];

updateLossPlotBoxes[] := 
	If[$round > 125,
		$lastLossEvolutionPlot = ToBoxes @ LossEvolutionPlot[
			BagPart[$roundLossHistory, All], 
			-$batchesPerRound,
			BagPart[$validationLossHistory, All],
			Ceiling[$round / 125.]
		];
	,
		$lastLossEvolutionPlot = ToBoxes @ LossEvolutionPlot[
			BagPart[$batchLossHistory, All], 
			$batchesPerRound,
			BagPart[$validationLossHistory, All],
			$plotBatchFactor
		];
	];

dropValCol[e_] := If[$doValidation, e, Delete[e, 6]];

printRow[items___] := Print @ RightAlignedRowString[
	dropValCol[$colSizes],
	dropValCol[{items}] /. {i_Integer :> IntegerString[i], r_Real :> SciString[r], None -> "---"}
];

colSize[n_] := Max[2+base10Digits[n], 6];

progressPrintFunction[] := Scope[
	elapsed = $timeElapsed;
	If[elapsed < 0.2, Return[]];
	If[$firstPrint, 
		printRow["%", "round", "batch", "round", "batch", "test", "inputs", "time", "time"];
		printRow["", {"/", maxTrainingRounds}, {"/", $batchesPerRound}, "loss", "loss", "loss", "/second", "elapsed", "left"];
		$firstPrint ^= False; 
	];
	printRow[
		Round[$progressFraction * 100], 
		$round, $batch, 
		$roundLoss, $batchLoss, $validationLoss,
		Round[$batchRate * batchSize],
		TimeString[elapsed],
		TimeString[$timeRemaining]
	];
];

makeTrainingBox[minint_] := 
	TrainingBox[{
		None :> Item[ProgressIndicator[$progressFraction], Alignment -> Center],
		"progress" :> Row[{Round[$progressFraction * 100], "%"}],
		"round" :> Row[{$round, " / ", maxTrainingRounds}],
		If[$batchesPerRound > 1 && maxTrainingRounds < 100, 
			"batch" :> Row[{$batch, " / ", $batchesPerRound}], 
			Nothing
		],
		"inputs/second" :> Round[$batchRate * batchSize],
		"time elapsed" :> TimeString[$timeElapsed], 
		"time remaining" :> If[$timeElapsed < 0.1, "", TimeString[$timeRemaining]],
		"batch size" -> batchSize,
		"batch loss" :> ScientificForm[$batchLoss, 3],
		"round loss" :> ScientificForm[$roundLoss, 3],
		If[$doValidation, "validation loss" :> ScientificForm[$validationLoss, 3], Nothing],
		None :> RawBoxes[$lastLossEvolutionPlot],
		None -> NiceButton["Stop", manualStop = stop = True]
		},
		"Training Progress",
		minint
	];

makeSimpleProgressBox[] := Dynamic[
	Row[{
		ProgressIndicator[$progressVar],
		"   ", Round[$progressVar * 100], "%"
		"   ",
		If[$timeElapsed < 0.1, "", Sequence @@ {TimeString[Round[$timeRemaining, .1]], " remaining"}]
	}], 
	BaseStyle -> {FontFamily -> CurrentValue["PanelFontFamily"]}, 
	TrackedSymbols :> {$progressVar}
];


(* argument parsing *)

GuessDataLength[data_] := Switch[data,
	{} | <||>, 		ThrowFailure["nodata"],
	_List, 			Length[data],
	_Association, 	Length[First[data]],
	_List -> _List, Length[First[data]],
	_Dataset, 		GuessDataLength[Normal[data]],
	_, 				Infinity
];

PackageScope["ParseTrainingData"]

ParseTrainingData[data_, inputs_, batchSize_, factory_] := Timed @ Scope[
	Which[
		data === {} || data === <||>,
			NetTrain::nodata = "No training data provided.";
			ThrowFailure["nodata"]
		,
		AssociationQ[data],
			If[!VectorQ[Values[data], ListQ],
				NetTrain::invtdata = "Training data should be an association of lists, or a rule from input to output examples.";
				ThrowFailure["invtdata"]
			]
		,
		MatchQ[data, _List -> _List],
			{ikey, okey} = getRuleKeys[inputs];
			data = <|ikey -> First[data], okey -> Last[data]|>;
		,
		Head[data] === Dataset,
			If[!MatchQ[Dataset`GetType[data], TypeSystem`Vector[_TypeSystem`Struct, _]],
				NetTrain::invdataset = "Datasets provided to NetTrain must consist of a list of associations with fixed keys.";
				ThrowFailure["invdataset"]
			];
			data = AssociationTranspose @ Normal[data]
		,
		AssociationVectorQ[data],
			data = AssociationTranspose @ data
		,
		factory === ConstructJITNet,
			NetTrain::jitgen = "Generator function cannot be used when the net to be trained has not been fully specified.";
			Message[NetTrain::jitgen];
			ReturnFailed[];
		,
		MatchQ[data, File[path_String /; FileType[path] === File]],
			data = OpenHDF5TrainingData[First @ data];
			AppendTo[$CleanupQueue, Hold[CloseHDF5TrainingData][data]];
			length = data[[2]];
		,
		Compose[System`Private`MightEvaluateWhenAppliedQ, data], (* custom generator *)
			If[ContainsVarSequenceQ[inputs], 
				NetTrain::novseqgen = "Generator function `` cannot currently be used for nets that take variable-length sequences; try fixing the length of the input sequence using a shape specification.";
				ThrowFailure["novseqgen", Shallow[data]];
			];
			$round ^= 1;
			$generatorInput ^= <|"BatchSize" -> batchSize, "Round" :> $round|>;
			wrapper = checkAndWrapGeneratorOutput[
				data,
				data[$generatorInput], 
				inputs, batchSize
			];
			length = batchSize;
			data = CustomGenerator[data /* wrapper, $generatorInput];
		,
		True,
			ThrowFailure["invtdata"]
	];

	(* this kicks in for HDF5 files and user-provided generators *)
	If[!AssociationQ[data], Goto[SkipChecks]];

	lengths = Map[Length, data];
	If[!Apply[SameQ, lengths], 
		NetTrain::invinlen = "Inconsistent numbers of examples provided to ports: lengths were ``.";
		ThrowFailure["invinlen", lengths]
	];

	length = First[lengths];

	NetTrain::invinslot = "No slot named `` is present.";
	NetTrain::missinslot = "Specification for slot `` is missing.";
	TestSetsEqual[
		Keys[inputs], Keys[data], 
		ThrowFailure["missinslot", #]&,
		Function[data = KeyTake[data, Keys[inputs]]]
	];

	Map[ (* <- 322488 *)
		If[!PackedArrayQ[#] && System`Private`CouldContainQ[#, Missing] && MemberQ[#, _Missing],
			NetTrain::contmiss = "NetTrain does not currently support data that contains missing values.";
			ThrowFailure["contmiss"]
		]&,
		data
	];

	Label[SkipChecks];

	If[batchSize > length && factory =!= ConstructValidationGenerator, batchSize = length];
	data = factory[data, inputs, batchSize];

	{data, length}
];


(* we test the very first batch for basic spec, to make sure it produces
a list of rules or an association with the right ports, and that the
length is correct. This is mainly done here for specificity of the
error message, and also to prevent you using a generator with e.g.
ValidationSet and have that fail when you've already been training a while.

Subsequently, ConstructXXXGenerator will create a compiled predicate
that checks the association's actual values. This will give a generic
error message when things aren't correct.
*)

NetTrain::invgenout = "Output of generator function `` was incorrect: ``."

genfpanic[args__] := genfpanic[TextString @ StringForm[args]];
genfpanic[arg_] := (Message[NetTrain::invgenout, Shallow[$genf], arg]; ThrowRawFailure[$Failed]);

$invgensize = "generator did not return data with length equal to the requested BatchSize ``";

checkAndWrapGeneratorOutput[genf_, data_, inputs_, batchSize_] := Scope[
	$genf = genf;
	If[AssociationQ[data], 
		TestSetsEqual[
			Keys[inputs], Keys[data], 
			genfpanic["no slot named `` is present", #]&,
			genfpanic["specification for slot `` is missing", #]&
		];
		lens = Length /@ Values[data];
		If[!AllSameAs[lens, batchSize],
			genfpanic[$invgensize, batchSize]
		];
		Return[Identity];
	];

	If[MatchQ[data, {__Rule}],
		{ikey, okey} = getRuleKeys[inputs];
		If[Length[data] =!= batchSize,
			genfpanic[$invgensize, batchSize]
		];
		Return @ With[{keys = {ikey, okey}},
			Function[input,
				If[!MatchQ[input, {__Rule}],
					genfpanic["generator did not return a list of rules"],
					AssociationThread[keys, KeysValues[input]]
				]
			]
		]
	];
	(* TODO: do detailed checks here on the actual data, so we can
	we can fail upfront if the payload is wrong, with a good error
	message *)
	genfpanic["generator did not return an association or list of rules"];
];

getRuleKeys[inputs_] := Scope[
	If[Length[inputs] != 2, 
		NetTrain::invsimplein = "Given training data specification can only be used when net has one input and one output, or two inputs and an explicit loss function.";
		ThrowFailure["invsimplein"]
	];
	{ikey, okey} = Keys[inputs];
	Which[
		ikey === "Input", Null,
		okey === "Input", {ikey, okey} = {okey, ikey},
		True, ThrowFailure["invsimplein"]
	];
	{ikey, okey}
];

(* parsing of TrainingProgressFunction option *)

parseTrainingProgressFunction[None] := Hold;
parseTrainingProgressFunction[f_] := Scope[
	setupCallbackInfo[];
	Replace[parsePF[f], l_List :> ApplyThrough[l]] /* handleStop
];

handleStop[e_List] := Scan[handleStop, e];
handleStop["StopTraining"] := (manualStop = stop = True;)
handleStop["AbortTraining"] := (softAbort = stop = True;)
handleStop[_] := Null;

parsePF[list_List] := Map[parsePF, list];
parsePF[{f_, opts__Rule}] := makeTimer[TrainingProgressFunction, optimizePF @ f, opts, "MinimumInterval" -> 0];
parsePF[f_] := makeTimer[TrainingProgressFunction, optimizePF @ f, "MinimumInterval" -> 0];

(* disabled, because getting one array out of a big net is more likely than looking up the same array multiple times. *)
(*$expensivePFKeys = {"Net", "Weights", "Gradients"};*)
$expensivePFKeys = {"Net"};

(* TODO: optimize looking up a *specific* array or array gradient *)
optimizePF[f_] := ModuleScope[
	defs = If[SymbolQ[f], DownValues[f], {f, Cases[f, sym]}];
	keys = Select[$expensivePFKeys, !FreeQ[defs, #]&];
	func = If[keys === {}, f, EvalMapAt[Identity, List /@ keys] /* f];
	Function[inarg, CheckAbort[func[inarg], softAbort ^= stop ^= True; $Aborted]]
];

setupCallbackInfo[] := If[$callbackInfo === None, Set[
	$callbackInfo,
	Association[
		"Net" :> $currentNet,
		"Weights" :> $currentArrays, "Gradients" :> $currentArrayGradients,
		"BatchLoss" :> $batchLoss, "RoundLoss" :> $roundLoss,
		"Batch" :> $batch, "Round" :> $round, "AbsoluteBatch" :> $absoluteBatch,
		"MaxRounds" -> maxTrainingRounds, "MaxBatches" -> maxBatches, 
		"BatchSize" -> batchSize, "BatchesPerRound" -> $batchesPerRound,
		"BatchesPerSecond" :> $batchRate, "InputsPerSecond" :> $batchRate * batchSize,
		"ProgressFraction" :> $progressFraction,
		"TimeRemaining" :> $timeRemaining, "TimeElapsed" :> $timeElapsed,
		"CheckpointFiles" :> BagPart[$checkpointFiles, All],
		"ValidationLoss" :> $validationLoss, "LowestValidationLoss" :> $lowestValidationLoss
	]
]];

(* parsing of Checkpoint option  *)

parseCheckpointSpec[None] := Hold;

parseCheckpointSpec[spec_] := Replace[parseCS[spec], l_List :> ApplyThrough[l]];

NetTrain::invchkfile = "The value of the \"File\" parameter to TrainingProgressCheckpointing should specify a file in a directory that already exists."
parseCS[{"File", path_String, opts___Rule}] := Scope[
	path = ExpandFileName[path];
	If[!DirectoryQ[DirectoryName[path]], ThrowFailure["invchkfile"]];
	makeTimer[TrainingProgressCheckpointing, checkpointFile[path], FilterOptions[opts]]
];
	
base10Digits[n_] := Floor[1 + Log[10, n]];
NetTrain::invchkdir = "The value of the \"Directory\" parameter to TrainingProgressCheckpointing should be a directory that already exists."
parseCS[{"Directory", path_String, opts___Rule}] := Scope[
	If[!DirectoryQ[path] && (!DirectoryQ[FileNameDrop[path]] || FailureQ[CreateDirectory[path]]),
		ThrowFailure["invchkdir"]
	];
	func = checkpointDir[
		path,
		dateStringForPath[] <> "_" <> IntegerString[$TrainingCounter++] <> "_",
		base10Digits[maxTrainingRounds], 
		base10Digits[maxBatches]
	];
	makeTimer[TrainingProgressCheckpointing, func, FilterOptions[opts]]
];

dateStringForPath[] := 
	If[$OperatingSystem === "Windows", StringReplace[":" -> "-"], Identity] @ DateString["ISODateTime"];

parseCS[list_List /; VectorQ[list, ListQ]] :=
	Map[parseCS, list];

NetTrain::invchkspec = "The value of the TrainingProgressCheckpointing option should be a spec of the form {\"File\"|\"Directory\", \"path\"}."
parseCS[spec_] := ThrowFailure["invchkspec", spec];


checkpointFile[path_][] := WLNetExport[path, $currentNet];

checkpointDir[path_, startString_, roundBase_, batchBase_][] := Scope[
	filename = StringJoin[
		startString,
		IntegerString[$round, 10, roundBase], "_", 
		IntegerString[$absoluteBatch, 10, batchBase], "_", lossString[$roundLoss],
		If[$doValidation, {"_", lossString[$validationLoss]}, {}], ".wlnet"
	];
	filepath = FileNameJoin[{path, filename}];
	StuffBag[$checkpointFiles, filepath];
	WLNetExport[filepath, $currentNet];
];

lossString[_] := "none";
lossString[r_Real] := SciString[r];

Clear[makeTimer];
Options[makeTimer] = {"Interval" -> 1, "MinimumInterval" -> 1.0, $defTimerUnit -> 2};

NetTrain::invsubopts = "`` is not a valid suboption to ``."
makeTimer[optsym_, func_, opts:OptionsPattern[]] := Scope[
	{int, minint, defunit} = UnsafeQuietCheck[
		OptionValue[{"Interval", "MinimumInterval", $defTimerUnit}],
		TestSetsEqual[
			Keys @ Flatten @ {opts}, {"Interval", "MinimumInterval"}, 
			ThrowFailure["invsubopts", #1, optsym]&
		]
	];
	int = parseInterval[int, defunit];
	minint = parseInterval[minint, 3];
	int[[3]] = Max[int[[3]], minint[[3]]];
	CreateTimer[func, int]
];

NetTrain::invtint = "The value of the \"Interval\" suboption should be a positive quantity with a unit of \"Rounds\", \"Batches\", \"Seconds\", \"Minutes\", or \"Hours\".";

parseInterval[
	HoldPattern @ Quantity[n_ ? Positive, unit_String | IndependentUnit[unit_String]] |
	{n_ ? Positive, unit_String}, _] := 
	Match[
		unit,
		"Batches"|"Batch" /; IntegerQ[n] :> {n, 0, 0},
		"Rounds"|"Round"|"Revolutions" :> If[
			IntegerQ[n], {1, n, 0},
			{Ceiling[n * $batchesPerRound], 0, 0}
		],
		"Seconds"|"Second" :> {1, 0, N[n]},
		"Minutes"|"Minute" :> {1, 0, 60 * N[n]},
		"Hours"|"Hour" :> {1, 0, 60 * 60 * N[n]},
		ThrowFailure["invtint"]
	];

parseInterval[n_ ? NonNegative, defaultInd_] := 
	ReplacePart[{0, 0, 0}, defaultInd -> N[n]]

parseInterval[_, _] := ThrowFailure["invtint"];

$none = Style["\[Dash]", Gray];


PackageExport["$LastBatchLossHistory"]
PackageExport["$LastRoundLossHistory"]
PackageExport["$LastValidationLossHistory"]

$LastBatchLossHistory = None;
$LastRoundLossHistory = None;
$LastValidationLossHistory = None;


PackageScope["TrainingBox"]

keystyle[x_] := Style[x, GrayLevel[0.4]];	

$TrainingBoxColor = RGBColor[0.9802, 0.9802, 0.9802];

TrainingBox[data_, title_, uinterval_] := Scope[
	grid = Dynamic[
		Grid[
			If[#1 === None, 
					{#2, SpanFromLeft},
					{keystyle[#1], #2}
			]& @@@ data,
			Dividers -> {
				{False, {Opacity[0.15]}, False}, 
				{}
			},
			ColumnWidths -> {Automatic, 15},
			ColumnAlignments -> {Right, Left},
			ColumnSpacings -> {1.6, 2.5},
			RowSpacings -> 1.5, RowMinHeight -> 1.2
		],
		TrackedSymbols :> {}, UpdateInterval -> uinterval
	];
	titleItem = Item[
		Framed[
			Style[title, Bold, 12, "SuggestionsBarText"],
			FrameMargins -> {{10,10},{-5,5}},
			FrameStyle -> None
		],
		ItemSize -> {Automatic,1},
		Alignment -> {Left, Bottom},
		FrameStyle -> Opacity[0.1],
		Background -> Darker[$TrainingBoxColor, 0.05],
		Frame -> {{False,False},{True, False}}
	];
	gridItem = Item[
		Framed[grid, FrameMargins -> {{10,10},{10,5}}, FrameStyle -> None],
		BaseStyle -> {FontWeight -> "Light", FontFamily -> CurrentValue["PanelFontFamily"], 
			NumberMarks -> False, 
			ScriptBaselineShifts -> {0, 0.5},
			ScriptMinSize -> 8, ScriptSizeMultipliers -> 0.5
		},
		Alignment -> Left	
	];
	Deploy[
		Style[Framed[
			Column[{
				titleItem,
				gridItem
				},
				ColumnWidths -> Automatic,
				ColumnAlignments -> Left,
				RowLines -> False,
				RowSpacings -> {3,1},
				StripOnInput -> True
			],
			Background -> $TrainingBoxColor,
			FrameMargins -> {{0,0},{0,0}},
			FrameStyle -> LightGray,
			RoundingRadius -> 5
		], LineBreakWithin -> False]
	]
];


PackageExport["LossEvolutionPlot"]

NNSetUsage @ "
LossEvolutionPlot[losses$, batchsPerRound$] plots a loss history.
* batchesPerRound$ will be used to switch the labelling of the x axis when exceeded.
* if batchesPerRound$ is negative, losses will be treaded as round losses instead. 
* this function isn't really intended for external use, its main patron is NetTrain 
"

LossEvolutionPlot[e_ /; Length[e] <= 2, b_ /; b > 0, _, _] := Spacer[$LossPlotImageSize];

LossEvolutionPlot[tloss_, batchesPerRound_, vloss_:{}] := Scope[
	len = Length[tloss]; batch = Ceiling[len / (First[$LossPlotImageSize] / 2.)];
	len = Floor[len-1, batch]+1;
	LossEvolutionPlot[Take[tloss, len], batchesPerRound, vloss, batch]
];

$lastmin = $lastmax = None;
LossEvolutionPlot[trainingLoss_, batchesPerRound_, validationLoss_, batchFactor_] := Scope[
	n = Length[trainingLoss];
	Which[
		batchesPerRound < 0,
			dx = 1; xn = n; label = "rounds"; 
			If[validationLoss =!= {},
				batchesPerRound = Abs[batchesPerRound];
				validationLoss = MapAt[# / batchesPerRound&, validationLoss, {All, 1}];
			],
		n > batchesPerRound, 
			dx = batchesPerRound; xn = Ceiling[n / dx]; label = "rounds",
		True,
			dx = 1; xn = n; label = "batches"
	];
	dividers = dx * makeTimeTicks[xn];
	coords = makeLinePlot[trainingLoss, batchFactor, $lossIsPositive];
	{min, max} = MinMax[coords[[All, 2]]];
	If[$lastmin =!= None, $lastmin ^= min = Min[$lastmin, min]; $lastmax ^= max = Max[$lastmax, max]];
	If[$lossIsPositive, 
		(* lastmin stops the floor from jumping around *)
		min = Floor[min, .5]; max = Ceiling[max, .25];
		{lossLabels, lossGridLines} = makeLogLossTicks[Floor[min], Ceiling[max]];
	,
		{lossLabels, lossGridLines, min, max} = makeLinearLossTicks[min, max];
	];
	xscale = First[$LossPlotImageSize];
	Graphics[
		{
			AbsoluteThickness[1.25], 
			Text[Round[#/dx], Offset[{-2,0}, {#, max + (max - min) * 0.05}], {1.,1.}]& /@ dividers,
			lossLabels,
			plotPoints[coords, $LossPointStyle, $LossLineStyle, xscale],
			plotPoints[validationLoss, $ValidationLossPointStyle, $ValidationLossLineStyle, xscale]
		},
		ImageSize -> $LossPlotImageSize,
		Frame -> True, Axes -> None,
		AspectRatio -> Full, 
		PlotLabel -> label,
		ImagePadding -> 1,
		BaseStyle -> {FontFamily -> "Verdana", FontSize -> 8, FontColor -> Gray, ScriptSizeMultipliers -> 0.2, ScriptMinSize -> 6},
		PlotRange -> {{0, n-1}, {min, max}},
		GridLines -> {dividers, lossGridLines},
		PlotRangePadding -> {0, Scaled[0.05]},
		PlotRangeClipping -> True,
		Background -> White,
		FrameStyle -> Gray,
		FrameTicks -> None
	]
];

plotPoints[points_, pointStyle_, lineStyle_, xscale_] := Block[{len = Length[points]}, {
	If[0 < len && lineStyle =!= None, {lineStyle, Line @ If[len > 2*xscale, points[[;; ;; Ceiling[len/(2*xscale)]]], points]}, {}],
	If[0 < len < xscale/4 && pointStyle =!= None, {pointStyle, Point[points]}, {}]
}];

PackageExport["$ValidationLossPointStyle"]
PackageExport["$ValidationLossLineStyle"]
PackageExport["$LossPointStyle"]
PackageExport["$LossLineStyle"]

$ValidationLossPointStyle = Hue[.59, .9, .6];
$ValidationLossLineStyle = Hue[.59, .9, .9];
$LossPointStyle = Hue[0.083, 1., 0.6]; 
$LossLineStyle = Hue[0.083, 1., 1.];

$LossPlotImageSize = {250,100};

makeTimeTicks[n_ /; n <= 100] := 
	Which[
		n > 50, {20,40,60,80,100},
		n > 30, {10,20,30,40,50},
		n > 20, {5,10,15,20,25},
		n > 10, Range[2,n,2],
		True, Range[1,n]
	];

makeTimeTicks[n_] := Scope[
	{man,exp} = MantissaExponent[N[n]];
	Which[
		man > 0.5, {0.2, 0.4, 0.6, 0.8, 1.0},
		man > 0.3, {0.1, 0.2, 0.3, 0.4, 0.5},
		man > 0.15, {0.05, 0.1, 0.15, 0.2, 0.25, 0.3},
		man == 0.10, {0.02, 0.04, 0.06, 0.08, 0.1},
		True, {0.025, 0.05, 0.075, 0.100, 0.125, 0.150}
	] * Power[10, exp]
];

Clear[makeLinearLossTicks];
makeLinearLossTicks[min_, max_] := Scope[
	{major, minor} = N@FindDivisions[{min, max}, {2,5}];
	labels = Text[ScientificForm[#], Offset[{3,0}, {0, #}], {-1,-1.}]& /@ major;
	{min, max} = MinMax[major];
	{labels, Join[{#, LightGray}& /@ Flatten[minor], {#, GrayLevel[0.3]}& /@ major], min, max}
];


Clear[makeLogLossTicks];
makeLogLossTicks[min_, max_] := makeLogLossTicks[min, max] = Scope[
	major = Range[N[min], max-1]; 
	minor = Which[
		max > min + 4, {}, 
		max > min + 2, makeMinorLogTicks[min, max, 2], 
		True, makeMinorLogTicks[min, max, 1]
	];
	labels = Text[Superscript[10, TextString[Round@#]], Offset[{3,0}, {0, #}], {-1,-1.}]& /@ major;
	{labels, Join[{#, GrayLevel[0.3]}& /@ major, {#, LightGray}& /@ minor]}
];

makeMinorLogTicks[min_, max_, sub_] := 
	Log[10, Flatten @ Table[Range[2*(10.^i),10^(i+0.999),sub*10^i],{i,min,max-1}]];

makeLinePlot = Compile[{{loss, _Real, 1}, {b, _Integer}, {isPos, True|False}}, 
	If[b === 1,
		Transpose[{Range[0,Length[loss]-1], If[isPos, Log10[loss], loss]}],
		Module[{n = Length[loss], val},
		Table[
			val = Mean[loss[[i ;; Min[n, i+b-1]]]];
			If[isPos, val = Log10[val]];
			{i-1 + (b/2), val}, 
			{i, 1, Max[Length[loss]-1, 1], b}
		]]
	]
];



PackageScope["JITInferTrainNet"]
PackageScope["$LastJITFailure"]

$pleaseSpecMsg = " Please specify shapes for the input and/or output ports of the net before calling NetTrain."

NetTrain::nettinf1 = "Could not automatically infer types for input and outputs of partially specified net ``." <> $pleaseSpecMsg;
JITInferTrainNet[NetP[head, assoc, meta], lossSpec_, data_] := Timed @ CatchFailureAsMessage @ Scope[
	inputs = Inputs[assoc]; outputs = Outputs[assoc];
	unreflectedPorts = Flatten @ findUnreflectedPorts @ ToList[lossSpec];
	KeyDropFrom[outputs, unreflectedPorts];
	types = Join[inputs, outputs]; 
	$port = None; $assoc = assoc; 
	$softmaxHint = !FreeQ[assoc, <|"Type" -> "Softmax", ___|>];
	ParseTrainingData[data, types, 1, inferNetTypes];
	net2 = CatchFailure[General, ConstructWithInference[head, $assoc, meta]];
	If[FailureQ[net2], 
		$LastJITFailure ^= net2;
		ThrowFailure["nettinf1", MsgForm[assoc]];
	];
	net2
];

findUnreflectedPorts = MatchValues[
	_ := {};
	str_String := str;
	list_List := Map[%, list];
	key_ -> (Automatic | _Scaled) := %[key];
	key_ -> (net_ ? ValidNetQ) := If[Length[Inputs[net]] === 1, %[key], {}];
	key_ -> other_ -> _Scaled := %[key -> other];
];

inferNetTypes[data_Association, itypes_Association, _] := Scope[
	$preserveCoders = True;
	KeyValueScan[
		Function[{key, column},
			t = itypes[$port = key];
			$isInput = KeyExistsQ[inputs, key];
			(* this is so that ambigious cases, like pure integers, can be resolved:
			if the actual output is a vector, the integers MUST be class labels *)
			$intPossible = MatchQ[TType[t], _IndexIntegerT | AtomT] || ($softmaxHint && !$isInput && TRank[t] =!= 0);
			$rankHint = TRank[t]; $dimsHint = TDimensions[t];
			inft = getTypeOrCoder[column];
			If[!$isInput, inft = toDecoders[inft]];
			res = bridge[t, inft];
			If[$isInput,
				$assoc["Inputs", key] = res,
				$assoc["Outputs", key] = toDecoders[res]
			]
		],
		data
	]
]

toDecoders[e_] := ReplaceAll[e, enc_NetEncoder :> EncoderToDecoder[enc]];
	
General::nettinf2 = "Could not automatically find way to encode training data, which consists of ``, to be compatible with port \"``\", which expects ``." <> $pleaseSpecMsg;
bridge[VectorT[1|SizeT] | TensorT[SizeListT[1]], ScalarT] := NetEncoder["Scalar"];
bridge[TensorT[{n_, 1|SizeT}], TensorT[{SizeT | LengthVar[-1]}, RealT]] := TensorT[{n}, NetEncoder["Scalar"]];

bridge[dst_, f_Failure] := 
	If[FullySpecifiedTypeQ[dst], dst, ThrowRawFailure[f]];

(* %HACK on the assumption this will become a CrossEntropyLoss["index"] layer *)
bridge[RealTensorT, IndexIntegerT[n_]] /; $softmaxHint := TensorT[{n}]; 

bridge[dst_, src_] := Scope[
	res = UnifyTypes[dst, src]; 
	If[FailureQ[res],
		If[FullySpecifiedTypeQ[dst], 
			res = dst,
			ThrowFailure["nettinf2", TypeString[src, True], $port, TypeString[dst, True]]
		];
	];
	res
];

nominalListQ[list_List] :=
	VectorQ[list, SymbolQ] || StringVectorQ[list] && CountDistinct[list] <= Ceiling[Sqrt[Length[list]], 2];

rangeQ[list_] := Scope[u = Union[list]; (DeleteDuplicates @ Differences @ u) === {1}];

getTypeOrCoder[list_List] := CatchFailure[NetTrain, Scope @ Which[
	$intPossible && VectorQ[list, VectorQ[#, PositiveIntegerQ]&],
		TensorT[{NewLengthVar[]}, IndexIntegerT[Max[list]]],
	$intPossible && VectorQ[list, IntegerQ] && rangeQ[list],
		If[Min[list] === 1, IndexIntegerT[Max[list]], NetEncoder[{"Class", Union[list], "UnitVector"}]],
	(* ^ todo: add cases for where we know the tensor TYPE is IntegerT, as well, e.g. EmbeddingLayer but unkonwn input rank *)
	MachineArrayQ[list],
		TensorT @ Rest @ MachineArrayDimensions[list],
	VectorQ[list, MachineArrayQ] && (SameQ @@ (dims = Map[MachineArrayDimensions /* Rest, list])),
		TensorT[Prepend[First[dims], LengthVar[-1]]],
	VectorQ[list, BooleanQ],
		NetEncoder["Boolean"],
	nominalListQ[list],
		NetEncoder[{"Class", Union @ list}],
	VectorQ[list, ImageQ],
		dims= getCommonValue[ImageDimensions, list];
		cspace = getCommonValue[ImageColorSpace, list];
		Replace[$Failed :> infpanic[]] @ Quiet @ Check[
			NetEncoder[{"Image", dims, cspace}],
			$Failed
		],
	True,
		infpanic[]
]];

getTypeOrCoder[r_Real | r_Rational | r_Integer] := ScalarT;

getTypeOrCoder[_] := infpanic[];

getCommonValue[f_, vals_] := Scope[
	res = Map[f, vals];
	If[SameQ @@ res, First[res], infpanic[]]
];

General::nettinf3 = "Could not automatically infer type to use for port \"``\"." <> $pleaseSpecMsg
infpanic[] := ThrowFailure["nettinf3", $port];