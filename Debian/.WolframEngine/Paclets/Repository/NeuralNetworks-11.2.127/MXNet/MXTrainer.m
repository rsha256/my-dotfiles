Package["NeuralNetworks`"]



PackageExport["ToMXTrainer"]

Clear[ToMXTrainer];

NNSetUsage @ "
ToMXTrainer[net$, opts$$] takes a net and instantiates it into an MXTrainer.
* The resulting MXTrainer[$$] can be used to perform training, by supplying it with batches.
* The following options are supported: 
| 'BatchSize' | Automatic | batch size to use |
| 'Context' | 1 | the device code to use |
| 'Optimizer' | 'ADAM' | the optimizer to use |
| 'TotalBatches' | 4096 | the anticipated number of batches that will be trained 
* 'TotalBatches' is used for SGD optimizer. It can be provided as a Hold[sym] in case it \
needs to be changed after creation (SGD supports this).
* See the usage of MXTrainer for more information.
* If net$ contains variable sequences, a BucketedExecutor will be created, and \
the master bucket will start out at size 4 just to get access to gradient arrays etc.
"

Options[ToMXTrainer] = {
	"BatchSize" -> Automatic,
	"MaxBatchSize" -> None,
	"Context" -> 1,
	"Optimizer" -> "ADAM",
	"TotalBatches" -> 4096,
	"LearningRates" -> Automatic
};

PackageExport["$DefaultRNNBatchSize"]

$DefaultRNNBatchSize = 32;

ToMXTrainer[net_, opts___Rule] := 
	ToMXTrainer[net, Automatic, opts];

ToMXTrainer[net_NetP, lossSpec_, OptionsPattern[]] := Timed @ Scope[
	UnpackOptions[batchSize, maxBatchSize, context, optimizer, totalBatches, learningRates];
	If[!FullySpecifiedNetQ[net], Panic["NetNotInitialized"]];
	{trainNet, lossPorts, prefixQ} = iAttachLoss[net, lossSpec];
	If[!FullySpecifiedNetQ[trainNet], ThrowNotSpecifiedFailure[trainNet, "train"]];
	If[learningRates === Automatic,
		gradients = True; learningRates = 1
		,
		learningRates = toMXLearningRates[net, learningRates, prefixQ];
		gradients = Keys @ Select[learningRates, # > 0 || # < 0&];
	];
	{optMethod, optData} = ParseMethod[
		Replace[optimizer, {"LearningRate" -> "InitialLearningRate", "StochasticGradientDescent" -> "SGD"}, {0,2}], 
		$OptimizerSpec
	];
	Block[{$net = net}, optData = IMap[toPerLayerAssoc, optData]];
	bucketed = ContainsVarSequenceQ[Inputs[trainNet]];
	If[!bucketed,
		If[Inputs[trainNet] === <||> && batchSize === Automatic, batchSize = 1];
		plan = ToMXPlan[trainNet, {lossPorts, <||>, True}];
		SetAutomatic[batchSize, ChooseTrainingBatchSize[plan, context]];
		batchSize = maxBatch[batchSize, maxBatchSize];
		executor = ToMXExecutor[
			plan, batchSize, 
			"Context" -> context, "Gradients" -> gradients, "ArrayCaching" -> False
		];
		executorData = Normal[executor];
	,
		SetAutomatic[batchSize, $DefaultRNNBatchSize];
		batchSize = maxBatch[batchSize, maxBatchSize];
		executor = ToBucketedMXExecutor[trainNet, {lossPorts, context, batchSize, True, gradients}];
		dummyBucket = Values @ MakeSeqIDLens[trainNet, 4];
		dummyExecutor = GetBucketExecutor[executor, dummyBucket];
		executorData = Normal[dummyExecutor];
		(* we want the GradientArrays NOW so we force the bucketed allocator
		to allocate an initial dummy master executor *)
	];
	UnpackAssociation[executorData, 
		gradientArrays, argumentArrays, outputArrays,
		auxilliaryArrays, metadata
	];
	gradientArrays = DeleteCases[None] @ gradientArrays;
	If[gradientArrays === <||>, Return[None]];
	gradKeys = Keys[gradientArrays];
	optimizer = CreateOptimizer[
		optMethod, 
		Lookup[argumentArrays, gradKeys], 
		Values @ gradientArrays,
		gradKeys,
		learningRates, (* <- custom learning rates *)
		batchSize, 
		totalBatches, 
		optData
	];
	lossGradients = CreateOutputGradients[outputArrays, 1.0];
	NDArraySetScalar[gradientArrays, 0.0];
	arrays = Join[auxilliaryArrays, argumentArrays];
	net = ReplacePart[net, KeyValueMap[unmangleToPath[#1] -> #2&, arrays]];
	MXTrainer @ Association[
		"Executor" -> executor, 
		"Inputs" -> Inputs[trainNet],
		"OutputNames" -> Keys[outputArrays],
		"Optimizer" -> optimizer,
		"LossGradients" -> lossGradients,
		"BatchSize" -> batchSize,
		"Context" -> context,
		"MutableNet" -> net,
		"Bucketed" -> bucketed,
		"CurrentNet" -> Module[{sym}, sym = None; Hold[sym]],
		"Arrays" -> KeyMap[unmangleToList, argumentArrays],
		"ArrayGradients" -> KeyMap[unmangleToList, gradientArrays],
		"OutputArrays" -> Module[{sym}, sym = outputArrays; Hold[sym]]
	]
];

PackageScope["CreateOutputGradients"]

(* also used for NetPortGradient in NetApply *)
CreateOutputGradients[outputArrays_, gradScale_] := Scope[
	lossGradients = NDArrayCloneShape[#, 0.0]& /@ Values[outputArrays];
	NDArraySetScalar[lossGradients, gradScale];
	lossGradients
];


maxBatch[n_, None] := n;
maxBatch[n_, m_] := Min[n, m];

unmangleToList[key_] := FromNetPath @ unmangleToPath[key];
unmangleToPath[key_] := Drop[StringSplit[key, "."], If[prefixQ, 2, 0]];

toPerLayerAssoc[k_, v_] := v;
toPerLayerAssoc[k_, ArrayCasesT[spec_List, d_]] := Scope[
	values = NetArrayCases[$net, spec, k, None, d];
	mangler = If[!prefixQ, MXManglePath, ".Nodes.Net" <> MXManglePath[#]&];
	KeyMap[mangler, values]
];

(* obviously try unify these *)

toMXLearningRates[net_, spec_, prefixq_] := Scope[
	rates = NetArrayCases[net, spec, LearningRateMultipliers, 0.0, 1.0];
	mangler = If[!prefixQ, MXManglePath, ".Nodes.Net" <> MXManglePath[#]&];
	KeyMap[mangler, rates]
];

General::invarrpattspec = "Specification for `` should be a list of rules from layer specs to values.";

NetArrayCases[net_NetP, spec_, head_, noneValue_, defaultValue_] := Scope[
	$nacasesHead = head; $noneValue = noneValue; $defaultValue = defaultValue;
	If[!ListQ[spec], arrpattpanic[]];
	$net = net;
	$lrrules = Dispatch @ Append[Map[parseArraySpec, spec], _ -> $defaultValue];
	CollectTo[{$learningRates}, sowArrayCases[net]];
	Association @ $learningRates
];

(* because technically _ doesn't match the root, i guess... *)
parseArraySpec[Verbatim[_] -> rate_] := parseArraySpec[___ -> rate];

parseArraySpec[lspec_ -> rate_] := Scope[
	lspec = ToNetPathPattern[$net, lspec];
	If[FailureQ[lspec], arrpattpanic[]];
	Which[
		rate === None, rate = $noneValue,
		NumericQ[rate], rate = N[rate],
		True, arrpattpanic[]
	];
	Append[lspec, ___] -> rate
];

parseArraySpec[_] := arrpattpanic[];

arrpattpanic[] := ThrowFailure["invarrpattspec", $nacasesHead];

DeclareMethod[sowArrayCases, sowArrayCaseMultipliers, Inherited, sowOperatorArrayCases];

sowArrayCaseMultipliers[layer_] :=
	ScanFields["Arrays", sowArrayCase, layer];

sowOperatorArrayCases[assoc_] := (
	sowArrayCaseMultipliers[assoc];
	ScanSubNets[sowArrayCases, assoc];
)

sowArrayCase[_] := 
	BagInsert[$learningRates, $path -> Replace[$path, $lrrules]];


PackageExport["TrainerUpdate"]

NNSetUsage @ "
TrainerUpdate[MXTrainer[$$], <|'port$1'->input$1,$$|>] sets the 'InputArrays' of the trainer, applies a \
forward step, then a backward step, and then does a weight update via the internal optimizers.
TrainerUpdate[MXTrainer[$$], Bucket[data$, key$]] looks up a bucketed executor to use based on key$.
* the input$i should have encoding already performed, MXTrainer[$$] will ignore the net's encoders
* For the bucketed case, the trainer must have been created with the 'MaxLengths' set.
"

TrainerUpdate[MXTrainer[assoc_], data_Association] := Timed @ Scope[
	exec = assoc["Executor"];
	HoldSet[assoc["CurrentNet"], None];
	PreemptProtect[
		NDArraySet[exec["InputArrays"], data];
		MXExecutorForward[exec, True];
		MXExecutorBackward[exec, assoc["LossGradients"]];
		assoc["Optimizer"][]
	]
];

TrainerUpdate[MXTrainer[assoc_], Bucket[data_Association, key_]] := Timed @ Scope[
	HoldSet[assoc["CurrentNet"], None];
	PreemptProtect[
		exec = GetBucketExecutor[assoc["Executor"], key];
		NDArraySet[exec["InputArrays"], data];
		MXExecutorForward[exec, True];
		MXExecutorBackward[exec, assoc["LossGradients"]];
		HoldSet[assoc["OutputArrays"], exec["OutputArrays"]];
		assoc["Optimizer"][]
	]
];

MXTrainer[assoc_][p_] := assoc[p];
MXTrainer[assoc_][p_, rest__] := assoc[p][rest]; (* first get out ExecutorData *)


PackageExport["TrainerCurrentLoss"]

TrainerCurrentLoss[MXTrainer[assoc_]] := Timed @ PreemptProtect[
	Map[NDArrayTotal, First @ assoc["OutputArrays"]]
];

TrainerCurrentLoss[_] := $Failed;

$OptimizerSpec := $OptimizerSpec = <|
	"ADAM" -> {{"ADAM", #}&, <|
		"Beta1" -> Defaulting[IntervalScalarT[0,1], 0.9],
		"Beta2" -> Defaulting[IntervalScalarT[0,1], 0.999],
		"Epsilon" -> Defaulting[IntervalScalarT[0,0.01], 10^-5]
	|>},
	"SGD" -> {{"SGD", #}&, <|
		"Momentum" -> Defaulting[IntervalScalarT[0.,1.], 0.93],
		"LearningRateSchedule" -> Defaulting[ExpressionT, "Polynomial"]
	|>},
	"RMSProp" -> {{"RMSProp", #}&, <|
		"Beta" -> Defaulting[IntervalScalarT[0.,1.], 0.95],
		"Momentum" -> Defaulting[IntervalScalarT[0.,1.], 0.9],
		"Epsilon" -> Defaulting[IntervalScalarT[0.,1.], 10^-8]
	|>},
	$CommonSuboptions -> <|
		"L2Regularization" -> ArrayCasesT[Nullable @ ScalarT, 0.],
		"GradientClipping" -> ArrayCasesT[Nullable @ ScalarT, None],
		"WeightClipping" -> ArrayCasesT[Nullable @ ScalarT, None],
		"InitialLearningRate" -> Defaulting[ScalarT, Automatic],
		"LearningRateSchedule" -> Defaulting[ExpressionT, None],
		(* for debugging: *)
		"GradientsBag" -> Defaulting[MatchT[_Bag], None],
		"GradientMapper" -> Defaulting[ExpressionT, RMSEnergy]
	|>
|>;


PackageScope["ChooseTrainingBatchSize"]

$MaxTrainingBatchSize = 256;

ChooseTrainingBatchSize[plan_, context_] := Scope[
	(* optimization we will probably need: don't bother copying weight arrays over. that's completely
	unnecessary and slow. do this via "DummyArray" option in MXNetExecutor *)
	executor = ToMXExecutor[
		plan, 1, 
		"Context" -> context, "Gradients" -> True, "ArrayCaching" -> False
	];
	NDArraySetScalar[executor["InputArrays"], 1.0];
	outputArrays = Values[executor["OutputArrays"]];
	If[Count[executor["GradientArrays"], _NDArray] === 0, Return[1]]; (* <- dummy value, will be dealt with later *)
	outGrads = NDArrayCreateZero[NDArrayDimensions[#]]& /@ outputArrays;
	time = First @ AbsoluteTiming[
		MXExecutorForward[executor, True];
		MXExecutorBackward[executor, outGrads];
		Scan[NDArrayGet, outputArrays];
		NDArrayWaitForAll[];
	];
	target = $TargetExecutorSize @ First @ FromContextCode[context];
	meminfo = MXExecutorMemoryInformation[executor];
	fixedcost = Total @ Lookup[meminfo, {"ArgumentArrays", "AuxilliaryArrays"}]; 
	batchcost = Total @ Lookup[meminfo, {"InternalArrays", "OutputArrays"}];
	(* we have a linear model for memory requirements as a function of batch size,
	and solve for the target memory size *)
	n = Ceiling[(target - fixedcost) / (batchcost + 1)];
	(* also prevent a batch from taking more than 2 seconds, because responsiveness
	starts to really suffer at that point *)
	n = Min[n, Ceiling[2.0 / time, 4]];
	Clip[n, {1, $MaxTrainingBatchSize}] 
];


PackageExport["TrainerComputeValidationLoss"]

NNSetUsage @ "
TrainerComputeValidationLoss[MXTrainer[$$}, generator$, n$] returns an association of average losses, where \
generator$ generates batches in order to provide a total of n$ individual examples.
* generator$ should return batches as a list containing one array for every input port.
* generator$ should use a batchsize identical to that used to set up the trainer.
"

TrainerComputeValidationLoss[MXTrainer[assoc_], generator_, n_] := Scope[
	UnpackAssociation[assoc, executor, batchSize, bucketed, outputNames];
	{batches, excess} = BatchCountExcess[n, batchSize];
	outTotals = ConstantArray[0., Length[outputNames]];
	If[!bucketed, 
		inputArrays = executor["InputArrays"];
		outArrays = Values @ executor["OutputArrays"];
	];
	Do[
		PreemptProtect[
			batch = generator[b];
			If[bucketed, 
				subexec = GetBucketExecutor[executor, Last[batch]];
				NDArraySet[subexec["InputArrays"], First[batch]];
				MXExecutorForward[subexec, False];
				outArrays = Values @ subexec["OutputArrays"];
			,
				NDArraySet[inputArrays, batch];
				MXExecutorForward[executor, False];
			];
			If[b === 1 && excess > 0,
				outTotals = Map[Total[Drop[NDArrayGet[#], excess], Infinity]&, outArrays];
			,
				outTotals += Map[NDArrayTotal, outArrays];
			];
		];
	,
		{b, batches}
	];		
	outTotals /= (batches * batchSize) - excess;
	AssociationThread[outputNames, outTotals]
];


PackageExport["TrainerSaveCheckpoint"]
PackageExport["TrainerLoadCheckpoint"]

NNSetUsage @ "TrainerSaveCheckpoint[MXTrainer[$$],'file$'] saves all of the trainer's arrays to file$."
NNSetUsage @ "TrainerLoadCheckpoint[MXTrainer[$$],'file$'] loads all of the trainer's arrays from file$."

TrainerSaveCheckpoint[MXTrainer[assoc_], file_] := 
	PreemptProtect @ NDArrayExport[file, Values @ assoc["Arrays"]];

TrainerLoadCheckpoint[MXTrainer[assoc_], file_] := (
	HoldSet[assoc["CurrentNet"], None];
	PreemptProtect @ NDArrayImport[file, Values @ assoc["Arrays"]];
)


PackageExport["TrainerCurrentNet"]

NNSetUsage @ "
TrainerCurrentNet[MXTrainer[$$]] substitutes the current weights \
from the trainer into the original net given to ToMXTrainer.
* There is a fair cost associated with constructing the net, \
and any evaluations performed with the net will currently cause new \
executors to be created (rather than using the training executor), \
so use with caution.
* The current net is cached after being constructed, but the cache \
is flushed when the trainer undergoes a training update.
"

TrainerCurrentNet[MXTrainer[assoc_]] := Scope[
	sym = assoc["CurrentNet"];
	Replace[
		First @ sym, 
		None :> HoldSet[sym,
			ConstructLayer @ ReplaceAll[
				assoc["MutableNet"],
				nd_NDArray :> RuleCondition[NDArrayGetRawArray[nd]]
			]
		]
	]
];


PackageExport["TrainerMemoryUsage"]

NNSetUsage @ "
TrainerMemoryUsage[MXTrainer[$$]] gives the memory usage of the trainer.
"

TrainerMemoryUsage[MXTrainer[assoc_]] := 
	Total @ MXExecutorMemoryInformation[assoc["Executor"]];


(* BOXES *)

PackageExport["MXTrainer"]

NNSetUsage @ "
MXTrainer[<|$$|>] represents an object that can be used to train a network, one batch at a time.
* Use TrainerUpdate[MXTrainer[$$], inputs$] to perform an update step, which does a forward and backward step \
and updates the weights using the optimizer.
* Use TrainerCurrentLoss[trainer$] to get the current total loss.
* Use TrainerCurrentNet[trainer$] to reconstruct the current net.
* An MXTrainer[$$] object contains the following fields:
| 'Executor' | the underlying MXExecutorData[$$] object |
| 'Optimizer' | a function that will be called to perform one optimization update |
| 'LossGradients' | an association of NDArrays containing the imposed loss gradients |
| 'Arrays' | mapping from original net paths to parameter NDArrays |
| 'ArrayGradients' | mapping from original net paths to gradient NDArrays |
| 'BatchSize' | the batch size the trainer was created with |
| 'MaxLengths' | the mapping from sequence id to max lengths |
| 'Context' | the device the trainer was instantiated on |
"

DefineCustomBoxes[MXTrainer,
	t:MXTrainer[_Association] :> MXTrainerBoxes[t]
];

MXTrainerBoxes[trainer:MXTrainer[assoc_]] := Scope[
	UnpackAssociation[assoc, executor, context, batchSize, bucketed];
	execID = ManagedLibraryExpressionID[executor["Executor"]];
	arrayInfo = If[bucketed, {}, Map[
		makeArrayItems[executor[#], #]&, 
		{"InputArrays", "OutputArrays", "SpecialArrays"}
	]];
	BoxForm`ArrangeSummaryBox[
		MXTrainer,
		trainer,
		None,
		ToList[
			makeItem["Bucketed", bucketed],
			makeItem["BatchSize", batchSize],
			arrayInfo, 
			If[bucketed, {
				makeItem["BucketCount", Length[executor[[1,1]]]]
			}, Nothing]
		],
		{
			makeItem["Context", context],
			makeItem["ExecutorID", execID],
			(*makeItem["CurrentLoss", TrainerCurrentLoss[trainer]],*)
			makeItem["MemoryUsage", memStr @ TrainerMemoryUsage[trainer]]
		},
		StandardForm
	]
];

memStr[x_] := Which[
	x > 1000000, StringForm["`` MB", NumberForm[x/1000000., {2,2}]],
	x > 1000, StringForm["`` KB", NumberForm[x/1000., {2,2}]],
	True, StringForm["`` B", x]
];

makeArrayItems[<||>|{}, key_] := 
	makeItem[key, {}];

makeArrayItems[assoc_, key_] := 
	makeItem[key, Style[KeyValueMap[arrayItem, assoc], "Code", FontWeight -> "Plain"]];

arrayItem[name_, array_] := Tooltip[name, NDArrayDimensions[array]];

makeItem[name_, {}|<||>] := makeItem[name, Style["none", Gray]];
makeItem[name_, value_] := BoxForm`MakeSummaryItem[{Pane[name <> ": ", {90, Automatic}], value}, StandardForm];
