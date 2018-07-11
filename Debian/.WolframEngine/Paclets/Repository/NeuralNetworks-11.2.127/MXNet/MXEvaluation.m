Package["NeuralNetworks`"]



PackageExport["NetPortGradient"]
(* ^ actually a system symbol *)

PackageScope["ToEvaluationFunction"]

NNSetUsage @ "
ToEvaluationFunction[net$, {batchspec$, context$, tmode$, pspec$}] \
creates and caches a function that takes as its arguments the inputs \
to net$, and returns the outputs as specified by the port spec pspec$.
ToEvaluationFunction[net$] is for quick debugging and uses the CPU \
context, no batching, and the default port spec.
* None of the inputs of net$ should be a variable-length seq, \
for that, use ToBucketedEvaluationFunction.
$ tmode$ specifies whether to apply Dropout, etc.
* The batchspec$ can be True, False, or an integer. 
* If batchspec$ is True, the batchsize will be chosen automatically.
* The port spec pspec$ is any of the specs supported by top-level
nets.
"

(* for debugging *)
ToEvaluationFunction[net_] := 
	iToEvaluationFunction[net, {False, 1, False, Automatic}];

ToEvaluationFunction[net_, meta_] := 
	SingletonCached[iToEvaluationFunction, net, meta];

iToEvaluationFunction[net_, {batchspec_, context_, tmode_, pspec_}] := Scope @ PreemptProtect[
	If[!FullySpecifiedNetQ[net], Panic["InvalidNet"]];
	If[!MissingQ[NProperty[net, "WLEquivalent"]], 
		Return @ ToEquivalentEvaluationFunction[net, batchspec =!= False, pspec]
	];

	inputs = Inputs[net];
	outputs = Outputs[net];

	If[ContainsVarSequenceQ[inputs], 
		If[!FreeQ[pspec, NetPortGradient], 
			General::noseqgrad = "NetPortGradient not supported for nets that operate on sequences.";
			ThrowFailure["noseqgrad"]
		];
		(* will end up getting cached twice, but whatever *)
		Return @ ToBucketedEvaluationFunction[net, {batchspec, context, tmode, pspec}];
	];

	(* EvaluationFunction as a whole will be cached, so no point caching only the 
	plan *)
	ospec = All;
	pspec = pspec /. np_NetPort :> procNP[net, np];
	If[!FreeQ[pspec, $Thread], pspec = unthread0[pspec]];
		
	plan = ToMXPlan[net, {ospec, <||>, tmode}];

	$batchq = True;
	batchsize = Switch[batchspec,
		True, If[context === 1, $MaxCPUEvaluationBatchSize, $MaxGPUEvaluationBatchSize],
		False, $batchq = False; 1,
		_Integer, batchspec
	];

	$grads = False;
	pspec = Replace[pspec, np_NetPortGradient :> procNPG[net, np], {0, 1}];
	executor = ToMXExecutor[plan, batchsize, "Gradients" -> $grads, "Context" -> context, "ArrayCaching" -> True];
	doGrads = $grads =!= False;

	exec = executor["Executor"];

	(* to amortize the cost of all the logic associated with evaluating a net, we build a pure
	function that does exactly what needs to be done, and we cache that pure function against
	the network. TODO: make this caching only happen on the 2nd or 3rd time the net is evaluated,
	because the cost of doing this compilation is probably also non-trivial *)
	If[$batchq,
		If[doGrads, ThrowFailure["nobatchgrad"]];
		setters = KeyValueMap[makeBatchSetter, inputs];
		{outputNames, getters} = KeysValues @ ToList @ toBatchGetters[pspec, outputs];
		postfunc = If[Length[outputNames] > 1, 
			Hold[AssociationThread][outputNames, #]&,
			Hold[First]
		];
		(* TODO: switch to my own implementation of batching *)
		CreateFunction[{
			makeBatchRaggedChecker[Length[inputs]],
			postfunc @ Hold[MXEvaluate][executor, {##}, outputNames, setters, getters, tmode]
		}, PreemptProtect]
	,
		inputArrays = executor["InputArrays"];
		outputArrays = executor["OutputArrays"];
		gradArrays = executor["GradientArrays"]; (* <- used only by NetPortGradient pspec *)
		CreateFunction[{
			MapThread[makeSetter, {Range[Length[inputs]], Normal @ inputArrays, Normal @ inputs}],
			Hold[MXExecutorForward][exec, If[doGrads, True, tmode]],
			If[!doGrads, Nothing,
				Hold[MXExecutorBackward][exec, CreateOutputGradients[outputArrays, 1.0]]
			],
			toGetter[pspec, outputArrays, outputs]
		}, PreemptProtect]
	]
];

(* complex, i know. its because some properties can be lists *)
unthread0[expr_List] := Map[unthread1, expr];
unthread0[$Thread[keys_]] := keys;
unthread0[$Thread[keys_] -> prop_] := # -> prop& /@ keys;
unthread0[e_] := e;

unthread1[$Thread[keys_]] := Apply[Sequence, keys];
unthread1[$Thread[keys_] -> prop_] := Apply[Sequence, # -> prop& /@ keys];
unthread1[e_] := e;


(* TODO: Support for RawArrays for input and output *)

General::nointgrad = "Gradient is not available for integer-valued ``."
General::nobatchgrad = "NetPortGradient is not supported when evaluating lists of inputs."
General::badportd = "`` does not correspond to an input or array of the net."
General::invportd = "`` is not a valid NetPortGradient specification."
General::nooutgrad = "Gradient is not available at the output `` of the net.";

procNPG[net_, npg:NetPortGradient[spec_]] := Scope[
	path = ToNetPath[net, spec];
	If[!FreeQ[NData[net] @@ path, _IndexIntegerT], ThrowFailure["nointgrad", NetPathString[path]]];
	If[FailureQ[path], ThrowFailure["badportd", npg]];
	mangled = MXManglePath[path];
	If[$grads === False, $grads ^= {}];
	AppendTo[$grads, mangled];
	NetPortGradient[spec, mangled]
];

procNPG[net_, npg_] := ThrowFailure["invportd", npg];

procNP[net_, NetPort[spec_, out_]] := 
	procNP[net, NetPort[ToList[spec, out]]];


General::npambig = "There is no output port named `1`. To refer to the output of a layer identified by `1`, use NetPort[{`1`}] or NetPort[{`1`}, \"port\"].";

procNP[net_NetP, NetPort[spec_]] := Scope[
	If[(StringQ[spec] || IntegerQ[spec]) && !KeyExistsQ[Outputs[net], spec], 
		ThrowFailure["npambig", MsgForm @ spec]];
	If[ospec === All, ospec ^= {}];
	paths = EnumerateInternalOutputs[net, spec];
	ports = NetPort[FromNetPath[#]]& /@ paths;
	ospec ^= Join[ospec, ports];
	ScanThread[Function[outputs[#1] = net @@ #2], {ports, paths}];
	If[Length[ports] === 1,
		First[ports],
		$Thread[ports]
	]
];

procNP[_, np_] := ThrowFailure["badporto", np];

$litNPelem = _String | _Integer;


PackageScope["EnumerateInternalOutputs"]

Clear[EnumerateInternalOutputs];

General::badporto = "`` does not correspond to an output of the net, or a subnet of the net, or a layer."

(* fast case: we have a literal port or layer, we can just canonicalize without traversing whole net *)
EnumerateInternalOutputs[net_NetP, spec:$litNPelem | {$litNPelem..}] := Scope[
	$path = ToNetPath[net, spec];
	If[FailureQ[$path], ThrowFailure["badporto", NetPort[spec]]];
	If[MatchQ[$path, NetPath[___, "Outputs", _]], Return[{$path}]];
	layer = UnsafeQuietCheck[net @@ $path, $Failed];
	ports = OutputPorts[layer];
	If[!MatchQ[ports, {__NetPath}], ThrowFailure["badporto", NetPort[spec]]];
	ports
];

General::badportpo = "`` is not a valid port or port pattern, which should be a hierarchical specification using NetPort, possibly including All, or Span."
General::emptportpo = "Port pattern `` did not match any ports."

(* slow case: we have a pattern, do a slow traverse and collect up all matching outputs (outermost only) *)
EnumerateInternalOutputs[net_NetP, spec_] := Scope[
	$patt = ToNetPathPattern[net, spec];
	If[FailureQ[$patt], ThrowFailure["badportpo", NetPort[spec]]];
	res = DeleteDuplicates @ Flatten @ ReapBag @ EnumerateMatchingOutputs[net];
	If[res === {}, ThrowFailure["emptportpo", res]];
	res
];

DeclareMethod[EnumerateMatchingOutputs, EnumerateLayerOutputs, EnumerateContainerOutputs];

(* we deliberately don't enumerate over operator's inner nets, becuase how to even read them off his highly
complicated and requires we inject ourselves into the unrolling process to collect up the right e.g. seq nodes *)

EnumerateLayerOutputs[net_] := Scope[
	oports = OutputPorts[net];
	If[MatchQ[$path, $patt], 
		SowBag @ oports, 
		SowBag @ Cases[oports, $patt]
	]
];

EnumerateContainerOutputs[net_] := If[
	MatchQ[$path, $patt], SowBag @ OutputPorts[net], 
	ScanFields["Nodes", EnumerateMatchingOutputs, net]
];


PackageScope["makeBatchRaggedChecker"]
(* also used by ConstructBucketedGeneratorFactory *)

makeBatchRaggedChecker[1] := Nothing;
makeBatchRaggedChecker[n_] := imakeBatchRaggedChecker @@ Range[n];

imakeBatchRaggedChecker[r__] := 
	Hold[If[!SameQ[r], ThrowFailure["enetragged"]]] /. i_Integer :> Length[Slot[i]];


General::enetragged = "When evaluating batches of inputs, all input ports must be given lists of the same length."

makeBatchSetter[name_, type_] := With[
	{encoder = ToEncoderFunction[type, True]},
	If[encoder === Identity, 
		Function[NDArraySetChecked[#1, #2, ComplainType[name, type]]],
		Function[NDArraySet[#1, encoder @ #2]]
	]
];

(* TODO: unify use of NDArraySet and NDArraySetChecked. If we just use
Checked, we can have the encoder return a $Failed[reason], and then
intercept that in ComplainType, so that encoder failures will automatically
get tagged with the name of the failed port, which is good. *)

makeSetter[i_, name_ -> arr_, _ -> type_] := With[
	{encoder = ToEncoderFunction[type, False]},
	If[encoder === Identity, 
		Hold @ NDArraySetChecked[arr, Slot[i], ComplainType[name, type]],
		Hold @ NDArraySet[arr, encoder @ Slot[i]]
	]
];


(* None means remove the encoder, Automatic means use the 'default' decoding *)
$PropPattern = _String | {_String, _} | None | Automatic;

toGetter[Automatic, arrays_, types_] := 
	toUniformGetter[arrays, types, Automatic];

toGetter[NetPortGradient[path_, mxname_], _, _] := Scope[
	gradArray = Lookup[gradArrays, mxname, ThrowFailure["badportd", NetPortGradient[path]]];
	If[StringFreeQ[mxname, "."], (* is it an input? *)
		With[{func = ToDecoderFunction[inputs[mxname], None, False]},
			Hold[NDArrayGetFlat /* func]
		], 
		Hold[NDArrayGet]
	] @ gradArray
];

toGetter[prop:$PropPattern, <|name_ -> array_|>, types_] /; name =!= prop := 
	makeGetter[name, array, First @ types, prop];

toGetter[(name_String | name_NetPort), arrays_, types_] :=
	makeGetter[name, arrays[name], types[name], Automatic];

toGetter[(name_String | name_NetPort) -> prop:$PropPattern, arrays_, types_] := 
	makeGetter[name, arrays[name], types[name], prop];

toGetter[All -> prop:$PropPattern, arrays_, types_] := 
	toUniformGetter[arrays, types, prop];

$pspeckey = _String | _NetPort | _NetPortGradient;
ListOfPortSpecsQ = MatchQ @ List @ Repeated[$pspeckey | ($pspeckey -> _)];

toGetter[list_List ? ListOfPortSpecsQ, arrays_, types_] :=
	Hold[Association][
		Function[toSpecKey[#] -> toGetter[#, arrays, types]] /@ list
	]

toSpecKey[a_ -> _] := toSpecKey[a];
toSpecKey[NetPortGradient[np_, _]] := NetPortGradient[np];
toSpecKey[other_] := other;

toUniformGetter[<|key_ -> arr_|>, <|key_ -> type_|>, prop_] :=
	makeGetter[key, arr, type, prop];

toUniformGetter[arrays_, types_, prop_] :=
	Hold[AssociationThread][
		Keys[arrays], 
		KeyValueMap[
			makeGetter[#1, #2, types[#1], prop]&,
			arrays
		]
	];

General::invppspec = "`` is not a valid property or port specification."
toGetter[spec_, _, _] := ThrowFailure["invppspec", spec];

General::invoutport = "`` is not the name of an output port for the net."
makeGetter[name_, _Missing, _, _] := ThrowFailure["invoutport", name];

General::nodeconport = "Property specification `` cannot be applied becase port \"``\" does not have an NetDecoder associated with it."
makeGetter[name_, arr_, type_, prop_] := With[
	{decoder = ToDecoderFunction[type, prop, False]},
	If[decoder === $Failed, ThrowFailure["nodeconport", prop, name]];
	Hold[decoder @ NDArrayGetFlat @ arr]
];

(* toBatchGetters must return a list of rules of outputName -> ndgetter function
*)

Clear[toBatchGetters];
toBatchGetters[Automatic, types_] := 
	toUniformBatchGetters[types, Automatic];

toBatchGetters[prop:$PropPattern, <|name_ -> type_|>] /; name =!= prop := 
	makeBatchGetter[name, type, prop];

toBatchGetters[(name_String | name_NetPort), types_] :=
	makeBatchGetter[name, types[name], Automatic];

toBatchGetters[(name_String | name_NetPort) -> prop:$PropPattern, types_] := 
	makeBatchGetter[name, types[name], prop];

toBatchGetters[All -> prop:$PropPattern, types_] := 
	toUniformBatchGetters[types, prop];

toUniformBatchGetters[types_, prop_] := 
	KeyValueMap[
		makeBatchGetter[#1, #2, prop]&,
		types
	];

toBatchGetters[list_List ? ListOfPortSpecsQ, type_] :=
	Flatten[Map[makeBatchGetter[#, type]&, list]];

toBatchGetters[spec_, _] := ThrowFailure["invppspec", spec];

makeBatchGetter[name_, _Missing, _] := ThrowFailure["invoutport", name];

makeBatchGetter[name_, type_, prop_] := With[
	{decoder = ToDecoderFunction[type, prop, True]},
	If[decoder === $Failed, ThrowFailure["nodeconport", prop, name]];
	name -> Function[decoder @ NDArrayGet @ #]
];


PackageExport["$TargetExecutorSize"]
PackageExport["$MaxCPUEvaluationBatchSize"]
PackageExport["$MaxGPUEvaluationBatchSize"]

$TargetExecutorSize = <|
	"CPU" :> ($SystemMemory/4),
	"GPU" -> (1024*10^20)
|>;

$MaxCPUEvaluationBatchSize = 512;
$MaxGPUEvaluationBatchSize = 32;


PackageScope["ToEquivalentEvaluationFunction"]

(* 
	So we actually make the assumption here that the net has just one input, which is 
	variadic. because that's what this whole mechanism is for, to avoid dealing with a complex
	mapping scheme on the MXNet side.
*)

ToEquivalentEvaluationFunction[net_NetP, batchq_:False, pspec_:Automatic] := With[
	{equivfn = NProperty[net, "WLEquivalent"]},
	equiv = equivfn[net["Parameters"], net];
	inputs = Inputs[net]; outputs = Outputs[net];
	If[Length[inputs] =!= 1 || Length[outputs] =!= 1, Panic[]];
	If[!FreeQ[pspec, NetPortGradient], ThrowFailure["notuplegrad"]];
	decoder = ToDecoderFunction[First[outputs], pspec, batchq];
	If[FailureQ[decoder], ThrowFailure["invppspec", pspec]];
	RightComposition[
		toEncoderOrChecker["Input", First[inputs], batchq], 
		If[batchq, Transpose /* Map[equiv], equiv],
		decoder
	]
];

General::notuplegrad = "NetPortGradient is not supported when evaluating a layer whose input is a list of tensors."

(* these guys have to verify dimensions, as ToEncoderFunction doesn't do this
because it knows that NDArraySetChecked WILL do this *)

seqSymbol[id_] := With[{sym = Unique["NeuralNetworks`Private`TempVars`seq" <> IntegerString[id]]},
	SetAttributes[sym, Temporary];
	sym
];

varsToBlanks[x_] := ReplaceAll[x, LengthVar[id_] :> RuleCondition @ Construct[Pattern, seqSymbol[id], Blank[]]];

toEncoderOrChecker[name_, t:{___TensorT}, batchq_] := With[
	{dims = If[batchq, Map[Prepend[_]], Identity] @ varsToBlanks @ Map[TDimensions, t], count = Length[t]},
	Function[tuple, If[
		And[
			ListQ[tuple],
			Length[tuple] === count,
			MatchQ[Map[MachineArrayDimensions, tuple], dims],
			!batchq || AllSameBy[tuple, Length]
		], 
		N, ComplainType[name, t]] @ tuple
	]
];

toEncoderOrChecker[name_, t_TensorT, batchq_] := With[
	{dims = If[batchq, Prepend[_], Identity] @ TDimensions[t]},
	Function[input,
		If[dimMatchQ[input, dims], N, ComplainType[name, t]] @ input
	]
];

toEncoderOrChecker[_, t_, batchq_] := ToEncoderFunction[t, batchq];


PackageScope["ToBucketedEvaluationFunction"]

NNSetUsage @ "
ToBucketedEvaluationFunction[net$, {isBatched$, context$, tmode$, pspec$}] \
creates and caches a function that takes as its arguments the inputs \
to net$, and returns the outputs as specified by the port spec pspec$.
ToBucketedEvaluationFunction[net$] is for quick debugging and uses \
the CPU context, no batching, and the default port spec.
* At least one of the inputs of net$ should be a variable-length seq.
* tmode$ determines whether dropout, etc is applied.
* A BucketedMXExecutor is used internally, whereaby the actual \
sequence lengths of the input sequences are used to lookup the \
concrete MXExecutor to use.
* Currently, batched-mode uses a hard-coded batch size of 16.
* The port spec pspec$ is any of the specs supported by top-level
nets.
"

(* for debugging *)
ToBucketedEvaluationFunction[net_] := 
	iToBucketedEvaluationFunction[net, {False, {"CPU", 0}, False, Automatic}];

ToBucketedEvaluationFunction[net_NetP, meta_] := 
	Cached[iToBucketedEvaluationFunction, net, meta];

iToBucketedEvaluationFunction[net_, {batchq_, context_, tmode_, pspec_}] := ModuleScope[
	If[!FullySpecifiedNetQ[net], Panic["InvalidNet"]];

	bucketedExecutor = ToBucketedMXExecutor[net, {All, context, batchq, tmode, False}];

	inputs = Inputs[net];
	outputs = Outputs[net];

	If[batchq,
		factory = ConstructBucketedGeneratorFactory[inputs, bucketedExecutor["BatchSize"]];
		{outputNames, getters} = KeysValues @ ToList @ toBatchGetters[pspec, outputs];
		BatchedBucketedEvaluationFunction[
			factory,
			bucketedExecutor,
			outputNames,
			getters,
			tmode
		]
	,
		pseudoArrays = Association @ Map[# -> $out[#]&, Keys @ outputs];
		getter = toGetter[pspec, pseudoArrays, outputs];
		{names, types} = KeysValues[inputs];
		withLengthCaptors[names,
			gencode = MapThread[makeBucketSetter, {Range[Length[names]], names, types}];
		];
		CreateFunction[Hold[
			TempVar[executor] ^= GetBucketExecutor[bucketedExecutor, RoundBucket @ Eval @ Keys[gencode]];
			TempVar[$in] ^= executor["InputArrays"];
			Eval @ Values[gencode];
			MXExecutorForward[executor, tmode];
			TempVar[$out] ^= executor["OutputArrays"];
			Eval[getter]
		], PreemptProtect]
	]
];

makeBucketSetter[n_, name_, t_ ? VarSequenceQ] := Module[
	{enc = ToEncoderFunction[t, False], id = GetLengthVarID[t]}, 
	Rule[
		makeLengthCaptor[enc, id, n, False],
		Hold @ NDArraySetChecked[$in[name], TempVar[n], ComplainType[name, t]]
	]
];

(* utility for finding repeated sequence ids in inputs, used by 
makeBucketSetters and ConstructBucketedGeneratorFactory
*)

PackageScope["withLengthCaptors"]
SetHoldRest[withLengthCaptors];
withLengthCaptors[names_, body_] := Block[
	{$names = names, $seqpositions = <||>},
	body
];


PackageScope["makeLengthCaptor"]
SetHoldFirst[makeLengthCaptor]; (* to keep the enc as a closure var *)
makeLengthCaptor[enc_, id_, n_, batchq_] := With[
	{lastpos = $seqpositions[id], names = $names, lenf = If[batchq, getSeqLengths, getSeqLength]},
	If[IntegerQ[lastpos],
		(* if this seq id occured previously, don't emit its length, but instead
		check its length matches the previous length *)
		Hold @ If[
			lenf[TempVar[n] = enc[Slot[n]]] === lenf[TempVar[lastpos]], Nothing,
			ThrowFailure["incseqlen", names[[n]], names[[lastpos]]]
		],
		$seqpositions[id] = n; 
		Hold @ lenf[TempVar[n] = enc @ Slot[n]]
	]
];

General::netinvseq = "Invalid sequence provided to net."

getSeqLength[list_List] := Length[list];
getSeqLength[{}] := ThrowFailure["netnullseq"];
getSeqLength[_] := ThrowFailure["netinvseq"];
getSeqLengths[list_] := If[!VectorQ[list, ListQ], ThrowFailure["netinvseq"], Map[getSeqLength, list]];

General::incseqlen = "Length of sequence provided to port \"``\" is inconsistent with length of sequence provided to port \"``\".";

makeBucketSetter[n_, name_, EncoderP["Scalar"] | ScalarT] :=
	Nothing -> Hold[NDArraySetChecked[$in[name], Slot[n], ComplainType[name, ScalarT]]]

makeBucketSetter[n_, name_, type_] := Module[
	{encf = ToEncoderFunction[type, False]},
	Nothing -> If[encf === Identity, 
		Hold[NDArraySetChecked[$in[name], Slot[n], ComplainType[name, type]]],
		Hold[NDArraySet[$in[name], encf @ Slot[n]]]
	]
];


PackageScope["BatchedBucketedEvaluationFunction"]

BatchedBucketedEvaluationFunction[factory_, bucketedExecutor_, outputNames_, getters_, tmode_][inputs__] := Scope[
	{generator, max, iperm, n} = factory[inputs];
	(* TODO: check that inconsistent batch sizes are dealt with here or elsewhere *)
	outputBags = Table[Bag[], Length[outputNames]];
	lastBucket = None;
	GetBucketExecutor[bucketedExecutor, max]; 
	Do[
		batch = generator[i];
		thisBucket = Last[batch];
		If[lastBucket =!= thisBucket,
			executor = GetBucketExecutor[bucketedExecutor, thisBucket];
			inputArrays = executor["InputArrays"];
			$outputArrays = executor["OutputArrays"];
			lastBucket = thisBucket;
		];
		NDArraySet[inputArrays, First @ batch];
		MXExecutorForward[executor, tmode];
		MapThread[
			StuffBag[#1, #2[$outputArrays[#3]], 1]&,
			{outputBags, getters, outputNames}
		];
	,
		{i, n}
	];
	outputBags = BagPart[#, iperm]& /@ outputBags;
	If[Length[outputNames] == 1, 
		First @ outputBags, 
		AssociationThread[outputNames, outputBags]
	]
];
