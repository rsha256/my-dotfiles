Package["NeuralNetworks`"]


PackageScope["NetApply"]

General::inpmiss = "Required input slot `` was not provided.";
General::inpunk = "Unknown input `` provided.";
General::nninit = "Cannot `` net: unspecified value for ``.";
General::nfspec = "Cannot `` net: `` is not fully specified."
General::nfspecdebug = "Cannot `` net: `` is not fully specified."
General::nfspecunk = "Cannot `` net: net is not fully specified."

PackageScope["$EvaluationContext"]
PackageScope["$EvaluationTrainingMode"]

$EvaluationContext = 1;
$EvaluationTrainingMode = False;

General::invnem = "NetEvaluationMode should be set to either \"Test\" or \"Train\".";
NetApply[net_, arg___, NetEvaluationMode -> tspec_] := Scope[
	$EvaluationTrainingMode = Switch[tspec, "Train", True, "Test", False, _, ThrowFailure["invnem"]];
	NetApply[net, arg]
];

NetApply[net_, arg___, TargetDevice -> device_] := Block[
	{$EvaluationContext = ParseContext[device]},
	NetApply[net, arg]
];

NetApply[net_] := NetApply[net, <||>];

NetApply[net_, data_, prop_:Automatic] := Scope[

	inputs = Inputs[net]; 
	numInputs = Length[inputs];

	If[AssociationQ[data],
		(* check that the right number of slots were provided *)
		If[numInputs =!= Length[data], 
			ThrowFailure["invargc", Length[data], numInputs]
		];
		(* get the data in canonical order *)
		data = ALookup[data, Keys[inputs], ThrowFailure["inpmiss", #]&];
	,
		(* non-association inputs correspond exactly to nets with only 1 input *)
		If[numInputs =!= 1, 
			(* unless all but one are nullable *)
			ThrowFailure["invargc", 1, numInputs];
		,
			data = {data};
		]
	];

	Which[
		FullySpecifiedNetQ[net], 
			(* it's ready to go *)
			$batchsize = Automatic
		, 
		InitializedNetQ[net], 
			(* arrays present, but we need to infer shape based on the input *)
			{net, $batchsize} = JITInferNet[net, data];
			inputs = Inputs[net];
		,
		True, 
			(* net is not ready *)
			ThrowNotSpecifiedFailure[net, "evaluate"];			
	];
	
	(* force $batchsize to be resolved if it wasn't resolved by JITInferNet becuase
	the net was alrady specified *)
	If[$batchsize === Automatic, 
		$batchsize = GuessBatchSize[First[inputs, Null], First[data, Null]];
	];

	func = ToEvaluationFunction[net, {$batchsize =!= None, $EvaluationContext, $EvaluationTrainingMode, prop}];

	Catch[
		func @@ data,
		EncodeFail
	]
];

NetApply[net_, len___] := ThrowFailure["invargc", Length[{len}], 1];

General::invargc = "`` inputs provided, `` were expected."
checkInputCount[netn_, datan_] := If[netn =!= datan, 
	ThrowFailure["invargc", datan, netn]
];

PackageScope["NetApplyFast"]

NetApplyFast[net_, data_] := 
	Replace[$FastPathCache["get"[net, Null]], Null :> ToFastPathFunction[net]] @ data;


PackageScope["ToFastPathFunction"]

Clear[$FastPathCache];
$FastPathCache = Language`NewExpressionStore["NNFastPathCache"];

ToFastPathFunction[net_] := ModuleScope[
	$net = net;
	Scan[$FastPathCache["remove"[First[#]]]&, $FastPathCache["listTable"[]]];
	If[!FullySpecifiedNetQ[net], Goto[False]];
	inputs = Inputs[net]; 
	numInputs = Length[inputs];
	If[numInputs =!= 1, Goto[False]];
	inputChecker = fastChecker @ First @ inputs;
	evalFunc = ToEvaluationFunction[net, {False, 1, False, Automatic}];
	func = With[{inputChecker = inputChecker, evalFunc = evalFunc},
		Function[If[inputChecker, evalFunc[#], NetApply[$net, #]]] /. Hold[h_] :> h
	];
	Goto[True];
	(* shouldn't we skip the fast path cache entirely when JIT specification is happening,
	as these nets will immediately evaporate anyway? *)
	Label[False]; func = NetApply[$net, #]&;
	Label[True];
	$FastPathCache["put"[net, Null, func]];
	func
];

fastChecker[TensorT[{d_Integer}]] := Hold @ And[VectorQ[#, NumberQ], Length[#] === d];
fastChecker[EncoderP["Scalar"]] := Hold @ NumberQ[#];
fastChecker[EncoderP["Boolean"]] := Hold @ BooleanQ[#];
fastChecker[EncoderP["Image"]] := Hold @ Image`ValidImageQ[#];
fastChecker[_] := Goto[False];



PackageScope["GuessBatchSize"]

GuessBatchSize[_, {}] := ThrowFailure["netemptin"];

GuessBatchSize[Null, Null] := None; (* for graphs with no inputs *)

GuessBatchSize[enc_NetEncoder, data_] := 
	If[ListQ[data],
		If[AcceptsListsQ[enc],
			If[FailureQ[Quiet[enc @ First @ data]],
				None,
				Length[data]
			],
			Length[data]
		],
		None
	];

GuessBatchSize[SequenceT[_, enc_NetEncoder], data_] :=
	If[!ListQ[data], None,
		If[VectorQ[data, ListQ],
			If[AcceptsListsQ[enc], 
				Replace[Dimensions[data], {
					{n_, _} :> n,
					_ :> None
				}]
			,
				Length[data]
			],
			None
		]
	];

GuessBatchSize[t_TensorT, data_] := 
	If[!ListQ[data], 
		None,
		tr = TRank[t];
		If[tr === 0, 
			Length[data],
				dr = maxRank[data];
				Which[
					dr == tr, None, 
					dr == tr + 1, Length[data],
					True, None
				]
		]
	];

GuessBatchSize[ListT[_, t_] | {t_, ___}, data_] := 
	If[!ListQ[data] || data === {}, 
		None, (* <- will fail later *)
		GuessBatchSize[t, First[data]]
	];

(* other types *)
GuessBatchSize[_, data_] := If[ListQ[data], Length[data], None];

(* TODO: Move this into kernel *)
maxRank[data_] := Scope[
	rank = 0; 
	While[ListQ[data] && data =!= {},
		If[PackedArrayQ[data], rank += ArrayDepth[data]; Break[]];
		data = First[data]; 
		rank++
	];
	rank
];


PackageScope["ThrowNotSpecifiedFailure"]

ThrowNotSpecifiedFailure[net_, action_] := Scope[
	part = FindUnspecifiedPath[net, action === "evaluate"];
	If[part === None, ThrowFailure["nfspecunk", action]];
	If[part[[-2]] === "Arrays", 
		ThrowFailure["nninit", action, NetPathString[part]],
		If[$DebugMode,
			ThrowFailure["nfspecdebug", action, NetPathString[part], NData[net] @@ part],
			ThrowFailure["nfspec", action, NetPathString[part]]
		]
	]
];


PackageScope["UnspecifiedPathString"]

UnspecifiedPathString[net_] := NetPathString @ FindUnspecifiedPath[net, True];


PackageScope["JITInferNet"]

$batchsize = Automatic;
$seqsizes = <||>;

JITInferNet[net:head_[assoc_Association, _], data_] := Scope[
	inputs = Inputs[assoc];
	(* if all the net's existing input types were already fully specified, adding the
	dimensions of the actual input data will add nothing, so bail now. *)
	If[AllTrue[inputs, FullySpecifiedTypeQ], 
		ThrowNotSpecifiedFailure[assoc, "evaluate"];
	];
	(* TODO: this prevents you from doing a property on a partially specified net *)
	checkInputCount[Length[inputs], Length[data]];
	{inputs, batchsize} = JITInferInputTypes[data, inputs];
	assoc["Inputs"] = inputs;
	net2 = CatchFailure[General, ConstructWithInference[head, assoc]];
	If[FailureQ[net2], 
		If[inferenceFailureQ[net2],
			net2 = net, (* punt to reporting the unspecified part *)
			ThrowRawFailure[net2] 
		];
	];
	If[!FullySpecifiedNetQ[net2], 
		If[$DebugMode, Print[PrettyForm[NData @ net2]]];
		ThrowNotSpecifiedFailure[net2, "evaluate"]
	];
	{net2, batchsize}
]; 

inferenceFailureQ[Failure[_, <|"MessageTemplate" :> MessageName[_, "tyinc"|"tyfail"|"tyufail"], _|>]] := True;
inferenceFailureQ[_] := False;

PackageScope["JITInferInputTypes"]

General::netemptin = "Empty inputs are not permitted."
dFirst[a_] := First[a, ThrowFailure["netemptin"]];

JITInferInputTypes[data_, types_] := Scope[
	{keys, types} = KeysValues[types];
	(* Note: this General actually has no effect. The inherited head is still the
	outermost one. *)
	unbatched = CatchFailure[General, inferUnbatchedInputs[data, types, keys]];
	If[FailureQ[unbatched],
		batched = inferBatchedInputs[data, types];
		If[FailureQ[batched], ThrowRawFailure[unbatched]];
		{AssociationThread[keys, batched], Length @ dFirst @ data}
	,
		{AssociationThread[keys, unbatched], None}
	]
];	

inferUnbatchedInputs[data_, types_, keys_] := 
	MapThread[
		Replace[infer[#1, #2], $Failed :> ComplainType[#3, #2][#1]]&,
		{data, types, keys}
	];

General::netein = "Net cannot be applied to an empty input."
checkZero[{0}] := ThrowFailure["netein"];
checkZero[e_] := e;

infer[d_, t_TensorT] := Scope[
	If[FailureQ[dims = checkZero @ MachineArrayDimensions[d]], ReturnFailed[]];
	st = Replace[TType[t], AtomT -> RealT];
	UnifyTypes[TensorT[dims, st], t]
];

infer[d_, enc_NetEncoder] := 
	If[ListQ[d] && !AcceptsListsQ[enc], $Failed, enc];

infer[d_, SequenceT[_LengthVar, type_]] := Scope[
	If[!ListQ[d] || d === {}, ReturnFailed[]];
	If[FailureQ[type = infer[dFirst[d], type]], ReturnFailed[]];
	If[!AllSameBy[d, Dimensions], ReturnFailed[]];
	SequenceT[NewLengthVar[], type]
];

infer[d_, EitherT[alts_]] := Scope[
	Do[If[!FailureQ[type = infer[d, alt]], Return[type, Block]], {alt, alts}];
	$Failed
];

infer[ds_, ListT[n_, t_]] := Scope[
	If[!ListQ[ds] || ds === {}, ReturnFailed[]];
	If[IntegerQ[n] && Length[ds] != n, ReturnFailed[]];
	Do[If[FailureQ[t = infer[d, t]], ReturnFailed[]], {d, ds}];
	Table[t, Length[ds]]
];

General::netcni = "Could not infer unspecified parameters from the given inputs."

(* this won't come up very often, maily layers that use a SwitchedType without
a fallback. totally heuristic, open to juggling the order here. *)
infer[d_, TypeT] := Which[
	RealQ[d], ScalarT,
	PositiveMachineIntegerQ[d], IndexIntegerT[SizeT],
	NumberQ[d], ScalarT,
	MachineArrayQ[d, PosIntegerT], TensorT[Dimensions[d], IndexIntegerT[SizeT]],
	MachineArrayQ[d], TensorT[Dimensions[d]],
	True, ThrowFailure["netcni"]
];

infer[d_, t_] := If[TestType[d, t], t, $Failed];

inferBatchedInputs[data_, types_] := Scope[
	If[!VectorQ[data, ListQ], ReturnFailed[]];
	If[!AllSameBy[data, Length], ReturnFailed[]];
	MapThread[
		Replace[inferBatch[#1, #2], $Failed :> ReturnFailed[]]&,
		{data, types}
	]
];

inferBatch[d_, t_TensorT] := Scope[
	If[FailureQ[dims = checkZero @ MachineArrayDimensions[d]], ReturnFailed[]];
	UnifyTypes[TensorT[Rest @ dims], t]
];

inferBatch[d_, SequenceT[len_LengthVar, type_]] := Scope[
	If[!ListQ[d] || d === {} || !VectorQ[d, ListQ], ReturnFailed[]];
	If[Apply[SameQ, lens = Length /@ d], len = dFirst[lens]];
	If[Min[lens] === 0, ReturnFailed[]];
	If[FailureQ[type = infer[d[[1,1]], type]], ReturnFailed[]];
	SequenceT[len, type]
];

inferBatch[d_, t_] := infer[dFirst[d], t];