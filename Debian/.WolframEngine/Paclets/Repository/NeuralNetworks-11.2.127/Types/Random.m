Package["NeuralNetworks`"]



PackageScope["RandomType"]

NNSetUsage @ "
RandomType[] evaluates to a pseudorandom type.
"

DefineMacro[Zipf,
Zipf[exprs__] := 
	ToQuoted[RandomChoice,
		1./Range[Length[Unevaluated[{exprs}]]] :> 
		{exprs}
	]
];

RandomType[] := randType;

randType := Zipf[
	randAtom,
	ListT[randSize, randAtom],
	EnumT[Zipf[{"A","B"},{"A","B","C"}, ListT[randSize, StringT]]],
	Zipf[
		Nullable[randType],
		With[t = randType, Defaulting[t, RandomInstance[t]]]
	],
	Zipf[
		StructT[{"A" -> randAtom, "B" -> randAtom}],
		AssocT[randType, randAtom],
		TypeT, ExpressionT
	]
];

randTensor := Zipf[
	TensorT[Table[randSize, randLen]],
	TensorT[randSize]
];

randAtom := Zipf[
	Zipf[ScalarT, IntegerT],
	Zipf[SizeT, NaturalT, PosIntegerT, IndexIntegerT[RandomInteger[10]]],
	Zipf[StringT, BooleanT],
	randTensor
];

randSize := RandomChoice[{SizeT, randLen}];
randLen := RandomInteger[{1,3}];


PackageScope["RandomInstance"]

NNSetUsage @ "
RandomInstance[type$] generates a random instance of type$.
"

intOrRand[n_] := If[IntegerQ[n], n, RandomInteger[{1, 5}]];

RandomInstance[type_] := CatchFailure @ Match[type,

	ScalarT :> RandomReal[],

	TensorT[dims_List, RealT] :> RandomReal[1, intOrRand /@ dims],
	
	TensorT[SizeListT[rank_Integer], RealT] :> RandomReal[1, RandomInteger[{1,5}, rank]],
	
	TensorT[dims_, IndexIntegerT[n_]] :> RandomInteger[{1,n}, dims],

	SequenceT[n_, TensorT[dims:{__Integer}, RealT]] :> RandomReal[1, Prepend[dims, intOrRand @ n]],

	SequenceT[n_, t_] :> ToPackedArray @ Table[RandomInstance[t], intOrRand @ n],
	
	BooleanT :> RandomChoice[{True,False}],

	StringT :> RandomChoice[{"A","B","C"}],

	IntegerT :> RandomInteger[{-10,10}],

	NaturalT :> RandomInteger[10],

	PosIntegerT | SizeT :> RandomInteger[{1,10}],

	IndexIntegerT[max_Integer] :> RandomInteger[{1, max}],

	IndexIntegerT[_] :> RandomInteger[{1,10}],

	EnumT[e_List] :> RandomChoice[e],

	EnumT[_] :> RandomChoice[{"A","B","C"}],

	EitherT[e_List] :> %[RandomChoice[e]],

	ListT[NaturalT, t_] :> %[ListT[RandomInteger[5], t]],

	ListT[n_Integer, t_] :> Table[%[t], n],

	list_List :> Map[%, list],

	StructT[rules_List] :> Map[%, Association[rules]],

	RuleT[k_, v_] :> Rule[%[k], %[v]],

	AssocT[k_, v_] :> Association @ Table[
		Rule[%[k], %[v]], RandomInteger[6]
	],

	ImageT[size_List, color_String] :> RandomImage[1, size /. SizeT :> RandomInteger[{1,32}], ColorSpace -> color],
	ImageT[SizeListT[2], space_] :> %[ImageT[{SizeT, SizeT}, space]],
	ImageT[size_, ColorSpaceT] :> %[ImageT[size, RandomChoice[{"RGB", "Grayscale"}]]],

	Defaulting[t_, _] :> %[t],

	Nullable[t_] :> If[RandomReal[] < 0.5, %[t], None],

	TypeT :> RandomType[],

	enc_NetEncoder :> EncoderRandomInstance[enc],
	dec_NetDecoder :> %[CoderType[dec]],

	TypeAliasP :> %[ResolveAlias[type]]
];


PackageExport["CreateRandomData"]

NNSetUsage @ "
CreateRandomData[net$] creates training data to feed to the inputs of a fully specified net.
CreateRandomData[itype$ -> otype$] creates a training dataset association with 'Input' data of itype$ and 'Output' data of otype$.
CreateRandomData[<|'port$i'->'type$i',$$|>] creates a training dataset with the given ports and types.
CreateRandomData[types$, n$] creates data of length n$.
* The default length is 1024.
* CreateRandomData is implicitly used by RandomNetTrain.
* The types can be one of the following:
| n$ | a vector of size n$ |
| {d$1,d$2,$$} | a tensor of the given dimensions |
| NetEncoder[$$] | data suitable for the given encoder |
| type$ | any internal type (TensorT, etc) |
"

CreateRandomData[types_, length_:1024] := CatchFailure @ Scope[
	$n = length; SeedRandom[1234];
	types = toDataTypes[types];
	Map[makeColumn, types]	
];

General::badrdspec = "Spec `` should be a net, an association of port to type, or type -> type.";
General::badrdtype = "`` is not a valid type.";

toDataTypes[net_ ? ValidNetQ] := IMap[toFullType, Inputs[net]];
toDataTypes[assoc_Association] := IMap[toFullType, assoc];
toDataTypes[in_ -> out_] := toDataTypes[<|"Input" -> in, "Output" -> out|>];
toDataTypes[spec_] := ThrowFailure["badrdspec", spec];

General::nfsrdtype = "Type `` for port `` is not fully specified.";
toFullType[k_, t_] := Scope[
	res = ToT[t, NetEncoder];
	If[FailureQ[res], ThrowFailure["badrdtype", t]];
	If[!FullySpecifiedTypeQ[res], ThrowFailure["nfsrdtype", t, k]];
	res
];

makePattern[id_] := BlockRandom[SeedRandom[id]; RandomInteger[{2,16}, $n]];

makeColumn[$Failed] := ThrowRawFailure[$Failed];
makeColumn[t_] := Table[RandomInstance[t], $n];
makeColumn[t:TensorT[{LengthVar[id_], ___}, _]] := Table[makeColumn[t /. _LengthVar -> len], {len, makePattern[id]}];


PackageExport["RandomNetTrain"]

NNSetUsage @ "
RandomNetTrain[net$] creates a training net for net$, and trains it with randomly generated data for a short time. 
RandomNetTrain[net$, opts$$] takes options (the same as for NetTrain).
RandomNetTrain[net$, lspec$, opts$$] takes a loss specification (as in for NetTrain).
* The net$ should be fully specified.
* The final, trained net is returned.
* The default training time is 0.2 seconds, with no training progress, and training data of length 31.
* RandomNetTrain uses CreateRandomData to create the training data.
* This is for bulk testing, basically - it should always return something that is a ValidNetQ.
"

RandomNetTrain::badloss = "Couldn't make loss network.";
RandomNetTrain::baddata = "Couldn't make random data: ``.";
RandomNetTrain::badtrain = "Training failed.";
RandomNetTrain::badeval = "Evaluation of trained net failed.";
RandomNetTrain::badbatcheval = "Batch evaluation of trained net failed.";

RandomNetTrain[net_, opts___Rule] := RandomNetTrain[net, Automatic, opts];

RandomNetTrain[net_, loss:Except[_Rule], opts___Rule] := CatchFailure @ Scope[
	tnet = NetAttachLoss[net, loss];
	If[FailureQ[tnet], ReturnFailed["badloss"]];
	trainData = CreateRandomData[tnet, 31];
	If[FailureQ[trainData], ReturnFailed["baddata", MsgForm[trainData]]];
	trained = NetTrain[net, trainData, opts, BatchSize -> 5, MaxTrainingRounds -> Quantity[0.2, "Seconds"], TrainingProgressReporting -> None];
	If[FailureQ[trained], ReturnFailed["badtrain"]];
	testData = CreateRandomData[Inputs @ trained, 15];
	If[FailureQ[testData], ReturnFailed["baddata", MsgForm[testData]]];
	res = trained @ Map[First, testData];
	If[FailureQ[res], ReturnFailed["badeval"]];
	res = trained @ testData;
	If[FailureQ[res], ReturnFailed["badbatcheval"]];
	trained
];