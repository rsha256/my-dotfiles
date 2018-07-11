Package["NeuralNetworks`"]



PackageScope["ImportTorchExpression"]

(* see https://github.com/bshillingford/python-torchfile/blob/master/torchfile.py *)

$TNIL = 0
$TNUMBER = 1
$TSTRING = 2
$TTABLE = 3
$TTORCH = 4
$TBOOLEAN = 5
$TFUNCTION = 6
$TRECURFUNCTION = 8
$TLEGACYRECURFUNCTION = 7

$objects = <||>;
$graphNodes = <||>;

ImportTorchExpression[path_String] := CatchFailure @ Scope[
	If[!FileExistsQ[path], ThrowFailure["noopen", path]];
	$stream = OpenRead[path, BinaryFormat -> True];
	If[FailureQ[$stream], ThrowFailure["noopen", path]];
	$objects = <||>;
	$graphNodes = <||>;
	result = readObject[];
	Close[$stream];
	{result, postProcNodes[$graphNodes]}
];


postProcNodes[nodes_] := Scope[
	nodes = Select[nodes, MatchQ @ TorchObject["nngraph.Node", _]];
	nodes = nodes //. TorchBackReference[id_] :> RuleCondition @ $objects[id];
	Lookup[#, "module", KeyDrop[#, "mapindex"]]& /@ nodes[[All, 2, "data"]]
]

readObject[] := Scope[
	type = readInt[];
	Switch[type,
		$TNIL, None,
		$TNUMBER, tryRound @ readDouble[],
		$TBOOLEAN, readBoolean[],
		$TSTRING, readString[],
		$TTABLE, readCached @ readTable[readInt[]],
		$TTORCH, readCached @ readTorch[],
		$TFUNCTION | $TLEGACYRECURFUNCTION | $TRECURFUNCTION, readCached @ readFunction[],
		_, Panic["UnknownTypeID", "`` is not a valid type id.", type]
	]
];

tryRound[r_] := If[Round[r] == r, Round[r], r];

SetHoldFirst[readCached];
 readCached[expr_] := readCached[expr, readInt[]];

PackageScope["TorchBackReference"]

readstack = {};
readCached[expr_, key_] := Block[
	{readstack = Append[readstack, key]},
	If[key > 4096, Panic["BadCacheKey", "`` is too large.", key]];
	If[KeyExistsQ[$objects, key] || Count[readstack, key] > 1, 
		res = TorchBackReference[key],
		res = CacheTo[$objects, key, expr];
	];
	If[MatchQ[res, TorchObject["nngraph.Node", _]],
		$graphNodes[key] = res;
		res = TorchGraphNode[key];
	];
	res
];

PackageScope["TorchGraphNode"]


readInt[] := BinaryRead[$stream, "UnsignedInteger32"]

readDouble[] := BinaryRead[$stream, "Real64"];

readBoolean[] := readInt[] === 1;

readBytes[n_] := BinaryReadList[$stream, "UnsignedInteger8", n];

readString[] := FromCharacterCode @ readBytes @ readInt[];

Clear[readTable];

readTable[size_] := Scope[
	If[size === 0, Return[<||>]];
	If[size > 8192, Panic["TooLarge", "Size of table `` appears too large.", res, StreamPosition[$stream]-4]]; 
	table = Table[
		key = readObject[];
		val = readObject[];
		key -> val
	, 
		size
	];
	If[VectorQ[keys = Keys[table], MachineIntegerQ] && Last[keys] === size && keys === Range[size],
		Values[table],
		Association[table]
	]
];

readLong[] := BinaryRead[$stream, "Integer64"];

readLongArray[n_] := BinaryReadList[$stream, "Integer64", n];

readTorch[] := Scope[
	version = readBytes @ readInt[];
	(* do something with version here *)
	className = readString[];
	reader = Lookup[$readers, className, readGeneric[className]];
	reader[]
];

PackageScope["TorchObject"]

$ignoredTorchKeys = {
	"gradInput", "train", "output", "_type", 
	"gradWeight", "gradBias", "benchmarked",
	"inplace"
};

readGeneric[name_][] := TorchObject[StringTrim[name, "cud"], KeyDrop[readObject[], $ignoredTorchKeys]];

$readers = Association[
	"torch.ByteTensor" -> readTensor["UnsignedInteger8"],
	"torch.CharTensor" -> readTensor["Integer8"],
	"torch.ShortTensor" -> readTensor["Integer16"],
	"torch.IntTensor" -> readTensor["Integer32"],
	"torch.LongTensor" -> readTensor["Integer64"],
	"torch.FloatTensor" -> readTensor["Real32"],
	"torch.DoubleTensor" -> readTensor["Real64"],
	
	"torch.CudaByteTensor" -> readTensor["UnsignedInteger8"],
	"torch.CudaCharTensor" -> readTensor["Integer8"],
	"torch.CudaShortTensor" -> readTensor["Integer16"],
	"torch.CudaIntTensor" -> readTensor["Integer32"],
	"torch.CudaTensor" -> readTensor["Real32"],
	"torch.CudaDoubleTensor" -> readTensor["Real64"],
	
	"torch.ByteStorage" -> readStorage["UnsignedInteger8"],
	"torch.CharStorage" -> readStorage["Integer8"],
	"torch.ShortStorage" -> readStorage["Integer16"],
	"torch.IntStorage" -> readStorage["Integer32"],
	"torch.LongStorage" -> readStorage["Integer64"],
	"torch.FloatStorage" -> readStorage["Real32"],
	"torch.DoubleStorage" -> readStorage["Real64"],

	"torch.CudaByteStorage" -> readStorage["UnsignedInteger8"],
	"torch.CudaCharStorage" -> readStorage["Integer8"],
	"torch.CudaShortStorage" -> readStorage["Integer16"],
	"torch.CudaIntStorage" -> readStorage["Integer32"],
	"torch.CudaStorage" -> readStorage["Real32"],
	"torch.CudaDoubleStorage" -> readStorage["Real64"]
];


PackageScope["EmptyTorchArray"]
PackageScope["SymbolicTorchArray"]

readTensor[type_][] := Scope[
	rank = readInt[];
	size = readLongArray[rank];
	stride = readLongArray[rank];
	offset = readLong[] - 1;
	storage = readObject[];
	total = Times @@ size;
	Which[
		total < 1,
			EmptyTorchArray[],
		storage === None,
			SymbolicTorchArray[size],
		True,
			sliced = Take[Normal[resolveBackrefs @ storage], {offset + 1, offset + total}];
			reshaped = ArrayReshape[sliced, size];
			RawArray[type, reshaped]
	]
];

 resolveBackrefs[expr_] := 
 	ReplaceRepeated[expr, TorchBackReference[id_] :> RuleCondition @ $objects[id]];

 readStorage[type_][] := Scope[
	size = readLong[];
	UnsafeQuietCheck[
		RawArray[type, BinaryReadList[$stream, type, size]],
		RawArray[type, ConstantArray[0, size]]
	]
];

PackageScope["TorchFunction"]

readFunction[] := Scope[
	size = readInt[];
	TorchFunction[
		readBytes @ size,
		readObject[]
	]
];


PackageExport["ImportTorchNet"]

PackageScope["$LastUnsupportedTorchNode"]

General::unsupnode = "Node not supported:\n``";

NNSetUsage @ "
ImportTorchNet['file$'] imports a torch network.
* ImportTorchNet effectively calls ImportTorchExpression, followed by ConvertTorchExpression.
* A limited subset of Torch functionality is currently supported.
* Always verify that the WL network has identical or near-identical output for a given input.
* Unlike Torch, Mathematica does not allow tables (for us, associations) to be produced or \
consumed by layers. This means that certain idioms, such as ConcatTable + CAddTable, must be \
explicitly recognized and converted. New idioms can be added to $chainrules.
* To add support for a layer, add a downvalue of the form to ImportExport/TorchFormat.m:
  torch['nn.name', arg1_, arg2_, $$ := EquivalentLayer[$$] 
"

ImportTorchNet[file_String] := CatchFailure @ Scope[
	res = ImportTorchExpression[file];
	If[FailureQ[res], Return[res]];
	{expr, nodes} = res;
	Off[General::stop];
	output = ConvertTorchExpression[expr, nodes];
	On[General::stop];
	output
];


PackageExport["ConvertTorchExpression"]

ConvertTorchExpression[expr_, nodes_:<||>] := CatchFailure @ Scope[
	$nodes = nodes;
	parse[expr]
];

Clear[torch, parse];

torch /: SetDelayed[torch[Verbatim[Pattern][sym_, assoc_ ? AssociationQ], patts:RepeatedNull[_Pattern|_Optional]], body_] :=
	With[{p = Pattern},
		makeParseDef[
			p[KEY, Alternatives @@ Keys[assoc]], {patts}, 
			With[{sym = Lookup[assoc, KEY]}, checkNet[body, KEY]]
		]
	];

torch /: SetDelayed[torch[name_String, patts:RepeatedNull[_Pattern|_Optional]], body_] := 
	makeParseDef[name, {patts}, checkNet[body, name]];

ClearAll[makeParseDef];

SetHoldRest[makeParseDef];
makeParseDef[name_, patts_, body_] /; FreeQ[Hold[patts], Optional] := 
	SetDelayed @@ With[
		{assocPatt = KeyValuePattern[toPatt /@ patts]}, 
		Hold[parse[TorchObject[name, assocPatt]], body]
	];

makeParseDef[name_, patts_, body_] := With[
	{defaults = getDefaults[Hold @ patts]},
	parse[TorchObject[name, assoc_Association]] := parseWithOptionals[TorchObject[name, Join[defaults, assoc]]]; 
	SetDelayed @@ With[
		{assocPatt = KeyValuePattern[toPatt /@ patts]}, 
		Hold[parseWithOptionals[TorchObject[name, assocPatt]], body]
	]
];

makeParseDef[___] := Print["Unrecognized torch[] definition"];

parseWithOptionals[node_] := failNode[node];

getDefaults[patts_] := Association @ DeepCases[patts, 
	Verbatim[Optional][Verbatim[Pattern][sym_, _], value_] :> Rule[symbolToKey[sym], value]
];

toPatt[Verbatim[Optional][lhs_, _]] := toPatt[lhs];
toPatt[p:Verbatim[Pattern][sym_, Verbatim[Blank[]]]] := symbolToKey[sym] -> p;

symbolToKey[sym_] := StringReplace[SymbolName[sym], "$" -> "_"];

parse[net_NetGraph] := net;

parse[node_] := failNode[node];

failNode[node_] := ThrowFailure["unsupnode", 
	($LastUnsupportedTorchNode = node) /. ra_RawArray :> RuleCondition["RawArray"[Dimensions[ra]]]];

toRA[ra_RawArray] := ra;
toRA[_] := None;

General::badtorchnode = "Error creating layer from torch node ``. The failure was ``."
checkNet[net_, name_] := If[ValidNetQ[net], net, ThrowFailure["badtorchnode", name, net]];

torchPattern[name_, modules_] := TorchObject[name, KeyValuePattern[{"modules" -> modules}]];

ParallelSummationGraph[modules_, final_] := Scope[
	n = Length[modules];
	NetGraph[
		Append[parse /@ modules, final],
		Thread[Range[n] -> n+1]
	]
];


(* these definitions actually expand into a slightly more complicated downvalue. the
names of the patterns like 'weight_' are significant, they match a parameter in the
torch association with the same name. 

also, if you use the form type:assoc as the argument, the downvalue will match any
of the keys, and 'type' will be bound to the corresponding value. see activations
for examples.
*)

(**** SIMPLE LAYERS ****)
(* https://github.com/torch/nn/blob/master/doc/simple.md#nn.simplelayers.dok *)
torch["nn.Linear", weight_, bias_] := DotPlusLayer["Weights" -> weight, "Biases" ->  toRA[bias]];
torch["nn.Dropout", p_] := DropoutLayer[p];
torch["nn.Add", scalar_] := ElementwiseLayer[# + scalar&];
torch["nn.Mul", scalar_] := Elementwiselayer[# * scalar&];
torch["nn.Power", scalar_] := ElementwiseLayer[Power[#, scalar]&];
torch["nn.Reshape", size_, batchMode_] := If[!batchMode, Panic[], ReshapeLayer[size]];
torch["nn.Transpose", permutations_] := TransposeLayer[Rule @@@ permutations];
torch["nn.MM", transA_, transB_] := If[transA || transB, Panic[], DotLayer[]];
torch["nn.View", size_] := ReshapeLayer[Normal@size];
	
(* nn.Min,Max,Mean,Sum require AggregateLayer *)


(**** ACTIVATION LAYERS ****)
(* https://github.com/torch/nn/blob/master/doc/transfer.md#nn.transfer.dok *)
$unaryFunctions = <|
	"nn.Tanh" -> Tanh, "nn.Sigmoid" -> LogisticSigmoid, "nn.ReLU" -> Ramp, "nn.Abs" -> Abs,
	"nn.Exp" -> Exp, "nn.Log" -> Log, "nn.Sqrt" -> Sqrt, "nn.Square" -> (Power[#, 2]&)
|>;
torch[type:$unaryFunctions] := ElementwiseLayer[type];
torch["nn.SoftMax"] := SoftmaxLayer[];


(**** CONTAINERS ****)
(* https://github.com/torch/nn/blob/master/doc/containers.md#nn.Containers *)
torch["nn.Sequential", modules_] := NetChain @ Map[parse, modules //. $chainrules];

$chainrules = {
	{L___, torchPattern["nn.ConcatTable", modules_], TorchObject["nn.CAddTable", <||>], R___} :>
		{L, checkNet[ParallelSummationGraph[modules, TotalLayer[]], "ConcatTable + CAddTable"], R},

	{L___, torchPattern["nn.ConcatTable", modules_], TorchObject["nn.JoinTable", KeyValuePattern["dimension" -> d_]], R___} :>
		{L, checkNet[ParallelSummationGraph[modules, CatenateLayer[d-1]], "ConcatTable + JoinTable"], R}

	(* we could add a case for multiple padding layers *)
};

torch["nn.Concat", modules_] := 
	ParallelSummationGraph[modules, CatenateLayer[]];

torch["nn.Identity"] := ElementwiseLayer[#&];

torch["nn.CAddTable"] := TotalLayer[];

(* "nn.gModule" contains two graph.Graph objects, one for the forward graph and one for the backward graph. *)
torch["nn.gModule", fg_, nInputs_] := parse[fg]

torch["graph.Graph", nodes_, edges_] := Block[{$brCount, $brMapping},
	$iCount = $oCount = $brCount = 0;
	$brMapping = <||>; 
	nodes2 = Map[parseNode, Values[nodes]];
	edges2 = Map[parseEdge, Values[edges]] /. i_Integer :> (1 + $brCount - i);
	initn = InitialVertices[edges2];
	If[Length[initn] > 1, edges2 = Join[edges2, Table[NetPort["Input" <> IntegerString[i]] -> initn[[i]], {i, Length[initn]}]]];
	NetGraph[Reverse @ nodes2, Reverse @ edges2]
];

parseEdge[TorchObject["graph.Edge", <|"to" -> TorchBackReference[to_], "from" -> TorchBackReference[from_]|>]] := Block[{f,t}, 
	{f, t} = Lookup[$brMapping, {from, to}, Panic["CouldNotResolveEdge"]];
	If[f === Null || t === Null, Nothing, f -> t]
];

parseEdge[_Integer] := Nothing; (* <- weird bidirectional table they use *)
parseEdge[e_] := Panic["CouldNotParseEdge", "Torch edge `` is unparseable.", e];

(* torch graphs treat ports and vertices the same. if its a port, then store it in the brmapping
as a string, so it shows up a string in the edge list, and suppress it as a node entirely *)
parseNode[TorchBackReference[br_]] := Scope[
	res = parseNode2[$nodes[br]];
	If[res === Null, 
		$brMapping[br] = Null; Nothing,
		$brMapping[br] = ++$brCount; res
	]
];

parseNode[_Integer] := Nothing; (* <- weird bidirectional table they use *)
parseNode[e_] := Panic["CouldNotParseNode", "Torch node `` is unparseable.", e];

parseNode2[_Association] := Null;
parseNode2[other_] := parse @ other;


(**** CONVOLUTIONS ****)
(* https://github.com/torch/nn/blob/master/doc/convolution.md#nn.convlayers.dok *)

torch["nn.SpatialReflectionPadding", pad$l_, pad$t_, pad$r_, pad$b_] :=
	PaddingLayer[{{0, 0}, {pad$b, pad$t}, {pad$l, pad$r}}, "Padding" -> "Reflected"];

torch["nn.SpatialReplicationPadding", pad$l_, pad$t_, pad$r_, pad$b_] :=
	PaddingLayer[{{0, 0}, {pad$b, pad$t}, {pad$l, pad$r}}, "Padding" -> "Fixed"];

torch["nn.SpatialBatchNormalization", running$mean_, running$var_, weight_, bias_, eps_, momentum_] :=
	BatchNormalizationLayer[
		"MovingMean" -> running$mean, "MovingVariance" -> running$var,
		"Gamma" -> weight, "Beta" -> bias, "Epsilon" -> eps, "Momentum" -> momentum
	];

torch["nn.SpatialUpSamplingNearest", outputSize_, scale$factor_, inputSize_] := (
	ResizeLayer[
		{Scaled[scale$factor], Scaled[scale$factor]}, 
		"Resampling" -> "Nearest",
		"Input" -> Round @ Rest @ Normal @ inputSize, "Output" -> Round @ Rest @ Normal @ outputSize
	]
);

torch["nn.PReLU", weight_] :=
	ParametricRampLayer["Slope" -> weight];

torch["nn.SpatialConvolution", dW_, dH_, weight_, bias_:None, padH_:0, padW_:0] :=
	ConvolutionLayer["Stride" -> {dH, dW}, "Weights" -> weight, "Biases" -> bias, "PaddingSize" -> {padH, padW}];

torch["nn.SpatialFullConvolution", dW_, dH_, weight_, bias_:None, adjw_:0, adjh_:0] :=
	DeconvolutionLayer[
		"Stride" -> {dH, dW}, "Weights" -> weight, "Biases" -> bias, 
		"PaddingSize" -> {adjh, adjw}
	];

torch["nn.SpatialDropout", p_] := DropoutLayer[p];

torch["nn.Padding", dim_, pad_, nInputDim_, value_, index_] := 
	makePaddingLayer[dim, nInputDim, pad, value, index];

makePaddingLayer[dim_, rank_, pad_, value_, 1|-1] := Scope[
	spec = ConstantArray[0, {rank, 2}];
	spec = ReplacePart[spec, dim -> If[pad < 0, {-pad, 0}, {0, pad}]];
	PaddingLayer[spec, "Padding" -> value]
];
	
torch["nn.SpatialDilatedConvolution", dW_, dH_, dilationW_, dilationH_, weight_, bias_] :=
	ConvolutionLayer["Stride" -> {dH, dW}, "Dilation" -> {dilationH, dilationW}, "Weights" -> weight, "Biases" -> bias];

$poolingTypes = <|"nn.SpatialMaxPooling" -> Max, "nn.SpatialAveragePooling" -> Mean|>;
torch[type:$poolingTypes, kW_, kH_, dW_, dH_, padW_, padH_] :=
	PoolingLayer[{kH, kW}, "Stride" -> {dH, dW}, "PaddingSize" -> {padH, padW}, "Function" -> type];


(**** LOSS FUNCTIONS ****)
(* https://github.com/torch/nn/blob/master/doc/criterion.md *)
torch["nn.CrossEntropyCriterion"] := NetChain[{SoftmaxLayer[], CrossEntropyLossLayer[]}];
torch["nn.AbsCriterion"] := MeanAbsoluteLossLayer[];
torch["nn.MSECriterion"] := MeanSquaredLossLayer[];


DefineCustomBoxes[TorchObject, 
	TorchObject[type_String, assoc_Association] :> TorchBoxes[type, assoc]
]

smallQ[e_] := ByteCount[e] < 100;

TorchBoxes[type_, assoc_] /; $InEntryFormatting := 
	StyleBox[type, Italic];

TorchBoxes[type_, assoc_] := Scope[
	e = fmtEntries[assoc];
	short = Select[e, smallQ];
	long = Discard[e, smallQ];
	OptimizedArrangeSummaryBox[TorchObject, StyleBox[type, Bold], long, short]
];





