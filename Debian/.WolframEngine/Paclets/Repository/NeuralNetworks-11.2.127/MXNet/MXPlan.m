Package["NeuralNetworks`"]



PackageScope["GetDynamicDimensionInfo"]

GetDynamicDimensionInfo[LengthVar[id_Integer]] := 
	Lookup[{$MaxSeqLens, $SeqLenNode}, id, Panic["MissingDynamicDimInfo"]];

GetDynamicDimensionInfo[n_Integer] := {n, None};


PackageScope["ToMaxLengths"]

ToMaxLengths[expr_] := expr /. LengthVar[id_] :> RuleCondition @ Lookup[$MaxSeqLens, id, Panic["MissingDynamicDimMaxLen"]];


PackageScope["ContributingNodeFilter"]

ContributingNodeFilter[net_, ports_] := Scope[
	graph = PortConnectivityGraph[net];
	contributing = VertexInComponent[graph, ports];
	all = VertexList[graph];
	Data`UnorderedAssociation[
		Thread[all -> False],
		Thread[contributing -> True]
	]
];

findInvalidJSON[result_] := Scope[
	pos = FirstPosition[result, Except[_List | _String | _Integer | _Association | _Real], Heads -> False];
	Panic["InvalidJSON", "Non-JSON value: ``.", Extract[result, Drop[pos, -2]]];
];

PackageExport["ToMXJSON"]

NNSetUsage @ "
ToMXJSON[net$] produces an association containing JSON and other data for the given net. 
The following options can be given:
| 'MaxLength' | maximum length to use for all 'varying' dimensions |
| 'TMode' | True for training mode, False for test mode |
| 'NameRewriter' | A function to rewrite the op names |
The returned association contains the following keys:
| \"JSON\" | the JSON string |
| \"Inputs\" | an association of input port name to size |
| \"Arguments\" | an association of internal argument names to RawArrays/sizes |
| \"Outputs\" | an association of output port name to MX name |
"

Options[ToMXJSON] = {
	"MaxLength" -> 8,
	"TMode" -> False,
	"NameRewriter" -> None
};

ToMXJSON[net_NetP, OptionsPattern[]] := Scope[
	UnpackOptions[maxLength, tMode, nameRewriter];
	Block[{$DebugMode = True, $nameRewriter = nameRewriter},
		meta = {All, MakeSeqIDLens[net, maxLength], tMode};
		plan = iToMXPlan[net, meta];
	];
	If[FailureQ[plan], Return[plan]];
	plan = First @ plan;
	UnpackAssociation[plan, inputArrays, argumentArrays, fixedArrays, specialArrays];
	dims = jsonDims /@ Join[inputArrays, argumentArrays, fixedArrays, specialArrays];
	Association[
		"JSON" -> $LastMXJSON,
		(*"ArrayMappings" -> None,*)
		"ArrayDimensions" -> dims,
		"Arrays" -> Select[Join[argumentArrays, fixedArrays], RawArrayQ]
		(*	"AuxilliaryArrays", "AuxilliaryArrayMapping"
		}]*)
	]
];

jsonDims[NDSequenceArray[dims_List, _]] := dims;
jsonDims[dims_List] := dims;
jsonDims[ra_RawArray] := Dimensions[ra];


PackageExport["ToMXSymbol"]

ToMXSymbol[args___] := Scope[
	plan = ToMXPlan[args];
	If[FailureQ[plan], Return[plan]];
	plan["Symbol"]
];


PackageExport["ToMXPlan"]
PackageExport["MXPlan"]

NNSetUsage @ "
ToMXPlan[net$, {outputs$, seqlens$, tmode$}] produces an MXPlan[$$] that contains enough 
information to instantiate MXExecutors via ToMXExecutor when given a BatchSize and target device.
ToMXPlan[net$] targets all outputs and uses dummy seqlens if necessary.
* outputs$ is either All or a list of output ports that should be included in the DAG.
* outputs$ can also contain NetPath[$$] for internal ports to 'tap off'. 
* seqlens$ is an association whose keys are sequence IDs and values are their corresponding lengths. 
* tmode$ is True if dropout etc should be turned on.
* ToMXPlan unrolls recurrent nets, and so maximum lengths must be communicated using seqlens$.
* The default spec computes all output ports, and uses 4 as the max sequence length (for debugging).
* See the usage of MXPlan for more information.
"

ToMXPlan[net_, meta_] := Cached[iToMXPlan, net, meta];

(* for debugging *)
ToMXPlan[net_, n_Integer] := ToMXPlan[net, {All, MakeSeqIDLens[net, n], False}];
ToMXPlan[net_, n_Integer, trainq_ ? BooleanQ] := ToMXPlan[net, {All, MakeSeqIDLens[net, n], trainq}];
ToMXPlan[net_] := iToMXPlan[net, {All, MakeSeqIDLens[net, 4], False}];

PackageExport["$MXNetVersion"]

(* this needs to be kept up to date *)
$MXNetVersion = 905;

PackageScope["MakeSeqIDLens"]
MakeSeqIDLens[net_, sz_] := 
	Association @ Cases[
		Inputs[net], 
		(HoldPattern[NetEncoder][_, _, VarSequenceP[id_]] | VarSequenceP[id_]) :> 
			Rule[id, sz]
	];

$nameRewriter = None;
iToMXPlan[net_NetP, {outputs_, seqlens_, tmode_}] := Timed @ Scope[
	
	If[!ConcreteNetQ[net], 
		Panic["UnspecifiedNet", "Can't compile plan for unspecified net. Unspecified part is ``", FindUnspecifiedPath[net]]
	];

	$ArgumentArrays = <||>;			(* mangled name -> RawArray *)
	$InputArrays = <||>;			(* input name -> dimensions *)
	$UsedStates = <||>;             (* used states NetPaths -> True *)
	$SpecialArrays = <||>;			(* seq node name -> dims *)
	$PathToNode = <||>;				(* net path -> node id *)
	$SeqLenNames = <||>;			(* seq id -> seq node name *)
	$NodeDimensions = <||>;			(* node id -> dimensions *)
	$AuxArrays = <||>;				(* mangled name -> RawArrays *)
	$HiddenOutputs = <||>;          (* mapping from mxnode to mx output name *)
	$NextNode = 0; 					(* next ID to use *)
	$MaxSeqLens = seqlens;			(* seq id -> max len *)
	$SeqLenNode = <||>;				(* seq ids -> node id *)
	$SeqCounter = 0;				(* counts consecutive seq ids *)
	$PortFilter = True&;			(* to skip nodes not contributing to desired outputs *)
	$FixedArrays = <||>;			(* unique name to RawArrays *)
	$BatchifiedCache = <||>;	(* cache batch-broadcast arrays from non-batched node inputs *)
	$FlattenedNodeCache = <||>;		(* cache reshapes used for Premapped inputs *)
	$ZeroArrayCache = <||>;			(* cache arrays just instantiated for their sizes *)
	$LocalBatchSize = BatchSize;	(* makes broadcasts choose the right batchsize within NetMaps etc *)
	$IsFreeState = <||>;			(* set to True for states set up by SowStateNode *)
	$TMode = tmode;					(* whether things like dropout should apply *)
	$OpTypes = Bag[];		(* duplicate info, but used for fast graph optimizations *)

	$outputs = Outputs[net];
	If[outputs === All, 
		onames = Keys @ $outputs;
		oports = Thread @ NetPath["Outputs", onames];
	,
		onames = outputs;
		oports = Replace[outputs, {
			s_String :> NetPath["Outputs", s],
			NetPort[np_] :> ToNetPath[net, np]
		}, {1}];
		If[!MatchQ[oports, {NetPath[__String]..}], Panic["InvalidOutputSpec", "`` is not a valid output spec.", outputs]];
		$PortFilter = ContributingNodeFilter[net, oports];
	];

	TimeLabel["MXScan"];
	$currentNet = net; 
	CollectTo[{$NodeList, $ArgNodeIDs, $HiddenOutputNodes}, 
		KeyValueScan[SowInputNode, StripCoders @ Inputs[net]];
		KeyValueScan[SowStateNode, InteriorStates[net]];
		MXScan[net];
		doGraphFusion[];
		heads = Flatten @ Map[GetFinalNode[#, net @@ #]&, oports];
	];
	If[$HiddenOutputNodes =!= {},
		heads = Join[heads, $HiddenOutputNodes];
	];

	If[$nameRewriter =!= None, 
		$NodeList = MapAt[$nameRewriter, $NodeList, {All, "name"}]
	];

	result = Association[
		"nodes" -> $NodeList,
		"arg_nodes" -> $ArgNodeIDs,
		"heads" -> (List @@@ heads),
		"attrs" -> <|"mxnet_version" -> {"int", $MXNetVersion}|>
	];

	TimeLabel["ToJSON"];
	jsonstr = assocToJSONString[result];

	TimeLabel["CreateSymbol"];
	symbol = CatchFailure[General, MXSymbolFromJSON[jsonstr]];
	If[Head[symbol] =!= MXSymbol, 
		$LastMXJSON ^= jsonstr; 
		ThrowFailure["mxneterr"]
	];

	mxouts = MXSymbolOutputs[symbol];

	If[$HiddenOutputNodes =!= {},
		{hiddennames, mxouts} = TakeDrop[mxouts, -Length[$HiddenOutputNodes]];
		$HiddenOutputs = AssociationThread[$HiddenOutputNodes, hiddennames];
	];

	outputArrays = Association @ MapThread[
		#1 -> toOutputSpec[#1, #2]&, 
		{onames, mxouts}
	];

	kmap = If[$nameRewriter === None, Identity, KeyMap[$nameRewriter]];
		
	TimeLabel["BuildAssoc"];
	MXPlan @ Association[
		"Symbol" -> symbol, 
		"ArgumentArrays" -> kmap @ $ArgumentArrays, 
		"InputArrays" -> $InputArrays,
		"SpecialArrays" -> $SpecialArrays, 
		"FixedArrays" -> $FixedArrays,
		"AuxilliaryArrays" -> DeleteCases[kmap @ $AuxArrays, None], (* remove any nullable arrays *)
		"OutputArrays" -> outputArrays
	]
];

toOPortName[NetPath["Outputs", out_]] := out;
toOPortName[path_] := NetPort @ FromNetPath[path];

assocToJSONString[assoc_] := Module[{jsonstr},
	jsonstr = UnsafeQuietCheck @ WriteRawJSONString[assoc];
	If[FailureQ[jsonstr], $LastMXJSON = assoc; findInvalidJSON[assoc]];
	jsonstr
];

assocToJSONString[assoc_] /; $DebugMode := Module[{jsonstr},
	jsonstr = Quiet @ WriteRawJSONString[assoc, "Compact" -> 2];
	If[FailureQ[jsonstr], $LastMXJSON = assoc; findInvalidJSON[assoc]];
	jsonstr = StringReplace[jsonstr, s:("arg_nodes\":[" ~~ Shortest[___] ~~ "]") :> StringReplace[s, Whitespace -> ""]];
	$LastMXJSON = jsonstr;
	$LastMXData = assoc;
	jsonstr
]

GetFinalNode[path_, type_] := Scope[
	path = ResolvePath[path];
	node = GetPackedNode[path];
	If[MatchQ[path, NetPath["Inputs", _]], SowPlusScalar[node, "0."], node]
];

SowStateNode[name_, path_] := Scope[
	mangled = MXManglePathWithSeq[path];
	$PathToNode[path] = SowNullNode[mangled];
	$IsFreeState[path] = True;
	$SpecialArrays[mangled] = toInputSpec[$currentNet @@ path];
];

PackageScope["$LastMXJSON"]
PackageScope["$LastMXData"]

$LastMXJSON = None;


NNSetUsage @ "
MXPlan[<|$$|>] represents an unrolled network, along with information about its weights, inputs, outputs, etc.
* MXPlans abstract the batchsize and target device away.
* The abstract batchsize is represented as the symbol BatchSize within dimension specifications.
* Use ToMXNetExecutor to turn a plan into an executor.
* Names of weights and layers are mangled from their position in the original net association by riffling '.'.
* Inputs or outputs corresponding to variable-length sequences use a compound NDSequenceArray specification.
* An MXPlan[$$] object contains the following fields:
| 'Symbol' | the MXSymbol that implements the computation DAG |
| 'ArgumentArrays' | assoc of learned arrays (as RawArrays) |
| 'InputArrays' | assoc of input port names to dimensions |
| 'SpecialArrays' | special arrays used for seq tracking |
| 'OutputArrays' | assoc of output port names to dimensions |
| 'AuxilliaryArrays' | dimensions of aux arrays |
| 'AuxilliaryArrayMapping' | mapping from MXNet's auto-generated names to mangled names |

"

SowInputNode[name_, type_List] := Scope[
	nodes = Table[SowNullNode[name <> "#" <> IntegerString[i]], {i, Length[type]}];
	$PathToNode[NetPath["Inputs", name]] = nodes;
	$InputArrays[name] = NDCompoundArray[toInputSpec /@ type];
];

SowInputNode[name_, type_] := Scope[
	node = SowNullNode[name]; 
	$PathToNode[NetPath["Inputs", name]] = node;
	$InputArrays[name] = toInputSpec[type];
];

(* TODO: one of these might go away when we seperate inputs from arguments in MXSymbolBind *)

toOutputSpec[name_, mxname_] := Match[
	StripCoders @ $outputs[name],
	VarSequenceP[id_] :> NDSequenceArray[mxname, GetOutSeqLenName[id]],
	mxname
];

Clear[toInputSpec];

toInputSpec[enc_NetEncoder] := toInputSpec[CoderType[enc]];

toInputSpec[_IndexIntegerT] := {BatchSize};

toInputSpec[ScalarT] := {BatchSize};

toInputSpec[TensorT[{LengthVar[id_], rest___}, RealT | _IndexIntegerT]] := 
	NDSequenceArray[
		ToList[
			BatchSize, Lookup[$MaxSeqLens, id, Panic["NoMaxLen"]], 
			getDims @ TensorT[{rest}]
		], 
		GetSeqLenName[id]
	];

toInputSpec[t_TensorT] := Prepend[getDims @ t, BatchSize];

toInputSpec[t_] := Panic["InvalidInput", "Don't know how to get shape of ``.", t];

General::mxneterr = "An error occured while compiling the net. Please contact technical support."

getDims[enc_NetEncoder] := getDims[CoderType[enc]];

getDims[type_] := Scope[
	dims = TDimensions[type];
	If[VectorQ[dims, IntegerQ], dims,
		SoftPanic["getDims", "Can't determine dims of ``.", type];
		ThrowFailure["mxneterr"];
	];
	dims
];

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)

doGraphFusion[] := Scope[
	types = BagPart[$OpTypes, All];
	nll = Flatten @ Position[types, "NLLLoss"];
	Scan[tryBackwardRewrite[$FuseNLL, {}], IndicesOf[types, "NLLLoss"]];
];

tryBackwardRewrite[dispatch_, init_][id_] := Scope[
	state = init; id--;
	While[IntegerQ[id],
		assoc = BagPart[$NodeList, id+1]; 
		f = Lookup[dispatch, assoc["op"], Break[]];
		state = f[assoc, state, id];
		If[state === $Break, Break[]];
		id = UnsafeQuietCheck @ assoc[["inputs", 1, 1]];
	];
];

$FuseNLL = <|
	"NLLLoss" -> Function[{#3 -> #1}],
	"transpose" -> Function[Prepend[#2, #op -> #attr]],
	"SwapAxis" -> Function[Prepend[#2, #op -> #attr]],
	"Reshape" -> Function[Prepend[#2, #op -> #attr]],
	"SoftmaxActivation" -> doNLLFuse
|>;

doNLLFuse[softmax_, {nodes___, id_ -> nll_}, _] := Scope[
	celoss = nll;
	celoss[["op"]] = "CrossEntropyLoss";
	celoss[["inputs", 1]] = cloneNodes[First @ softmax["inputs"], {nodes}];
	BagPart[$NodeList, id+1] = celoss;
	$Break
];

cloneNodes[node_, {}] := node;
cloneNodes[node_, list_List] := List @@ Fold[cloneNode, MXNode @@ node, list];
cloneNode[in_, op_ -> params__] := SowNode[op, in, Sequence @@ Normal[params]];

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)

MXPlan[assoc_Association][arg1_, rest___] :=
	assoc[arg1][[rest]];

DefineCustomBoxes[MXPlan,
	mx:MXPlan[assoc_Association] :> MakeMXPlanBoxes[mx]
]

MakeMXPlanBoxes[plan:MXPlan[assoc_Association]] := Scope[
	UnpackAssociation[assoc, symbol];
	plot = With[{symbol = symbol}, Dynamic[MXSymbolPlot[symbol], TrackedSymbols :> {}]];
	items = makeItem[#, assoc[#]]& /@ {"InputArrays", "OutputArrays", "SpecialArrays", "FixedArrays", "ArgumentArrays", "AuxilliaryArrays"};
	BoxForm`ArrangeSummaryBox[
		MXPlan, plan, None, items,
		List @ Framed[plot, FrameStyle -> LightGray, ImageMargins -> {{0, 0}, {0, 5}}],
		StandardForm
	]	
];

makeItem[name_, value_] := 
	BoxForm`MakeSummaryItem[{
		Pane[name <> ": ", {100, Automatic}], 
		itemGrid[value]
	}, StandardForm];

itemGrid[<||>] := Pane[Style["none", Gray], {Automatic,15}, Alignment -> Bottom, BaselinePosition -> Baseline];
itemGrid[a_] := If[Length[a] > 4, OpenerView[{Row[{Length[a], " items"}], niceGrid[a]}], niceGrid[a]];

SowNullNode[name_] := Scope[
	BagInsert[$NodeList, Association[
		"op" -> "null", "name" -> name, 
		"attr" -> <||>, "inputs" -> {}
	]];
	mxid = $NextNode++;
	BagInsert[$ArgNodeIDs, mxid];
	BagInsert[$OpTypes, Null];
	MXNode[mxid, 0, 0]
];


(* if this output seqid is also an input seqid, then it'll have been cached 
in $SeqLenNames from toInputSpec, so find it there. 
Otherwise, it's a derived seqid, and so we need to lookup up its mxname via $HiddenOutputs *)

GetOutSeqLenName[seqid_] := 
	Lookup[$SeqLenNames, seqid, Lookup[$HiddenOutputs, $SeqLenNode[seqid], Panic["MissingOutSeqID"]]];

GetSeqLenName[seqid_] := CacheTo[$SeqLenNames, seqid, sowSeqLenNode[seqid]];

sowSeqLenNode[seqid_] := Scope[
	name = "seq_" <> IntegerString[$SeqCounter++];
	mxid = SowNullNode[name];
	$SeqLenNode[seqid] = mxid;
	$SpecialArrays[name] = {BatchSize};
	name
];

PackageScope["SowDerivedSequenceLengthNode"]

SowDerivedSequenceLengthNode[lenNode_MXNode, LengthVar[newseqid_], n_] := Scope[
	oldseqid = FirstPosition[$SeqLenNode, lenNode][[1, 1]];
	oldmxid = $SeqLenNode[oldseqid];
	newmxid = SowNode["_PlusScalar", oldmxid, "scalar" -> N[n]];
	$SeqLenNode[newseqid] = newmxid;
	$MaxSeqLens[newseqid] = $MaxSeqLens[oldseqid] + n;
	BagInsert[$HiddenOutputNodes, newmxid];
	newmxid
]

PackageExport["MXPlanPlot"]

NNSetUsage @ "
MXPlanPlot[MXPlan[$$]] shows the DAG graph of an MXPlan.
MXPlanPlot[net$] makes a plan for net$ and returns its graph.
MXPlanPlot[net$,i$] specifies the seq len to use (defaults to 3).
MXPlanPlot[net$,i$,$tmode] specifies whether training mode should be used.
"

MXPlanPlot[HoldPattern @ MXPlan[assoc_]] := MXSymbolPlot[assoc["Symbol"], "VertexLabels" -> {Placed["ID",Above], Placed["Type", Below]}];
MXPlanPlot[net_NetP, n_:3, tmode_:False] := MXPlanPlot @ iToMXPlan[net, {All, MakeSeqIDLens[net, n], tmode}];

PackageScope["$TMode"]

$TMode = False;


PackageScope["NodeDimensions"]

(* needed for sowing mx ops like Dot that need known dims. *)
NodeDimensions[id_] := 
	Lookup[
		$NodeDimensions, Key[id],
		Panic["NodeDimensions", "Couldn't find dimensions of node ``, `` are available.", id, Keys @ $NodeDimensions]
	];


PackageScope["SowInnerNet"]

SowInnerNet[inputs_, subpath_, net_, batchmodifier_:1] := 
	Block[{$path = Join[$path, NetPath @@ subpath], $LocalBatchSize = $LocalBatchSize * batchmodifier},
		Block[{$path = Append[$path, "Inputs"]}, 
			KeyValueScan[
				Set[$PathToNode[Append[$path, #1]], #2]&,
				inputs
			];
		];
		MXScan[net];
		Association @ Map[
			Last[#] -> PathNode[#]&,
			OutputPorts[net]
		]
	];


PackageScope["SowNode"]

$internalid = 0;

SowNode[op_, inputs_, params___Rule] := Scope[
	inputIDs = List @@@ PathNode[ToList @ inputs];
	$op = op; params = Association[params];
	BagInsert[$NodeList, Association @ {
		"op" -> op, 
		"name" -> MXManglePathWithSeq[$path] <> "$" <> IntegerString[$internalid++],
		"attr" -> $mirrorf[Map[hyperstr, params]], 
		"inputs" -> inputIDs
	}];
	BagInsert[$OpTypes, op];
	MXNode[$NextNode++, 0, 0]
];

SowNode[args___] := Panic["InvalidArgs", "Invalid args provided to SowNode: ``.", {args}];

(* these are used for internal nodes, don't have to be too fancy *)
hyperstr[e_] := Match[e,
	_String :> e,
	True :> "true",
	False :> "false",
	i_Integer :> intString[i],
	i:{__Integer} :> writeIntList[i],
	r_Real :> If[MachineRealQ[r] && Abs[r] < 3.402823466*^38, HighPrecisionDoubleString[r], ThrowFailure["invfpval"]],
	Panic["InvalidSowNode", "Cannot serialize `` in SowNode[``, ``, ``] for MXNetFunction of ``.", e, $op, inputIDs, 
		Normal[params$], NetPathForm @ $path];
];

General::invfpval = "The net contained a floating point value that was too large to express with 32-bit machine precision."


PackageScope["MirrorStage"]
PackageExport["$MemoryMirroring"]

If[!ValueQ[$MemoryMirroring], $MemoryMirroring = True];

SetHoldFirst[MirrorStage];
$mirrorf = Identity;
MirrorStage[e_] := If[TrueQ[$MemoryMirroring], Block[{$mirrorf = Append["__force_mirroring__" -> "True"]}, e], e];


PackageScope["SowFixedArray"]
(* Used for non-batch 'ROM' arrays needed to do certain hacky things like
use spatial transformer as an affine resizer. *)

SowFixedArray[data_RawArray] := Scope[
	name = "fixedarray" <> IntegerString[Length[$FixedArrays]];
	$FixedArrays[name] = data;
	node = SowNullNode[name];
	$NodeDimensions[node] = Dimensions[data];
	node
];


PackageScope["SowBatchified"]

SowBatchified[node_] := 
	CacheTo[$BatchifiedCache, {node, $LocalBatchSize}, sowBatchified[node]];

sowBatchified[node_] := Scope[
	dims = Lookup[$NodeDimensions, node, Panic["UnknownBroadcastSourceDims"]];
	zeros = SowZeroArray[dims];
	SowNode["broadcast_add", {node, zeros}]
]


PackageScope["SowZeroArray"]

SowZeroArray[dims_] := 
	CacheTo[$ZeroArrayCache, {dims, $LocalBatchSize}, sowZeroArray[dims]];

sowZeroArray[dims_] := Scope[
	(* %HACK remove this LocalBatchSize thing in favor of MXExecutorData 
	having embedded batchsize (and other length vars) *)
	factor = $LocalBatchSize / BatchSize; 
	If[factor =!= 1, dims = Prepend[dims, factor]];
	name = "zeroarray" <> IntegerString[Length[$ZeroArrayCache]];
	$SpecialArrays[name] = Prepend[dims, BatchSize];
	node = SowNullNode[name];
	If[factor =!= 1, node = SowFlatten1[node]];
	node
];


PackageScope["SowSourceFixup"]

SowSourceFixup[node_, dims_:{}] := SowNode["_plus", {node, SowZeroArray[dims]}];


PackageScope["MXManglePath"]

(* we preserve input ports so that we don't need a translation layer back and forth
from MXNet to top-level *)
MXManglePath[NetPath["Inputs", name_]] := name;

MXManglePath[NetPath[p___, i_Integer]] :=
	MXManglePath[NetPath[p]] <> "_output" <> IntegerString[i];

MXManglePath[NetPath[p___]] := StringJoin[".", Riffle[{p}, "."]];

MXManglePathWithSeq[path_] := $seqPathStr <> MXManglePath[path];


PackageScope["MXUnmanglePath"]

MXUnmanglePath[s_String] := NetPath @@ StringSplit[s, "."];


PackageScope["SowMXConnections"]

SowMXConnections[rules_] := Scope[
	prefixed = PrefixPorts[rules];
	AssociateTo[$UsedStates, Cases[prefixed, Rule[_, state:NetPath[___, "States", _]] :> Rule[state, True]]];
	AssociateTo[$PathToNode, prefixed]
];

PackageScope["PathNode"]
PackageScope["MXNode"]
PackageScope["NodeQ"]

GetPackedNode[path_] := PathNode[path] /. mn_MetaNode :> mn["Batchwise"];

(* this can recurse, as it makes doing connections easy. *)
SetAttributes[PathNode, Listable];
PathNode[n_MXNode] := n;
PathNode[n_NetPath] := PathNode @ Lookup[$PathToNode, n, Panic["NetPathNotFound", "`` not found. Available paths: ``", n, Keys[$PathToNode]]];
PathNode[e_] := e;

NodeQ[_MXNode] := True;
NodeQ[_] := False;

(* this chases NetPaths until they resolve, returning the last NetPath *)
ResolvePath[n_NetPath] := Match[
	$PathToNode[n],
	n2_NetPath :> %[n2],
	n
];

toOutputPath[name_] := Join[$path, NetPath["Outputs", name]];
toStatePath[name_] := Join[$path, NetPath["States", name]];
toInputPath[name_] := Join[$path, NetPath["Inputs", name]];


MXNode /: MakeBoxes[MXNode[i_Integer, j_Integer, k_Integer], StandardForm] := 
	FrameBox[RowBox[{i, ":", j, ":", k}], Background -> LightRed, BaseStyle -> {FontSize -> 10}, ContentPadding -> False]


PackageScope["GetInput"]
PackageScope["GetInputMetaNode"]

NNSetUsage @ "
GetInput['name'] is used within MXNetFunction bodies to get the input to the current layer named 'name'.
GetInput['name', 'type'] controls what is returned, where 'type' is one of:
| 'Batchwise' | return an MXNode in which the 0th dim is batch dim (default) |
| 'Timewise' | return an MXNode in which 0th dim is sequence dim |
| 'Packed' | return an MXNode, which may be batchwise or timewise |
| 'Unpacked' | return a list of MXNodes |
"

GetInput[name_] := GetInput[name, "Batchwise"];

GetInputMetaNode[name_] := Scope[
	path = ResolvePath @ toInputPath[name];
	Replace[$PathToNode[path], mx_MXNode :> createMetaNode[path, mx]]
];

GetInput[name_, "Batchwise"] := ReplaceAll[
	PathNode @ ResolvePath @ toInputPath[name],
	mn_MetaNode :> mn["Batchwise"]
];

GetInput[name_, form_String] := GetInputMetaNode[name][form];

createMetaNode[path_, node_] := ModuleScope[
	dims = TDimensions[$currentNet @@ path];
	{lnode, maxlen} = Match[
		First[dims, 1],
		LengthVar[id_] :> {$SeqLenNode[id], $MaxSeqLens[id]},
		n_Integer :> {None, n}
	];
	timewise = unpacked = None; batchwise = node;
	meta = With[{lnode = lnode, maxlen = maxlen, dims = If[dims === {}, None, Rest @ dims]},
		MetaNode[batchwise, timewise, unpacked, lnode, maxlen, dims]
	];
	$PathToNode[path] = meta;
	meta
]


PackageScope["GetInputDims"]

GetInputDims[name_] := TDimensions[inputs[name]];
(* ^ only used by Dot, currently, cleanest that way apparently *)


PackageScope["GetState"]

(* we have to tile interior states by the local batch factor 
(states that aren't hooked up and were thus propogated outward 
by containers + operators).
to detect these, we check $IsFreeState, and if set, we broadcast
by the batch factor

*)
GetState[name_] := Scope[
	path = toStatePath[name];
	node = PathNode[path];
	If[$IsFreeState[path] && (factor = $LocalBatchSize/BatchSize) =!= 1, 
		node = SowFlatten1 @ SowBroadcastAt[node, 1, factor];
	];
	node
];


PackageScope["GetArray"]

GetArray[name_] := PathNode @ Join[$path, NetPath["Arrays", name]];


PackageScope["MXDo"]

(* we have to name nodes in repeated subgraphs differently for each 
iteration, that is what MXDo does *)

SetAttributes[MXDo, HoldAll];

$seqPathStr = "";
$seqPath = {};

MXDo[expr_, ispec_] := Block[
	{$seqPath = Append[$seqPath, 0], i = 0, $seqPathStr},
	Do[$seqPath[[-1]] = IntegerString[++i] <> ":"; $seqPathStr = StringJoin[$seqPath]; expr, ispec]
];


PackageScope["SetPassthroughOutput"]

SetPassthroughOutput[oname_, iname_] := 
	$PathToNode[toOutputPath[oname]] = toInputPath[iname];


PackageScope["SetOutput"]

SetOutput[name_, out_] :=
	$PathToNode[toOutputPath[name]] = checkNode @ out;

checkNode[n_MXNode | n_MetaNode] := n;
checkNode[e_] := Panic["SetOutput", "MXNetFunction tried to set an ouput to ``.", e];


PackageScope["SetState"]

(* The fact that lengths behind MXNodes are tracked separately from MXNodes
is a big pain and shows up here, where we have to know the length of the
'spec' -- currently not a problem because spec is always unpacked and hence
carries the metadata, but when we use custom CUDA layers to do some RNN stuff
then 'spec' will just be an MXNode and we won't know where to find its length *)
SetAttributes[SetState, HoldRest];
SetState[name_, spec_] := Scope[
	path = toStatePath[name];
	If[$UsedStates[path], $PathToNode[path] = spec]
];


PackageScope["WriterFallthrough"]	
PackageScope["SowDefaultNode"]

DeclareMethod[MXScan, MXScanLayer, MXScanContainer];

MXScanContainer[assoc_] := (
	SowMXConnections[assoc["Edges"]]; 
	ScanFields["Nodes", MXScan, assoc];
);

MXScanLayer[assoc_] := Scope[
	If[!$PortFilter[$path], Return[]];
	type = assoc["Type"];
	UnpackAssociation[assoc, type, outputs, inputs, parameters, arrays];
	UnpackAssociation[$LayerData[type], auxArrays, writer, mxinfo:"MXNet"];
	arrayIDs = If[EmptyQ[arrays], {}, dumpArrays[]];
	If[writer === None, 
		MXWriteDefault[]
	,
		$internalid ^= 0; 
		Catch[
			writer[parameters],
			WriterFallthroughTag
		]
	];
];

MXWriteDefault[] := Scope[
	If[!$PortFilter[$path], Return[Nothing]];
	$id = First @ SowDefaultNode[];
	$i = 0;
	Block[{$path = Append[$path, "Outputs"]},
		KeyValueScan[sowOutputPort, outputs];
	]
];

SowDefaultNode[] := Scope[
	op = mxinfo["Name"];
	inputIDs = List @@@ Join[Flatten[GetInput /@ Keys[inputs]], arrayIDs];
	paramWriter = toLayerWriter[type];
	paramStrings = Association @ paramWriter[parameters, arrays];
	BagInsert[$NodeList, Association[
		"op" -> op, "name" -> MXManglePathWithSeq[$path], 
		"attr" -> paramStrings, "inputs" -> inputIDs
	]];
	BagInsert[$OpTypes, op];
	MXNode[$NextNode++, 0, 0]
];

sowOutputPort[port_, type_] := 
	$PathToNode[Append[$path, port]] = MXNode[$id, $i++, 0];

sowOutputPort[port_, l_List] := Scope[
	port = Append[$path, port];
	Do[
		$PathToNode[Append[port, $i++]] = MXNode[$id, $i, 0],
		Length[l]
	];
];

dumpArrays[] := Block[
	{$path = Join[$path, NetPath["Arrays", Null]]},
	KeyValueMap[sowArrayNode, arrays]
];

WriterFallthrough[] := (MXWriteDefault[]; Throw[Null, WriterFallthroughTag]);

sowArrayNode[name_, None] := (
	parameters[name] = None;
	Nothing
)

sowArrayNode[name_, arr_] := Scope[
	$path[[-1]] = name;
	If[!$PortFilter[$path], Return[Nothing]];
	If[KeyExistsQ[$PathToNode, $path], 
		(* this caching kicks in in the case of NetOperators
		TODO: support aux nodes as well *)
		parameters[name] = $PathToNode[$path];
		Return[$PathToNode[$path]]
	];
	mxname = MXManglePath[$path];
	mxid = SowNullNode[mxname];
	aos = toArrayOrShape[arr];
	If[MemberQ[auxArrays, name],
		$AuxArrays[mxname] = aos,
		$ArgumentArrays[mxname] = aos
	];
	$PathToNode[$path] = mxid;
	parameters[name] = mxid;
	$NodeDimensions[mxid] = If[RawArrayQ[arr], Dimensions[arr], TDimensions[arr]];
	mxid
];

toArrayOrShape[ra_RawArray] := ra;
toArrayOrShape[t_] := TDimensions[t, Panic["BadArray", "``", t]];

PackageScope["toLayerWriter"]
PackageScope["toValueWriter"]

Clear[toLayerWriter];

toLayerWriter[name_] := Memoized @ makeLayerReaderWriter[name, "Writer"];

toFieldWriter[param_, key_] := With[
	{writer = toValueWriter @ ALookup[$mxParamTypes, param]}, 
	key -> Quoted[writer[Slot[param]]]
];

IntP = SizeT | PosIntegerT | IntegerT | NaturalT;
toValueWriter[t_] := Match[t,
	ListT[_, IntP] :> writeIntList,
	SizeT|PosIntegerT :> IntegerString,
	IntegerT|NaturalT :> intString,
	ScalarT | _IntervalScalarT :> HighPrecisionDoubleString,
	BooleanT :> $ToBoolean,
	PoolingFunctionT :> $ToPooling,
	Defaulting[type_, _] :> %[type],
	Identity
];

intString[i_Integer] := If[Negative[i], "-" <> IntegerString[i], IntegerString[i]];
intString[_] := $Unreachable;

PackageScope["writeIntList"]

writeIntList[{a_, b_}] := StringJoin["(", intString[a], ", ", intString[b], ")"];
writeIntList[{a_, b_, c_}] := StringJoin["(", intString[a], ", ", intString[b], ", ",  intString[c], ")"];
writeIntList[list_] := StringRiffle[list, {"(", ", ", ")"}];

PackageScope["makeLayerReaderWriter"]
PackageScope["$mxParamTypes"]
PackageScope["$mxParamDefaults"]

makeLayerReaderWriter[name_, type_] := Scope[
	mxinfo = $LayerData[name, "MXNet"];
	$mxParamTypes = $LayerData[name, "Parameters"];
	$mxParamDefaults = $LayerData[name, "ParameterDefaults"];
	If[mxinfo === None, Return[{}&]];
	readerWriter = Select[FreeQ[$Failed]] @ KeyValueMap[
		If[type === "Writer", toFieldWriter, toFieldReader],
		Lookup[mxinfo, "Parameters", <||>]
	];
	If[mxinfo[type] =!= None,
		AppendTo[readerWriter, Quoted @@ mxinfo[type]]
	];	
	ReplaceAll[Compose[Function, readerWriter], Quoted[h_] :> h]
];

ToFrom[rules___] := {Association[rules], Association[Reverse[{rules}, 2]]};


PackageScope["$ToActivation"]
PackageScope["$FromActivation"]
PackageScope["$ToBoolean"]
PackageScope["$FromBoolean"]
PackageScope["$ToPooling"]
PackageScope["$FromPooling"]

{$ToBoolean, $FromBoolean} = ToFrom[True -> "True", False -> "False"];
$FromBoolean = Join[$FromBoolean, <|"true" -> True, "false" -> False|>];
{$ToPooling, $FromPooling} = ToFrom[Max -> "max", Mean -> "avg", Total -> "sum"];
{$ToActivation, $FromActivation} = ToFrom[Ramp -> "relu", Tanh -> "tanh", LogisticSigmoid -> "sigmoid", SoftRamp -> "softrelu"];




PackageScope["MXSandbox"]

NNSetUsage @ "
MXSandbox[{iname$1 -> otype$1,$$}, {oname$1 -> otype$1,$$}, expr$] simulates a piece of code that \
would otherwise go in an MXNetFunction definition, and return the MX-ready JSON output.
MXSandbox[inputs$, expr$] assumes expr$ will return a unique output.
* GetInput['name'] etc. will work in the body of expr.
* The type$i can be internal types or user-friendly type specs.
* For an example, try MXSandbox[{}, SowRandomInteger[{5, 5}]]
"

SetAttributes[{MXSandbox, MXSandboxPlot}, HoldAll];

MXSandbox[inputs_, expr_] := 
	MXSandbox[inputs, {"Output" -> RealTensorT}, SetOutput["Output", expr]];

MXSandbox[inputs_, outputs_, expr_] := Scope[

	$PathToNode = <||>;				(* net path -> node id *)
	$SeqLenNames = <||>;			(* seq id -> seq node name *)
	$HiddenOutputs = <||>;          (* mapping from mxnode to mx output name *)
	$NextNode = 0; 					(* next ID to use *)
	$MaxSeqLens = <||>;				(* seq id -> max len *)
	$SeqLenNode = <||>;				(* seq ids -> node id *)
	$SeqCounter = 0;				(* counts consecutive seq ids *)
	$FixedArrays = <||>;			(* unique name to RawArrays *)
	$BatchifiedCache = <||>;	(* cache batch-broadcast arrays from non-batched node inputs *)
	$FlattenedNodeCache = <||>;		(* cache reshapes used for Premapped inputs *)
	$ZeroArrayCache = <||>;			(* cache arrays just instantiated for their sizes *)
	$LocalBatchSize = BatchSize;	(* makes broadcasts choose the right batchsize within NetMaps etc *)
	$IsFreeState = <||>;			(* set to True for states set up by SowStateNode *)
	$TMode = False;					(* whether things like dropout should apply *)
	$OpTypes = Bag[];		(* duplicate info, but used for fast graph optimizations *)

	$currentNet = Association[
		"Inputs" -> Map[ToT, Association[inputs]], 
		"Outputs" -> Map[ToT, Association[outputs]]
	];
	$MaxSeqLens = MakeSeqIDLens[$currentNet, 4];

	CollectTo[{$NodeList, $ArgNodeIDs, $HiddenOutputNodes}, 
		KeyValueScan[SowInputNode, Inputs[$currentNet]];
		res = expr;
		doGraphFusion[];
		heads = Flatten @ Map[GetFinalNode[#, net @@ #]&, OutputPorts[$currentNet]];
	];

	If[$HiddenOutputNodes =!= {},
		heads = Join[heads, $HiddenOutputNodes];
	];

	result = Association[
		"nodes" -> $NodeList,
		"arg_nodes" -> $ArgNodeIDs,
		"heads" -> (List @@@ heads),
		"attrs" -> <|"mxnet_version" -> {"int", $MXNetVersion}|>
	];

	result
];

PackageScope["MXSandboxPlot"]

NNSetUsage @ "
MXSandboxPlot takes the same argument as MXSandbox, but produces a plot, like MXPlanPlot.
* For an example, try MXSandboxPlot[{}, SowRandomInteger[{5, 5}]]
"

MXSandboxPlot[args___] := MXJSONPlot @ MXSandbox[args];


PackageScope["CheckNotDynamic"]

CheckNotDynamic[layer_, t_, msg_:Automatic] :=
	If[!FreeQ[t, _LengthVar],
		FailValidation[layer, Replace[msg, Automatic :> "Layer does not support varying dimensions."]]
	]
