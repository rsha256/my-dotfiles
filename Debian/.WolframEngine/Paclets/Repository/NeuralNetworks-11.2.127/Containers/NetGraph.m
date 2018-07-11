Package["NeuralNetworks`"]


PackageExport["NetGraph"]

SetupGenericDispatch[NetGraph, False];

(ng_NetGraph ? System`Private`HoldEntryQ) := 
	UseMacros @ RuleCondition @ CatchFailureAsMessage[NetGraph, make[ng]];

SetHoldAll[make];

make[NetGraph[chain_NetChain]] := Scope[
	layers = GetNodes[chain];
	NetGraph[layers, Table[i -> (i+1), {i, Length[layers]-1}]]
];

make[NetGraph[layers_Association | layers_List, conns_List, rules___Rule]] := 
	toNetGraph[ToContainerLayers[layers], Flatten @ conns, {rules}];

make[ng:NetGraph[<|"Type" -> "Graph", ___|>]] :=
	UpgradeAndSealNet[ng];

make[ng:NetGraph[<|"Type" -> "Graph", ___|>, _Association]] :=
	MaybeDowngradeAndSealNet[ng];

make[ng_NetGraph] /; PatternFreeQ[Unevaluated[ng]] :=
	(CheckArgumentCount[ng, 2, 2]; $Failed);

make[_] := Fail;


PackageExport["NetParallel"]

NetParallel[layers___] := Scope[
	layers = Map[ToLayer, {layers}]; 
	len = Length[layers];
	NetGraph[Append[$Raw /@ layers, CatenateLayer[]], Thread[Range[len]->(len+1)]]
];


PackageScope["toNetGraph"]

NetGraph::acyclic = "The connectivity of layers within a NetGraph must be acyclic."
NetGraph::empty = "NetGraph objects must contain at least one vertex."

toNetGraph[nodes_Association, edges_List, rules_List] := Scope[
	$currentNodes = $nodes = nodes; $keys = Keys[$nodes];
	$currentNodeName = "node"; (* <- err reporting shared with NetChain *)

	If[Length[nodes] === 0, ThrowFailure["empty"]];

	(* mapping from layer input and output ports to their types *)
	$itypes = $otypes = $stypes = $istates = $inputs = $outputs = Association[];
	KeyValueScan[collectPortTypes, $nodes];

	(* canonicalize edges *)
	$alltypes = Join[$itypes, $otypes, $stypes];
	edges = ReplaceAll[edges, {NetPort[a_, b_] :> NetPort[{a,b}], NetPort[{a_}] :> NetPort[a]}];

	edges = toEdge /@ edges;

	(* find in and out ports that aren't bound *)
	ofree = Complement[Keys[$otypes], Flatten @ Values @ edges];
	ifree = Complement[Keys[$itypes], Flatten @ Keys @ edges];

	(* if these types clash that'll get picked up in the inference step *)
	AssociateTo[$inputs, KeyMap[Last] @ KeyTake[$itypes, ifree]];
	AssociateTo[$outputs, KeyMap[Last] @ KeyTake[$otypes, ofree]];

	(* add on connections to virtual ports for the unbound inputs and outputs *)
	edges = ToList[
		edges, 
		virtIn /@ ifree, 
		Flatten @ KeyValueMap[virtOut, GroupBy[ofree, Last]]
	];

	If[!DuplicateFreeQ[edges[[All, 1]]],
		edges = Flatten @ Map[handleDuplicateEdge, GatherBy[edges, First]];
	];

	(* sort the edges into topological order *)
	graph = NetPathGraph[edges];
	If[!AcyclicGraphQ[graph] || !LoopFreeGraphQ[graph], ThrowFailure["acyclic"]];

	(* ensure the nodes are in topological order, so that when we scan
	them to feed to MXNet a node can assume its inputs have already been scanned. *)
	nodeOrder = TopologicalSort @ graph;
	$nodes = KeySortBy[$nodes, -First[FirstPosition[nodeOrder, #]]&];
	$nodes = StripCoders[$nodes];

	(* handle custom type specifications *)
	scanTypeHints[rules, $inputs, $outputs];

	(* make sure that inputs that are used first come earlier. should not matter but
	a temporary workaround for a problem tracking the input vs label in NetTrain. *)
	$inputs = KeySortBy[$inputs, FirstPosition[edges, #]&];
	
	(* check that none of the inputs are ListTs, because those are pseudo objects that
	are an accounting trick rather than something that MXNet knows about *)
	CheckForTuplePorts[$inputs, NetGraph];

	(* build the association*)
	assoc = Association[{
		"Type" -> "Graph",
		"Inputs" -> $inputs, 
		"Outputs" -> $outputs,
		"Nodes" -> $nodes,
		"Edges" -> edges,
		If[$istates === <||>, Nothing, "InteriorStates" -> $istates]
	}];

	ConstructWithInference[NetGraph, assoc]
];

SetHoldRest[scanTypeHints];

scanTypeHints[{}, _, _] := Null;

scanTypeHints[rules_, inputs_, outputs_] := Scope[
	$parseinputhead = NetGraph;
	Scan[
		Match[#,
			(name_String -> spec_) :> Which[
				KeyExistsQ[inputs, name], 
					setType[inputs[name], ParseInputSpec, spec],
				KeyExistsQ[outputs, name],
					setType[outputs[name], ParseOutputSpec, spec],
				True,
					ThrowFailure["netinvgport", name]
			],
			(NetPath["Inputs", name_] -> spec_) :> 
				setType[inputs[name], ParseInputSpec, spec],
			(NetPath["Outputs", name_] -> spec_) :> 
				setType[outputs[name], ParseOutputSpec, spec],
			ThrowFailure["invtypehint", #]
		]&,
		rules
	];
];

NetGraph::invtypehint = "`` should be a rule of the form \"port\" -> type."

SetHoldFirst[setType];

setType[slot:(_[name_]), func_, spec_] := If[
	MissingQ[slot], ThrowFailure["netinvgport", name], 
	Set[slot, func[name, slot, spec]]
];

NetGraph::netinvgport = "`` is neither a valid input or output port for the given NetGraph.";


PackageScope["CheckForTuplePorts"]

General::notplprts = "Port `` expects a tuple of tensors; such ports are not currently supported as direct inputs or outputs for ``. "
CheckForTuplePorts[assoc_, head_] := 
	KeyValueMap[
		If[MatchQ[#2, _List | _ListT], ThrowFailure["notplprts", #1, head]]&,
		assoc
	];

Clear[handleDuplicateEdge];

handleDuplicateEdge[{rule_}] := rule;

handleDuplicateEdge[list_List] /; MatchQ[$itypes[list[[1,1]]], _ListT | _List] :=
	list[[1,1]] -> Flatten[list[[All, 2]]];

NetGraph::dupedges =  "Multiple vertices have been connected to the ``.";

handleDuplicateEdge[list_List] := If[
	True,
	ThrowFailure["dupedges", NetPathString @ list[[1,1]]]
];

handleDuplicateEdge[other_] := Panic[];

collectPortTypes[i_, assoc_] := (
	AssociateTo[$itypes, iosTypes[i, "Inputs", assoc]];
	AssociateTo[$otypes, iosTypes[i, "Outputs", assoc]];
	AssociateTo[$stypes, iosTypes[i, "States", assoc]];
	AssociateTo[$istates, interiorStateRules["Nodes"][i, assoc]];
);

iosTypes[i_, field_, layer_] := KeyValueMap[
	NetPath["Nodes", i, field, #1] -> #2&, 
	Lookup[layer, field, <||>]
]

PackageScope["interiorStateRules"]

interiorStateRules[prefix_][key_, assoc_] := Scope[
	states = InteriorStates[assoc];
	If[states === <||>, Return[{}]];
	name = If[IntStringQ[key], FromDigits[key], key];
	path = NetPath[prefix, key];
	KeyValueMap[Prepend[#1, name] -> Join[path, #2]&, states]
];


gfail[msg_, args___] := ThrowTaggedFailure["InvalidGraph", Evaluate["net" <> msg], args];

(* this takes care of remapping pure int keys (i.e. keys from a NetGraph
that used a list spec), as well as preserving port hints (i.e. Encoders and Decoders *)
makeSubNetGraph[oldnet_, nodes_, edges_] := Scope[
	{ledges, redges} = KeysValues[edges];
	{ipaths, opaths} = PortTypes[oldnet];
	hints = Join[
		Cases[ipaths, Rule[port_, type_] /; !FreeQ[redges, port] :> 
			Rule[port, $Raw[type]]],
		Cases[opaths, Rule[port_, type_] /; !FreeQ[ledges, port] :> 
			Rule[port, $Raw[type]]]
	];
	If[DigitStringKeysQ[oldnet["Nodes"]],
		{nodes, mapping} = RemapKeys[nodes];
		rule = NetPath["Nodes", node_, rest__] :> NetPath["Nodes", mapping @ node, rest];
		edges = edges /. rule;
		hints = hints /. rule;
	];
	edges = edges /. $DanglingPort -> NetPath["Inputs", "Input"];
	toNetGraph[nodes, $Raw /@ edges, hints]
];


PackageScope["NetPathGraph"]

NetPathGraph[edges_] := Graph[
	Flatten[Thread /@ edges][[All, All, 1;;2]], 
	VertexLabels -> "Name"
];


PackageScope["FullNetPathGraph"]

FullNetPathGraph[edges_, nodes_] := Scope[
	rules = Flatten[{
		Thread /@ edges,
		KeyValueMap[Function[
			node = NetPath["Nodes", #1];
			{Thread[node -> Thread[NetPath["Nodes", #1, "Inputs", InputNames[#2]]]], 
			 Thread[Thread[NetPath["Nodes", #1, "Outputs", OutputNames[#2]]] -> node]}], 
			nodes
		]
	}];
	Graph[rules, VertexLabels -> Placed["Name",Tooltip]]
];


typeMatchQ[s:Except[_List|_ListT], ListT[_, t_]] := typeMatchQ[s, t];
typeMatchQ[s_, t_] := (s === t) || !FailureQ[UnifyTypes[s, t]];

typeMatchTupleQ[a_, ListT[_, b_], _] := typeMatchQ[a, b];
typeMatchTupleQ[a_, list_List, i_] := i <= Length[list] && typeMatchQ[a, list[[i]]];
typeMatchTupleQ[_, _, _] := False;


(* when someone specifies 1 -> 2, we must lookup the first input of 1 and 
	the first output of 2 *)
toPortAuto[node_, type_] :=
	toPort[node, type, First @ Keys @ PartElse[$nodes, toVertex[node], type, vertexPanic[node]]];

(* check the port exists and canonicalize it *)
General::invport = "The `` does not exist.";
toPort[node_, type_, name_] := Scope[
	vert = toVertex[node];
	path = NetPath["Nodes", vert, type, name];
	(* check the provided path actually exists *)
	If[MissingQ[$nodes @@ Rest[path]],
		path2 = NetPath["Nodes", vert, "States", name];
		If[MissingQ[$nodes @@ Rest[path2]],
			ThrowFailure["invport", NetPathString @ path],
			path = path2;
		];
	];
	path
];
	
Clear[toVertex];
toVertex[n_ ? PositiveMachineIntegerQ] := Block[{ns = IntegerString[n]}, 
	If[KeyExistsQ[$nodes, ns], ns, UnsafeQuietCheck[$keys[[n]], vertexPanic[n]]]
];
toVertex[name_String /; KeyExistsQ[$nodes, name]] := name;
toVertex[spec_] := vertexPanic[spec];

NetGraph::invvertex = "The vertex `` does not exist.";
vertexPanic[spec_] := ThrowFailure[NetGraph::invvertex, spec];

toLPort[NetPort[{p:NetPathElemP, name_String}]] := toPort[p, "Outputs", name];
toLPort[p:NetPathElemP] := toPortAuto[p, "Outputs"];
toLPort[NetPort[name_String]] := toGraphPort[$otypes, NetPath["Inputs", name]];

NetGraph::invedgesrc = "`` is not a valid source for an edge."
toLPort[e_] := ThrowFailure[NetGraph::invedgesrc, e];

toRPort[NetPort[{p:NetPathElemP, name_String}]] := toPort[p, "Inputs", name];
toRPort[p:NetPathElemP] := toPortAuto[p, "Inputs"];
toRPort[NetPort[name_String]] := toGraphPort[$itypes, NetPath["Outputs", name]];

NetGraph::invedgedst = "`` is not a valid destination for an edge."
toRPort[e_] := ThrowFailure[NetGraph::invedgedst, e];

SetHoldAll[toGraphPort];
toGraphPort[typesym_, p_] := (
	(* TODO: do we even need typesym any more? 
	is itypes only distinguished from otypes for the purposes of taking Keys
	to know the dangling ports? we should then unify the types and separately
	collect the keys. *)
	If[!KeyExistsQ[typesym, p], typesym[p] = TypeT]; 
	If[!KeyExistsQ[$alltypes, p], $alltypes[p] = TypeT]; 
	p
);

Clear[toEdge];

toEdge[$Raw[edge_]] := (
	Cases[edge, Alternatives[
		NetPath["Outputs", name_] /; AppendTo[$outputs, name -> TypeT],
 		NetPath["Inputs", name_] /; AppendTo[$inputs, name -> TypeT]],
		Infinity];
	edge
);

threadedEdgeQ[sources_, node_] := Scope[
	inputs = node["Inputs"];
	AssociationQ[inputs] && Length[inputs] == Length[sources]
]

(* {1, 2} -> LossLayer, where LossLayer has inputs "Input", "Target" *)
toEdge[ins_List -> out:NetPathElemP] /; threadedEdgeQ[ins, $nodes[[out]]] := 
	Sequence @@ MapThread[toEdge[#1 -> NetPort[{out, #2}]]&, {ins, InputNames[$nodes[[out]]]}];

toEdge[in_ -> outs_List] := Sequence @@ Map[toEdge[in -> #]&, outs];

(* a -> b -> c chain *)
toEdge[in_ -> (mid_ -> out_)] := Sequence[
	toEdge[in -> mid],
	toEdge[mid -> out]
];

(* {1, 2} -> CatenateLayer *)
toEdge[ins_List -> out_] := Scope[
	opath = toRPort[out]; otype = $alltypes[opath]; $i = 1;
	ipaths = Table[
		ipath = toLPort[in]; itype = $alltypes[ipath];
		If[!typeMatchTupleQ[itype, otype, $i++],
			edgeTypeError[ipath, opath, itype, otype]
		]; 
		If[MatchQ[ipath, NetPath["Inputs", _]], $inputs[Last[ipath]] ^= stripMulti @ otype]; 
		ipath
	,
		{in, ins}
	];
	opath -> ipaths
];

(* plain case *)
toEdge[in_ -> out_] := Scope[
	ipath = toLPort[in];  itype = $alltypes[ipath]; 
	opath = toRPort[out]; otype = $alltypes[opath];
	If[!typeMatchQ[itype, otype],
		edgeTypeError[ipath, opath, itype, otype]
	];
	If[MatchQ[ipath, NetPath["Inputs", _]],   $inputs[Last[ipath]] ^= stripMulti @ otype]; 
	If[MatchQ[opath, NetPath["Outputs", _]], $outputs[Last[opath]] ^= itype]; 
	opath -> ipath
];

stripMulti[ListT[_, t_]] := t;
stripMulti[t_] := t;

General::invedge = "`` is not a valid edge specification.";
toEdge[e_] := ThrowFailure[NetGraph::invedge, e];

General::ninctyp = "Incompatible types for `` (``) and `` (``).";
General::ninctyp2 = "Incompatible types for output of ``, ``, and input to ``, ``; `` is not compatible with ``, respectively.";

PackageScope["edgeTypeError"]
PackageScope["$currentNodes"]
PackageScope["$currentNodeName"]

$currentNodeName = "node";
(* also used by NetChain *)

edgeTypeError[NetPath["Nodes", snode_, "Outputs", "Output"], NetPath["Nodes", dnode_, "Inputs", "Input"], stype_, dtype_] := (
	ThrowFailure["ninctyp2", pnameform @ snode, MsgForm @ $currentNodes @ snode, pnameform @ dnode, MsgForm @ $currentNodes @ dnode, MsgForm @ stype, MsgForm @ dtype];
);

pnameform[s_String] := $currentNodeName <> " " <> If[IntStringQ[s], s, MsgForm[s]];

edgeTypeError[spath_, dpath_, stype_, dtype_] := 
	ThrowFailure["ninctyp", MsgForm[spath], MsgForm[stype], MsgForm[dpath], MsgForm[dtype]];


virtOut[name_, {p_NetPath}] := NetPath["Outputs", name] -> p;

(* make a unique output port for each clashing dangling output *)
virtOut[name_, list_] := Scope[
	(* this removes the original clashing port from the outputs var in toNetGraph *)
	KeyDropFrom[$outputs, name]; 
	Table[
		newName = name <> IntegerString[i];
		If[KeyExistsQ[$outputs, newName], ThrowFailure["multoutclash", name, newName]];
		$outputs[newName] = $otypes[list[[i]]];
		toGraphPort[$itypes, NetPath["Outputs", newName] -> list[[i]]]
	, 
		{i, Length[list]}
	]
];

General::multoutclash = "Cannot use integer suffix to automatically disambiguate multiple dangling output ports with name `` from each other because of existing port with name ``. Please specify names for these output ports manually.";

virtIn[ns:NetPath[__, name_]] := 
	ns -> NetPath["Inputs", name];

uniqueLabelling[list_] :=
	Values @ Sort @ Flatten @ KeyValueMap[labelGroup, PositionIndex[list]];

labelGroup[name_, {ind_}] := ind -> name;
labelGroup[name_, inds_List] := 
	Table[inds[[i]] -> name <> IntegerString[i], {i, Length[inds]}];

Language`SetMutationHandler[NetGraph, netGraphHandler];

SetHoldAllComplete[netGraphHandler];

netGraphHandler[Set[_Symbol, _]] := Language`MutationFallthrough;
netGraphHandler[Set[sym_Symbol[[pos_]], newlayer_]] := Scope[
	res = ReplaceLayer[sym, pos -> newlayer];
	If[FailureQ[res], res, sym ^= res]
];


PackageExport["ReplaceLayer"]

ReplaceLayer[HoldPattern[NetGraph[assoc_Association, meta_]], pos_ -> newlayer_] := CatchFailureAsMessage @ Scope[
	If[!NetLayerQ[newlayer], ReturnFailed[]];
	newlayer = NData[newlayer];
	nodes = assoc["Nodes"];
	Which[
		StringQ[pos],		
			If[KeyExistsQ[nodes, pos],
				nodes[[pos]] = newlayer,
				AppendTo[nodes, pos -> newlayer];
			],
		IntegerQ[pos],
			If[1 <= Abs[pos] <= Length[nodes],
				nodes[[pos]] = newlayer,
				ThrowFailure[NetGraph::oorpart, pos];
			],
		True,
			ThrowFailure[NetGraph::badpart, pos];
	];
	assoc["Nodes"] = nodes;
	ConstructWithInference[NetGraph, assoc, meta]
];


DefineCustomBoxes[NetGraph, 
	NetGraph[assoc_Association, _Association] ? System`Private`HoldNoEntryQ :> formatNetGraph[assoc]
];

formatNetGraph[assoc_Association] :=
	MakeCustomHeadBox["NetGraph", netGraphPlot[assoc], Top -> Scaled[3]];

NetGraph /: MakeBoxes[
	HoldPattern[NetGraph[assoc_Association, meta_Association] ? System`Private`HoldNoEntryQ],
	TraditionalForm
] := ToBoxes @ Block[{$NetGraphInteractivity = False},
	If[$LowercaseTraditionalForm, decapitalizeStrings, Identity] @ netGraphPlot[assoc, True]
];

Format[HoldPattern[NetGraph[assoc_Association, _Association]] ? System`Private`HoldNoEntryQ, OutputForm] := 
	StringJoin[
		"NetChain[<", 
		IntegerString @ Length @ assoc["Nodes"], ">, <", 
		IntegerString @ Length @ assoc["Edges"], ">]"
	];

(* we don't want chains and graphs to get a vertex legend label in trad mode *)
vpathType[NetPath["Nodes", id_]] := Replace[
	SummaryForm[#, $tradq] -> #Type& @ nodes[id],
	("Subnetwork" -> t_) :> (Null -> t)
];

vpathType[NetPath[_, name_]] := Null -> Null;

NetTypeColor[name_String] := Lookup[$NetGraphVertexColors, name, $NetGraphVertexDefaultColor];

NetTypeColor[other_] := Null;

$Red = Hue[0, 0.7, 0.9];
$Green = Hue[0.22, 1, 0.6];
$Yellow = Hue[0.13, 0.8, 0.85];

PackageExport["$NetGraphVertexColors"]
PackageExport["$NetGraphVertexDefaultColor"]

$NetGraphVertexDefaultColor = Gray;

$NetGraphVertexColors = <|
	"Graph" -> 				Black,
	"Chain" -> 				Black,
	"Linear" -> 			GrayLevel[0.4],
	"Softmax" -> 			RGBColor[0.50, 0.32, 0.99],
	"CrossEntropyLoss" -> 	Cyan,
	"MeanAbsoluteLoss" -> 	Lighter[Magenta,.2],
	"MeanSquaredLoss" ->	Lighter[Magenta,.4],
	"Elementwise" -> 		RGBColor[0.29, 0.29, 0.83],
	"Threading" -> 			RGBColor[0.01, 0.54, 0.67],
	"Flatten" -> 			RGBColor[1., 0.4, 0.],
	"Summation" -> 			RGBColor[1., 0.81, 0.],
	"Total" -> 				Brown,
	"Transpose" ->			RGBColor[0.82, 0.58, 0],
	"Reshape" -> 			GrayLevel[0.8],
	"Replicate" ->			RGBColor[0.82, 0.74, 0],
	"Catenate" -> 			RGBColor[1., 0.700, 0.31],
	"Part" -> 				RGBColor[1., 0.37, 0.31],
	"Aggregation" -> 		RGBColor[0, 0.47, 0.38],
	"Convolution" ->		Hue[0.61, 0.75, 1],
	"Pooling" -> 			Darker[$Green, 0.05],
	"Deconvolution" -> 		Darker[$Green, 0.25],
	"BatchNormalization" ->			Blend[{GrayLevel[0.8], Red}, .1],
	"InstanceNormalization"->		Blend[{GrayLevel[0.8], Red}, .2],
	"LocalResponseNormalization"->	Blend[{GrayLevel[0.8], Red}, .3],
	"ImageAugmentation" -> 			Blend[{GrayLevel[0.8], Red}, .45],
	"SpatialTransformation"-> RGBColor[0.86, 0.19, 0],
	"Dropout" -> 			Blend[{GrayLevel[0.8], Red}, .8],
	"LongShortTermMemory"-> Purple,
	"GatedRecurrent" -> 	Lighter[Purple, .2],
	"BasicRecurrent" -> 	Lighter[Purple, .4],
	"SequenceAttention" ->	RGBColor[0.78, 0, 0.56],
	"SequenceMost" -> 		RGBColor[0, 0.66, 0.68],
	"SequenceRest" -> 		RGBColor[0.24, 0.77, 0.56],
	"SequenceLast" -> 		RGBColor[0.49, 0.87, 0.45],
	"SequenceReverse" -> 	RGBColor[0.73, 1., 0.33],
	"NetMap" ->				RGBColor[0.240, 0.277, 0.583],
	"NetFold" -> 			RGBColor[0.43, 0.458, 0.687],
	"NetNest" -> 			RGBColor[0.62, 0.639, 0.792],
	"NetPairEmbedding" -> 	RGBColor[0.81, 0.819, 0.896],
	"Embedding" -> 			GrayLevel[0.6],
	"Dot" ->				RGBColor[0.78, 0.64, 0.98],
	"ConstantTimes" ->		RGBColor[0.987854, 0.15693, 0.536385],
	"ConstantPlus" ->		RGBColor[0.987854, 0.31693, 0.536385],
	"ConstantArray" ->		RGBColor[0.987854, 0.42693, 0.536385]
|>;

PackageExport["$NetGraphInteractivity"]

$NetGraphInteractivity := $NetInteractivity;

PackageScope["netGraphPlot"]

Clear[netGraphPlot];
netGraphPlot[assoc_Association, tradq_:False] := Scope[
	UnpackAssociation[assoc, nodes, edges, inputs, outputs];
	$tradq = tradq;
	edges = List @@@ Flatten[Thread /@ Reverse[edges, 2]];
	edges = DeleteDuplicatesBy[edges, ReplaceAll[p_NetPath :> Take[p, 2]]]; 
	(* ^ temp workaround for 316828 *)
	edgeTypes = StripCoders @ Extract[assoc, List @@@ edges[[All, 1]]];
	edges = edges /. p_NetPath :> Take[p, 2];
	{edges, vpaths} = Labelling[edges, 2];
	labels = vpaths[[All, 2]];
	bigmode = Length[nodes] > 16;
	{opTypes, opNames, extraLabels} = makeVertexTypes @ Map[vpathType, vpaths];
	opStyles = Map[NetTypeColor, Values[opNames]];
	opNames = Keys[opNames];
	If[!tradq && TrueQ[$NetGraphInteractivity],
		With[{
			nodes2 = ReplaceRawArraysWithDummies[nodes], edges2 = edges, bigmode2 = bigmode, inputs2 = inputs, 
			outputs2 = outputs, labels2 = labels, vpaths2 = vpaths, extraLabels2 = extraLabels,
			opStyles2 = opStyles, opTypes2 = opTypes, opNames2 = opNames, edgeTypes2 = edgeTypes},
		RawBoxes @ DynamicModuleBox[
			{selection = None},
			DynamicBox[GridBox[
				List /@ List[
					ToBoxes[
						netLayerPlot[edges2, Dynamic[selection], bigmode2, labels2, opStyles2, opTypes2, opNames2, edgeTypes2, extraLabels2]
					],
					If[IntegerQ[selection],
						ItemBox[
							vpathInfo[vpaths2[[selection]], inputs2, outputs2, nodes2], 
							Alignment -> Center
						],
						Nothing
					]
				], 
				GridBoxAlignment -> {"Columns" -> {{Left}}}
			],
			TrackedSymbols :> {selection} 
			],
			Initialization :> {NetGraph}
		]],
		If[tradq, Identity, Deploy] @ netLayerPlot[edges, None, bigmode, labels, opStyles, opTypes, opNames, edgeTypes, extraLabels]
	]
];

(* if a specialized form occurs with multiple specializations, we cannot use it,
because the color would be ambiguous, so we'll inject it as a label above the vertex
*)
makeVertexTypes[types_] := Scope[
	ambigq = GroupBy[Cases[types, _SpecializedSummary, 2], First -> Last, CountDistinct[#] > 1&];
	If[ambigq == <||>, 
		extraLabels = None,
		Cases[types, (s_Symbol -> _) :> Set[ambigq[s], True]];
		extraLabels = Match[(SpecializedSummary[t_ ? ambigq, label_] -> _) :> label, None] /@ types;
		extraLabels = extraLabels /. HoldForm[_[u_UniqueSkeleton]] :> u;
		If[MatchQ[extraLabels, {None...}], extraLabels = None];
	];
	types = types /. SpecializedSummary[t_, l_] :> RuleCondition @ If[TrueQ @ ambigq[t], t, l];
	Append[Labelling[types], extraLabels]
];


vpathInfo[NetPath["Inputs", port_], inputs_, outputs_, nodes_] := 
	typeInfo[port -> inputs[[port]]];

vpathInfo[NetPath["Outputs", port_], inputs_, outputs_, nodes_] := 
	typeInfo[port -> outputs[[port]]];

vpathInfo[NetPath["Nodes", name_], inputs_, outputs_, nodes_] := 
	itemInfo[name -> Lookup[nodes, name]];


PackageExport["$NetGraphLegendEnabled"]

$NetGraphLegendEnabled = True;

netLayerPlot[edges_, selectionVar_, bigmode_, labels_, opStyles_, opTypes_, opNames_, edgeTypes_, extraLabels_] :=
RawBoxes @ ReplaceAll[
	ToBoxes @ LayerPlot[
		edges, 
		"BaseEdgeStyle" -> GrayLevel[0.7],
		"VertexSelectionVariable" -> selectionVar, 
		"VertexLabels" -> If[bigmode, None, 
			If[extraLabels =!= None, Append[Placed[centerLabel /@ extraLabels, Above]], Identity] @ 
			{Placed[labels, Below]}
		],
		"VertexTooltips" -> If[bigmode, labels, None],
		"VertexTypeData" -> <|
			"VertexStyles" -> ReplaceAll[opStyles, Null -> Gray],
			"VertexSizes" -> Replace[opStyles, {Null -> 4, Black -> 5 (* want 6, but messes LayerPlot scaling *), _ -> 5}, {1}]
		|>,
		"VertexTypes" -> opTypes,
		"VertexTypeLabels" -> If[TrueQ[$NetGraphLegendEnabled], 
			Replace[
				Replace[opNames, {Null -> Null, e_ :> Style[e, GrayLevel[0.35]]}, {1}],
				{Null...} :> None
			],
			None
		],
		"EdgeLabels" -> If[bigmode, None, Placed[Map[fmtDims, edgeTypes], Above]],
		"BaseEdgeLabelStyle" -> {FontColor -> Gray, FontSize -> 8, FontWeight -> "Thin"},
		"RotateEdgeLabels" -> True,
		"Rotated" -> True,
		"ArrowShape" -> "NarrowKite",
		"DuplicateInputVertices" -> True,
		"ImagePadding" -> {{0, 0}, {5, 10}},
		"ImageMargins" -> {{0, 0}, {5, 5}}
	],
	(InterpretationFunction :> _) -> (InterpretationFunction :> None)
];

(* This would be much easier if we just defined everything in terms of functions *)
centerLabel[None] := None;
centerLabel[lbl_] := Pane[lbl, ImageMargins -> {{0,0},{7,0}}, BaseStyle -> {8}];

PackageExport["ExtendNetGraph"]

ExtendNetGraph[NetGraph[oldAssoc_], newNodes_Association, newEdges_List] := CatchFailureAsMessage @ Scope[
	assoc = oldAssoc;
	newNames = Keys[newNodes];
	oldNames = Keys @ assoc["Nodes"];

	(* we don't allow overwriting of old layers... (shoudl we?) *)
	If[IntersectingQ[oldNames, newNames], Panic[]];

	(* ensure new layers are actually layers *)
	newNodes = ToLayer /@ newNodes;
	
	(* add new nodes *)
	$nodes = assoc["Nodes"];
	AssociateTo[$nodes, newNodes];

	(* gather up types in preparation for checking etc *)
	$itypes = $otypes = $stypes = Association[];
	KeyValueScan[collectPortTypes, $nodes];

	(* canonicalize new edges *)
	newEdges = toEdge /@ newEdges;

	(* update the assoc *)
	assoc["Nodes"] = $nodes;
	assoc["Edges"] = Join[assoc["Edges"], newEdges];

	(* construct a new graph *)
	ConstructWithInference[NetGraph, assoc]
];


PackageExport["GetNodes"]

NetGraph /: Normal[ng_NetGraph] := GetNodes[ng, True];

GetNodes[HoldPattern @ NetGraph[assoc_Association, _]] := 
	Map[ConstructLayer, assoc["Nodes"]];

NetGraph /: Take[ng_NetGraph, spec_] := CatchFailureAsMessage[NetGraph, NetGraphTake[ng, spec]];

NetGraph::seqs = "Sequence specification {start, end} expected."

NetGraphTake[net_NetGraphP, {a_, b_}]:= Scope[
	$nodes = net["Nodes"];
	edges = net["Edges"];
	$inputs = InputNames[net];
	$outputs = OutputNames[net];
	
	$starting = procTakeStartElem[a];
	$ending = procTakeEndElem[b];

	graph = FullNetPathGraph[edges, $nodes];

	(* get component *)
	in = VertexOutComponent[graph, $ending];
	out = VertexInComponent[graph, $starting];

	newGraph = Intersection[in, out];
	$newNodes = Cases[newGraph, NetPath["Nodes", v_] :> v];
	
	(* subset nodes *)
	danglingRules = Cases[$starting, np:NetPath["Nodes", _, "Outputs", _] :> Rule[np, $DanglingPort]];
	If[danglingRules =!= {}, edges = edges /. danglingRules];
	$nodes = KeySelect[$nodes, MemberQ[$newNodes, #]&];
	edges = Select[edges, keepEdgeQ];

	(* $DanglingPort is resolved later, because if we change it to Inputs/Input now then
	it will incorrectly pick up a type hint of the *original* input, see 331716.
	*)
	makeSubNetGraph[net, $nodes, edges]
]

NetGraphTake[_, _] := ThrowFailure[NetGraph::seqs];

procTakeStartElem[elem_] := 
	Block[{$io = $inputs, $type = "Inputs"}, Flatten @ List @ procTakeElem[elem]];

procTakeEndElem[elem_] := 
	Block[{$io = $outputs, $type = "Outputs"}, Flatten @ List @ procTakeElem[elem]];

procTakeElem[e_List] := procTakeElem /@ e;
procTakeElem[All] := NetPath[$type, #]& /@ $io;

NetGraph::invportspec = "The port specification `` is invalid."
procTakeElem[p:NetPort[name_String]] :=
	If[MemberQ[$io, name], 
		NetPath[$type, name],
		ThrowFailure[NetGraph::invportspec, p]
	];

procTakeElem[NetPort[a_, b_]] := procTakeElem[NetPort[{a, b}]];

procTakeElem[p:NetPort[{part_Integer | part_String, name_String}]] := Scope[
	part = toVertex[part];
	inputs = $nodes[[part, "Inputs"]];
	outputs = $nodes[[part, "Outputs"]];
	NetPath["Nodes", part, Which[
		KeyExistsQ[inputs, name], "Inputs",
		KeyExistsQ[outputs, name], "Outputs",
		True, ThrowFailure[NetGraph::invportspec, p]
	], name]
];

procTakeElem[part_] := NetPath["Nodes", toVertex[part]];

Clear[keepEdgeQ];

keepEdgeQ[NetPath["Nodes", name1_, __] -> NetPath["Nodes", name2_, __]] := 
	MemberQ[$newNodes, name1] && MemberQ[$newNodes, name2]

keepEdgeQ[end:NetPath["Outputs", _] -> NetPath["Nodes", name_, __]] :=
	MemberQ[$newNodes, name] && MemberQ[$ending, end | NetPath["Nodes", name]];

keepEdgeQ[NetPath["Nodes", name_, __] -> start:NetPath["Inputs", _]] :=
	MemberQ[$newNodes, name] && MemberQ[$starting, start | NetPath["Nodes", name]];

NetGraph::invsplit = "Vertex `` (``) has multiple inputs that are not all in the graph."

keepEdgeQ[NetPath["Nodes", name1_, __] -> sources_List] := 
	MemberQ[$newNodes, name1] && Or[
		AllTrue[sources, keepSourceQ],
		ThrowFailure[NetGraph::invsplit, name1, MsgForm @ $nodes[[name1]]]
	];

keepSourceQ[NetPath["Nodes", name_, ___]] := MemberQ[$newNodes, name];
keepSourceQ[np:NetPath["Inputs", _]] := MemberQ[$starting, np];
keepSourceQ[$DanglingPort] := True;
keepSourceQ[_] := False;

keepEdgeQ[pat_] := False


PackageScope["NetGraphNodes"]

NetGraph /: VertexList[ng_NetGraph] := NetGraphNodes[ng];

NetGraphNodes[ng_NetGraphP] := Map[ConstructLayer, Values @ ng["Nodes"]];


PackageExport["NetGraphEdges"]

NetGraph /: EdgeList[ng_NetGraph] := NetGraphEdges[ng];

NetGraphEdges[ng_NetGraphP] := Scope[
	UnpackAssociation[ng, nodes, edges];
	$dedig = If[DigitStringKeysQ[nodes], FromDigits, Identity];
	$simpleq = Map[
		And[Length[#Inputs] == 1, Length[#Outputs] == 1]&, 
		nodes
	];
	edges = tlNode[#2] -> tlNode[#1]& @@@ edges;
	edges = Catenate @ Lookup[GroupBy[edges, edgeType], {0, 2, 1}, {}];
	edges
];

edgeType[NetPort[_String] -> _] := 0;
edgeType[_ -> NetPort[_String]] := 1;
edgeType[_] := 2;

SetListable[tlNode];

tlNode[NetPath["Nodes", name_, "Inputs"|"Outputs"|"States", in_]] := 
	If[$simpleq[name], $dedig @ name, NetPort[{$dedig @ name, in}]];

tlNode[NetPath["Inputs"|"Outputs", port_]] := NetPort[port];


PackageExport["NetGraphPortContributors"]

NetGraphPortContributors[ng_NetGraph, port_] := Scope[
	outNames = OutputNames[ng];
	If[!MemberQ[outNames, port], Return[{}]];
	UnpackAssociation[NData[ng], nodes, edges];
	graph = NetPathGraph[edges];
	delPorts = Cases[outNames, port];
	keepPorts = DeleteCases[outNames, port];
	contribEdges = Complement[
		VertexOutComponent[graph, NetPath["Outputs", #]& /@ delPorts],
		VertexOutComponent[graph, NetPath["Outputs", #]& /@ keepPorts]
	];
	Cases[contribEdges, NetPath["Nodes", name_] :> name]
];

PackageExport["DeleteLayers"]

NetGraph /: VertexDelete[ng_NetGraph, e_] := 
	CatchFailureAsMessage[NetGraph, DeleteLayers[ng, ToList[e]]];

DeleteLayers[net_NetGraphP, deleteList_List] := Scope[
	UnpackAssociation[net, $nodes:"Nodes", edges];
	deleteList = toVertex /@ deleteList;
	elisions = KeyValueMap[toElisionRules, $nodes[[deleteList]]];
	KeyDropFrom[$nodes, deleteList];
	edges = Select[edges /. elisions, FreeQ[Alternatives @@ deleteList]];
	makeSubNetGraph[net, $nodes, edges]
];

toElisionRules[name_, node_] :=
	Match[
		{node["Inputs"], node["Outputs"]}, 
		{<|inname_ -> type1_|>, <|outname_ -> type2_|>} /; EquivalentTypeQ[type1, type2] :> Rule[
			NetPath["Nodes", name, "Outputs", outname],
			Replace[
				NetPath["Nodes", name, "Inputs", inname], 
				Append[edges, _ :> Return[Nothing]]
			]
		],
		Nothing
	];

PackageExport["NetGraphRandomInput"]

NetGraphRandomInput::spec = "NetGraph is not fully specified."

NetGraphRandomInput[ng_NetGraph] := Scope[
	If[!FullySpecifiedNetQ[ng], ThrowFailure["spec"]];
	RandomInstance /@ Inputs[ng]
]
