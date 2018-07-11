Package["NeuralNetworks`"]


(* TODO: Emit arrays that are prefixed with aux: and arg: as appropriate in the params file --
find out why they even do that.
*)

toArrayPath[jsonPath_String] := Scope @ Which[
	pathEndsQ[jsonPath, "-symbol.json"],
		First[StringDrop[jsonPath, -12] <> "-*.params", StringDrop[jsonPath, -12] <> "-0000.params"],
	pathEndsQ[jsonPath, ".json"],
		StringDrop[jsonPath, -4] <> "params",
	True,
		ThrowFailure["nninvjp", jsonPath]
];

pathEndsQ[path_, end_] := StringEndsQ[path, end, IgnoreCase -> True];

PackageExport["MXNetExport"]

(* TODO: detect dynamic dims and fail, rather than using some fixed number *)

General::nnvarexp = "Cannot export a network that has variable-length tensor input \"`1`\". To export such a net, use NetReplacePart[net, \"`1`\" -> shape] to produce a net that operates on a fixed-size tensor."
General::nninvap = "Cannot export arrays to path ``.";
General::nninvjp = "Invalid json file path specification ``. File path must end with either \"-symbol.json\" or \".json\"";

Options[MXNetExport] = {
	"SaveArrays" -> True,
	"ArrayPath" -> Automatic
};

MXNetExport[jsonPath_, net_ ? ValidNetQ, OptionsPattern[]] := CatchFailureAsMessage[Export, Scope[
	UnpackOptions[saveArrays, arrayPath];
	If[!ConcreteNetQ[net], ThrowNotSpecifiedFailure[net, "export"]];
	If[varlen = FindVarLengthInput[net]; StringQ[varlen], ThrowFailure["nnvarexp", varlen]];
	data = ToMXJSON[net, "NameRewriter" -> toExportName];
	If[FailureQ[data], ReturnFailed[]];
	json = data["JSON"];
	arrays = data["Arrays"];
	jsonPath = ExpandFileName[jsonPath];
	Check[
		stream = OpenWrite[jsonPath, BinaryFormat -> True];
		WriteString[stream, json];
		Close[stream];
	,
		Return[$Failed]
	];

	(* Note: can't do arrays =!= <||> as arrays is an UnorderedAssociation! *)
	If[saveArrays && (Length[arrays] =!= 0),
		If[arrayPath === Automatic, arrayPath = toArrayPath[jsonPath]];
		If[Head[arrayPath] === File, arrayPath = First[arrayPath]];
		If[!StringQ[arrayPath], ThrowFailure["nninvap", arrayPath]];
		NDArrayExport[arrayPath, Map[NDArrayCreate, arrays]];
		If[!FileExistsQ[arrayPath], ThrowFailure["nninvap", arrayPath]];
	];
	If[StringQ[arrayPath], {jsonPath, arrayPath}, jsonPath]
]];

toExportName["."] = "Layer";

toExportName[str_String] := If[
	StringFreeQ[str, "."], str,
	StringRiffle[StringSplit[StringTrim[str, "."], "."][[2 ;; ;; 2]], "."]
];

MXNetExport[path_, net_, _] := (
	Message[Export::invnet2, HoldForm[Export][jsonPath, Shallow[net]]];
	$Failed
);


PackageExport["MXNetImport"]

Options[MXNetImport] = {
	"ArrayPath" -> Automatic,
	"InsertSoftmaxCrossEntropy" -> True
};

General::mxparam1 = "Specified array file \"``\" does not exist.";
General::mxparam3 = "\"ArrayPath\" should be a string containing a file path, Automatic, or None."
General::mxparam4 = "Array file \"``\" appears to be corrupt."
General::mxparam5 = "Cannot import the element \"``\" from an Array file."
General::mxparam6 = "Array file cannot be found, and is required for the Import element \"``\"."

MXNetImport[a___] := Print[a];

MXNetImport[file_String, elem_:"Net", OptionsPattern[]] := CatchFailureAsMessage[Import, Scope[
	UnpackOptions[arrayPath, insertSoftmaxCrossEntropy];
	file = ExpandFileName[file];
	If[!FileExistsQ[file], ThrowFailure["nffil", Import]];

	elemM = elem; (* hack: this needs to be changed if file is .params file *)

	$paramElements = {
		"ArrayList", "RawArrayList", "ArrayAssociation",
		"RawArrayAssociation", "ArrayNames"
	};
	(* file can be json or param file *)
	If[StringEndsQ[file, ".params", IgnoreCase -> True],
	(* Case 1: file is .params *)
		arrayPath = ExpandFileName[file];
		$doesntNeedArrays = False;
		elemM = "ArrayAssociation"; (* hack till kernel import handles this properly *)
		(* Check: if content element is not compatible with the main file being 
			a params file, throw failure. HACK: exclude "Net", as single arg import always "Net" *)
		If[!MemberQ[$paramElements, elem] && (elem =!= "Net"),
			ThrowFailure["mxparam5", elem]
		],
	(* Case 2: file is .json *)
		$doesntNeedArrays = !MemberQ[$paramElements, elem];
	];

	Switch[arrayPath,
		(_String | _File),
			arrayPath = FindFile[arrayPath];
			If[FailureQ[arrayPath], ThrowFailure["mxparam1", arrayPath]],
		Automatic /; $doesntNeedArrays, 
			arrayPath = toArrayPath[file];
			If[FailureQ[arrayPath] || !FileExistsQ[arrayPath], 
				arrayPath = None
			]
			,
		Automatic,
			arrayPath = toArrayPath[file];
			If[FailureQ[arrayPath] || !FileExistsQ[arrayPath], 
				ThrowFailure["mxparam6", elemM]
			]
			,
		None,
			Null,
		_,
			ThrowFailure["mxparam3"]
	];

	nds := importArrays[arrayPath];
	Switch[elemM,
		"ArrayList", Normal /@ Values[nds],
		"RawArrayList", Values[nds],
		"ArrayAssociation", KeyMap[stripMXPrefix] @ Map[Normal] @ nds,
		"RawArrayAssociation", KeyMap[stripMXPrefix] @ nds,
		"InputNames", $returnInputs = True; iMXNetImport[file, None],
		"ArrayNames", Map[stripMXPrefix] @ Keys[nds],
		"NodeGraphPlot", MXJSONPlot[Developer`ReadRawJSONFile[file], "EdgeBundling" -> False],
		"NodeGraph", MXJSONGraph[Developer`ReadRawJSONFile[file]],
		"NodeDataset", $returnDataset = True; iMXNetImport[file, None],
		"Net", iMXNetImport[file, arrayPath],
		"UninitializedNet", iMXNetImport[file, None],
		"LayerAssociation", $returnLayers = True; iMXNetImport[file, arrayPath],
		_, $Failed
	]
]];

importArrays[path_] := Replace[
	CatchFailure[General, Quiet @ NDArrayImport[path]],
	Except[_Association] :> ThrowFailure["mxparam4", path]
];

(* i'm not sure why mxnet prefixes the names in the param file like this compared to the 
names of the ops in the JSOn file, but it does *)
stripMXPrefix[str_] := StringTrim[str, "arg:"|"aux:"];

Import::mxjson1 = "Could not read JSON definition from ``.";
Import::mxjson2 = "JSON definition in `` appears to be corrupt.";

(* There is an important subtlety here: MXNet doesn't distinguish
inputs to the net versus learned arrays. We need to tell the difference
because genuine inputs need to become hints of the form 
NetPort["iname"] -> layer, whereas array inputs are just used to feed
the right RawArrays from the .param file to the right layers. However
this is a chicken-and-egg problem: we only know the difference based
on how they are used. 

The most explicit way to solve this is to human-curate the distinction,
which is what $MXOpsWithParamInputs does: it lists those layers with
learnable array -- every input after the first is assumed to be a 
learned array. 

The algorithm for distinguishing input and array arguments is to find
the array inputs of these layers anywhere in the graph -- they must
immediately terminate in a argnode, and if they don't, we can't import
the graph for other reasons. if they do, that argnode is an arraynode.
all argnodes that aren't arraynodes are inputnodes.
*)

$MXOpsWithParamInputs = 
	Alternatives["FullyConnected", "Embedding", "Convolution", "Deconvolution", "InstanceNorm", "BatchNorm"];

(* opt-in to specific special-case outputs *)
$returnInputs = False;
$returnArrays = False;

General::mximpfut = "Importing from version `` of MXNet may fail, this version of Wolfram Language supports version `` and below."
General::mxconvfail = "Failed to convert MXNet symbol from version `` to required version ``."
verstr[v_] := TextString[v / 1000.];

convfail[oldv_] := ThrowFailure["mxconvfail", verstr @ oldv, verstr @ $MXNetVersion];

$ValidMXJSONPattern = KeyValuePattern[{"nodes" -> {__Association}, "arg_nodes" -> {___Integer}, "heads" -> _List, "attrs" -> _Association}];

iMXNetImport[jsonPath_, paramPath_] := Scope[
	$strictLayerArgs = False;
	$ArgumentArrays = If[StringQ[paramPath], importArrays[paramPath], <||>];

	json = ReadRawJSONFile[jsonPath];
	If[FailureQ[json], ThrowFailure["mxjson1", jsonPath]];

	mxversion = Match[json["attrs", "mxnet_version"], {"int", n_} :> n, 800];
	If[mxversion > $MXNetVersion, 
		(* warn that we might have problems *)
		Message[Import::mximpfut, verstr @ mxversion, verstr @ $MXNetVersion]
	];
	If[mxversion =!= $MXNetVersion,
		(* MXNet has its own upgraders, use those *)
		sym = Quiet @ CatchFailure[General, MXSymbolFromJSON @ json];
		If[Head[sym] =!= MXSymbol, convfail[mxversion]];
		json = Quiet @ CatchFailure[General, MXSymbolToJSON @ sym];
		If[!AssociationQ[json], convfail[mxversion]];
	];

	If[!MatchQ[json, $ValidMXJSONPattern], ThrowFailure["mxjson2", jsonPath]];

	nodes = json["nodes"];
	If[!DuplicateFreeQ[nodes[[All, "name"]]], 
		nodes = renameByIndex@nodes
	];
	$argNodes = json["arg_nodes"];
	heads = json["heads"];

	If[$returnDataset,
		Return @ Dataset[
			Association[#name -> KeyDrop[#, "name"]& /@ nodes], 
			TypeSystem`Assoc[
				TypeSystem`StringT,
				TypeSystem`Struct[
					{"op", "attr", "inputs"}, 
					{TypeSystem`StringT, 
					TypeSystem`AnyType, 
					TypeSystem`AnyType}],
				Length[nodes]
			]
		];
	];

	$argNodes += 1;
	$allNames = nodes[[All, "name"]];
	namedTensors = Part[nodes, $argNodes, "name"];
	
	$arrayNodes = Cases[nodes, KeyValuePattern[{"op" -> $MXOpsWithParamInputs, "inputs" -> ins_}] :> Rest[ins]];
	$arrayNodes = Part[Union @@ $arrayNodes, All, 1] + 1;
	$inputNodes = Complement[$argNodes, $arrayNodes];

	If[$returnInputs, Return[$allNames[[$inputNodes]]]];
	If[$returnArrays, Return[$allNames[[$arrayNodes]]]];

	(* wrap NetPort around the true inputs to the graph *)
	$allNames = MapAt[NetPort, $allNames, List /@ $inputNodes];

	CollectTo[{$nodes, $edges},
		Scan[If[TrueQ[$returnLayers], readNodeSafe, readNode], nodes]
	];

	If[$returnLayers, Return[Association[$nodes]]];

	$edges = Join[$edges,
		Thread@Rule[
			nodes[[heads[[All,1]] + 1, "name"]],
			Table[NetPort["Output"<>ToString[i]], {i, Length@heads}]
		]
	]; 
	
	{from, to} = KeysValues[$edges];
	If[Rest[from] === Most[to] && DeleteCases[Append[from, Last[to]], _NetPort] === Keys[$nodes],
		NetChain[Association[$nodes]],
		NetGraph[Association[$nodes], $edges]
	]
];

renameByIndex[nodes_] := MapIndexed[
	ReplacePart[#1, Key["name"] -> #1["name"] <> "_" <> ToString[First[#2]-1]] &, 
	nodes
]

$multiInputLayers = Alternatives @@ Join[
	{"Concat", "ElementWiseSum", "add_n", "elemwise_add", "_sub", "SoftmaxOutput"},
	Values @ $MXBinaryFunctionNameMapping
];

readNodeSafe[layer_] := Scope[
	res = CatchFailure[Import, readNode[layer]];
	If[FailureQ[res], stuffNode[layer["name"] -> res]]
];

readNode[layer_] := Scope[
	$LastReadMXNetLayer ^= layer;
	{type, name, inputs, attr} = Lookup[layer, {"op", "name", "inputs", "attr"}];
	If[type === "null", Return[Nothing]];
	inputs = inputs[[All, 1]] + 1; 
	$inputNames = Part[$allNames, Complement[inputs, $arrayNodes]];
	readLayer[type, name, inputs, attr];
	Which[
		MatchQ[type, $multiInputLayers],
			stuffEdge[$inputNames -> name],
		Length[$inputNames] == 1,
			stuffEdge[First[$inputNames] -> name],
		True,
			ThrowFailure["badmxlayer", name, "layer has multiple inputs",  type, $inputNames]
	];
];


PackageScope["readLayer"]
PackageScope["toLayerReader"]
PackageScope["toValueReader"]
PackageScope["$LastReadMXNetLayer"]

stuffNode[rule_] := StuffBag[$nodes, rule];
stuffEdge[edge_] := StuffBag[$edges, edge];

General::invmxlayer = "Layer named \"``\" couldn't be created, layer type \"``\" is not supported.";
General::badmxlayer = "Layer named \"``\" couldn't be created, error was ``. Original MXNet type was ``, with inputs: ``.";

(* this kind of Pooling layer should never make it into the framework, its a workaround for an Agg Layer *)
readLayer["Pooling", name_, _, <|"global_pool" -> "True", "kernel" -> "(1, 1)", "pool_type" -> ptype_|>] :=
	stuffNode[name -> AggregationLayer[$FromPooling[ptype], 2;;]];


(* custom implementation that unfolds a SoftmaxOutput into Softmax and then subsequent CELossLayer *)
readLayer["SoftmaxOutput", name_, inputs_, _] := 
	If[!insertSoftmaxCrossEntropy, 
		$inputNames = Take[$inputNames, 1];
		stuffNode[name -> SoftmaxLayer[]],
	Block[{smName},
		smName = name <> "_softmax";
		(* insert an new layer and edge! *)
		stuffEdge[$inputNames[[1]] -> smName];
		$inputNames[[1]] = smName; 
		stuffNode[smName -> SoftmaxLayer[]];
		stuffNode[name -> CrossEntropyLossLayer["Index"]];
	]
];

readLayer[type_, name_, inputs_, attr_] := Scope[
	wlname = Lookup[$FromMXNetName, type, result = readLayerCustom[type, attr]; Goto[done]];
	UnpackAssociation[$LayerData[wlname], symbol, arrays];
	reader = toLayerReader[wlname];
	rules = Flatten @ reader @ attr;
	If[arrays =!= <||>, 
		arrayNames = Keys @ arrays; noBias = MatchQ[attr["no_bias"], "true" | "True"];
		arrayInputs = Part[$allNames, Intersection[inputs, $arrayNodes]];
		If[noBias, arrayNames = {"Weights"}]; (* <- applies to Conv, Deconv, FC *)
		arrayRules = DeleteMissing @ MapThread[#1 -> lookupArray[#2]&, {arrayNames, arrayInputs}];
		If[noBias, AppendTo[rules, "Biases" -> None]]; (* <- too complex otherwise *)
		rules = Join[rules, arrayRules];
	];
	result = symbol @@ rules;
	Label[done];
	If[FailureQ[result],
		ThrowFailure["badmxlayer",
			name, ToString @ result, 
			type, rules /. _RawArray -> RawArray
		]
	];
	stuffNode[name -> result];
];

lookupArray[mxname_] := 
	Lookup[$ArgumentArrays, mxname, 
	Lookup[$ArgumentArrays, "arg:" <> mxname, 
	Lookup[$ArgumentArrays, "aux:" <> mxname, Missing[]]]];

readLayerCustom["elemwise_add", _] := ThreadingLayer[Plus];

readLayerCustom["_sub", _] := readLayerCustom["_Minus", Null];

readLayerCustom["mean", attr_] := AggregationLayer[Mean, readIntList @ attr["axis"]];

KeyValueScan[
	Function[readLayerCustom[#2, attr_] := ThreadingLayer[#1]],
	$MXBinaryFunctionNameMapping
];

KeyValueScan[
	Function[readLayerCustom[#2, attr_] := ElementwiseLayer[#1]],
	$MXUnaryFunctionNameMapping
];

readLayerCustom["add_n", _] := TotalLayer[];

readLayerCustom[op_, _] := ThrowFailure["invmxlayer", $LastReadMXNetLayer["name"], op];

toLayerReader[name_] := Memoized @ makeLayerReaderWriter[name, "Reader"];


PackageScope["toFieldReader"]

toFieldReader[param_, key_] := 
	toFieldReader[param, key, toValueReader @ $mxParamTypes[param], $mxParamDefaults[param]];

toFieldReader[param_, key_, reader_, _Missing] := 
	param -> Quoted[reader[ALookup[#, key]]];

toFieldReader[param_, key_, reader_, default_] := 
	param -> Quoted[Replace[Lookup[#, key], {_Missing :> default, v_ :> reader[v]}]];

toValueReader[t_] := Match[t,
	ListT[SizeT | NaturalT, SizeT | NaturalT] :> readIntList,
	ListT[SizeT | NaturalT, PosIntegerT] :> readIntList /* checkPos,
	ListT[2, SizeT | NaturalT] :> readIntPair,
	ListT[2, PosIntegerT] :> readIntPair /* checkPos,
	EnumT[list_List] :> readEnum[list],
	NaturalT :> readInt /* checkNat,
	PosIntegerT :> readInt /* checkPos,
	SizeT :> readInt /* checkPos,
	ScalarT :> readReal,
	BooleanT :> $FromBoolean,
	PoolingFunctionT :> $FromPooling,
	IntervalScalarT[x_, y_] :> readReal,
	ExpressionT :> $Failed, (* will get removed later *)
	Panic["UnreadableType", "No reader for ``.", t]
];

toMissingValueReader[Defaulting[_, v_]] := Function[v];
toMissingValueReader[_] := Panic["MissingRequiredMXNetParameter", "Param `` is missing.", #]&;

readEnum[list_List][str_String] := If[MemberQ[list, str], str, Panic[]]; 
readEnum[_][_] := Panic[];
readReal[real_String] := StringToDouble[real];
readInt[int_Integer] := int;
readInt[str_String] := FromDigits[str];
readIntPair[str_String] := StringMatch[str, "(" ~~ a:Number ~~ (", " | ",") ~~ b:Number ~~ ")" :> Map[FromDigits, {a,b}]];
readIntList[str_String] := Map[FromDigits, StringSplit[str, "(" | ", " | "," |")"]];
checkPos[p_] := If[Min[p] < 1, Panic[], p];
checkNat[p_] := If[Min[p] < 0, Panic[], p];

Scan[
	Function[sym,
		sym[val_] := Panic["MalformedMXNetParameter", "Value `` could not be read by ``.", val, sym]
	], 
	{readReal, readInt, readIntPair, readIntList}
];


(******************************************************************************)
SetUsage[ImportMXNetChain, "
ImportMXNetChain[json$, params$, inputSize$] imports an MXNet chain given a path to the MXNet \
NDArray params$ files, Symbol json$ file, and the input size inputSize$. Extra options:
|'FinalLayers' | <||> | extra layers  to be attached to output (association) | 
|'NetEncoder'  | None | NetEncoder to attach to input |
|'NetDecoder'  | None | NetDecoder to attach to output |
|'ModuleName'  | None | A string pattern matching the module name |
|'RenameInternalLayers'  | True | Whether to remove the matched ModuleName from layer names into each module or not. 
"
]

PackageExport["ImportMXNetChain"]

Options[ImportMXNetChain] =
{
	"NetEncoder" -> None,
	"NetDecoder" -> None,
	"ModuleName" -> None,
	"RenameInternalLayers" -> True,
	"FinalLayers" -> <||>
};

ImportMXNetChain[json_String, params_String, inputSize_List, opts:OptionsPattern[]] := Scope[

	imported = MXNetImport[json, "Net", "ArrayPath" -> params, "InsertSoftmaxCrossEntropy" -> False];

	ImportMXNetChain[imported, inputSize, opts]
]

ImportMXNetChain[inputNet_, inputSize_List, opts:OptionsPattern[]] := Scope[
	UnpackOptions[netEncoder, netDecoder, moduleName, renameInternalLayers, finalLayers];
	(* we assume there is only one input *)
	network = NetReplacePart[inputNet, First@Keys@NetInputs[inputNet] -> inputSize];
	(* netGraphPoolingFix must know all sizes in net *)
	network = netGraphPoolingFix[network];


	layers = NetExtract[network, All];
	(* create modules if needed *)
	If[moduleName =!= None,	
		layers = netSubmoduleSplit[network, moduleName, renameInternalLayers]
	];
	(* Add extra layers *)
	layers = Join[layers, finalLayers];
	(* encoders *)
	encoders = {};
	If[netEncoder =!= None, AppendTo[encoders, "Input" -> netEncoder]];
	If[netDecoder =!= None, AppendTo[encoders, "Output" -> netDecoder]];
	(* Create and return chain *)
	NetChain[layers, Sequence@@encoders]
]

netSubmoduleSplit[net_, namePattern_, renameInternalLayers_] := Module[
	{
		$namespaceChar, netLayers, splitF, layerGroups,
		layerNames, startLayers, layers
	}
	,
	$namespaceChar = ("_"|"/"|"-");
	netLayers = NetExtract[net, All];
	splitF = If[StringContainsQ[#, namePattern], First@StringCases[#, Shortest[x : namePattern] -> x], #]&;
	layerGroups = SplitBy[Keys@netLayers, splitF];
	(* Create new layer names: modules + layers have names *)
	layerNames = If[Length@# == 1, First@#,
 		First@StringCases[First@#, 
    	Shortest[x : namePattern ~~ __] -> x]] & /@ layerGroups;
    layerNames = StringTrim[layerNames, $namespaceChar];
	(* Get start for Take *)
	startLayers = moduleTakeStart[#, net]& /@ layerGroups;
	layers = Association@MapThread[(#1 -> 
		If[Length@#2 == 1, 
			netLayers@First@#2, 
			Take[net, {NetPort[{#3, "Output"}], Last@#2}]
			])&,
		{layerNames, layerGroups, startLayers}];

	(* Rename module layers *)
  If[renameInternalLayers, 
    layers = If[Head@# === NetGraph,  
      RenameLayers[#, Shortest[namePattern ~~ $namespaceChar] ~~ x_ -> x], 
      # 
    ]& /@ layers; 
  ]; 
 
  layers 
]

(* module is a list of layers, net is the netgraph *)
moduleTakeStart[module_, net_] := Module[
	{g, moduleVertices, subgraph, inVert, beginLayer}
	,
	(* Case 1: not a module *)
	If[Length@module === 1, Return@None];
	(* now assuming we have a module *)
	g = NeuralNetworks`LayerDependencyGraph@net;
	moduleVertices = NetPath["Nodes", #]& /@ module;
	startNode = Complement[VertexInComponent[g, moduleVertices, 1], moduleVertices];
	startNode[[1, 2]]
]

(*--------------------------------------------------------------------*)

PackageScope["netGraphPoolingFix"]

netGraphPoolingFix[net_NetGraph] := Scope[
	edges = EdgeList @ net;
	layers = NetExtract[net, All];
	
	poolingLayers = Select[layers, (Head[#] === PoolingLayer)&];
	names = Keys[poolingLayers];	
	Do[
		name = names[[l]];
		layer = layers[name];

		(* replace Global Pooling *)
		If[NData[layer]["Parameters", "$MXGlobalPool"],
			layers[name] = globalPoolReplace[layer];
			Continue[];
		];

		(* deal with pooling convention *)
		newLayers = caffePoolingLayerConvert[layer];
		If[Length[newLayers] > 0,
			layers[name] = Last[newLayers];
			padName = name <> "_pad";
			layers[padName] = First[newLayers];
			edges = ReplaceAll[edges, Rule[x_, name] :> Rule[x, padName]];
			PrependTo[edges, Rule[padName, name]]
		];
	,
	{l, Length[poolingLayers]}
	];
	NetGraph[layers, edges]
]

General::caffepad = "Cannot convert Caffe pooling layer when PaddingSize is not the same in all dimensions.";
General::caffemax = "Cannot convert Caffe pooling layer when Function is not Max.";

caffePoolingLayerConvert[x_PoolingLayer] := Scope[
	params = NData[x]["Parameters"];
	inSize = params["$InputSize"];
	outSize = params["$OutputSize"];
	channels = params["Channels"];
	{p, s, k, f} = params /@ {"PaddingSize", "Stride", "KernelSize", "Function"};
	
	valOut = PoolingShape[inSize, p, k, s, "valid"];
	If[params["$MXGlobalPool"], Return @ {}];
	If[valOut === outSize,
		{}
		,
		If[Max[p] =!= Min[p], ThrowFailure["caffeprs"]];
		If[f =!= Max, ThrowFailure["caffemax"]];
		newPool = PoolingLayer[k, 
			"Stride" -> s, "PaddingSize" -> p, "Function" -> f
		];
		extraPad = Mod[s - Mod[inSize + 2*(p) - k, s], s];
		extraPad = Join[{{0, 0}}, {0, #}& /@ extraPad];
		padType = If[Max[p] === 0, "Fixed", 0];
		padLayer = PaddingLayer[extraPad, "Padding" -> padType, "Input" -> Join[{channels}, inSize]];
		{padLayer, newPool}
	]
]

globalPoolReplace[x_PoolingLayer] := Scope[
	params = NData[x]["Parameters"];
	inSize = params["$InputSize"];
	channels = params["Channels"];
	If[TrueQ @ params["$MXGlobalPool"],
		AggregationLayer[params["Function"], "Input" -> Prepend[inSize, channels]],
		x
	]
]

globalPoolReplace[x_] := x
   