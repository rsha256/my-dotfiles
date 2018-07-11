Package["NeuralNetworks`"]



PackageScope["GetConstructorFunction"]

General::nnincmpb = "Cannot `` in version `` a network produced using version ``."

GetConstructorFunction[metadata_, action_] := Scope[
	version = ToVersionString @ Lookup[metadata, "Version", Panic["CorruptMetadata"]];

	(* net is from this version *)
	If[version === "11.2.0", Return[ConstructLayer]];  
	
	(* net is from previous version *)
	If[version === "11.1.1", Return[Construct11V1Layer]];  

	(* net is prior to the LinearLayer arb-dim change *)
	If[version === "11.1.0", Return[Construct11V1PrereleaseLayer]];

	(* net is from the previous-previous version *)	
	If[version === "11.0.0", Return[Construct11V0Layer]];
	
	(* net is from the future... *)
	compat = Lookup[metadata, "CompatibleVersions", None];

	(* ...and is compatible with this version... *)
	If[ListQ[compat] && MemberQ[compat, $$NeuralNetworksVersionNumber], 
		Return[ConstructLayer[#1]&]
	];

	(* ...or has a downgrader function associated with it *)
	Lookup[
		metadata, "DowngraderFunction", 
		ThrowFailure["nnincmpb", action, $NeuralNetworksVersionNumber, version]
	]
];

PackageScope["Construct11V1Layer"]

Construct11V1Layer[net_, meta_] := Scope[
	net = net /. UpdatePatterns["11.1"];
	meta["Version"] = $NeuralNetworksVersionNumber;
	ConstructLayer[net, meta]
];

PackageScope["Construct11V1PrereleaseLayer"]

Construct11V1PrereleaseLayer[net_, meta_] := Scope[
	net = net /. UpdatePatterns["11.1"];
	net = net /. t:<|"Type" -> "Linear", ___|> :> RuleCondition @ 
		updateLinear[t, TDimensions @ t["Inputs", "Input"], t["Parameters", "Size"]];
	meta["Version"] = $NeuralNetworksVersionNumber;
	ConstructLayer[net, meta]
];

PackageScope["Construct11V0Layer"]

$layersWithRanks = {"Catenate", "Elementwise", "Flatten", "MeanAbsoluteLoss", "Reshape", "Softmax", "Transpose"};

Import::netupgr = "Upgrading neural network to 11.1 format."
Off[Import::netupgr];

Construct11V0Layer[net_Association, _] := Scope[
	Message[Import::netupgr];
	net = net /. UpdatePatterns["11.0"];
	net = net //. $TensorUpdateRule;
	net = net /. $CoderUpdateRule;
	net = net //. HoldPattern[t_TensorT] :> RuleCondition[t];
	net = net /. NetPort -> NetPath;
	ConstructLayer[net]
];


PackageScope["$TensorUpdateRule"]

$CoderUpdateRule = (EncodedType|DecodedType)[coder_, t_] :> RuleCondition @ UnifyCoderWith[coder, t //. $TensorUpdateRule];
$TensorUpdateRule = HoldPattern[t:TensorT[_Integer | SizeT | NaturalT, dims_]] :> TensorT[dims, RealT];

Clear[UpdatePatterns];

UpdatePatterns[version_] := UpdatePatterns[version] = 
	Dispatch[makeUpdatePattern @@@ $UpdateRules[version]];

makeUpdatePattern[type_, func_] := assoc:<|"Type" -> type, ___|> :> RuleCondition[func[assoc]];

updateParams[f_] := MapAt[f, "Parameters"];

(* in future we may want to have the rules be between successive versions
instead of all being from version X to the current version, that way we 
just catenate together a bunch to make the full upgrader *)

$UpdateRules = <||>;

$UpdateRules["11.1"] = {
	"Resize" -> updateParams[updateResize],
	"Aggregation" -> updateParams[updateAggregation],
	"Dropout" -> updateParams[updateDropout],
	"Replicate" -> updateParams[updateReplicate],

	"BatchNormalization" -> reorderBatchNormArrays
};

$UpdateRules["11.0"] = {
	(* simple updaters that just modify params *)
	"Pooling" -> updateParams[updatePooling],
	"Softmax" -> updateParams[updateSoftmax],
	"Convolution" -> updateParams[updateConvolution],
	"CrossEntropyLoss" -> updateParams[updateCrossEntropyLoss],
	"Elementwise" -> updateParams[updateElementwise],

	"Split" -> splitGone,
	"BroadcastPlus"-> broadcastGone,
	"Upsample" -> renameTo["Resize"] /* updateParams[updateUpsample],
	"ScalarTimes" -> renameTo["Elementwise"] /* updateParams[scalarFunctionParams[Times]],
	"ScalarPlus" -> renameTo["Elementwise"] /* updateParams[scalarFunctionParams[Plus]],
	"Catenate" -> updateCatenate,

	(* more complicated updaters *)
	"Graph"|"Chain" -> renameContainerKeys,
	"MeanAbsoluteLoss"|"Reshape"|"Softmax"|"Transpose" -> removeRanks,
	"Flatten" -> updateFlatten,
	"DotPlus" -> updateDotPlus,

	"BatchNormalization" -> reorderBatchNormArrays	
};

reorderBatchNormArrays[assoc_] :=
	MapAt[KeySortBy[Position[{"Gamma", "Beta", "MovingMean", "MovingVariance"}, #]&], assoc, "Arrays"];

scalarFunctionParams[f_][assoc_] := Association[
	"Function" -> ValidatedParameter[
		CompileScalarFunction[1, f[#, assoc["Scalar"]]&]
	],
	"$Dimensions" -> assoc["$Dimensions"]
];

updateConvolution[params_] := 
	Append[params, "Dimensionality" -> 2];

updateElementwise[params_] := 
	MapAt[ValidatedParameter, KeyDrop[params, "$Rank"], "Function"];

updatePooling[params_] := Scope[
	Function[
		newSize = PoolingShape[#$InputSize, #PaddingSize, #KernelSize, #Stride, "valid"];
		If[VectorQ[newSize, IntegerQ] && newSize =!= #$OutputSize,
			FailValidation[PoolingLayer, 
				"PoolingLayer with given stride and input size cannot be imported into Mathematica " <> 
					ToString[$VersionNumber] <> "."
			]
		]
	] @ params;
	Append[params, {"Dimensionality" -> 2, "$MXPoolingConvention" -> "valid", "$MXGlobalPool" -> False}]
];

(* TODO: Convert other updates to use updateParams *)

updateSoftmax[params_] := 
	Association["$Dimensions" -> {params["Size"]}];

updateCrossEntropyLoss[params_] := Association[
	"TargetForm" -> params["TargetForm"],
	"$InputDimensions" -> {},
	"$Classes" -> params["$Dimensions"]
]

updateResize[params_] := Append[params, "Resampling" -> "Linear"];

updateAggregation[params_] := Join[
	KeyDrop[params, "$Channels"], 
	<|
		"Levels" -> ValidatedParameter[2;;],
		"$InputDimensions" -> Join[{params["$Channels"]}, params["$InputDimensions"]],
		"$OutputDimensions" -> {params["$Channels"]}
	|>
];

updateDropout[params_] := Append[params, "Method" -> "Dropout"];

updateReplicate[params_] := <|
	"Specification" -> ValidatedParameter@params["Specification"],
	"Level" -> 1,
	"OutputSize" -> params["OutputSize"],
	"$InsertedDimCount" -> Length@params["Specification"],
	"$InsertedDimensions" -> params["Specification"],
	"$InputSize" -> params["$InputSize"]
|>;

updateFlatten[assoc_] := 
	ReplacePart[assoc, 
		"Parameters" -> Association[
			"Level" -> Infinity,
			"$InputSize" -> OldTDimensions[assoc["Inputs", "Input"]],
			"OutputSize" -> OldTDimensions[assoc["Outputs", "Output"]]
		]
	];

updateCatenate[assoc_] := 
	ReplacePart[assoc,
		"Parameters" -> Association[
			"Level" -> 1,
			"$InputShapes" -> assoc["Inputs", "Input"],
			"$InputCount" -> assoc["Parameters", "$InputCount"],
			"$OutputShape" -> assoc["Outputs", "Output"]
		]
	];

updateDotPlus[assoc_] := Scope[
	idims = OldTDimensions @ assoc["Inputs", "Input"];
	osize = assoc["Parameters", "Size"];
	updateLinear[assoc, idims, osize]
];

updateLinear[assoc_, idims_, osize_] := Scope[
	assoc = assoc;
	assoc["Type"] = "Linear";
	assoc["Parameters"] = Association[
		"OutputDimensions" -> {osize},
		"$OutputSize" -> osize,
		"$InputSize" -> First[idims, SizeT],
		"$InputDimensions" -> idims
	];
	assoc
];

OldTDimensions[EncodedType[_, t_] | DecodedType[_, t_]] := OldTDimensions[t];
OldTDimensions[ChannelT[n_, inner_]] := Replace[OldTDimensions[inner], l_List :> Prepend[l, n]];
OldTDimensions[TensorT[n_Integer, dims_List]] := dims;
OldTDimensions[TensorT[size_, _]] := If[IntegerQ[size], Table[SizeT, size], SizeListT[]];
OldTDimensions[_] := SizeListT[];

renameContainerKeys[assoc_] := 
	MapAt[ReplaceAll[UpdatePatterns["11.0"]], "Nodes"] @ 
	MapAt[ReplaceAll["Vertices"|"Layers" -> "Nodes"], "Edges"] @ 
	KeyMap[Replace[{"Vertices"|"Layers" -> "Nodes", "Connections" -> "Edges"}]] @ 
	assoc;

removeRanks[layer_] := MapAt[
	KeyDrop[{"$Rank", "Rank", "$InputRank", "$OutputRank"}], 
	layer, "Parameters"
];

renameTo[name_] := ReplacePart["Type" -> name];

broadcastGone[_] := goneFail["BroadcastPlusLayer", "ReplicateLayer followed by ElementwiseLayer[Plus]"];
splitGone[_] := goneFail["SplitLayer", "several PartLayers"]

goneFail[name_, replaceHint_] := upgradeFail @ StringJoin[
	"the experimental layer ", name, " no longer exists, and has no direct analogue in this version. ",
	"The same functionality can be achieved with ", replaceHint
];

updateUpsample[assoc_] := Key["Parameters"] @ NData @ ResizeLayer[
	{1,1} * Scaled[assoc["Scale"]], 
	"Input" -> {assoc["Channels"], Automatic, Automatic}
];

General::netupgfail = "Net could not be upgraded: ``.";
upgradeFail[msg_] := ThrowFailure["netupgfail", msg];
