Package["NeuralNetworks`"]


(******************************************************************************)
PackageExport["NetDeinitialize"]

NNSetUsage @ "
NetDeinitialize[net$] removes all arrays from a net.
"

NetDeinitialize[NetP[net, meta]] := ConstructLayer[RemoveRawArrays[net], meta];

(******************************************************************************)
(* NetInitializer: a graph based parameter initializer *)

PackageExport["NetInitialize"]

Options[NetInitialize] = {
	Method -> Automatic
};

$NetInitializeMethods := $NetInitializeMethods = <|
	"Xavier" -> {Values /* Apply[XavierInitializer], <|
		"FactorType" -> Defaulting[EnumT[{"In", "Out", "Mean"}], "Mean"],
		"Distribution" -> Defaulting[EnumT[{"Uniform", "Normal"}]]
	|>},
	"Orthogonal" -> {OrthogonalInitializer&, <||>},
	"Identity" -> {
		IdentityInitializer[
			#Distribution]&, 
			<|"Distribution" -> Defaulting[DistributionT, NormalDistribution[0,0.01]]|>
	},
	"Random" -> {DistributionInitializer[#Weights, #Biases, #Scaling]&, <|
		"Weights" -> Defaulting[DistributionT, NormalDistribution[]], 
		"Biases" -> Defaulting[DistributionT, None],
		"Scaling" -> Defaulting[DistributionT, None]
	|>}
|>;

NetInitialize::notfspec = "All parameters must be fully specified before a network can be initialized."
NetInitialize::notudist = "`` should be a valid univariate distribution."

NetInitialize[list_List, opts:OptionsPattern[]] :=
	Map[NetInitialize[#, opts]&, list];

NetInitialize[net_, None] := NetDeinitialize[net];

NetInitialize[expr:(head_Symbol[assoc_Association, meta_]), replaceExisting:Except[_Rule]:Automatic, OptionsPattern[]] := CatchFailureAsMessage @ Scope[
	$replace = (replaceExisting === All);
	If[FullySpecifiedNetQ[expr] && !$replace, Return[expr]];
	$makeArray = makeArray;
	Match[OptionValue[Method],
		{"Custom", method_} :> (
			$makeArray = makeCustomArray;
			$customFunction = method;
		),
		method_ :> (
			$initializer = ParseMethod[method, $NetInitializeMethods];
		)
	];	
	$assoc = assoc;
	$initialLayers = None;
	$graph = LayerDependencyGraph[assoc];
	Scan[updateVertex, TopologicalSort[$graph]];
	System`Private`ConstructNoEntry[head, $assoc, meta]
]

NetInitialize::arg1 = "First argument to NetInitialize should be a net or list of nets."

NetInitialize[__] := RuleCondition[Message[NetInitialize::arg1]; Fail];

getVertexDepth[port_] := (
	If[$initialLayers === None, 
		$initialLayers = Complement[VertexList[$graph], EdgeList[$graph][[All, 1]]];
	];
	Min[GraphDistance[$graph, port, #]& /@ $initialLayers]
);

updateVertex[port:NetPath[pos___]] := Scope[
	subnet = Part[$assoc, pos];
	arrays = DeleteCases[None] @ subnet["Arrays"];
	type = subnet["Type"];
	If[!$replace, arrays = DeleteCases[_RawArray] @ arrays];
	$port = port;
	$depth := $depth = getVertexDepth[port];
	KeyValueMap[
		Set[
			$assoc[[pos, "Arrays", #1]], 
			$makeArray[type, #1, getDims[#2]]
		]&,
		arrays
	];
]

getDims[Nullable[t_]] := getDims[t];
getDims[arr_RawArray] := Dimensions[arr];
getDims[t_] := Replace[TDimensions[t], SizeT | $Failed :> ThrowNotSpecifiedFailure[$assoc, "initialize"], {0, Infinity}];

(* this is used as needed by makeArray. *)
LookupForwardLayer[port_, n_Integer] := Scope[
	If[n < 1, Panic[]];
	out = Complement[VertexOutComponent[$graph, port, n], VertexOutComponent[$graph, port, n - 1]];
	If[Length[out] === 0, None, Part[$assoc, Sequence @@ Last[out]]]
];

(******************************************************************************)
(* This contains the special Port/Layer level init logic. If adding a new layer
	with special init needs, add it here.
*)

spatialTransformerQ[type_] := 
(type === "Linear") && (LookupForwardLayer[$port, 1]["Type"]  === "SpatialTransformation") ||
(type === "Convolution") && (LookupForwardLayer[$port, 2]["Type"] === "SpatialTransformation")


(***************************************)

seluQ["Linear"] := Scope[
	forward = LookupForwardLayer[$port, 1];
	If[forward["Type"]  =!= "Elementwise", Return[False]];
	f = First[forward["Parameters", "Function"]];
	If[Head[f] =!= ScalarFunctionObject, Return[False]];
	ssa = f["SSA"];
	If[Length[Keys[ssa]] =!= 1, Return[False]];
	val = First[ssa];
	If[Not@ListQ[val] || (Length[val] === 0), Return[False]];
	If[First[val] === "ScaledExponentialLinearUnit",
		Return[True],
		Return[False]
	];
]

seluQ[_] := False

(***************************************)

NetInitialize::badcustominit = "Custom initializer did not return a number, tensor, or distribution."

randVar[dist_, dims_] := RandomVariate[dist, checkBigDims @ dims]
constArr[val_, dims_] := ConstantArray[val, checkBigDims @ dims]

PackageExport["$NetInitializeMaxTensorByteCount"]

$NetInitializeMaxTensorByteCount = $SystemMemory / 10;

General::tnsrmaxsz = "Cannot create a tensor of dimensions `` because it would consume ``, which is ``% of total system memory."

checkBigDims[dims_] := Scope[
	sz = 4 * Apply[Times, dims];
	If[sz > $NetInitializeMaxTensorByteCount,
		ThrowFailure["tnsrmaxsz", Row[dims, "\[Times]"], 
			PositiveSIString[sz] <> "bytes", 
			Round[(sz / $SystemMemory) * 100]
		]];
	dims
];


makeCustomArray[type_, name_, dim_List] := UseMacros @ 
	RawArray["Real32", Match[
		$customFunction[<|
			"Type" -> $TypeToSymbol[type], "Name" -> name, "Dimensions" -> dim, 
			"Outputs" -> First[dim], "Depth" -> $depth, 
			"NextLayer" -> LookupForwardLayer[$port, 1]|>],
		0.|0 :> constArr[0., dim],
		r_ ? NumberQ :> randVar[NormalDistribution[0, N[r]], dim],
		e_List ? MachineArrayQ :> If[Dimensions[e] =!= dim, Panic[], e],
		e_ ? UnivariateDistributionQ :> randVar[e, dim],
		ThrowFailure["badcustominit"]
	]];

(* Weights: Weight ports have special methods that can depend on activation functions *)
makeArray[type_, "Weights", dim_List] := Scope[
	result = Which[
		spatialTransformerQ[type],
			RawArray["Real32", constArr[0., dim]],
		seluQ[type],
			(* stddev = sqrt(1 / fan_in), fan_in = Last[dim] *)
			RawArray["Real32", 
				randVar[NormalDistribution[0, Sqrt[1 / Last[dim]]], dim] 
			],
		True,	(* defualt *)
			$initializer[type, dim, LookupForwardLayer[$port, 1]]
	];

	If[!RawArrayQ[result], Panic["NotRawArray"]];
	result
];

makeArray[_, "Biases", dim_List] /; setsBiasesQ[$initializer] := Scope[
	result = $initializer[type -> "Biases", dim, LookupForwardLayer[$port, 1]];
	If[!RawArrayQ[result], Panic["NotRawArray"]];
	result
];

makeArray[_, "Scaling", dim_List] /; setsScalingQ[$initializer] := Scope[
	result = $initializer[type -> "Scaling", dim, LookupForwardLayer[$port, 1]];
	If[!RawArrayQ[result], Panic["NotRawArray"]];
	result
];

(* Biases: All arrays named Biases are zero initialized *)

makeArray[type_, "ForgetGateBiases", dims_List] :=
	RawArray["Real32", ConstantArray[1., dims]];

makeArray[type_, "Biases", dim_List] := Which[
	spatialTransformerQ[type] && (First@dim === 6),
		RawArray["Real32", {1, 0, 0, 0, 1, 0}],	
	True,	
		RawArray["Real32", constArr[0., dim]]
]

(* Scale: All arrays named Scale are one initialized *)
makeArray[_, "Scaling", dim_List] :=
	RawArray["Real32", constArr[1., dim]]

(* ParametricRampLayer slope *)
makeArray[_, "Slope", dim_List] :=
	RawArray["Real32", constArr[0.1, dim]]

(* all other-named arrays are treated as weights if matrices otherwise zero-initialized *)
makeArray[type_, other_, dim_List] := 
	makeArray[type, If[Length[dim] == 2, "Weights", "Biases"], dim];

setsBiasesQ[_] := False;

setsScalingQ[_] := False;

(* BatchNorm Arrays: special init *)
makeArray["BatchNormalization" | "InstanceNormalization", "Beta" | "MovingMean", dim_List] := 
	RawArray["Real32", constArr[0., dim]]

makeArray["BatchNormalization" | "InstanceNormalization", "Gamma" | "MovingVariance", dim_List] := 
	RawArray["Real32", constArr[1., dim]]

(******************************************************************************)
(* Weight Initialization Methods: Xavier, Orthogonal, Distribution, and Identity. *)

setsScalingQ[DistributionInitializer[_, _, sdist_]] := sdist =!= None;

setsBiasesQ[DistributionInitializer[_, bdist_, _]] := bdist =!= None;

DistributionInitializer[wdist_, bdist_, sdist_][_, dim_, nextLayer_:None] :=
	RawArray["Real32", randVar[wdist, dim]];

DistributionInitializer[wdist_, bdist_, sdist_][_ -> "Scaling", dim_, nextLayer_:None] :=
	RawArray["Real32", randVar[sdist, dim]];

DistributionInitializer[wdist_, bdist_, sdist_][_ -> "Biases", dim_, nextLayer_:None] :=
	RawArray["Real32", randVar[bdist, dim]];

IdentityInitializer[dist_][type_, dim_List, nextLayer_:None] :=
	RawArray["Real32", AddNoise[IdentityTensor @@ dim, dist]];

IdentityInitializer[dist_]["Convolution"|"Deconvolution", dims:{nout_, nin_, w_, h_}, nextLayer_:None] := 
	RawArray["Real32", AddNoise[IdentityConvolutionKernel @@ dims, dist]];

AddNoise[arr_, dist_] := arr + randVar[dist, Dimensions[arr]];

IdentityConvolutionKernel[nout_, nin_, w_, h_] := Scope[
	unitKernel = ToPackedArray @ System`CenterArray[1, {w, h}];
	zeroKernel = constArr[0, {w, h}];
	Table[
		If[out == in, unitKernel, zeroKernel],
		{out, nout},
		{in, nin}
	]
];

IdentityTensor[a_, b_] := Take[IdentityMatrix @ Max[a, b], a, b];
IdentityTensor[m_, rest__] := Table[IdentityTensor[rest], m];


(***********
	xavierInitializer: based on the following papers
	1. Understanding the difficulty of training deep feedforward neural networks,
		X. Glorot and Y. Bengio
	2. Delving Deep into Rectifiers: Surpassing Human-Level Performance on ImageNet Classification,
		K. He et al
***********)

XavierInitializer[factorType_, distribution_][type_, dim_List, nextLayer_:None] := Scope[
	(* 1. Get number of input + output neurons *)
	fanin = Times @@ Rest[dim];
	fanout = First[dim];
	
	(* 2. The scale depends on the activation function. See He et al *)
	variance = Switch[factorType, 
		"In", 		2 / fanin,
		"Out", 		2 / fanout,
		"Mean", 	2 / (fanin + fanout)
	];
	scaleFactor = activationScaleFactor[nextLayer];
	variance *= scaleFactor;
	(* 3. Sample from distribution of given variance *)
	stddev = Sqrt[variance];
	values = Switch[distribution,
		"Normal",
			RawArray["Real32", randVar[NormalDistribution[0, stddev], dim]],
		"Uniform",
			(* using StandardDeviation@UniformDistribution[{-n, n}] = n/Sqrt[3] *)
			RawArray["Real32", randVar[UniformDistribution[{-1,1} * -stddev * Sqrt[3.]], dim]]
	]
]

(***********
 orthogonalInitializer: based on:
	Exact solutions to the nonlinear dynamics of learning in deep linear neural networks
		http://arxiv.org/pdf/1312.6120v3.pdf
		A.M. Saxe et al 2014
	NOTE: we follow Lasagne implementation (https://github.com/Lasagne/Lasagne/blob/master/lasagne/init.py),
		and 
***********)

OrthogonalInitializer[type_, dim_List, nextLayer_:None] := Scope[
	(* 1. Get number of input + output neurons *)
	fanin = Times @@ Rest[dim];
	fanout = First[dim];
	flatShape = {fanout, fanin};
	
	scaleFactor = activationScaleFactor[nextLayer];
	a = randVar[NormalDistribution[0, 1], flatShape];
	{u, w, v} = SingularValueDecomposition[a, Min@Dimensions@a];

	(* Choose one with correct shape *)
	q = If[Dimensions[u] === flatShape, u, v];
	q = ArrayReshape[q, dim] * Sqrt[scaleFactor];

	RawArray["Real32", q]
]

(***********
	 Scale factors:
		Decide on the scale depending on type of rectifier. 
		Used by orthogonalInitializer + xavierInitializer
		see http://arxiv.org/pdf/1505.00853.pdf for overview of rectifiers
***********)

(* should factor this out at some point.. *)
reluQ[ValidatedParameter[func_]] := Scope[
	(* check for ramp. Simplest case *)
	If[func === Ramp, Return[True]];
	(* now check for string arg form of ramp *)
	If[Head[func] =!= ScalarFunctionObject, Return[False]];
	ssa = func["SSA"];

	(* for compound elementwise layers, return false *)
	If[Length[Keys[ssa]] =!= 1, Return[False]];
	val = First[ssa];
	If[Not[ListQ @ val] || (Length[val] === 0), Return[False]];
	If[First[val] === "RectifiedLinearUnit",
		Return[True],
		Return[False]
	];
]

reluQ[___] := Panic["reluQ assumes input is ValidatedParameterT[...]. Invalid input."]

(* high level scale *)
activationScaleFactor[None] := 1
activationScaleFactor[assoc_Association] := 
	iActivationScaleFactor[assoc["Type"], assoc["Parameters"]];

(* Case 1: Relu/Ramp *)
iActivationScaleFactor["Elementwise", param_] := If[reluQ @ param["Function"], 2, 1];

(* Case Default: Relu/Ramp *)
iActivationScaleFactor[other_, param_] := 1;

