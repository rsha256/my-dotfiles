Input: RealTensorT

Output: $Input

PostInferenceFunction: Function[
	If[TRank[$Input] === 1, CheckNotDynamic[SoftmaxLayer, $Input, "SoftmaxLayer does not currently support an input vector of varying length."]];
	idims = TDimensions[$Input];
	If[idims === {} || Last[idims, 0] === 1, FailValidation[SoftmaxLayer, "SoftmaxLayer of `` will produce a constant output of '1's no matter what input is given.", MsgForm @ $Input]];
]

AllowDynamicDimensions: True

Writer: Function[
	inputDims = GetInputDims["Input"];
	in = GetInput["Input"];
	If[Length[inputDims] === 1,
		out = SowNode["SoftmaxActivation", {in}]
	,
		(* rank > 1 case: need reshaping till MXNet supports this case *)
		in = SowUReshape[in, {-1, Last[inputDims]}];
		in = SowNode["SoftmaxActivation", {in}];
		maxLen = ToMaxLengths[First[inputDims]]; (*first dim could be dynamic*)
		out = SowReshape[in, Join[{maxLen}, Rest[inputDims]]]
	];
	SetOutput["Output", out];
]

Tests: {
	{"Input" -> {3}} -> "3_B7HUGnzh1Fk_Czr4LI/q+yE",
	{"Input" -> {3, 2}} -> "3*2_MKuq5+IjuZw_aDXMxKIKoOg",
	{"Input" -> {2, 3, 4}} -> "2*3*4_XoaIjPheTCw_UcIcNwjE4rs",
	{"Input" -> {"Varying", 4}} -> "3*4_eYnrf3zNzus_CknCSjCQIQM"
}