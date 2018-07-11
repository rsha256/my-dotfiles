Input: SequenceT[2, $$InputShape]

Output: ScalarT

Suffix: "Operator"

Parameters:
	$Net: NetT[<|"Input" -> $$InputShape|>, <|"Output" -> $$OutputShape|>]
	$DistanceFunction: Defaulting @ EnumT[{EuclideanDistance, CosineDistance}]
	$$InputShape: AnyTensorT
	$$OutputShape: AnyTensorT

PostInferenceFunction: Function[
	PostSet[$Input, SequenceT[2, $Net["Inputs", "Input"]]]
]

Writer: Function[
	idims = TDimensions[#$InputShape]; 
	odims = TDimensions[#$OutputShape];
	orank = Length[odims];
	pre = SowFlatten1 @ GetInput["Input", "Timewise"];
	out = SowInnerNet[<|"Input" -> pre|>, {"Parameters", "Net"}, #Net, 2]["Output"];
	out = SowUnflatten1[out, 2];
	{out1, out2} = SowUnpack[out, 2, 0];
	ofunc = Switch[#DistanceFunction,
		CosineDistance, SowCosineDistance,
		EuclideanDistance, SowEuclideanDistance
	];
	SetOutput["Output", ofunc[out1, out2, orank]];
]

Tests: {
	{Hold @ ElementwiseLayer[Ramp, "Input" -> 2]} -> "_Twwpo0AiU+o_KzteSorRuVk"
}