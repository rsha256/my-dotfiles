Input: TensorT[$Dimensions]

Output: ScalarT

Parameters:
	$Dimensions: SizeListT[]

MinArgCount: 0

PostInferenceFunction: Function[
	If[$Dimensions === {}, 
		FailValidation[SummationLayer, "input tensor should have rank at least 1."];
	]
]

Writer: Function[
	If[Length[#Dimensions] === 1, WriterFallthrough[]];
	in = GetInput["Input"];
	out = SowNode["sum", in, "axis" -> Range[Length[#Dimensions]], "keepdims" -> False];
	SetOutput["Output", out];
]

MXNet:
	Name: "sum"
	Writer: Function[{
		"axis" -> "(1)",
		"keepdims" -> "false"
	}]

Tests: {
	{"Input" -> 3} -> "_bt+kDbKWQgU_BwIYzpiqozg",
	{"Input" -> {3, 3}} -> "_Vd3jC2T9XxE_OyXiD9uspTI"
}