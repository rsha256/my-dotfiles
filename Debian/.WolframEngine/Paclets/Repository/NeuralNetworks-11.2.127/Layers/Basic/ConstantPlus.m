Input: TensorT[$Dimensions]

Output: TensorT[$Dimensions]

Arrays:
	$Biases: TensorT[$Dimensions]

Parameters:
	$Dimensions: SizeListT[]

MinArgCount: 0

AllowDynamicDimensions: True

Writer: Function[
	reshaped = SowNode["Reshape", #Biases, "shape" -> Prepend[#Dimensions, 1]];
	SetOutput["Output", SowNode["broadcast_plus", {GetInput["Input"], reshaped}]]
]

Tests: {
	{"Input" -> 3} -> "3_QemtvEXs7rE_M2JMbKSj9a8",
	{"Input" -> {2, 3}} -> "2*3_VyAOG9jA63Y_GE3y5h8yo8A",
	{"Input" -> "Scalar"} -> "1_W6/4dnFZXcA_TfPVz8ZUa+w",
	{"Input" -> {3}, "Biases" -> {1, 2, 3}} -> "3_JOFasL+2yeg_ZvkzEF+Qyso"
}