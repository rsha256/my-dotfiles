Input: TensorT[$Dimensions]

Output: TensorT[$Dimensions]

Arrays:
	$Scaling: TensorT[$Dimensions]

Parameters:
	$Dimensions: SizeListT[]

MinArgCount: 0

AllowDynamicDimensions: True

Writer: Function[
	reshaped = SowNode["Reshape", #Scaling, "shape" -> Prepend[#Dimensions, 1]];
	SetOutput["Output", SowNode["broadcast_mul", {GetInput["Input"], reshaped}]]
]

Tests: {
	{"Input" -> 3} -> "3_AIvAa0t3efE_dSrbrONjgxw",
	{"Input" -> {2, 3}} -> "2*3_XNJtjBjJ1rk_Luzvstmun4A",
	{"Input" -> "Scalar"} -> "1_dZt2SS1lszQ_EpNq1f2dLlk",
	{"Input" -> {3}, "Scaling" -> {1, 2, 3}} -> "3_EDrB4+vbRz0_M+TnFc63bxs"
}