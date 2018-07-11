Inputs: <||>

Output: TensorT[$Dimensions]

Parameters:
	$Dimensions: SizeListT[]

MinArgCount: 0
MaxArgCount: 1

Arrays:
	Array: TensorT[$Dimensions]

Writer: Function[
	SetOutput["Output", SowBatchified[#Array]]
]

Tests: {
	{"Output" -> {3, 3}} -> "3*3_S+gOgNB2PmI_S+gOgNB2PmI"
}
