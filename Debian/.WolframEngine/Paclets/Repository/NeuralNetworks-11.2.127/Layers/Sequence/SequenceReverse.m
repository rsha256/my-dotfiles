Input: AnyTensorT

Output: $Input

AllowDynamicDimensions: True

Writer: Function[
	input = GetInputMetaNode["Input"];
	reverse = SowMetaReverse[input];
	SetOutput["Output", reverse];
]

Tests: {
	{"Input" -> {3, 2, 2}} -> "3*2*2_MWzfPOzBY/k_MzZ7ruTUvlo",
	{"Input" -> {"Varying", 1}} -> "3*1_c/ktmxJfUAw_RpRzvno9DU4",
	{"Input" -> {"Varying", 2, 2}} -> "3*2*2_MWzfPOzBY/k_O24ncqqz3QM",
	{"Input" -> {"Varying"}} -> "3_GvtHLiAnhdk_D20EjFN5x2g"
}
