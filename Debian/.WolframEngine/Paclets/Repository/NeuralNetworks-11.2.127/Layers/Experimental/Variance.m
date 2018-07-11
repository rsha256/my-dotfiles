Input: VectorT[$$Size]

Output: ScalarT

Parameters:
	$$Size: SizeT

Writer: Function[
	in = GetInput["Input"];
	summed = SowNode["sum", in, "axis" -> {1}, "keepdims" -> True];
	mean = SowNode["_DivScalar", summed, "scalar" -> #$Size];
	diff = SowNode["broadcast_minus", {in, mean}];
	diff = SowNode["square", diff];
	var = SowNode["sum", diff, "axis" -> {1}, "keepdims" -> False];
	var = SowNode["_DivScalar", var, "scalar" -> (#$Size-1)];
	SetOutput["Output", var];
]

Tests: {
	{"Input" -> {4}} -> "_aJLkKrdtg3k_M5ae7/fcCDA"
}