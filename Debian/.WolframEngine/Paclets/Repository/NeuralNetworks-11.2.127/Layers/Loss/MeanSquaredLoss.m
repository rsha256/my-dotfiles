InheritsFrom: "MeanAbsoluteLoss"

Writer: Function[
	MeanLossImplementation["L2"];
]

SummaryFunction: Function[
	If[$TradQ, TFCase @ "Mean Squared Error", MeanSquaredLossLayer]
]

Tests: {
	{"Input" -> 3} -> "_akQbzL2Wru4_CXMm+k8IbXU",
	{"Input" -> {3, 3}} -> "_MlpwwRo1v4c_NqUr6V94NH8",
	{"Input" -> "Scalar", "Target" -> "Scalar"} -> "_XDP256GjoCY_fvTJ02EfWtY",
	{"Input" -> "Varying"} -> "_akQbzL2Wru4_RqvfcP3TWoc",
	{"Input" -> {"Varying", 2}} -> "_ZHCL9OGeDLY_DJ54zC4pVkk"
}