Inputs: 
	$Input: TensorT[$$Dimensions]
	$Target: TensorT[$$Dimensions]

Outputs:
	$Loss: ScalarT 

Parameters:
	$$Dimensions: SizeListT[]

AllowDynamicDimensions: True

IsLoss: True

Writer: Function[
	MeanLossImplementation["L1"];
]

SummaryFunction: Function[
	If[$TradQ, TFCase @ "Mean Absolute Error", MeanAbsoluteLossLayer]
]

Tests: {
	{"Input" -> 3} -> "_BpevayjPuJI_OdsqpH0qyNc",
	{"Input" -> {3, 3}} -> "_X60SkhiiT3Y_PBgdmHGIFR4",
	{"Input" -> "Scalar", "Target" -> "Scalar"} -> "_U8T8OMC1S5E_CnBwi6oyuUo",
	{"Input" -> "Varying"} -> "_BpevayjPuJI_H9tlpdKg/i0",
	{"Input" -> {"Varying", 2}} -> "_RzpwkzDVovY_T2HVyzGRH3w"
}