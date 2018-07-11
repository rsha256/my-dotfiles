Input: TensorT[$$IDimensions]

Output: TensorT[$Dimensions]

Parameters:
	$Dimensions: SizeListT[]
	$$IDimensions: SizeListT[]

PostInferenceFunction: Function[
	If[VectorQ[$Dimensions, IntegerQ] && VectorQ[$$IDimensions, IntegerQ] && Apply[Times, $Dimensions] =!= Apply[Times, $$IDimensions], 
		FailValidation["number of elements in output tensor must equal number of elements in input tensor."]
	]
]

MXNet:
	Name: "Reshape"
	Writer: Function[
		"shape" -> writeIntList[Prepend[#Dimensions, -1]]
	]

Tests: {
	{{4, 2}, "Input" -> {2, 4}} -> "4*2_Liad5oMW3eI_Vg5nFN5ntso",
	{{1, 2, 3}, "Input" -> {6}} -> "1*2*3_SINkZe0REmI_A/KyG112bzs",
	{{3, 2}, "Input" -> {2, 4}} -> "Validation failed for ReshapeLayer: number of elements in output tensor must equal number of elements in input tensor."
}