Input: $$Shape
Output: $$Shape

Parameters:
	$ChannelWindowSize: Defaulting[PosIntegerT, 2]
	$Alpha: Defaulting[ScalarT, 1.0]
	$Beta: Defaulting[ScalarT, 0.5]
	$Bias: Defaulting[ScalarT, 1.0]
	$$Channel: SizeT
	$$Shape: TensorT[{$$Channel, SizeT, SizeT}]

MXNet:
	Name: "LRN"
	Parameters: 
		$Alpha: "alpha"
		$Beta: "beta"
		$Bias: "knorm"
	Writer: Function["nsize" -> ToString[2 * #ChannelWindowSize + 1]]
	Reader: Function["ChannelWindowSize" -> (ToExpression@#nsize - 1)/2]
	
PostInferenceFunction: Function[
	If[($ChannelWindowSize * 2 + 1) > $$Channel,
		FailValidation["2 * channel window size + 1 cannot be larger than the number of channels in the input."]
	];
]

Tests: {
	{"Input" -> {5, 4, 4}} -> "5*4*4_LvY7WFc5wAY_Foi5OFUK+vI"
}