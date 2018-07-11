Input: ChannelT[$InputChannels, TensorT[$$InputSize]]

Output: ChannelT[$OutputChannels, TensorT[$$OutputSize]]

Arrays:
	$Weights: TensorT[{$InputChannels, $OutputChannels}, TensorT[$KernelSize]]
	$Biases: Nullable[VectorT[$OutputChannels]]

Parameters:
	$OutputChannels: 	SizeT
	$KernelSize:  		ArbSizeListT[2, PosIntegerT, None]
	$Stride: 			ArbSizeListT[2, PosIntegerT, 1]
	$PaddingSize: 		ArbSizeListT[2, NaturalT, 0]
	$InputChannels: 	SizeT
	$$GroupNumber: 		Defaulting[SizeT, 1]
	$$InputSize: 		SizeListT[2]
	$$OutputSize: 		ComputedType[SizeListT[2], DeconvolutionShape[$$InputSize, $PaddingSize, $KernelSize, $Stride]]

MinArgCount: 0

PostInferenceFunction: Function[
	If[Min[$$OutputSize] < 1, FailValidation["choice of parameters results in a zero-size output tensor."]]
]

MXNet:
	Name: "Deconvolution"
	Parameters: 
		$OutputChannels: "num_filter"
		$KernelSize: "kernel"
		$PaddingSize: "pad"
		$Stride: "stride"
		$$GroupNumber: "num_group"
	Arrays:
		$Weights: "weight"
		$Biases: "bias"
	Writer: Function[{
		"no_bias" -> If[#2["Biases"] === None, "True", "False"]
	}] 