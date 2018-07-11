Input: ChannelT[$InputChannels, TensorT[$$InputSize]]

Output: ChannelT[$OutputChannels, TensorT[$$OutputSize]]

Arrays:
	$Weights: TensorT[{$OutputChannels, $InputChannels}, TensorT[$KernelSize]]
	$Biases: Nullable[VectorT[$OutputChannels]]

Parameters:
	$OutputChannels: SizeT
	$KernelSize:     ArbSizeListT[$Dimensionality, SizeT, None]
	$Stride:         ArbSizeListT[$Dimensionality, PosIntegerT, 1]
	$PaddingSize:    ArbSizeListT[$Dimensionality, NaturalT,    0]
	$Dilation:       ArbSizeListT[$Dimensionality, PosIntegerT, 1]
	$Dimensionality: NaturalT
	$InputChannels:  SizeT
	$$GroupNumber:   Defaulting[SizeT, 1]
	$$InputSize:     SizeListT[$Dimensionality]
	$$OutputSize:    ComputedType[SizeListT[$Dimensionality], ConvolutionShape[$$InputSize, $PaddingSize, $KernelSize, $Stride, $Dilation]]

MinArgCount: 0

PostInferenceFunction: Function[
	CheckConvolutionOrPoolingFunction[ConvolutionLayer, $Dimensionality, $$InputSize, $KernelSize, $$OutputSize, $PaddingSize]
]

MXNet:
	Name: "Convolution"
	Parameters: 
		$OutputChannels: "num_filter"
		$KernelSize: "kernel"
		$Dilation: "dilate"
		$PaddingSize: "pad"
		$Stride: "stride"
		$$GroupNumber: "num_group"
	Arrays:
		$Weights: "weight"
		$Biases: "bias"
	Writer: Function[{
		"no_bias" -> If[#2["Biases"] === None, "True", "False"]
	}] 

Tests: {
	{2, 2, "Input" -> {2, 4, 4}} -> "2*3*3_B430te9futY_S8Wlsk2ZI0Y",
	{1, 3, "Input" -> {2, 4, 4}} -> "1*2*2_XDSMO0pOdfY_IwDq3mBlVa0",
	{2, 2, "Stride" -> 2, "Input" -> {2, 8, 8}} -> "2*4*4_ADOzcKP1tl4_bFjVdOX3H9s",
	{2, 2, "Dilation" -> 2, "Input" -> {2, 8, 8}} -> "2*6*6_J6F6xTgybso_VpmU0KssAvw",

	(* 1-d *)
	{2, {2}, "Input" -> {1, 16}} -> "2*15_e2JXK6TJUSs_YQG1ClW5njs"
}

