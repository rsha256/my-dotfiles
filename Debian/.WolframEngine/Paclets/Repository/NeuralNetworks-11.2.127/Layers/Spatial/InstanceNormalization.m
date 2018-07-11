Input: ChannelT[$$Channels, TensorT[$$InputDimensions]]
Output: ChannelT[$$Channels, TensorT[$$InputDimensions]]

Parameters:
	$Epsilon: Defaulting[ScalarT, 0.001]
	$$Channels: SizeT
	$$InputDimensions: SizeListT[SizeT]

Arrays:
	$Gamma: VectorT[$$Channels]
	$Beta: VectorT[$$Channels]

MXNet:
	Name: "InstanceNorm"
	Parameters: 
		$Epsilon: "eps"
	Arrays:
		$Gamma: "gamma"
		$Beta: "beta"