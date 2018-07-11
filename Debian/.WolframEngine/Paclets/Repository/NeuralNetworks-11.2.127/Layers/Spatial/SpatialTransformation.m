Inputs:
	$Input: TensorT[{$$Channels, SizeT, SizeT}]
	$Parameters: VectorT[6]

Output: TensorT[{$$Channels}, TensorT[$Dimensions]]

Parameters:
	$Dimensions: SizeListT[2]
	$$Channels: SizeT

MinArgCount: 1

MXNet: 
	Name: "SpatialTransformer"
	Parameters:
		$Dimensions: "target_shape"
	Writer: Function[{
		"transform_type" -> "affine",
		"sampler_type" -> "bilinear"
	}]