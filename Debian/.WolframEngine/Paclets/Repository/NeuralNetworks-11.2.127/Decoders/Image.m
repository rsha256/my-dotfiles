Input: TensorT[{$$Channels}, TensorT[$$Dimensions]]

Parameters:
	$ColorSpace: Defaulting[ColorSpaceT, "RGB"]
	$MeanImage: Defaulting[Nullable[EitherT[{ScalarT, ListT[SizeT, ScalarT], ImageT[]}]]]
	$VarianceImage: Defaulting[Nullable[EitherT[{ScalarT, ListT[SizeT, ScalarT], ImageT[]}]]]
	$$Dimensions: SizeListT[2]
	$$Channels: SizeT

DecoderToEncoder: Function[
	dims = TDimensions[#2];
	NetEncoder[{"Image", 
		"ImageSize" -> Reverse[Rest[dims]], "ColorChannels" -> First[dims],
		"ColorSpace" -> #ColorSpace, "MeanImage" -> #MeanImage,
		"VarianceImage" -> Lookup[#, "VarianceImage", None]
	}]
]

ArrayDepth: 3

ToDecoderFunction: Function @ With[
	{cspace = #ColorSpace, mean = #MeanImage, var = #VarianceImage},
	meaner = Switch[mean, 
		None, Identity,
		_Image, ImageAdd[#, ImageResize[mean, ImageDimensions[#]]]&,
		_, ImageAdd[#, mean]&
	];
	MapB[#2,
		Function[input, 
			If[cspace === "RGB" && Length[input] =!= 3,
				ColorConvert[Image[input, Interleaving -> False], "RGB"],
				Image[input, ColorSpace -> cspace, Interleaving -> False]
			]
		] /* meaner
	]
]


