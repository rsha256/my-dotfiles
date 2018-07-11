Inputs:
	$Input: ChannelT[$$Channels, TensorT[$$InputDimensions]]

Output: ChannelT[$$Channels, TensorT[$OutputDimensions]]

Parameters:
	$OutputDimensions: SizeListT[2]
	$ReflectionProbabilities: Defaulting[ListT[2,IntervalScalarT[0,1]], {0,0}]
	$$Channels: SizeT
	$$InputDimensions: SizeListT[2]

MinArgCount: 1

PostInferenceFunction: Function[
	If[Min[$$InputDimensions - $OutputDimensions] < 0, 
		FailValidation[ImageAugmentationLayer, "output dimensions `` must be smaller than input dimensions ``.", $OutputDimensions, $$InputDimensions]
	];
]

Writer: Function[
	input = GetInput["Input"];
	before = #$InputDimensions;
	after = #OutputDimensions;
	rprob = #ReflectionProbabilities;
	If[!$TMode,
		output = SowNode["Crop", input, "center_crop" -> True, "num_args" -> 1, "h_w" -> after];
	,
		cropLimit = SowFixedArray[RawArray["Real32", before - after + 1]];
		rcrop = SowNode["floor", SowBHad[SowUniformRandom[{2}], cropLimit]];
		If[Max[rprob] > 0, 
			rprob = SowFixedArray[RawArray["Real32", rprob - 0.5]];
			reflect = SowNode["round", SowBPlus[SowUniformRandom[{2}], rprob]];
		,
			reflect = SowZeroArray[{2}];
		];
		output = SowNode["ImageAugment", {input, rcrop, reflect}, "crop_size" -> after];
	];
	SetOutput["Output", output];
]