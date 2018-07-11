Inputs: 
	$Input: SequenceT[$$SequenceLength, VectorT[$$InputSize]]

Outputs: 
	$Output: SequenceT[$$SequenceLength, VectorT[$OutputSize]]

States:
	$State: VectorT[$OutputSize]

Parameters:
	$OutputSize: SizeT
	$Dropout: RecurrentDropoutMethodT
	$$InputSize: SizeT
	$$SequenceLength: LengthVar[]

MaxArgCount: 1

Arrays:
	$InputWeights: TensorT[{$OutputSize, $$InputSize}]
	$StateWeights: TensorT[{$OutputSize, $OutputSize}]
	$Biases: VectorT[$OutputSize]

PostInferenceFunction: Function[
	If[MatchQ[First @ $Dropout, KeyValuePattern["UpdateVector" -> n_ /; n > 0]],
		FailValidation["\"UpdateVector\" dropout method is not applicable to BasicRecurrentLayer."]
	]
]

Writer: Function[
	in = GetInputMetaNode["Input"];
	odims = {#OutputSize};
	out = NewSequenceMetaNode[in, odims];
	s = GetState["State"];	
	{varq, {{xdrop, sdrop}}, sudrop} = MakeRNNDropoutData[First @ #Dropout, 1];
	batching = If[varq, Automatic, False];
	xg = SowMetaMap[FCGate[xdrop, #InputWeights, #Biases], in, odims, batching];
	Do[
		x = in[[i]];
		s = SowTanh @ SowPlus[xg[[i]], SowDot[sdrop @ s, #StateWeights]];
		out[[i]] = s;
	,
		{i, 1, Length[in]}
	];
	SetOutput["Output", out];
	SetState["State", SowMetaLast @ out];
]

Tests: {
	{4, "Input" -> {3,2}} -> "3*4_W3d/l8OrNmk_cO9y4bq8eFc",
	{4, "Input" -> {"Varying",2}} -> "3*4_W3d/l8OrNmk_WwyfiKjZRMk"
}
