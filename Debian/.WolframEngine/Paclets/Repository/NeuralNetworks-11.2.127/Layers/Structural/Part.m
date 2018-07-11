Input: ChannelT[$$Channels, TensorT[$$Dimensions]]
Output: TensorT[$$OutputDimensions]

Parameters:
	$PartSpecification: ValidatedParameterT[checkPartSpec, Automatic]
	$$Dimensions: SizeListT[]
	$$Channels: SizeT
	$$OutputDimensions: ComputedType[
		SizeListT[],
		ValidateShape[PartLayer, PartShape[Prepend[$$Dimensions, $$Channels], First @ $PartSpecification]],
		{$$Channels, $$Dimensions}
	]

checkPartSpec[s:Span[i:(_Integer|All), j:(_Integer|All)]] := Which[
	j === 0 || i === 0, 
		FailValidation[PartLayer, "part specification must be non-zero."],
	TrueQ[i > j && Positive[i*j]],
		FailValidation[PartLayer, "`` is an invalid part specification.", s],
	True,
		s /. All -> -1
];

checkPartSpec[i_Integer] := If[i === 0, 
	FailValidation[PartLayer, "part specification must be non-zero."],
	i
];

checkPartSpec[i_] := FailValidation[PartLayer, "`` is an invalid part specification.", i];

Writer: Function[
	spec = First[#PartSpecification];
	spec = spec /. i_Integer ? Negative :> (i + 1 + #$Channels);
	input = GetInput["Input"];
	sliced = Match[spec,
		Span[a_, b_] :> SowTake[input, {a-1, b}, 1],
		a_Integer :> SowReshape[SowTake[input, {a-1, a}, 1], #$OutputDimensions]
	];
	SetOutput["Output", sliced];
]

Tests: {
	{1, "Input" -> {3, 3, 3}} -> "3*3_YNBbVYLSHUk_Purc7467SgU",
	{3, "Input" -> {3, 3, 3}} -> "3*3_cQuubcUVLKw_amG3uZozvmA",
	{3, "Input" -> {5}} -> "_akQbzL2Wru4_SGnsYBYFcVs",
	{-5, "Input" -> {7}} -> "_akQbzL2Wru4_H57BCjL1SF4",
	{2 ;; 4, "Input" -> {5}} -> "3_cwgaM9SDvGc_Avbt9Hpc5tY",
	{1 ;; 2, "Input" -> {3, 3, 3}} -> "2*3*3_Qitdrci69/U_N6HuS/ImClk",
	{2 ;; 2, "Input" -> {3, 3, 3}} -> "1*3*3_USjn1b6jTHw_VnjfaAO82oo",
	{-2 ;; All, "Input" -> {3, 3, 3}} -> "2*3*3_OlQBee2x8QY_ZHQb7ZWMjVM",
	{1 ;; All, "Input" -> {3, 3, 3}} -> "3*3*3_O/ez3TErae8_bl6p06Ct7OM",
	{4, "Input" -> {3, 3, 3}} -> "Validation failed for PartLayer: the part specification 4 cannot reference positions greater than 3.",
	{3 ;; 2, "Input" -> {3, 3, 3}} -> "Validation failed for PartLayer: Span[3, 2] is an invalid part specification."
}
