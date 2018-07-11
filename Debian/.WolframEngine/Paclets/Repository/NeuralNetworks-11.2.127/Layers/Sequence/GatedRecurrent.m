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
	$InputGateInputWeights: TensorT[{$OutputSize, $$InputSize}]
	$InputGateStateWeights: TensorT[{$OutputSize, $OutputSize}]
	$InputGateBiases: VectorT[$OutputSize]
	$ResetGateInputWeights: TensorT[{$OutputSize, $$InputSize}]
	$ResetGateStateWeights: TensorT[{$OutputSize, $OutputSize}]
	$ResetGateBiases: VectorT[$OutputSize]
	$MemoryGateInputWeights: TensorT[{$OutputSize, $$InputSize}]
	$MemoryGateStateWeights: TensorT[{$OutputSize, $OutputSize}]
	$MemoryGateBiases: VectorT[$OutputSize]

Writer: Function[
	s = GetState["State"];
	in = GetInputMetaNode["Input"];
	{varq, {{xidrop, sidrop}, {xrdrop, srdrop}, {xmdrop, smdrop}}, sudrop} = MakeRNNDropoutData[First @ #Dropout, 3];
	batching = If[varq, Automatic, False];
	odims = {#OutputSize};
	xig = SowMetaMap[FCGate[xidrop, #InputGateInputWeights,  #InputGateBiases],  in, odims, batching];
	xrg = SowMetaMap[FCGate[xrdrop, #ResetGateInputWeights,  #ResetGateBiases],  in, odims, batching];
	xmg = SowMetaMap[FCGate[xmdrop, #MemoryGateInputWeights, #MemoryGateBiases], in, odims, batching];
	out = NewSequenceMetaNode[in, odims];
	Do[
		z = SowSigmoid @ SowPlus[xig[[i]], SowDot[sidrop @ s, #InputGateStateWeights]];
		r = SowSigmoid @ SowPlus[xrg[[i]], SowDot[srdrop @ s, #ResetGateStateWeights]];
		h =    SowTanh @ SowPlus[xmg[[i]], SowHad[r, SowDot[smdrop @ s, #MemoryGateStateWeights]]];
		s = SowMix[sudrop @ h, s, z];  (* s = (1-z)*s + z*h *)
		out[[i]] = s;
	,
		{i, 1, Length[in]}
	];
	SetOutput["Output", out];
	SetState["State", SowMetaLast @ out];
]

SummaryFunction: Function[
	If[$TradQ, "GRU", GatedRecurrentLayer]
]

Tests: {
	{4, "Input" -> {3,2}} -> "3*4_GZ+VnS6rVlk_IvY3AX/1N9U",
	{4, "Input" -> {"Varying", 2}} -> "3*4_GZ+VnS6rVlk_OiuvVkqvBWg"
}
