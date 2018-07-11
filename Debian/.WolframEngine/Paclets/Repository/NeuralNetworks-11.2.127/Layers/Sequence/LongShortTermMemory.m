Inputs: 
	$Input: SequenceT[$$SequenceLength, VectorT[$$InputSize]]

Outputs: 
	$Output: SequenceT[$$SequenceLength, VectorT[$OutputSize]]

States:
	$State: VectorT[$OutputSize]
	$CellState: VectorT[$OutputSize]

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
	$OutputGateInputWeights: TensorT[{$OutputSize, $$InputSize}]
	$OutputGateStateWeights: TensorT[{$OutputSize, $OutputSize}]
	$OutputGateBiases: VectorT[$OutputSize]
	$ForgetGateInputWeights: TensorT[{$OutputSize, $$InputSize}]
	$ForgetGateStateWeights: TensorT[{$OutputSize, $OutputSize}]
	$ForgetGateBiases: VectorT[$OutputSize]
	$MemoryGateInputWeights: TensorT[{$OutputSize, $$InputSize}]
	$MemoryGateStateWeights: TensorT[{$OutputSize, $OutputSize}]
	$MemoryGateBiases: VectorT[$OutputSize]

Writer: Function[
	s = GetState["State"];
	c = GetState["CellState"];
	in = GetInputMetaNode["Input"];
	{varq, {{xidrop, sidrop}, {xodrop, sodrop}, {xfdrop, sfdrop}, {xmdrop, smdrop}}, sudrop} = 
		MakeRNNDropoutData[First @ #Dropout, 4];
	batching = If[varq, Automatic, False];
	odims = {#OutputSize};
	xig = SowMetaMap[FCGate[xidrop, #InputGateInputWeights, #InputGateBiases],   in, odims, batching];
	xog = SowMetaMap[FCGate[xodrop, #OutputGateInputWeights, #OutputGateBiases], in, odims, batching];
	xfg = SowMetaMap[FCGate[xfdrop, #ForgetGateInputWeights, #ForgetGateBiases], in, odims, batching];
	xmg = SowMetaMap[FCGate[xmdrop, #MemoryGateInputWeights, #MemoryGateBiases], in, odims, batching];
	outs = NewSequenceMetaNode[in, odims];
	cells = NewSequenceMetaNode[in, odims];
	Do[
		i = SowSigmoid @ SowPlus[xig[[t]], SowDot[sidrop @ s, #InputGateStateWeights]];
		o = SowSigmoid @ SowPlus[xog[[t]], SowDot[sodrop @ s, #OutputGateStateWeights]];
		f = SowSigmoid @ SowPlus[xfg[[t]], SowDot[sfdrop @ s, #ForgetGateStateWeights]];
		m = SowTanh @    SowPlus[xmg[[t]], SowDot[smdrop @ s, #MemoryGateStateWeights]];
		c = MirrorStage @ SowPlus[SowHad[f, c], SowHad[i, sudrop @ m]];
		s = MirrorStage @ SowHad[o, SowTanh[c]];
		outs[[t]] = s;
		cells[[t]] = c;
	,
		{t, 1, Length[xig]}
	];
	SetOutput["Output", outs];
	SetState["State", SowMetaLast @ outs];
	SetState["CellState", SowMetaLast @ cells]; 
]

SummaryFunction: Function[
	If[$TradQ, "LSTM", LongShortTermMemoryLayer]
]

Tests: {
	{4, "Input" -> {3, 2}} -> "3*4_Vj7LfXURExY_LUShOwEGM/U",
	{4, "Input" -> {"Varying", 2}} -> "3*4_Vj7LfXURExY_VQJXAFORTEg"
}
