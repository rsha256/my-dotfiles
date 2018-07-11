Inputs: 
	Input: SequenceT[$$SequenceLength, $$InputShape]

Outputs: 
	Output: SequenceT[$$SequenceLength, $$OutputShape]

States:
	$State: $$OutputShape

Suffix: "Operator"

Parameters:
	$Net: NetT[<|"Input" -> $$InputShape, "State" -> $$OutputShape|>, <|"Output" -> $$OutputShape|>]
	$$SequenceLength: LengthVar[]
	$$InputShape: AnyTensorT
	$$OutputShape: AnyTensorT

PostInferenceFunction: Function[
	PostSet[$Input, SequenceT[$$SequenceLength, $Net["Inputs", "Input"]]];
	PostSet[$Output, SequenceT[$$SequenceLength, $Net["Outputs", "Output"]]];
]

Writer: Function[
	in = GetInputMetaNode["Input"];
	state = GetState["State"];
	out = NewSequenceMetaNode[in, TDimensions[#$OutputShape]];
	MXDo[
		state = SowInnerNet[<|"Input" -> in[[i]], "State" -> state|>, {"Parameters", "Net"}, #Net];
		state = Lookup[state, "Output"];
		out[[i]] = state;
	,
		{i, Length[in]}
	];
	SetOutput["Output", out];
	SetState["State", SowMetaLast @ out];
]

Tests: {
	{Hold @ NetGraph[{2,Times},{NetPort["Input"]->1,{1,NetPort["State"]}->2}],"Input"->SequenceOf[2]} -> "3*2_detGAPjfd7M_OXUcrmPXDXA",
	{Hold @ NetGraph[{ThreadingLayer[Plus]},{{NetPort["Input"],NetPort["State"]}->1}],"Input"->SequenceOf[2]} -> "3*2_QlYGSoBwysk_D17I3FDKchA"
}