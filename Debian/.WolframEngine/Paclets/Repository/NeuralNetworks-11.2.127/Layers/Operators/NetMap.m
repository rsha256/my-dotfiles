Input: SequenceT[$$SequenceLength, $$InputShape]

Output: SequenceT[$$SequenceLength, $$OutputShape]

Suffix: "Operator"

Parameters:
	$Net: NetT[<|"Input" -> $$InputShape|>, <|"Output" -> $$OutputShape|>]
	$$SequenceLength: LengthVar[]
	$$InputShape: AnyTensorT
	$$OutputShape: AnyTensorT

PostInferenceFunction: Function[
	PostSet[$Input, SequenceT[$$SequenceLength, $Net["Inputs", "Input"]]];
	PostSet[$Output, SequenceT[$$SequenceLength, $Net["Outputs", "Output"]]];
]

SummaryFunction: Function[
	sub = SummaryForm[#Net];
	If[$TradQ,
		Row[{"Mapped", " ", SummaryForm[#Net]}],
		HoldForm[NetMapOperator][sub]
	]
]

Writer: Function[
	out = SowMetaMap[
		Function[{in, n}, SowInnerNet[<|"Input" -> in|>, {"Parameters", "Net"}, #Net, n]["Output"]],
		GetInputMetaNode["Input"], TDimensions[#$OutputShape],
		True (* force batching to avoid copying inner net many times *)
	];
	SetOutput["Output", out];
]

Tests: {
	{Hold @ LinearLayer[2, "Input" -> 1]} -> "3*2_TqdzaQfy9+w_fPNZLgMdZc0"
}
