Inputs: 
	Input: SequenceT[$$SequenceLength, $$InputShape]

Outputs: 
	Output: SequenceT[$$SequenceLength, $$OutputShape]

States:
	$State: $$StateShape

Suffix: "Operator"

Parameters:
	$Net: NetT[<|"Input" -> $$InputShape, "State" -> $$StateShape|>, <|"Output" -> $$OutputShape, "State" -> $$StateShape|>]
	$$SequenceLength: LengthVar[]
	$$InputShape: TypeT
	$$OutputShape: TypeT
	$$StateShape: TypeT

PostInferenceFunction: Function[
	PostSet[$Input, SequenceT[$$SequenceLength, $Net["Inputs", "Input"]]];
	PostSet[$Output, SequenceT[$$SequenceLength, $Net["Outputs", "Output"]]];
]

Writer: Function[
	inputs = GetInput["Input", "Unpacked"];
	state = GetState["State"];
	outs = ChildSequence[inputs];
	states = ChildSequence[inputs];
	MXDo[
		result = SowInnerNet[<|"Input" -> inputs[[i]], "State" -> state|>, {"Parameters", "Net"}, #Net];
		states[[i]] = state = Lookup[result, "State"];
		outs[[i]] = Lookup[result, "Output"];
	,
		{i, Length[inputs]}
	];
	SetOutput["Output", outs];
	SetState["State", states];
]

Tests: {
}