Input: ScalarT

ArrayDepth: 0

DecoderToEncoder: Function[NetEncoder["Boolean"]]

ToDecoderFunction: Function[
	If[#2,
		Function[in, Thread[Flatten[in] > 0.5]],
		Function[in, First[in, in] > 0.5]
	]
]
