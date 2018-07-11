Output: TensorT[{1}]

ToEncoderFunction: Function[
	If[#2, wrap, Replace[N[#], {r_Real :> {r}, _ :> EncodeFail["input was not a numeric value"]}]&]
]

RandomInstance: Function[
	RandomReal[]
]

MLType: Function["Numerical"]

EncoderToDecoder: Function[NetDecoder["Scalar"]]

wrap[input_] := UnsafeQuietCheck[wrapCF[N[input]], EncodeFail["input was not a list of numeric values"]];

wrapCF = Compile[{{r, _Real, 1}},
	Table[{i}, {i, r}]
];