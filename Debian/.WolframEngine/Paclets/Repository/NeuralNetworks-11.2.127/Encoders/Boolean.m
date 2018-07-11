Output: ScalarT

ToEncoderFunction: Function[
	If[#2, 
		Replace1 @ $BooleanDispatch,
		Replace[$BooleanDispatch]
	]
]

MLType: Function["Boolean"]

EncoderToDecoder: Function[NetDecoder["Boolean"]]

RandomInstance: Function[
	RandomChoice[{False,True}]
]

$BooleanDispatch = Dispatch[{
	True -> 1, 
	False -> 0, 
	l_ :> EncodeFail["`` should be either True or False", l]
}];