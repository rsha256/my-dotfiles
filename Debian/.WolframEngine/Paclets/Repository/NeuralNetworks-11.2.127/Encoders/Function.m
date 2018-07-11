Output: $OutputType

Parameters:
	$Function: ValidatedParameterT[Identity]
	$OutputType: TypeT

HiddenFields: {"OutputType"}

(* TODO: add checking of output *)
ToEncoderFunction: Function[
	type = #OutputType;
	func = First @ #Function;
	If[#2,
		Map[func] /* checkOutputType[ToLiteralTypeTest[ListT[SizeT, type]], type],
		func /* checkOutputType[ToLiteralTypeTest[type], type]
	]
]

Kind: "expression"

checkOutputType[test_, type_][data_] :=
	If[!TrueQ[test[data]], 
		EncodeFail["\"Function\" encoder did not produce an output that was ``", TextString @ TypeForm[type]],
		data
	];

MLType: Function["Numerical"]