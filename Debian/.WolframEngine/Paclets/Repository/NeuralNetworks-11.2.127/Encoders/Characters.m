Output: SwitchedType[$OutputForm,
	"Index" -> SequenceT[LengthVar[], IndexIntegerT[$Count]], 
	"UnitVector" -> SequenceT[LengthVar[], VectorT[$Count]]
]

Parameters:
	$Encoding: ValidatedParameterT[ParseCharacterEncodingSpec, Automatic]
	$OutputForm: Defaulting[EnumT[{"Index", "UnitVector"}], "Index"]
	$IgnoreCase: Defaulting[BooleanT, False]
	$Count: ComputedType[SizeT, MXNetLink`CharacterEncodingDataSize @ First @ $Encoding]

MaxArgCount: 2

ToEncoderFunction: Function[
	With[{
		postFunc = If[#OutputForm === "Index", Identity, makeOneHotTable[#Count]],
		encoderFunc = MXNetLink`ToCharacterEncodingFunction[First @ #Encoding, charEncoderFail]
	},
		If[#2, Map, Identity] @ (encoderFunc /* postFunc)
	]
]

MLType: Function["Text"]

EncoderToDecoder: Function[NetDecoder[{"Characters", First @ #Encoding, IgnoreCase -> #IgnoreCase}]]

Kind: "string"

charEncoderFail[] := EncodeFail["input was not a string, or contained invalid characters"];

makeOneHotTable[n_] := ModuleScope[
	vectors = IdentityMatrix[n]; 
	Part[vectors, #]&
];

