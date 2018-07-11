Input: EitherT[{VectorT[$Count], SequenceT[LengthVar[], VectorT[$Count]]}]

Parameters:
	$Encoding: ValidatedParameterT[ParseCharacterEncodingSpec, Automatic]
	$IgnoreCase: Defaulting[BooleanT, False]
	$Count: ComputedType[SizeT, MXNetLink`CharacterEncodingDataSize @ First @ $Encoding]

MaxArgCount: 1

ToDecoderFunction: Function[
	enc = First @ #Encoding;
	MapB[#2] @ Switch[#3, 
		_EitherT, DepthWrapper[MXNetLink`ToCharacterDecodingFunction[enc]],
		VectorT[_], With[{chars = Replace[Characters @ MXNetLink`CharacterEncodingAlphabet[enc], "\:0000" -> "", {1}]},
			Function[in, Part[chars, MaxIndex[in]]]
		],
		_TensorT, MXNetLink`ToCharacterDecodingFunction[enc]
	]
]

Kind: Function @ Switch[#2,
	VectorT[_], "character",
	_, "string"
]

DecoderToEncoder: Function[
	NetEncoder[{"Characters", First @ #Encoding, IgnoreCase -> #IgnoreCase}]
]

ArrayDepth: Function[
	Switch[#2, 
		_EitherT, None,
		VectorT[_], 1,
		_TensorT, 2
	]
]


