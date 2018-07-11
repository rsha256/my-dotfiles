Input: EitherT[{VectorT[$Count], SequenceT[LengthVar[], VectorT[$Count]]}]

Parameters:
	$Tokens: ValidatedParameterT[ToTokenEncodingData, "English"]
	$IgnoreCase: Defaulting[BooleanT, True]
	$Count: ComputedType[SizeT, Length[First @ $Tokens]+1]

MaxArgCount: 1

ToDecoderFunction: Function @ Module[{tokens},
	tokens = Normal @ First @ #Tokens;
	MapB[#2] @ Switch[#3, 
		_EitherT, DepthWrapper[TokenDecoder[tokens]],
		VectorT[_], SingleTokenDecoder[tokens],
		_TensorT, TokenDecoder[tokens]
	]
]

SingleTokenDecoder[tokens_][in_] :=
	UnsafeQuietCheck @ If[in === {}, in, Part[tokens, MaxIndex[in]]];

TokenDecoder[tokens_][in_] := 
	UnsafeQuietCheck @ If[in === {}, in, StringJoin @ Riffle[Part[tokens, ListMaxIndex[in]], " "]];

DecoderToEncoder: Function[
	NetEncoder[{"Tokens", First @ #Tokens, IgnoreCase -> #IgnoreCase}]
]

ArrayDepth: Function[
	Switch[#2, 
		_EitherT, None,
		VectorT[_], 1,
		_TensorT, 2
	]
]