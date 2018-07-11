Output: SequenceT[LengthVar[], IndexIntegerT[$Count]]

Parameters:
	$Tokens: ValidatedParameterT[ToTokenEncodingData, "English"]
	$SplitPattern: ValidatedParameterT[toSplitPattern, $DefaultTokenSplitPattern]
	$IgnoreCase: Defaulting[BooleanT, True]
	$Count: ComputedType[SizeT, Length[First @ $Tokens]+1]

MaxArgCount: 2

ToEncoderFunction: Function[
	casefunc = If[#IgnoreCase, ToLowerCase, Identity];
	assoc = MakeTokenDictionary @ casefunc @ Normal @ First @ #Tokens;
	If[#2, Map, Identity] @ TokenEncode[assoc, First @ #SplitPattern, casefunc]
]

MLType: Function["Text"]

EncoderToDecoder: Function[NetDecoder[{"Tokens", First @ #Tokens, IgnoreCase -> #IgnoreCase}]]

Kind: "string"

toSplitPattern[e_] := If[StringPatternOrRulesQ[e], e, FailCoder["`` is not a valid value for \"SplitPattern\".", e]];
