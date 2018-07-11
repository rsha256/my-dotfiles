Input: SequenceT[$$Length, $$Shape]

Output: SequenceT[$$LengthOut, $$Shape]

Parameters:
	$$Length: LengthVar[]
	$$Shape: AnyTensorT
	$$LengthOut: ComputedType[LengthVar[], 
		If[$$Length > 1, $$Length - 1, FailValidation["input tensor must be at least length 1."]],
		$$Length, 
		IntegerQ[$$Length]
	]

Writer: Function[
	input = GetInputMetaNode["Input"];
	most = SowMetaDrop[input, #$LengthOut, False];
	SetOutput["Output", most];
]

Tests: {
	{"Input" -> 4} -> "3_AIvAa0t3efE_fSBwZO7kXLY",
	{"Input" -> {3, 5}} -> "2*5_M6g8ur0bTdU_SSxhQeurDpc",
	{"Input" -> "Varying"} -> "2_IMH4E1wGKP4_DHJCNcE9jWQ",
	{"Input" -> {"Varying", 2}} -> "2*2_SFJTSGHA1RE_Zq4fwNcCW84",
	{"Input" -> {"Varying", 2, 2}} -> "2*2*2_HBb+77u2kgc_LLC5owrlxt0"
}