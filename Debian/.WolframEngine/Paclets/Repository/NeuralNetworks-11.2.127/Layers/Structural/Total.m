Input: ListT[$$InputCount, RealTensorT]

Output: ComputedType[
	RealTensorT,
	dims = Map[TDimensions, $Input];
	If[SameQ @@ dims,
		First[$Input, FailValidation["at least one input must be provided."]],
		FailValidation["all inputs must have same dimensions."]
	]
]

PostInferenceFunction: Function[
	If[ListQ[$Input] && Length[$Input] > 1, 
		final = Fold[UnifyTypes, $Input];
		len = Length[$Input];
		If[!FailureQ[final], PostSet[$Input, Table[final, len]]];
		RestartInference[]
	];
]

Parameters:
	$$InputCount: SizeT

MXNet:
	Name: "ElementWiseSum"
	Parameters:
		$$InputCount: "num_args"

WLEquivalent: Function[Total]