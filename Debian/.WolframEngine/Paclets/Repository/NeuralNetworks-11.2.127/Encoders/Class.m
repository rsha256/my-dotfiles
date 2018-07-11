Output: ComputedType[
	EitherT[{IndexIntegerT[SizeT], VectorT[$Count]}], 
	Switch[$OutputForm, 
		"Index", IndexIntegerT[$Count], 
		"UnitVector", VectorT[$Count],
		_, $Failed
	]
]

Parameters:
	$Labels: ListT[$Count, ExpressionT]
	$OutputForm: Defaulting[EnumT[{"Index", "UnitVector"}]]
	$Count: SizeT

MinArgCount: 1
MaxArgCount: 2

HiddenFields: {"Count"}

ToEncoderFunction: Function[
	If[#2, Replace1, Replace] @ 
	If[#OutputForm === "Index", makeSparseDispatch, makeOneHotDispatch][#Labels, #Count]
]

RandomInstance: Function[
	RandomChoice[#Labels]
]

MLType: Function["Nominal"]

EncoderToDecoder: Function[NetDecoder[{"Class", #Labels}]]

makeSparseDispatch[labels_, dim_] :=
	Thread[labels -> Range[dim]] //
	Append[l_ :> EncodeFail["`` is not one of ``", l, labels]] //
	Dispatch;

makeOneHotDispatch[labels_, dim_] :=
	Thread[labels -> IdentityMatrix[dim]] //
	Append[l_ :> EncodeFail["`` is not one of ``", l, labels]] //
	Dispatch;
