Input: TensorT[$$InputDimensions, $$Type]

Output: TensorT[$$OutputDimensions, $$Type]

Parameters:
	$Specification: ValidatedParameterT[checkTransposeSpec, {1 -> 2}]
	$$InputDimensions: ComputedType[
		SizeListT[],
		transposeShape[$$OutputDimensions, First @ $Specification, False],
		{$$OutputDimensions}
	]
	$$Type: AtomT
	$$OutputDimensions: ComputedType[
		SizeListT[],
		transposeShape[$$InputDimensions, First @ $Specification, True],
		{$$InputDimensions}
	]

MXNet: 
	Name: "transpose"
	Writer: Function[
		"axes" -> writeIntList[toMXSpec[Length[#$InputDimensions], First @ #Specification]]
	]

Tests: {
	{"Input" -> {3, 3}} -> "3*3_THXxbAO5Pe0_SgDj9BedOjY",
	{"Input" -> {2, 3, 4}} -> "3*2*4_DnnyJzj6W/0_cCbq19VBOuM",
	{2 -> 3, "Input" -> {2, 3, 4}} -> "2*4*3_cdDZVZ8yoik_XQe7JaCDO/k",
	{{1 -> 2, 3 -> 4}, "Input" -> {1, 2, 3, 4}} -> "2*1*4*3_JO0cFHUMQCs_N6Kq7cpYFOA",
	{3, "Input" -> {1, 2, 3, 4}} -> "3*2*1*4_dAMpV1WoWCE_FV2sz6kAHNg"
}

Unprotect[TransposeLayer];
TransposeLayer[a_Integer -> b_Integer, args___] ? System`Private`HoldEntryQ := TransposeLayer[{a -> b}, args];

checkTransposeSpec[spec_] := Match[spec,
	_Integer :> {1 -> spec},
	Rule[_Integer, _Integer] :> {spec},
	{Repeated[_Integer -> _Integer]} :> spec,
	FailValidation[TransposeLayer, "`` is not a valid transpose specification.", spec]
];

toMXSpec[rank_, rules:{__Rule}] := Scope[
	indices = Range[rank];
	swap[indices[[#1]], indices[[#2]]]& @@@ rules;
	Prepend[indices, 0]
];

transposeShape[idims_, rules:{__Rule}, dir_] := Scope[
	r = Length[idims];
	If[r > 4, FailValidation[TransposeLayer, "only tensors up to rank 4 are currently supported."]];
	If[!AllTrue[Flatten[List @@@ rules], 1 <= Abs[#] <= r&],
		FailValidation[TransposeLayer, "transpose specification `` is incompatible with input tensor dimensions ``.", rules, idims];
	];
	odims = idims;
	If[dir,
		swap[odims[[#1]], odims[[#2]]]& @@@ rules,
		swap[odims[[#2]], odims[[#1]]]& @@@ Reverse[rules]
	];
	odims
];

SetAttributes[swap, HoldAll]; 
swap[lhs_, rhs_] := Block[{t = lhs}, lhs = rhs; rhs = t];

