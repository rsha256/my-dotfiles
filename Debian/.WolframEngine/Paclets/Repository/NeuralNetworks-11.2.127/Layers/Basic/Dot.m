Input: ListT[2, EitherT[{MatrixT[], VectorT[]}]]

Output: ComputedType[
	RealTensorT, 
	ValidateShape[DotLayer, DotShape[TDimensions /@ $Input], TensorT]
]

PostInferenceFunction: Function[
	Match[
		$Input /. SizeListT[n_Integer] :> Table[SizeT, n],
		{VectorT[_Integer], VectorT[_Integer]} :> Null,
		{VectorT[_], b:VectorT[_Integer]} :> (PostSet[$Input, {b, b}]; PostSet[$Output, ScalarT]),
		{a:VectorT[_Integer], VectorT[_]} :> (PostSet[$Input, {a, a}]; PostSet[$Output, ScalarT]),
		{VectorT[_], VectorT[_]} :> PostSet[$Output, ScalarT],
		Null (* TODO: add matrix cases *)
	];
	RestartInference[]; (* if a PostSet was called, redoes all inference *)
]

Writer: Function[
	{lid, rid} = GetInput["Input"]; {ld, rd} = GetInputDims["Input"]; 
	ln = Length[ld]; rn = Length[rd];
	If[ln === rn === 2, WriterFallthrough[]];
	od = DotShape[{ld, rd}];
	Switch[{ln, rn},
		{1, 2},
			{lid, rid} = {SowTranspose[rid], SowReshape[lid, First[ld], 1]},
		{2, 1},
			rid = SowReshape[rid, First @ rd, 1],
		{1, 1},
			lid = SowReshape[lid, 1, First @ ld];
			rid = SowReshape[rid, First @ rd, 1];
	];
	oid = SowNode["batch_dot", {lid, rid}];
	oid = SowReshape[oid, Sequence @@ od];
	SetOutput["Output", oid];
]

WLEquivalent: Function[Apply[Dot]]

MXNet:
	Name: "batch_dot"

Tests: {
	{"Input" -> {3, 3}} -> "_YMJo2dfNev4_flD+uz32U+w",
	{"Input" -> {{2, 3}, 3}} -> "2_QTLgdUUrczU_cGTSnF9S108",
	{"Input" -> {3, {3, 2}}} -> "2_WASBZ9NBC8E_JHfv5hpgJWI"
}
