Input: TensorT[$$InputSize]

Output: TensorT[$OutputSize]

Parameters:
	$Level: NormalizedT[EitherT[{IntegerT, MatchT[Infinity]}], checkNotZero, Infinity]
	$OutputSize: ComputedType[SizeListT[], checkDepth[$Level, TRank[$Input]]; FlattenShape[$$InputSize, $Level]]
	$$InputSize: SizeListT[]

AllowDynamicDimensions: True

PostInferenceFunction: Function[
	in = $$InputSize;
	checkDepth[$Level, TRank[$Input]];
	If[MatchQ[in, {lv_LengthVar, 1}] && ListQ[$OutputSize],
		in[[1]] = UnifyTypes[in[[1]], $OutputSize[[1]]];
		PostSet[$$InputSize, in];
	];
	RestartInference[]
]

checkNotZero[0] := FailValidation[FlattenLayer, "level specification should be a non-zero integer."];
checkNotZero[e_] := e;

checkDepth[Infinity, _] := Null;
checkDepth[n_Integer, r_Integer] := If[
	Abs[n] >= r, FailValidation[FlattenLayer, "level specification of `` is incompatible with input tensor, which has rank ``.", n, r],
	True, Null
];

Writer: Function[
	odims = #OutputSize;
	If[#Level === Infinity && VectorQ[odims, IntegerQ], WriterFallthrough[]];
	id = SowNode["Reshape", GetInput["Input"], "shape" -> Prepend[odims /. _LengthVar -> 0, 0]];
	SetOutput["Output", id];	
]

MXNet:
	Name: "Flatten"

inf = Infinity;
Tests: {
	{"Input" -> {3, 3}}           -> "9_QX4+1gAwAAs_FV86edJNxyw",
	{inf, "Input" -> {3, 3}}      -> "9_QX4+1gAwAAs_FV86edJNxyw",
	{1, "Input" -> {3, 3}}        -> "9_QX4+1gAwAAs_FV86edJNxyw",
	{1, "Input" -> {3, 3, 3}}     -> "9*3_GK0x+JYTZFo_HPby7MRz6QM",
	{inf, "Input" -> {3, 3, 3}}   -> "27_L+sl+aBXySM_TxW4wMfFyC0",
	{2, "Input" -> {3, 3, 3}}     -> "27_L+sl+aBXySM_TxW4wMfFyC0",
	{-2, "Input" -> {3, 3, 3}}    -> "27_L+sl+aBXySM_TxW4wMfFyC0",
	{-1, "Input" -> {3, 3, 3}}    -> "3*9_VL+OaXWNx0M_ebN0bgjfa2M",
	{4, "Input" -> {3, 3, 3}} -> "Validation failed for FlattenLayer: level specification of 4 is incompatible with input tensor, which has rank 3."
}