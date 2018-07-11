Package["NeuralNetworks`"]



PackageScope["$AtomicTypes"]

$AtomicTypes = {
	RealT, AtomT,
	PosIntegerT, IntegerT, NaturalT,
	SizeT,
	StringT,
	BooleanT,
	TypeT, ExpressionT, FunctionT, TypeExpressionT
};


PackageScope["ValidTypeQ"]

NNSetUsage @ "
ValidTypeQ[type$] gives True if type$ is a well-formed type.
"

ValidTypeQ[e_] := Match[e, 
	MacroEvaluate[Alternatives @@ $AtomicTypes] :> True,
	IndexIntegerT[SizeP] :> True,
	ListT[NatP, t_] :> %[t],
	EitherT[ts_] :> VectorQ[ts, %],
	Defaulting[t_, _] :> %[t],
	Nullable[t_] :> %[t],
	TensorT[SizeListP, t_] :> %[t],
	EnumT[ListT[NatP, StringT]] :> True,
	EnumT[_List] :> True,
	ImageT[SizePairP, ColorSpaceT | MacroEvaluate[Alternatives @@ $ColorSpaces]] :> True,
	MacroEvaluate[$TypeAliasPatterns] :> True,
	MatchT[_] :> True,
	RuleT[k_, v_] :> %[k] && %[v],
	StructT[{Repeated[_String -> (_?%)]}] :> True,
	AssocT[k_, v_] :> %[k] && %[v],
	_NetEncoder :> True,
	_NetDecoder :> True,
	SequenceT[_LengthVar, t_] :> %[t],
	_ValidatedParameter :> True,
	_LengthVar :> True,
	False
];

PackageScope["ValidTypeExpressionQ"]

NNSetUsage @ "
ValidTypeExpressionQ[type$] gives True if type$ is a well-formed type that can potentially include NetPath expressions.
"

NatPathP = NatP | NetPathP;
SizePathP = SizeP | NetPathP;
SizeListPathP = ListT[NatPathP, SizeT] | {___LengthVar, SizePathP...} | NetPathP;

ValidTypeExpressionQ[e_] := Match[e,
	NetPathP :> True,
	IndexIntegerT[SizePathP] :> True,
	ListT[NatPathP, t_] :> %[t],
	EitherT[ts_] :> VectorQ[ts, %],
	Defaulting[t_, _] :> %[t],
	TensorT[SizeListPathP, t_] :> %[t],
	ImageT[SizeListPathP, _String | ColorSpaceT | NetPathP] :> True,
	_ComputedType :> True,
	NetT[_ ? validTypeAssocQ, _ ? validTypeAssocQ] :> True,
	NormalizedT[t_, _] | NormalizedT[t_, _, _]:> %[t],
	ValidatedParameterT[_] | ValidatedParameterT[_, _] :> True,
	Nullable[t_] :> %[t],
	TypeReplace[t_, rules_] :> %[t] && VectorQ[Values[rules], weakVTEQ],
	IntervalScalarT[_, _] :> True,
	ValidTypeQ[e]
];

weakVTEQ[e_] := ValidTypeExpressionQ[e] || ConcreteParameterQ[e];

validTypeAssocQ[assoc_Association] := And[
	StringVectorQ[Keys[assoc]], 
	VectorQ[Values[assoc], ValidTypeExpressionQ]
];


PackageScope["ConcreteParameterQ"]

NNSetUsage @ "
ConcreteParameterQ[param$] gives True if param$ is a value (e.g. a RawArray) or a fully-specialized type (e.g. a Tensor with known rank and dimensions).
"

ConcreteParameterQ[param_] := Match[param,
	_String :> True,
	True|False|None :> True,
	t_List :> VectorQ[t, %],
	t_RawArray :> RawArrayQ[t],
	TensorT[{___LengthVar, ___Integer}, RealT|IndexIntegerT[_Integer]] :> True,
	ImageT[{_Integer, _Integer}, _String] :> True,
	_Integer | _Real :> True,
	enc_NetEncoder :> True,
	dec_NetDecoder :> True,
	IndexIntegerT[_Integer] :> True,
	Nullable[t_] :> %[t],
	_ScalarFunctionObject :> True,
	None :> True,
	Infinity :> True,
	LengthVar[_Integer] :> True,
	_ValidatedParameter :> True,
	assoc_Association :> ConcreteNetQ[assoc],
	Min|Max|Mean|Total :> True, 
	EuclideanDistance|CosineDistance :> True,
	False
];


(* TODO: Explain why there is a difference betwen these two. Maybe there isn't.
It's awkward. Probably reason to go with a separate "Types" key to help separate
runtime for spec-time parametricity. Then
"Type" -> FullySpecifiedTypeQ.
"Parameter" -> FullySpecifiedParmeterQ (no higher-level types like ListT etc)
"Array" -> FullySpecifiedArrayQ (TensorT, or actual RawArrays etc)
 *)

PackageExport["FullySpecifiedTypeQ"]

NNSetUsage @ "
FullySpecifiedTypeQ[param$] gives True if param$ is a value (e.g. a RawArray) or a fully-specialized type (e.g. a Tensor with known rank and dimensions).
"

FullySpecifiedTypeQ[t_ ? System`Private`ValidQ] := True;
FullySpecifiedTypeQ[t_] := If[fullq[t], System`Private`SetValid[t]; True, False];

fullq[type_] := Match[type,
	TensorT[{___LengthVar, ___Integer}, tt_] :> fullttq[tt],
	IntegerT|PosIntegerT|BooleanT :> True,
	IndexIntegerT[_Integer] :> True,
	_NetEncoder | _NetDecoder :> True,
	t_List :> VectorQ[t, %],
	False
];

(* TODO: should this be IndexIntegerT[_Integer]? *)
fullttq[RealT|_IndexIntegerT] := True;
fullttq[c:CoderP] := fullq[CoderType[c]];
fullttq[_] := False;


PackageScope["ParseMethod"]
PackageScope["$HiddenMethod"]
PackageScope["$CommonSuboptions"]

NNSetUsage @ "
ParseMethod[spec, <|'name$1' -> {f, <|'opt$1' -> type$1, $$|>}, $$|>]'
"

ParseMethod[Automatic, methods_] := 
	defaultMethod @ getMethod[methods, First @ Keys @ methods];

defaultMethod[{f_, args_}] := 
	Compose[f, args /. (Defaulting|ArrayCasesT)[_, v_] :> v];

General::invmethname = "Value of option Method -> `` is not Automatic, ``.";
getMethod[methods_, spec_] := Scope[
	mdata = Lookup[methods, spec, Lookup[methods, $HiddenMethod[spec], 
		ThrowFailure["invmethname", QuotedString[spec], QuotedStringList @ Select[Keys[methods], StringQ]]]
	];
	If[MissingQ[common = methods[$CommonSuboptions]], 
		mdata,
		MapAt[Join[common, #]&, mdata, 2]
	]
];

ParseMethod[spec_String, methods_] := 
	defaultMethod @ getMethod[methods, spec];

ParseMethod[{spec_String, rules___Rule}, methods_] := Scope[
	{f, args} = getMethod[methods, spec];
	f @ CoerceParam[spec, Association[rules], StructT @ Normal @ args]
];

General::invmethodspec = "Value of Method option should be a string or a string with options."
ParseMethod[_, _] :=
	ThrowFailure["invmethodspec"];


PackageScope["ContainsVarSequenceQ"]

ContainsVarSequenceQ[types_] := MemberQ[types, _ ? VarSequenceQ];


PackageScope["VarSequenceQ"]

VarSequenceQ[VarSequenceP[]] := True;
VarSequenceQ[c:CoderP] := SequenceCoderQ[c];
VarSequenceQ[_] := False;


PackageScope["TestType"]

NNSetUsage @ "
TestType[data$, type$] returns True if data$ matches type type$.
"

TestType[data_, type_] :=
	ToLiteralTypeTest[type] @ data;


PackageScope["ToBatchTypeTest"]

NNSetUsage @ "
ToBatchTypeTest[<|'name$1'->type$1,$$|>,batchsize$] makes a predicate that \
tests whether an input association matches elementwise with the given types.
* Each vector in the association must be a batch of the given length.
"

ToBatchTypeTest[types_, batchsize_] := With[
	{len = Length[types], patt = Normal @ Map[makeBatchTest, types]},
	Function[input, 
		And[
			Length[input] === len,
			MatchQ[input, KeyValuePattern[patt]]
		]
	] /. $batchsize -> batchsize
];

makeBatchTest[enc_NetEncoder] := With[{var = Unique["NeuralNetworks`Private`TempVars`$"]}, 
	First @ {l_List /; Length[l] === $batchsize} /. l -> var];

makeBatchTest[other_] := With[{test = makeTest[SequenceT[$batchsize, other]]}, _List ? test];


PackageScope["ToLiteralTypeTest"]

NNSetUsage @ "
ToLiteralTypeTest[type$] constructs a predicate function for type$.
"

ToLiteralTypeTest[type_] := Memoized[makeTest[type], Method -> "Inline"];

makeTest[type_] := Match[type,

	ScalarT :> RealQ,
	
	TensorT[dims_List, IndexIntegerT[max_]] :> With[
		{dpatt = dims /. {SizeT -> _, x_LengthVar -> _}}, 
		Function[
			z,
			MatchQ[MachineArrayDimensions[z, PosIntegerT], dpatt] && 
				Max[z] <= max
		]
	],

	TensorT[dims_List, AtomT|RealT] :> Composition[
		MatchQ[dims /. {SizeT -> _, x_LengthVar -> _}],
		MachineArrayDimensions
	],
	
	TensorT[SizeListT[rank_Integer], RealT] :> Composition[
		EqualTo[rank],
		MachineArrayRank
	],

	(* can this be removed? seb *)
	SequenceT[_LengthVar, t_] :> %[ListT[n, t]],

	TensorT[_, _] :> MachineArrayQ,

	BooleanT :> BooleanQ,

	StringT :> StringQ,

	IntegerT :> MachineIntegerQ,

	NaturalT :> NonNegativeMachineIntegerQ,

	PosIntegerT | SizeT | _LengthVar :> PositiveMachineIntegerQ,

	IndexIntegerT[max_] :> If[IntegerQ[max],
		Function[in, PositiveMachineIntegerQ[in] && in <= max],
		PositiveMachineIntegerQ
	],

	TypeT :> ValidTypeQ,

	ExpressionT :> (True&),

	FunctionT :> MatchQ[_Function],

	EitherT[{a_, b_}] :> With[{ap = %[a], bp = %[b]}, ap[#] || bp[#]&],

	EitherT[ts_List] :> OrOperator[% /@ ts],
	
	ListT[n_Integer, t_] :> With[t2 = %[t], Length[#] === n && VectorQ[#, t2]&],

	ListT[_, t_] :> With[t2 = %[t], VectorQ[#, t2]&],

	StructT[rules_List] :> StructPredicate[MapAt[%, rules, {All, 2}]],
	
	RuleT[k_, v_] :> With[{kt = %[k], vt = %[v]}, MatchQ[Rule[_ ? kt, _ ? vt]]],	

	AssocT[k_, v_] :> With[{kt = %[k], vt = %[v]}, 
		AssociationQ[#] && VectorQ[Keys[#], kt] && VectorQ[Values[#], vt]&
	],

	MatchT[patt_] :> MatchQ[patt],

	ImageT[size_, color_] :> ReplaceAll[
		Function @ And[
			Image`ValidImageQ[#],
			%[size] @ ImageDimensions[#],
			%[color] @ ImageColorSpace[#]
		],
		m_makeTest :> RuleCondition[m]
	],

	Defaulting[t_, _] :> With[p = %[t],
		MissingQ[#] || p[#]&
	],

	Nullable[t_] :> With[p = %[t],
		If[# === None, True, p[#]]&
	],

	TypeAliasP :> %[ResolveAlias[type]]
];



PackageScope["StructPredicate"]

NNSetUsage @ "
ToLiteralTypeTest[type$] constructs a predicate function for type$.
"

StructPredicate[rules_][assoc_] := 
	AssociationQ[assoc] && SubsetQ[Keys[rules], Keys[assoc]] && Catch[
		MapThread[
			If[!#1[#2], Throw[False]]&,
			{Values[rules], Lookup[assoc, Keys[rules]]}
		];
		True
	];



PackageScope["VectorTypeQ"]

VectorTypeQ[t_TensorT] := TRank[t] === 1;
VectorTypeQ[_] := False;