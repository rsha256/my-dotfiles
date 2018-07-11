Package["NeuralNetworks`"]



PackageScope["Coerce"]

NNSetUsage @ "
Coerce[data$, type$] coerces data$ to match type$, or panics if that is impossible.
"

Coerce[in_, type_] := ($type = None; coerce[in, type]);

Clear[coerce];

coerce[m_Missing, (ArrayCasesT|Defaulting)[_, d_]] := d;

General::netreqparam = "Required parameter `1` was not specified.";
coerce[m_Missing, _] := panic["netreqparam", key];

coerce[in_, TypeT] := Scope[
	If[ValidTypeQ[t = ToT[in]], t, tpanic[in, TypeT]]
];

coerce[in_, t:IntervalScalarT[min_, max_]] := Scope @
	If[!MachineRealQ[n = N[in]] || n < min || n > max, tpanic[in, t], n];

coerce[in_, ScalarT] := 
	Replace[N[in], {t_ ? MachineRealQ :> t, _ :> tpanic[in, ScalarT]}];

$SimplePredicates = Association[
	RealT -> RealQ,
	BooleanT -> BooleanQ,
	StringT  -> StringQ, 
	IntegerT -> MachineIntegerQ,
	NaturalT -> NonNegativeMachineIntegerQ,
	PosIntegerT -> PositiveMachineIntegerQ,
	SizeT -> PositiveMachineIntegerQ,
	LengthVar -> PositiveMachineIntegerQ,
	TypeT -> ValidTypeQ,
	TypeExpressionT -> ValidTypeExpressionQ,
	FunctionT -> MatchQ[_Function]
];

coerce[in_, type:(Alternatives @@ Keys[$SimplePredicates])] :=
	If[TrueQ[$SimplePredicates[type] @ in],
		in, tpanic[in, type]
	];

(* to make InheritsFrom work, bit of a hack/special case *)
coerce[x_, Defaulting[_, x_]] := x;

(* currently coercing type, used for 'panic' *)
$type = None; 

coerce[RepeatedInteger[n_Integer], ListT[len_Integer, type_]] := ConstantArray[Coerce[n, type], len];
coerce[RepeatedInteger[n_Integer], ListT[SizeT|NaturalT, type_]] := RepeatedInteger[Coerce[n, type]];

coerce[in_, a:ArrayCasesT[t_, d_]] := Scope[
	$type = a; 
	If[RuleVectorQ[in], ArrayCasesT[MapAt[coerce[#, t]&, in, {All, 2}], d], coerce[in, t]]
];

coerce[net_, type:NetT[ins_, outs_]] := Scope[
	If[!ValidNetQ[net], tpanic[in, type]];
	MapAssocAssoc[checkIO, ins, Inputs[net], missIO["input"], extraIO["input"]];
	MapAssocAssoc[checkIO, outs, Outputs[net], missIO["output"], extraIO["output"]];
	NData @ net (* strip off the symbol head to allow type inference to reach inside *)
];

General::netinvio = "Port `2` of net specified for parameter `1` takes `3`, but a port taking `4` was expected."

checkIO[key_, givent_, reqt_] := 
	If[UnifyTypes[givent, reqt] === $Failed,
		panic["netinvio", key, TypeForm[givent], TypeForm[reqt]]
	];

General::netmissio = "Specified net for parameter `` is missing required `` port ``."
General::netextraio = "Specified net for parameter `` has unexpected `` port ``."

missIO[dir_][key_] := panic["netmissio", dir, key];
extraIO[dir_][key_] := panic["netextraio", dir, key];

coerce[in_, type_] := Match[
	If[$type === None, $type = type, type]
	,
	RuleT[kt_, vt_] :>
		Match[in, 
			Rule[k_, v_] :> Rule[%[k, kt], %[v, vt]],
			tpanic[in]
		],

	(* REFACTOR: Take into account the type *)
	t_TensorT :> CoerceArray[in, TDimensions[t]],

	IndexIntegerT[max_Integer] :> 
		If[PositiveMachineIntegerQ[in] && in <= max,
			in, tpanic[in]
		],

	ImageT[size_, color_] :> Scope[
		If[!Image`ValidImageQ[in], tpanic[in]];
		img = in;
		If[MatchQ[size, {_Integer,_Integer}] && ImageDimensions[in] =!= size, 
			img = ImageResize[img, size]];
		If[StringQ[color] && ImageColorSpace[in] =!= color,
			img = ColorConvert[img, color]];
		img
	],

(*  this shouldn't come up, and wouldn't TypeForm correctly on panic anyway.
	t_List :> (
		If[!ListQ[in] || Length[in] =!= Length[t], panic[in]];
		MapThread[coerce, {in, t}]
	),
*)
	AssocT[StringT, v_] :> 
		If[!AssociationQ[in] || !StringVectorQ[Keys[in]], tpanic[in],
			Association @ KeyValueMap[
				#1 -> CoerceParam[#1, #2, v]&, 
				in
			]
		],

	AssocT[k_, v_] :> 
		If[!AssociationQ[in], tpanic[in],
			Association @ KeyValueMap[
				%[#1, k] -> %[#2, v]&, 
				in
			]
		],

	StructT[rules_] :> Scope @ 
		If[!AssociationQ[in], tpanic[in],
		If[!SubsetQ[Keys[rules], Keys[in]], 
			keypanic[First @ Complement[Keys[in], Keys[rules]]],
			Association[
				#1 -> CoerceParam[#1, Lookup[in, #1], #2]& @@@
				rules
			]
		]],

	EnumT[alts_] :> 
		If[!MatchQ[in, Alternatives @@ alts], tpanic[in], in],

	EitherT[ts_] :> Scope[
		Do[
			If[!FailureQ[res = CoerceSoft[in, t]], Return[res, Block]],
			{t, ts}
		];
		tpanic[in]
	],

	ListT[n_, t_] :> (
		If[!ListQ[in], tpanic[in]];
		If[IntegerQ[n] && n =!= Length[in], tpanic[in]];
		Catch @ Block[{panic := Throw[$Failed]},
			Map[coerce[#, t]&, in]
		] // OnFail[tpanic[in]]
	),

	Defaulting[t_, _] :> %[in, t],

	Nullable[t_] :> If[in === None, None, %[in, t]],

	MatchT[t_] :> If[!MatchQ[in, t], tpanic[in], in],

	TypeAliasP :> %[in, ResolveAlias @ type],

	ExpressionT :> in,

	DistributionT :> Which[
		UnivariateDistributionQ[in], in, 
		NumericQ[in], If[in < $MachineEpsilon, 
			UniformDistribution[{0,0}], 
			NormalDistribution[0, N[in]]
		],
		True, tpanic[in]
	],

	tpanic[in]
];

General::netinvkey = "`2` is not a valid parameter to `1`.";
keypanic[key_] := panic["netinvkey", key];


PackageScope["CoerceArray"]

CoerceArray[arr_List, dims_] /; ArrayQ[arr, _, NumberQ] && dimsMatchQ[arr, dims] := Scope[
	res = Quiet @ RawArray["Real32", arr];
	If[!RawArrayQ[res], panic["netinvtensorvals"]];
	res
];

General::netinvtensorvals = "The value specified for `` should be a real-valued tensor."

CoerceArray[arr_RawArray ? RawArrayQ, dims_] /; dimsMatchQ[arr, dims] := 
	If[RawArrayType[arr] === "Real32", arr,
		RawArray["Real32", Normal[arr]]
	];

dimsMatchQ[arr_, type_] := Match[type, 
	$Failed :> True,
	RankTP :> True,
	n_Integer :> ArrayDepth[arr] === n,
	list_List :> And[
		ArrayDepth[arr] === Length[list],
		MatchQ[Dimensions[arr], list /. SizeT -> _]
	]
];

General::netinvrank = "The value specified for `` should be a numeric tensor of rank ``.";
CoerceArray[_, n_Integer] := panic["netinvrank", n];

General::netinvdims = "The value specified for `` should be a numeric tensor of dimensions ``.";
CoerceArray[_, dims_List] := 
	If[MatchQ[dims, {SizeT..}],
		panic["netinvrank", Length[dims]],
		panic["netinvdims", dims]
	];

General::netinvtensor = "The value specified for `` should be a numeric tensor.";
CoerceArray[_, _] := panic["netinvtensor"];


PackageScope["CoerceSoft"]

NNSetUsage @ "
CoerceSoft[data$, type$] coerces data$ to match type$, or returns $Failed if that is impossible.
"

CoerceSoft[data_, type_] := Block[{panic := Throw[$Failed], $type}, Catch @ coerce[data, type]];

General::netinvparam = "The value specified for `` should be ``.";
General::netinvopt = "The value specified for `` should be ``.";
General::netinvparamdbg = "The value specified for the parameter `` should be ``. But it was ``.";

tpanic[in_] := tpanic2[in, $type];
tpanic[in_, t_] := tpanic2[in, If[$type === None, t, $type]];

(* for one or two custom places that don't use $type *)
$invmsgname = "netinvparam";
tpanic2[in_, type_] := If[$DebugMode, 
	panic["netinvparamdbg", TypeForm[type], in],
	panic[$invmsgname, TypeForm[type]];
];

panic[msg_, args___] := 
	ThrowTaggedFailure["InvalidParameter", msg, $currParam, args];


PackageScope["CoerceParam"]

NNSetUsage @ "
CoerceParam[name$, data$, type$] calls Coerce[data$, param$], but ensures error messages refer to the parameter as name$.
"

CoerceParam[_, Automatic, type_] := Replace[type, Defaulting[_, value_] :> value];

CoerceParam[name_, data_, type_] := Scope[
	$currParam = name;
	Coerce[data, type]
];



PackageScope["CoerceOption"]

NNSetUsage @ "
CoerceOption[name$, data$, type$] calls Coerce[data$, param$], but ensures error messages refer to an invalid option called name$.
CoerceOption[name$, data$, type$ :> auto$] uses auto as the default if data$ is Automatic.
"

CoerceOption[name_, data_, type_ :> auto_] :=
	CoerceOption[name, Replace[data, Automatic :> auto], type];

CoerceOption[name_, data_, type_] := Scope[
	$currParam = name; $invmsgname = "netinvopt";
	Coerce[data, type]
];


PackageScope["ThrowInvOpt"]

ThrowInvOpt[sym_, type_] := ThrowFailure["netinvopt", sym, TypeString[type]];


PackageScope["ToUnaryElementwiseFunction"]
PackageScope["ToBinaryElementwiseFunction"]

General::invscf = "`` could not be symbolically evaluated as a `` scalar function."

ToUnaryElementwiseFunction[in_] := 
	If[MemberQ[$PrimitiveUnaryElementwiseFunctions, in], in,
		OnFail[ThrowFailure["invscf", in, "unary"]] @ CompileScalarFunction[1, in]
	];

ToBinaryElementwiseFunction[in_] :=
	If[MemberQ[$PrimitiveBinaryElementwiseFunctions, in], in,
		OnFail[ThrowFailure["invscf", in, "binary"]] @ CompileScalarFunction[2, in]
	];

