Package["NeuralNetworks`"]



$SSA = <||>;
$SSACache = <||>
$NextID = 1;


PackageScope["ScalarSymbol"]
PackageScope["NegatedSymbol"]

DerivedScalar[body_] := 
	CacheTo[$SSACache, Hash[body],
		With[{new = ScalarSymbol[$NextID++]}, 
			$SSA[new] = body; 
			new
		]
	];

ScalarSymbol /: "ReLU"[a_ScalarSymbol] := DerivedScalar[{"RectifiedLinearUnit", a}];
ScalarSymbol /: "RectifiedLinearUnit"[a_ScalarSymbol] := 
	DerivedScalar[{"RectifiedLinearUnit", a}];

ScalarSymbol /: "SELU"[a_ScalarSymbol] := 
	DerivedScalar[{"ScaledExponentialLinearUnit", a}];
ScalarSymbol /: "ScaledExponentialLinearUnit"[a_ScalarSymbol] := 
	DerivedScalar[{"ScaledExponentialLinearUnit", a}];

ScalarSymbol /: "ELU"[a_ScalarSymbol] := DerivedScalar[{"ExponentialLinearUnit", a}];
ScalarSymbol /: "ExponentialLinearUnit"[a_ScalarSymbol] := 
	DerivedScalar[{"ExponentialLinearUnit", a}];

ScalarSymbol /: "SoftSign"[a_ScalarSymbol] := DerivedScalar[{"SoftSign", a}];

ScalarSymbol /: Times[-1, a_ScalarSymbol] := NegatedSymbol[a];

NegatedSymbol /: Plus[1|1., NegatedSymbol[a_]] := DerivedScalar[{Subtract, 1.0, a}];

declareUnary[f_] := (
	ScalarSymbol /: f[a_ScalarSymbol] := DerivedScalar[{f, a}];
	NegatedSymbol /: f[n_NegatedSymbol] := f[norm[n]];
);

norm[NegatedSymbol[f_]] := DerivedScalar[{Minus, f}];
norm[e_] := e;

Scan[declareUnary, $PrimitiveUnaryElementwiseFunctions];

ScalarSymbol /: Clip[x_ScalarSymbol] := DerivedScalar[{Clip, x, {-1., 1.}}];
ScalarSymbol /: Clip[x_ScalarSymbol, {a_ ? NumericQ, b_ ? NumericQ}] := DerivedScalar[{Clip, x, {N[a], N[b]}}];

declareBinary[f_] := (
	ScalarSymbol /: f[a_ScalarSymbol, b_ScalarSymbol] := DerivedScalar[{f, a, b}];
	ScalarSymbol /: f[a_ScalarSymbol, b_ ? NumericQ] := DerivedScalar[{f, a, N[b]}];
	ScalarSymbol /: f[a_? NumericQ, b_ScalarSymbol] := DerivedScalar[{f, N[a], b}];
	NegatedSymbol /: f[lhs___, n_NegatedSymbol, rhs___] := f[lhs, norm[n], rhs];
);

Scan[declareBinary, $PrimitiveBinaryElementwiseFunctions];

(* Scalar upvalues *)

PackageScope["ScalarFunctionToPureFunction"]

ScalarFunctionToPureFunction[e_, head_:None] := If[head =!= None, head[e], e];
ScalarFunctionToPureFunction[ScalarFunctionObject[in_, out_, ssa_], head_:None] := Scope[
	expr = If[head =!= None, Function[head[out]], Function[out]];
	expr = expr //. s_ScalarSymbol :> RuleCondition @ Lookup[ssa, s, Fail];
	expr /. ScalarSymbol[n_] :> Slot[n] //. {s_Symbol | s_String, args__} :> s[args]
];

PackageScope["CompileScalarFunction"]
PackageScope["ScalarFunctionObject"]

CompileScalarFunction::message = "Function issued message during symbolic evaluation."
CompileScalarFunction::invret = "Function did not return a ScalarSymbol, returned `` instead."
CompileScalarFunction[ninputs_, f_] := CatchFailure @ Scope[
	$SSA = <||>;
	$SSACache = <||>;
	$NextID = 1;
	insyms = Table[ScalarSymbol[$NextID++], ninputs];
	result = norm @ UnsafeQuietCheck[f @@ insyms, $Failed];
	If[FailureQ[result], ReturnFailure["message"]];
	If[!MatchQ[result, _ScalarSymbol], ReturnFailure["invret", result]];
	ScalarFunctionObject[insyms, result, $SSA]
];

DefineCustomBoxes[
	ScalarFunctionObject,
	t:ScalarFunctionObject[in_List, out_, ssa_Association] :> 
		makeScalarFunctionObjectBoxes[t]
];

ScalarFunctionObject[in_, out_, ssa__Association]["SSA"] := ssa;

ScalarSymbol /: MakeBoxes[ScalarSymbol[n_Integer], TraditionalForm] := 
	SubscriptBox["x",StyleBox[IntegerString[n],5]];

nonFirstSeqToQ[from_ -> to_] := MatchQ[$SSA[to], {PackScalar, {__, Except[from]}}];
nonFirstSeqFromQ[from_ -> to_] := MatchQ[$SSA[to], {Part, _, Except[1]}];

makeScalarFunctionObjectBoxes[ScalarFunctionObject[in_, out_ScalarSymbol, <||>]] :=
	MakeBoxes[out, TraditionalForm];

makeScalarFunctionObjectBoxes[ScalarFunctionObject[in_, out_, ssa_]] := Scope[
	$SSA = ssa;
	rules = DependenceRules[ssa];
	{edges, vnames} = Labelling[rules, 2];
	inids = Flatten @ Position[vnames, Alternatives @@ in];
	outids = Flatten @ Position[vnames, out];
	plot = LayerPlot[List @@@ edges, 
		"VertexLabels" -> Placed[makeLabel /@ vnames, Right],
		"VertexSizes" -> 5,
		"Rotated" -> False,
		"ImageScale" -> 45,
		"ArrowSize" -> 6,
		"ArrowShape" -> "Chevron",
		"DuplicateInputVertices" -> True,
		"ImagePadding" -> {{5,15}, {15, 5}},
		"VertexTypes" -> {inids -> 1, outids -> 2, _ -> 3},
		"VertexTypeData" -> <|"VertexStyles" -> {RGBColor[0, 0.61, 1], RGBColor[0.83, 0.18, 0], Black}|>,
		"BaseLabelStyle" -> {FontSize -> 7},
		"MaximumImageSize" -> None
	];
	plot = Framed[plot, FrameStyle -> LightGray];
	ToBoxes[ScalarFunctionObject[plot]]
]

makeLabel[vert_] := fmtDerivation[vert, Lookup[$SSA, vert]];

DependenceRules[tsrcs_] := Flatten @ KeyValueMap[
	Thread[DeepCases[#2, _ScalarSymbol] -> #1]&,
	tsrcs
];

fmtDerivation[lhs_, _Missing] := lhs;
fmtDerivation[lhs_, {f_, rest___}] := Inactive[Set][lhs, HoldForm[f[rest]]];


PackageScope["SowScalarFunction"]

$MXBinOps = <|
	Plus     -> {"_Plus", "_PlusScalar", "_PlusScalar"},
	Subtract -> {"_Minus", "_MinusScalar", "_RMinusScalar"},
	Times    -> {"_Mul", "_MulScalar", "_MulScalar"},
	Divide   -> {"_Div", "_DivScalar", "_RDivScalar"},
	Power    -> {"_Power", "_PowerScalar", "_RPowerScalar"},
	Min      -> {"_Minimum", "_MinimumScalar", "_MinimumScalar"},
	Max      -> {"_Maximum", "_MaximumScalar", "_MaximumScalar"}
|>;

SowScalarFunction[ScalarFunctionObject[ins_, out_, ssa_], inIDs___] := Scope[
	inIDs = {inIDs};
	Assert[Length[inIDs] == Length[ins]];
	$SFID = AssociationThread[ins, inIDs];
	KeyValueScan[SowSSA, ssa];
	$SFID @ out
];

SowScalarFunction[f:(Alternatives @@ $PrimitiveUnaryElementwiseFunctions), id1_] :=
	SowNode[$MXUnaryFunctionNameMapping @ f, id1];

SowScalarFunction[f:(Alternatives @@ $PrimitiveBinaryElementwiseFunctions), id1_, id2_] :=
	SowNode[$MXBinaryFunctionNameMapping @ f, {id1, id2}];

SowScalarFunction[___] := Panic[];


SowSSA[sym_, op_] := Set[$SFID[sym], Apply[SSAtoOp, op]];

Clear[SSAtoOp];

SSAtoOp[Clip, x_, {a_Real, b_Real}] := SowNode["clip", $SFID[x], "a_min" -> a, "a_max" -> b];
SSAtoOp[f_, a_, b_Real] := SowNode[$MXBinOps[f][[2]], $SFID[a], "scalar" -> b];
SSAtoOp[f_, a_Real, b_] := SowNode[$MXBinOps[f][[3]], $SFID[b], "scalar" -> a];
SSAtoOp[f_, a_, b_] :=     SowNode[$MXBinOps[f][[1]], {$SFID[a], $SFID[b]}];

SSAtoOp[Power, a_, 2.] :=  SowNode["square", $SFID[a]];

SSAtoOp[Minus, a_] := SSAtoOp[Subtract, 0.0, a];

SSAtoOp[f_, a_] := SowScalarFunction[f, $SFID[a]];

(*----------------------------------------------------------------------------*)
(* String Arg Case *)

SSAtoOp["RectifiedLinearUnit", a_] := SowNode["relu", $SFID[a]]

(* "Self-Normalizing Neural Networks", https://arxiv.org/abs/1706.02515
Implementation taken from official TF implementation: 
	https://github.com/bioinf-jku/SNNs/blob/master/selu.py
*)
SSAtoOp["ScaledExponentialLinearUnit", a_] := Block[{temp},
	temp = SowNode["LeakyReLU", $SFID[a], "act_type" -> "elu", "slope" -> 1.6732632423543];
	SowNode["_MulScalar", temp, "scalar" -> 1.0507009873554]
]

(* Definition from Torch: 
	https://github.com/torch/nn/blob/master/doc/transfer.md#elu *)
SSAtoOp["ExponentialLinearUnit", a_] := 
	SowNode["LeakyReLU", $SFID[a], "act_type" -> "elu", "slope" -> 1]


(* Definition from Torch: 
	https://github.com/torch/nn/blob/master/doc/transfer.md#softsign *)
SSAtoOp["SoftSign", a_] := Block[{denom},
	denom = SowNode["abs", $SFID[a]];
	denom = SowNode["_PlusScalar", denom, "scalar" -> 1];
	SowNode["_Div", {$SFID[a], denom}]
]

(*----------------------------------------------------------------------------*)

SSAtoOp[___] := Panic[];






