Package["NeuralNetworks`"]



PackageScope["$Raw"]


PackageExport["$NetHeads"]

$NetHeads = Bag[];


PackageExport["$NetInteractivity"]

If[!ValueQ[$NetInteractivity],
	$NetInteractivity = !$CloudEvaluation && !TrueQ[Developer`$DisableInteractivity];
];

PackageScope["SetCatchFailureAsMessage"]
PackageScope["TagSetCatchFailureAsMessage"]

SetHoldAllComplete[SetCatchFailureAsMessage];
SetHoldAllComplete[TagSetCatchFailureAsMessage];

(* this exists for speed purposes: avoid invoking the macro system *)
SetCatchFailureAsMessage[head_, lhs_, rhs_] := 
	SetDelayed @@ Hold[lhs, Catch[rhs, 
		GeneralUtilities`PackageScope`CatchFailureTag, 
		GeneralUtilities`PackageScope`CatchFailureAsMessageHandler[head]
	]];

TagSetCatchFailureAsMessage[head_, lhs_, rhs_] := 
	TagSetDelayed @@ Hold[head, lhs, Catch[rhs, 
		GeneralUtilities`PackageScope`CatchFailureTag, 
		GeneralUtilities`PackageScope`CatchFailureAsMessageHandler[head]
	]];


PackageScope["NNSetUsage"]

(*
SetHoldFirst[NNSetUsage];

NNSetUsage[str_] := Scope[
	str = StringTrim[str];
	sym = First @ StringCases[str, WordCharacter..];
	ToExpression[sym, InputForm, Function[s, MessageName[s, "usage"] = str, {HoldAll}]]
];

NNSetUsage[sym_, str_] :=
	MessageName[sym, "usage"] = str;
*)

NNSetUsage = SetUsage;

(* make sure this definition gets applied before downvalues are created *)
ScalarT = TensorT[{}, RealT];
TensorT[dims_] := TensorT[dims, RealT];


PackageScope["AnyTensorT"]
PackageScope["RealTensorT"]

AnyTensorT = TensorT[ListT[NaturalT, SizeT], AtomT];
RealTensorT = TensorT[ListT[NaturalT, SizeT], RealT];


PackageScope["VarSequenceP"]

VarSequenceP[] := TensorT[{_LengthVar, ___}, _]
Quiet[
VarSequenceP[lvar_] := TensorT[{LengthVar[lvar], ___}, _]
]


PackageExport["SequenceT"]

SequenceT[n_, t_] := TensorT[{n}, t];


PackageScope["EncoderP"]
PackageScope["DecoderP"]
PackageScope["CoderP"]

EncoderP[kind_] := HoldPattern[NetEncoder[kind, _, _]];
EncoderP[kind_, assoc_] := HoldPattern[NetEncoder[kind, assoc, _]];
EncoderP[kind_, assoc_, type_] := HoldPattern[NetEncoder[kind, assoc, type]];
DecoderP[kind_] := HoldPattern[NetDecoder[kind, _, _]];
DecoderP[kind_, assoc_] := HoldPattern[NetDecoder[kind, assoc, _]];
DecoderP[kind_, assoc_, type_] := HoldPattern[NetDecoder[kind, assoc, type]];
CoderP = _NetEncoder | _NetDecoder;


PackageScope["Self"]


PackageScope["NetP"]
PackageScope["NetGraphP"]

ClearAll[NetP];

(* This makes it easier to declare patterns that work on either heads or pure assocs *)
Quiet[
NetP[head_Symbol, data_Symbol, meta_Symbol] := head_Symbol[data_Association, meta_Association];
NetP[data_Symbol, meta_Symbol] := _Symbol[data_Association, meta_Association];
NetP /: Verbatim[Pattern][sym_Symbol, NetP] := sym_Association | _Symbol[sym_Association, _Association];
NetP /: Verbatim[Blank][NetP] := NetP;
NetGraphP /: Verbatim[Pattern][sym_Symbol, NetGraphP] := sym_Association | HoldPattern[NetGraph[sym_Association, _Association]];
NetGraphP /: Verbatim[Blank][NetGraphP] := NetGraphP;
,
RuleDelayed::rhs
];


PackageScope["MachineQ"]

 MachineQ[x_] := MachineRealQ[x] || MachineIntegerQ[x];


PackageScope["EmbedVariables"]

SetAttributes[EmbedVariables, HoldRest];
EmbedVariables[expr_, avoid_] := 
	With[{vars = Complement[findSetVariables[expr], 
		Map[HSym, Unevaluated[avoid]]]},
		Fold[ReplaceAll, expr, {
			HoldPattern[Block][body_ ? checkMult] :> Block[vars, body],
			{HSym[z_] :> z, Inherited[z_] :> z},
			HoldPattern[Set[Derivative[1][sym_Symbol], val_]] :> EchoSet[sym, val],
			HoldPattern[Set[Derivative[1][val_]]] :> EchoRaw[val],
			HoldPattern[Block[{}, body_]] :> body,
			HoldPattern[Return[f_]] :> Return[f, Block]
		}]
	];

findSetVariables[hc_] := Complement[
	findDeep[hc, (Set|SetDelayed)[lhs_, _] :> scanSymbols[lhs]],
	findDeep[hc, Inherited[sym_Symbol] :> HSym[sym]]
];

SetAttributes[checkMult, HoldAllComplete];

checkMult[CompoundExpression[___, _Times, ___]] :=
	(Message[Scope::mult]; True);

checkMult[___] := True;

Scope::mult = "Multiplication detected in Scoped declaration.";


PackageScope["ScopePureFunction"]

ScopePureFunction[HoldPattern[Function[body_]]] := 
	Function @@ EmbedVariables[HoldComplete[Block[body]], {}];

ScopePureFunction[HoldPattern[Function[vars_, body_]]] := 
	Function @@ EmbedVariables[HoldComplete[vars, Block[body]], {}];

ScopePureFunction[HoldPattern[Function[vars_, body_, attrs_]]] := 
	Function @@ EmbedVariables[HoldComplete[vars, Block[body], attrs], {}];


PackageScope["findDeep"]

findDeep[hc_, rule_] := Dedup @ Flatten @ Cases[hc, rule, Infinity, Heads -> True];

SetAttributes[scanSymbols, HoldAllComplete];
 scanSymbols[sym_Symbol] := HSym[sym];
 scanSymbols[syms:{__Symbol}] := Thread @ HSym[syms];
 scanSymbols[Derivative[1][e_]] := scanSymbols[e];
 scanSymbols[___] := {};
 HSym[Inherited] := {};


PackageScope["AMap"]

AMap[f_, list_] := Association[# -> f[#]& /@ list];
AMap[f_][list_] := AMap[f, list];


PackageScope["IMap"]

IMap[f_, assoc_Association] := MapIndexed[f[#2[[1,1]], #1]&, assoc];
IMap[f_, list_] := MapIndexed[f[#2[[1]], #1]&, list];
IMap[f_][list_] := IMap[f, list];


PackageScope["ScanAssocList"]

ScanAssocList[f_, assoc_, list_] := Scope[
	i = 1; KeyValueMap[f[#1, #2, list[[i++]]];&, assoc];
];


PackageScope["MapFirst"]
PackageScope["MapLast"]

MapFirst[f_, e_] := MapAt[f, e, 1];
MapFirst[f_][e_] := MapFirst[f, e];
MapLast[f_, e_] := MapAt[f, e, -1];
MapLast[f_][e_] := MapLast[f, e];


PackageScope["All1"]
 All1[e_] := Part[e, All, 1];


PackageScope["Dedup"]
 Dedup[e_] := DeleteDuplicates[e];


PackageScope["SplitSelect"]

 SplitSelect[e_List, f_] := Scope[
	pred = Map[f /* TrueQ, e];
	{Pick[e, pred, True], Pick[e, pred, False]}];

 SplitSelect[e_Association, f_] := 
 	Association /@ SplitSelect[Normal[e], Last /* f];

 SplitSelect[f_][e_] := SplitSelect[e, f];


PackageScope["OnFail"]

SetAttributes[OnFail, HoldFirst];

OnFail[expr_][result_] := OnFail[expr, result];
OnFail[expr_, result_] := If[FailureQ[result], expr, result];


PackageScope["SizeP"]
PackageScope["RankTP"]
PackageScope["PosIntP"]
PackageScope["NatP"]
PackageScope["StringP"]
PackageScope["SizePairP"]
PackageScope["NetPathP"]
PackageScope["NetPathElemP"]
PackageScope["SizeListP"]
PackageScope["TTypeP"]

RankTP = SizeT | NaturalT;
PosIntP = _Integer ? Positive;
NatP = _Integer ? NonNegative | SizeT | NaturalT;
SizeP = PosIntP | SizeT;
StringP = _String | StringT;
SizePairP = ListT[2, SizeT] | {SizeP, SizeP};
NetPathElemP = _Integer | _String;
NetPathP = NetPath[NetPathElemP..];
SizeListP = ListT[NatP, SizeT] | {___LengthVar, NatP...};
TTypeP = _IndexIntegerT|RealT|AtomT;

SetAttributes[ComputedType, HoldRest];

(* canonicalization code: this has to live here to ensure these short forms 
evaluate properly in LHS code *)


PackageScope["indexOf"]

SetAttributes[indexOf, HoldRest];
indexOf[list_List, thing_, else_] := Block[{t = thing},
	Do[If[list[[i]] === t, Return[i, Block]], {i, Length[list]}];
	else
];


PackageScope["niceGrid"]

niceGrid[e_] := Block[{$inner = False}, igrid[e]];

igrid[e:{__Rule}] := assocGrid[Association[e], $inner];
igrid[<||>] := <||>;
igrid[{}] := {};
igrid[a_Association] := assocGrid[a, $inner];
igrid[e_] := e;
igrid[f_Function] := PrettyForm[f];

$hicolor := GrayLevel[0.96];
assocGrid[assoc_, inner_:False] := Scope[
	$inner = True;
	rowLabelGrid[List /@ Map[igrid] @ Values[assoc], Keys[assoc], inner]
];


PackageScope["rowLabelGrid"]

rowLabelGrid[rows_, labels_, inner_:False] := Scope[
	sz = If[StringVectorQ[labels], 1+0.5 * Max[StringLength /@ labels, 8], Automatic];
	If[NumberQ[sz], sz = Max[sz, 4]];
	grid = Grid[
		MapThread[Prepend, {rows, fmtLabels @ labels}],
		Background -> {{$hicolor}, {}}, FrameStyle -> LightGray,
		Alignment -> {Left, Baseline}, Dividers -> All, 
		If[inner, 
			{Spacings -> {1.1, 0.3}, ItemSize -> {{sz, Automatic}, 1.1}},
			{Spacings -> {1.5, 0.3}, ItemSize -> {{sz, Automatic}, 1.2}, 
			BaseStyle -> {ShowStringCharacters -> True, FontFamily -> "Source Code Pro", FontSize -> 10}}
		],
		BaselinePosition -> 1
	];
	If[!inner, grid, Framed[grid, FrameStyle -> None, FrameMargins -> {{3,3},{2,2}}, BaselinePosition -> Baseline]]
];

fmtLabels[e_] := Map[fmtLabel1, e];

fmtLabel1[e_] := Pane[fmtLabel2[e], {Automatic, 14}, Alignment -> Bottom, BaselinePosition -> Baseline];
fmtLabel2[e_String] := Style[e, ShowStringCharacters -> False, LineBreakWithin -> False];
fmtLabel2[e_] := e;


PackageScope["ALookup"]

ALookup[assoc_Association, keys_] := 
	ALookup[assoc, keys, Panic["KeyMissing", "Key `` missing.", #]&];

ALookup[assoc_Association, keys_, func_] :=
	Lookup[assoc, keys, Return[ALookup2[assoc, keys, func], Lookup]];

ALookup2[assoc_, key_, func_] := func[key];

ALookup2[assoc_, keys_List, func_] := 
	Table[
		Lookup[assoc, key, func[key]],
		{key, keys}
	];


PackageScope["PrefixPorts"]
PackageScope["$path"]

PrefixPorts[e_] := If[$path === NetPath[], e, e /. p_NetPath :> RuleCondition[Join[$path, p]]];

$path = NetPath[];


PackageScope["MapAtFields"]

MapAtFields[field_, f_, net_NetP] := Block[
	{$path = Join[$path, NetPath[field, Null]]},
	MapAt[
		MapIndexed[($path[[-1]] = #2[[1,1]]; f[#1])&],
		net,
		field
	]
];

MapAtFields[field_, f_][net_] := MapAtFields[field, f, net];


PackageScope["ScanFields"]

ScanFields[field_, f_, net_NetP] := Block[
	{$path = Join[$path, NetPath[field, Null]]},
	KeyValueScan[($path[[-1]] = #1; f[#2])&, net[field]];
];
ScanFields[field_, f_][net_] := ScanFields[field, f, net];


PackageScope["ScanSubNets"]

ScanSubNets[f_, assoc_] := Scan[
	Block[{$path = Join[$path, #]}, f[assoc @@ #]]&,
	$LayerData[assoc["Type"], "SubNets"]
];


PackageScope["MapFields"]

MapFields[field_, f_, net_NetP] := Block[
	{$path = Join[$path, NetPath[field, Null]]},
	KeyValueMap[($path[[-1]] = #1; f[#2])&, net[field]]
];
MapFields[field_, f_][net_] := MapFields[field, f, net];


PackageScope["OrOperator"]

OrOperator[list_][e_] := AnyTrue[list, #[e]&];


PackageScope["Replace1"]

Replace1[rules_] := With[{u = Unique["input"]}, Function[u, Replace[u, rules, {1}]]];


PackageScope["ComposeThread"]

ComposeThread[funcs_][args_] := ComposeThread[funcs, args];

ComposeThread[funcs_, args_] :=
	MapThread[Compose, {funcs, args}];

ComposeThread[funcs_, assoc_Association] :=
	Block[{i = 0}, Map[funcs[[++i]][#]&, assoc]];


PackageScope["MapAssocAssoc"]

NNSetUsage @ "
MapAssocAssoc[f, assoc$1, assoc$2, missf$, extraf$] calls f[key$, v$1, v$2] for corresponding\
values in assoc$1, assoc$2, in the order of the keys in assoc$1, but if a key in assoc$2
is missing, missf$[key$] is called, or if an extra key is present, extraf$[key$] is called.
"

MapAssocAssoc[f_, a_, b_, missf_, extraf_] :=
	If[Length[a] =!= Length[b],
		findBadKeys[a, b, missf, extraf],
		KeyValueMap[
			f[#1, #2, Lookup[b, #1, Return[missf[#1]]]]&,
			a
		]
	];

findBadKeys[a_, b_, missf_, extraf_] := Scope[
	ka = Keys[a]; kb = Keys[b];
	missf @ First[
		Complement[ka, kb],
		Return @ extraf @ First @ Complement[kb, ka]
	]
];


PackageScope["FindDuplicates"]

FindDuplicates[elems_] :=
	First @ SelectFirst[Tally[elems], #[[2]] > 1&, Return[$Failed]];


PackageScope["PartElse"]

SetHoldRest[PartElse];
PartElse[expr_, p___, else_] := 
	Replace[
		UnsafeQuietCheck[expr[[p]], Missing[]],
		_Missing :> else
	];


PackageScope["PartExistsQ"]

PartExistsQ[expr_, part___] := 
	UnsafeQuietCheck[expr[[part]]; True, False];


PackageScope["MapB"]

MapB[b_] := If[b, Map, Identity];
MapB[False, t_] := t;
MapB[True, t_] := Map[t];


PackageScope["ParseContext"]

ParseContext["CPU"] = 1; 
ParseContext["GPU"] := ParseContext[{"GPU", 1}];

General::badtrgdev = "TargetDevice -> `` could not be used, please ensure that you have a compatible NVIDIA graphics card and have installed the latest operating system drivers."
General::trgdev32 = "TargetDevice -> \"GPU\" is not supported on 32-bit platforms."
ParseContext[context:{"GPU", n_Integer ? Positive}] := Scope[
	code = ToContextCode[{"GPU", n - 1}];
	Which[
		$SystemWordLength === 32,
			ThrowFailure["trgdev32"],
		Quiet @ FailureQ @ CatchFailure[General, MXNetLink`NDArrayCreate[{1,2,3}, code]],
			ThrowFailure["badtrgdev", If[n === 1, "GPU", {"GPU", n}]],
		True,
			ParseContext[context] = code
	]
]

General::invtrgdev = "Invalid setting for option TargetDevice. TargetDevice should be either \"CPU\" or \"GPU\", or {\"GPU\", n} for multi-GPU systems, where n is a positive integer that specifies which system GPU to use."
ParseContext[spec_] := ThrowFailure["invtrgdev", spec];


PackageExport["ValidNetQ"]

ValidNetQ[_] := False;


PackageScope["TestPositiveInteger"]
PackageScope["TestPositiveIntegerVector"]

General::notposint = "Expected a positive integer."
General::notposintvec = "Expected a vector of positive integers."

TestPositiveInteger[in_] := If[!PositiveMachineIntegerQ[in], ThrowFailure["notposint"], in];
TestPositiveIntegerVector[in_] := If[!VectorQ[in, PositiveMachineIntegerQ], ThrowFailure["notposintvec"], in];


PackageScope["TestIndexInteger"]
PackageScope["TestIndexIntegerVector"]

General::notiint = "Expected an index between 1 and ``."
General::notiintvec = "Expected a vector of indices between 1 and ``."

TestIndexInteger[max_][in_] := 
	If[PositiveMachineIntegerQ[in] && in <= max, in,
		ThrowFailure["notiint", max]];

TestIndexIntegerVector[max_][in_] := 
	If[VectorQ[in, PositiveMachineIntegerQ] && Max[in] <= max, in,
		ThrowFailure["notiintvec", max]];


PackageScope["DigitStringKeysQ"]

DigitStringKeysQ[assoc_] := VectorQ[Keys[assoc], StringMatchQ[DigitCharacter..]];


PackageScope["RemapKeys"]

RemapKeys[assoc_] := Scope[
	newkeys = IntegerString @ Range[Length[assoc]];
	remapping = AssociationThread[Keys[assoc], newkeys];
	{KeyMap[remapping, assoc], remapping}
];


PackageScope["MakeCustomHeadBox"]

MakeCustomHeadBox[head_, contents_, baseline_] := Scope[
	boxes = ToBoxes @ Panel[contents, BaselinePosition -> If[$CloudEvaluation, Automatic, baseline]];
	StyleBox[TagBox[TagBox[
		RowBox[{head, RowBox[{"[", boxes, "]"}]}],
		False
	], Deploy], LineBreakWithin -> False]
];


PackageScope["RemoveRawArrays"]

RemoveRawArrays[expr_Association] := 
	expr /. ra_RawArray :> RuleCondition @ TensorT @ Dimensions[ra];


PackageScope["ReplaceRawArraysWithDummies"]
PackageScope["DummyRawArray"]

(* see 331893 for idea of why this is necessary *)

ReplaceRawArraysWithDummies[expr_Association] := 
	expr /. ra_RawArray :> RuleCondition @ DummyRawArray @ Dimensions[ra];


PackageScope["fromStringForm"]

fromStringForm[e_] := If[$CloudEvaluation, ReplaceAll[e, s_StringForm :> ToString[s]], e];


PackageScope["CreateAliasesInContext"]

CreateAliasesInContext[newContext_String, symbols_List] := Scan[
	With[{sym = Symbol[newContext <> SymbolName[#]]},
		sym = #
	]&, 
	symbols
];


PackageScope["CreateFunction"]
PackageScope["Eval"]
PackageScope["TempVar"]

CreateFunction[statements_] := Scope[
	body = ReplaceRepeated[
		Hold @@ Flatten[List[statements]],
		Eval[e_] :> RuleCondition[e]
	];
	If[!FreeQ[body, TempVar],
		rules = Map[# -> toVar[First @ #]&, DeleteDuplicates @ DeepCases[body, _TempVar]];
		body = Hold[Block][Values[rules], body /. rules];
	];
	Compose[Function, body] //. {
		Hold[e_] :> e,
		Hold[e__] :> CompoundExpression[e],
		HoldPattern[Identity[e_]] :> e
	}
];

CreateFunction[body_, head_] := MapAt[head, CreateFunction[body], 1];

toVar[i_Integer] := Symbol["NeuralNetworks`Private`$" <> IntegerString[i]];
toVar[sym_Symbol] := sym;


PackageScope["pluralStr"]

pluralStr[row_Row] := MapAt[pluralStr, row, {1,1}];

pluralStr[s_String] := Switch[
	StringTake[s, -1],
	"x", StringDrop[s, -1] <> "ces", 
	"s", s <> "es",
	_, s <> "s"
];

pluralStr[e_] := e;


PackageScope["MatchValues"]

SetHoldAll[MatchValues, setMatchDefs];

MatchValues /: (Set|SetDelayed)[sym_Symbol, MatchValues[args___]] := setMatchDefs[sym, args];

Clear[setMatchDefs];
setMatchDefs[sym_Symbol, CompoundExpression[args___SetDelayed, last_]] := Module[{holds},
	Clear[sym];
	holds = Hold @@@ Hold[args];
	PrependTo[holds, Hold[case_, UnmatchedCase[sym, case]]];
	holds = ReplaceAll[holds, HoldPattern[Out[]] :> sym];
	Switch[Unevaluated[last], 
		_SetDelayed, AppendTo[holds, Hold @@ Unevaluated[last]],
		Null, Null,
		_, AppendTo[holds, Hold[_, last]]
	];
	Replace[List @@ holds, Hold[a_, b_ | Scope[b_]] :> SetDelayed[sym[a], Scope[b]], {1}];
];

setMatchDefs[sym_, args___] := 
	Panic["InvalidMatchValues", "Symbol `` used invalid MatchValues body: ``", sym, HoldForm[args]];


PackageScope["$PrimitiveUnaryElementwiseFunctions"]

PackageExport["SoftRamp"]

$PrimitiveUnaryElementwiseFunctions = {
	ArcCos, ArcCosh, ArcSin, ArcSinh, ArcTan, ArcTanh, Cosh, Sinh, Gamma, LogGamma, (* <- new in 11.1 *)
	LogisticSigmoid, Ramp, SoftRamp, Tanh, Sin, Cos, Log, Exp, Sqrt, Square, Abs, Round, Ceiling, Floor, Sign
};


PackageScope["$PrimitiveBinaryElementwiseFunctions"]

$PrimitiveBinaryElementwiseFunctions = {Times, Plus, Divide, Subtract, Power, Min, Max};


PackageScope["UpgradeAndSealNet"]

SetHoldAll[UpgradeAndSealNet];

UpgradeAndSealNet[head_Symbol[data_Association ? AssociationQ]] := 
	If[!FreeQ[data, HoldPattern @ TensorT[_Integer | SizeT | NaturalT, _]],
		Construct11V0Layer[data, Null],
		ThrowFailure["wlprerl"]
	];


PackageScope["MaybeDowngradeAndSealNet"]

SetHoldAll[MaybeDowngradeAndSealNet];

MaybeDowngradeAndSealNet[_Symbol[data_Association ? AssociationQ, meta_Association]] := 
	GetConstructorFunction[meta, "interpret"][data, meta];



PackageScope["DeclareContainer"]

DeclareContainer[name_, symbol_] := (
	$SymbolToType[symbol] = name;
	$TypeToSymbol[name] = symbol;
	$VTable[name] = $VTable["GenericContainer"];
	symbol /: Flatten[net_symbol] := NetFlatten[net];
	symbol /: Export[path_String ? jsonPathQ, net_symbol] := MXNetExport[path, net];
);

jsonPathQ[path_] := StringEndsQ[path, ".json", IgnoreCase -> True]

PackageScope["$VTable"]

$VTable = Data`UnorderedAssociation[
	"GenericLayer" -> Data`UnorderedAssociation[],
	"GenericContainer" -> Data`UnorderedAssociation[],
	"GenericOperator" -> Data`UnorderedAssociation[]
];

PackageScope["DeclareMethod"]

DeclareMethod[method_, layerf_, containerf_:Inherited, operatorf_:Inherited] := (
	method[net_Association] := $VTable[net["Type"], method] @ net;
	$VTable["GenericLayer", method] = layerf;
	$VTable["GenericContainer", method] = If[containerf === Inherited, ScanFields["Nodes", method], containerf];
	$VTable["GenericOperator", method] = If[operatorf === Inherited, layerf, operatorf];
);


PackageScope["Call"]

Call[net_, method_] := $VTable[net["Type"], method] @ net;
Call[method_][net_] := Call[net, method];


PackageScope["MachineArrayDimensions"]

MachineArrayDimensions[ZeroTensor[dims_]] := dims;

MachineArrayDimensions[e_] := 
	If[RawArrayQ[e] || PackedArrayQ[e] || ArrayQ[e, _, MachineQ], 
		Dimensions[e],
		If[MachineQ[e], {}, $Failed]
	];

MachineArrayDimensions[e_, PosIntegerT] := 
	If[(PackedArrayQ[e, Integer] && Min[e] > 0) || ArrayQ[e, _, PositiveMachineIntegerQ], 
		Dimensions[e],
		If[PositiveMachineIntegerQ[e], {}, $Failed]
	];


PackageScope["MachineArrayRank"]

MachineArrayRank[ZeroTensor[dims_]] := Length[dims];

MachineArrayRank[e_] := 
	If[RawArrayQ[e] || PackedArrayQ[e] || ArrayQ[e, _, MachineQ],
		ArrayDepth[e],
		$Failed
	];


PackageExport["MachineArrayQ"]

(* %KERNEL This should be a kernel function, its hard to do this fast in top-level *)
MachineArrayQ[e_] := RawArrayQ[e] || PackedArrayQ[e] || ArrayQ[e, _, MachineQ];

MachineArrayQ[e_, PosIntegerT] := PackedArrayQ[e, Integer] || ArrayQ[e, _, PositiveMachineIntegerQ];

NNSetUsage @ "
MachineArrayQ[expr$] gives True if expr$ is a RawArray, a packed array, or an array of machine numbers.
"


PackageScope["SetupGenericDispatch"]

Clear[SetupGenericDispatch];

SetupGenericDispatch[sym_Symbol, addFastPath_] := (
	ValidNetQ[HoldPattern[sym[assoc_Association, _]] ? System`Private`NoEntryQ] := True;
	TagSetCatchFailureAsMessage[sym, Part[HoldPattern[sym[assoc_Association, _] ? System`Private`NoEntryQ], part___], NetPart[assoc, part]];
	Language`SetMutationHandler[sym, NetMutationHandler];
	If[addFastPath, SetCatchFailureAsMessage[sym, (s_sym ? System`Private`NoEntryQ)[arg_], NetApplyFast[s, arg]]];
	SetCatchFailureAsMessage[sym, (s_sym ? System`Private`NoEntryQ)[args___], NetApply[s, args]];
);


PackageScope["StripCoders"]

StripCoders[e_] := ReplaceAll[e, c:CoderP :> RuleCondition[CoderType[c]]];


PackageScope["HoldSet"]

HoldSet[Hold[sym_], value_] := Set[sym, value];


PackageScope["$CloudOrPlayer"]

$CloudOrPlayer := $CloudOrPlayer = TrueQ[$CloudEvaluation] || MatchQ[$LicenseType, "Player" | "Player Pro"];


PackageScope["PrereleaseError"]

General::prerelerr = "Support for `` is not implemented in this pre-release version of Mathematica. Please try this feature again in the next pre-release."

PrereleaseError[feature_] := (
	InheritedMessage["prerelerr", feature];
	ThrowRawFailure[$Failed];
);


PackageScope["ToIntPair"]

ToIntPair[i_Integer] := {i,i};
ToIntPair[e_] := e;


PackageScope["ToIntTuple"]

ToIntTuple[i_Integer] := RepeatedInteger[i];
ToIntTuple[e_] := e;


PackageScope["$MXUnaryFunctionNameMapping"]

$MXUnaryFunctionNameMapping = Association[
	Ramp -> "relu",
	Sin -> "sin", 
	Cos -> "cos", 
	Log -> "log", 
	Exp -> "exp", 
	Sqrt -> "sqrt", 
	Square -> "square", (* yes i know *)
	Abs -> "abs", 
	Round -> "round", 
	Ceiling -> "ceil", 
	Floor -> "floor",
	Sign -> "sign",
	(* new in 11.1: *)
	ArcCos -> "arccos",
	ArcSin -> "arcsin",
	ArcSinh -> "arcsinh",
	ArcTan -> "arctan",
	ArcTanh -> "arctanh",
	ArcCosh -> "arccosh",
	Cosh -> "cosh",
	Sinh -> "sinh",
	Tanh -> "tanh",
	Gamma -> "gamma",
	LogGamma -> "gammaln",
	LogisticSigmoid -> "sigmoid"
];


PackageScope["$MXBinaryFunctionNameMapping"]

$MXBinaryFunctionNameMapping = Association[
	Times -> "_Mul",
	Subtract -> "_Minus",
	Divide -> "_Div",
	Plus -> "_Plus",
	Power -> "_Power",
	Min -> "_Minimum",
	Max -> "_Maximum"
];