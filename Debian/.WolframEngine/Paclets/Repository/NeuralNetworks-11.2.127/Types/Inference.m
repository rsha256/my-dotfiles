Package["NeuralNetworks`"]



DeclareMethod[ScanInferenceRules, LayerInferenceRules, ContainerInferenceRules];

ContainerInferenceRules[assoc_] := (
	SowInferenceRules[assoc["Edges"]];
	ScanFields["Nodes", ScanInferenceRules, assoc];
);

LayerInferenceRules[assoc_] := Scope[
	UnpackAssociation[$LayerData[assoc["Type"]], inferenceRules, subNets, postInferenceFunction];
	SowInferenceRules[inferenceRules];
	SowPostInferenceFunction[postInferenceFunction];
	SowSubnetRules[assoc, subNets];
];

SowSubnetRules[assoc_, list_] := Scan[
	Block[{$path = Join[$path, #]}, ScanInferenceRules[assoc @@ #]]&,
	list
];

SowInferenceRules[e_List] := 
	StuffBag[$irules, PrefixPorts[e]];

SowPostInferenceFunction[None] := Null;
SowPostInferenceFunction[f_] := 
	StuffBag[$pfunctions, PrefixPorts[f]];

PackageScope["ReapInferenceRules"]

ReapInferenceRules[e_] := Scope[
	CollectTo[{$irules, $pfunctions}, ScanInferenceRules[e]];
	{$irules, $pfunctions}
];


PackageScope["ConstructWithInference"]

ConstructWithInference[head_, assoc_] := 
	ConstructWithInference[head, assoc, $StandardMetadata];

ConstructWithInference[head_Symbol, assoc_Association, meta_Association] :=
	System`Private`ConstructNoEntry[head, InferNData[assoc], meta]


PackageScope["InferNData"]

InferNData[assoc_Association] := Scope[
	{irules, pfuncs} = ReapInferenceRules[assoc];
	DoInference[assoc, irules, pfuncs]
];


PackageScope["DoInference"]

DoInference[expr_, rules_, pfuncs_] := Scope[
	paths = Dedup @ Cases[rules, _NetPath, Infinity];
	pathVals = UnsafeQuietCheck[
		Extract[expr, List @@@ paths],
		findInvalidPaths[expr, paths]
	];
	$psetdirty = $restart = False; 
	(* $vals is sometimes looked at for message generation: spot origins of problems *)
	$vals = Data`UnorderedAssociation @ AssociationThread[paths, pathVals];
	newvals = iDoInference[$vals, Flatten @ rules, paths];
	$expr = expr; 
	KeyValueMap[setPart, newvals]; 	
	Scan[executePostFunc, pfuncs];
	If[$restart, $expr = DoInference[$expr, rules, pfuncs]];
	$expr
];


PackageScope["RestartInference"]

(* only causes re-inference if a PostSet made a change *)
RestartInference[] := If[$psetdirty, $restart = True];



PackageScope["PostSet"]

NNSetUsage @ "PostSet[lhs$, rhs$] can be used in a PostInferenceFunction to set lhs$ to rhs$."

PostSet[a_, b_] := Scope[
	$preserveCoders = True;
	setPath = psetPath;
	{ea, eb} = {a, b} /. p_NetPath :> ($expr @@ p); 
	Catch[
		u = unify[ea, eb];
		If[ea =!= u, set[a, u]];
		If[eb =!= u && !FreeQ[b, NetPath], set[b, u]]
	,
		unify, catchUnifyFailure
	];
];


psetPath[NetPath[p___], value_] := If[$expr[p] =!= value, $psetdirty = True; $expr[p] = value];

executePostFunc[f_] := 
	Apply[f /. {
		p_PostSet :> p,
		p_NetPath :> RuleCondition[$expr @@ p]
	}, {}];

findInvalidPaths[expr_, paths_] :=
	Do[
		If[MissingQ[slice[expr]], 
			Panic["InvalidPort", "`` is not present in expression.", NetPath @@ slice]],
		{slice, Slice @@@ paths}
	];

(*setPart[NetPath[p___], val_] := If[!ConcreteParameterQ[$expr[p]], $expr[p] = val];*)
(* the above is too strong in the case of LengthVar[id], but we do need to avoid
replacing raw arrays that we turned into types at inference time. *)

setPart[NetPath[p___], val_] := ComposeTo[$expr[p], replaceWith[val]];

replaceWith[new_][c:CoderP|SequenceT[_, CoderP]] := UnifyCoderWith[c, new];
replaceWith[new_][old_RawArray] := old;
replaceWith[new_][old_] := new;

makeIndex[list_, keys_] := Scope[
	rules = Dispatch[MapIndexed[# -> (Place @@ #2)&, keys]];
	invert = MapIndexed[Cases[#, Place[n_] :> Rule[n, First[#2]], {0, Infinity}]&, list /. rules];
	KeyMap[
		Part[keys, #]&, 
		Data`UnorderedAssociation @ Merge[Flatten @ invert, DeleteDuplicates]
	]
];

PackageExport["$MaxInferenceSteps"]

$MaxInferenceSteps = 32768;

iDoInference[types_, rules_, keys_] := Scope[
	
	$LastTypeInferenceStack ^= {};
	$LastFailedTypeInferenceData ^= None;
	$LastFailedTypeInferenceLHS ^= None;
	$LastFailedTypeInferenceRHS ^= None;
	$types = types /. r_RawArray :> RuleCondition @ TensorT[Dimensions[r]];
	$rules = List @@@ rules;

	$ruleindex = makeIndex[$rules, keys];
	n = Length[$rules];
	$dirty = ConstantArray[1, n];
	range = Range[n];

	maxCount = Min[32 * Length[keys], $MaxInferenceSteps];

	count = 0;
	While[
		Total[$dirty] > 0,
		Do[
			pair = {a, b} = Part[$rules, i] /. TypeReplace[s_, rs_] :> Replace[strongeval[s], rs];
			{ea, eb} = eval @ pair; 
			u = Null;
			Catch[
				u = unify[ea, eb];
				If[ea =!= u, set[a, u]];
				If[eb =!= u && !FreeQ[b, NetPath], set[b, u]]
			,
				unify, catchUnifyFailure
			];
			Part[$dirty, i] = 0;
			If[count++ >= maxCount, 
				AppendTo[$LastTypeInferenceStack, {ea, eb} -> u];
				If[Length[$LastTypeInferenceStack] > 3, ThrowFailure["netinfexc"]];
			];
			,
			{i, Pick[range, $dirty, 1]}
		];
	];

	(* condense expanded-out types that don't actually contain any info *)
	$types /. TensorT[dims:{SizeT..}] :> RuleCondition @ TensorT[SizeListT[Length[dims]]]
];

PackageScope["$LastTypeInferenceStack"]
PackageScope["$LastFailedTypeInferenceData"]
PackageScope["$LastFailedTypeInferenceLHS"]
PackageScope["$LastFailedTypeInferenceRHS"]

$LastTypeInferenceStack = {};
$LastFailedTypeInferenceLHS = None;
$LastFailedTypeInferenceRHS = None;

General::netinfexc = "The given net appears to be too large to correctly infer all types. Consider increasing NeuralNetworks`.`$MaxInferenceSteps from its default value."

General::tyfaildebug = "Type unification or setting failure:\n`` = ``\n`` = ``\nUnified = ``\nSet = ``";

catchUnifyFailure[_, _] := If[$DebugMode,
	$LastFailedTypeInferenceData = {a, ea, b, eb, u};
	$LastFailedTypeInferenceLHS = ea; 
	$LastFailedTypeInferenceRHS = eb;
	ThrowFailure["tyfaildebug", a, ea, b, eb, u, $setFailure],
	If[!FreeQ[{ea, eb}, _EitherT], 
		reportAmbigType[],
		reportFailure[a, b, ea, eb]
	];
];

General::tyambig2 = "Could not resolve an ambigious type within the net. The origin of the problem appears to be ``, which is ``. Please specify a type for this part manually, and try again.";
General::tyambig1 = "Could not resolve an ambigious type within the net. Try specify the input or output shapes of layers with flexible input types, such as LinearLayer, EmbeddingLayer, BatchNormalizationLayer, and DotLayer."

reportAmbigType[] := Scope[
	pos = Keys @ Select[$vals, !FreeQ[#, _EitherT]&];
	pos = SortBy[pos, -Count[#, "Input"]&];
	pos = First[pos, None];
	If[pos === None,
		ThrowFailure["tyambig1"],
		ThrowFailure["tyambig2", MsgForm[pos], MsgForm[$vals @ pos]]
	];
];

General::tyinc = "Value for `` (``) is inconsistent with value for `` (``)."
reportFailure[p1_NetPath ? visiblePath, p2_NetPath ? visiblePath, t1_, t2_] :=
	ThrowFailure["tyinc", NetPathString[p1], TypeString[t1], NetPathString[p2], TypeString[t2]];

General::tyfail = "Inferred inconsistent shapes for `` (`` versus ``).";
reportFailure[p_NetPath ? visiblePath, p2_, t1_, t2_] := 
	ThrowFailure["tyfail", NetPathString[p], TypeString[t1], TypeString[t2]];

General::tyufail = "Net contains inconsistent shapes.";
reportFailure[___] :=
	ThrowFailure["tyufail"];

visiblePath[path_] := FreeQ[path, s_String /; StringStartsQ[s, "$"]];

fmtSlot[name_, s_, pos_, col_] := 
	Interpretation[
		Tooltip[
			Style[StringRiffle[pos, ":"], Bold, Darker[col, .2], ShowStringCharacters -> False],
			RawBoxes @ Cell[BoxData @ RowBox[{"NetPath", "[", 
				Sequence @@ Riffle[
					ToBoxes[Style[#, ShowStringCharacters -> True]]& /@ pos, 
					","], 
				"]"
			}], "Input"]
		],
		s
	];


PackageScope["RunTestInference"]

RunTestInference[types_, rules_] := CatchFailure @ Scope[
	types = KeyMap[NetPath, types];
	rules = MapAt[NetPath, rules, {All, 1}];
	result = iDoInference[types, rules, Keys[types]];
 	KeyMap[First, result]
];

Clear[set];

set[p_NetPath, value_] := setPath[p, value];

setPath[p_, value_] := 
	If[$types[p] =!= value,
		Part[$dirty, $ruleindex[p]] = 1;
		$types[p] = value;
	];

set[t1_TensorT, t2_TensorT] := setTensor[t1, t2];

set[ListT[n1_, t1_], ListT[n2_, t2_]] := (set[n1, n2]; set[t1, t2];)
set[ListT[n_, t_], e_List] := (set[n, Length[e]]; Scan[set[t, #]&, e];)
set[_, RepeatedInteger[n_]] := Null;

set[e_List, ListT[n_, t_]] := (set[Length[e], n]; Scan[set[#, t]&, e];)
set[a_List, b_List] /; Length[a] == Length[b] := MapThread[set, {a, b}];

set[EnumT[t1_], EnumT[t2_]] := set[t1, t2];

set[ImageT[sz_, c_], img_Image] := 
	(set[sz, ImageDimensions[img]]; set[c, ImageColorSpace[img]];);

set[ImageT[sz1_, c1_], ImageT[sz2_, c2_]] :=
	(set[sz1, sz2]; set[c1,c2];);

set[PosIntegerT, PosIntP] := Null;
set[NaturalT, NatP] := Null;
set[SizeT, PosIntP] := Null;
set[SizeT, SizeT] := Null;

set[SizeT, _LengthVar] := Null;

set[a_Integer, b_Integer] := If[a =!= b, spanic[a, b]];

set[_ComputedType, _] := Null;

set[ExpressionT, _] := Null;

set[Nullable[_], None] := Null;

set[Nullable[t_], d_] := set[t, d];

set[EitherT[list_List], d_] := Block[
	{spanic := Throw[$Failed]},
	Do[ (* gah, this technically has to be transactional if types are ambigious. f that. *)
		If[!FailureQ[Catch @ set[t, d]], Break[];],
		{t, list}
	]
];

set[Defaulting[t_, _], d_] := set[t, d];

set[IndexIntegerT[n_Integer], x_Integer] := If[x < 1 || x > n, spanic[n, x]];

set[IndexIntegerT[a_], IndexIntegerT[b_]] := set[a, b];

(* comes up in using LogPerplexityLossLayer for seq2seq *)
set[i1_IndexIntegerT, TensorT[{}, i2_IndexIntegerT]] := set[i1, i2];

set[LengthVar[_], _] := Null

set[t_TensorT, coder:CoderP] :=
	set[t, CoderType[coder]];

set[_, _EitherT] := Null

set[a_, b_] := If[a =!= b, spanic[a, b]];

spanic[] := Throw[$Failed, unify];
spanic[a_, b_] := ($setFailure = {a, b}; spanic[]);

eval[e_] := e /. {
	c_ComputedType :> evalCT[strongeval @ c],
	p_NetPath :> Lookup[$types, p, Panic["MissingNetPath", "Could not find path `` in types ``.", p, $types]]
};

strongeval[e_] := e //. p_NetPath :> RuleCondition[$types[p]];

evalCT[ComputedType[type_, expr_, deps_, trigger_:False]] := Scope[
	If[!VectorQ[deps, ConcreteParameterQ] && !TrueQ[trigger],
		Return[type]];
	Check[
		res = expr;
		If[!FailureQ[res] && !FailureQ[UnifyTypes[res, type]], res, type]
	, 
		type
	]
]

Clear[setTensor, setRank, length, dropRank, takeRank];

setTensor[TensorT[d_, t_], ScalarT] := (
	set[t, RealT]; 
	If[length[d] === 1, set[d, {1}], set[d, {}]];
);

setTensor[TensorT[d1_, t_], TensorT[d2_, t_]] := set[d1, d2];

setTensor[TensorT[d1_, t1:TTypeP], TensorT[d2_, t2:TTypeP]] :=
	(set[d1, d2]; set[t1, t2]);

setTensor[TensorT[d1_, t1_], TensorT[d2_, t2_]] := Scope[
	r1 = length[d1]; r2 = length[d2];
	Which[
		!IntegerQ[r1] && IntegerQ[r2], setTensor2[TensorT[d1, t1], TensorT[d2, t2]],
		!IntegerQ[r2], Null,
		r1 == r2, set[d1, d2]; setRank[t1, t2],
		r1 < r2, set[d1, takeRank[d2, r1]]; setRank[t1, TensorT[dropRank[d2, r1], t2]],
		r1 > r2, set[d2, takeRank[d1, r2]]; setRank[t2, TensorT[dropRank[d1, r2], t1]]
	]
]

length[list_List] := Length[list];
length[ListT[n_Integer, _]] := n;
length[ListT[p_NetPath, _]] := intOrNull @ Lookup[$types, p];
length[p_NetPath] := length @ Lookup[$types, p];
length[_] := Null

intOrNull[i_Integer] := i;
intOrNull[_] := Null;

setRank[p_NetPath, t_TensorT] := set[p, t];
(* avoid type vars getting set to e.g. RealT, which should never be naked *)
setRank[p_NetPath, t_] := set[p, TensorT[{}, t]]; 
setRank[t1_TensorT, t2_TensorT] := setTensor[t1, t2];
setRank[t1_TensorT, t2_] := setTensor[t1, TensorT[{}, t2]];
setRank[t1_, t2_TensorT] := setTensor[TensorT[{}, t1], t2];
setRank[t1_, t2_] := set[t1, t2];

dropRank[list_List, n_] := Drop[list, n];
dropRank[ListT[n_Integer, z_], m_] := ListT[n - m, z];
dropRank[_, _] := Null

takeRank[list_List, n_] := Take[list, n];
takeRank[ListT[n_Integer, z_], m_] := ListT[n - m, z];
takeRank[_, _] := Null


setTensor[a_, b_] := spanic[a, b];

(* more complex case:
we hope that the inner tensor is fixed rank, and the right hand size
is fixed rank, so the outer tensor's rank can be chosen.
ex: 
RunTestInference[
 <|"A" -> TensorT[{5, 3}], "B" -> TensorT[{3}], "C" -> SizeListT[]|>,
 {"A" -> TensorT[NetPath["C"], NetPath["B"]]}
 ]
*)
setTensor2[TensorT[n_NetPath, t1_], t2_TensorT] := Scope[
	r1 = TRank[strongeval @ t1];
	r2 = TRank[t2];
	If[IntegerQ[r1] && IntegerQ[r2],
		dims = TDimensions[t2];
		set[n, Take[dims, r2 - r1]];
		set[t1, TensorT[Drop[dims, r2 - r1], TType[t2]]];
	]
];




PackageScope["TypeDependenceGraph"]

(* TODO: make this easier to use directly, e.g. takes a net directly *)

TypeDependenceGraph[rules_, index_] := Scope[
	edges = Flatten @ KeyValueMap[toRule[#1, rules[[#2]]]&, index];
	edges = DeleteDuplicatesBy[edges, Sort[First[#]]&];
	Graph[edges, VertexLabels -> Placed["Name", Tooltip]]
];

SetAttributes[toRule, Listable];
toRule[port_, rule_Rule] := Tooltip[port <-> #, rule]& /@ DeleteCases[Dedup @ DeepCases[rule, _NetPath], port];



PackageScope["FailValidation"]

General::valfail = "Validation failed for ``: ``";

FailValidation[layer_Symbol, reason_] := ThrowFailure["valfail", layer, fromStringForm @ reason];
FailValidation[layer_Symbol, reason_String, args__] := FailValidation[layer, StringForm[reason, args]];
FailValidation[___] := Panic["BadFailValidation"];
