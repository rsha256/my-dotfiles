Package["NeuralNetworks`"]



$genericIconBoxes := $genericIconBoxes = Uncompress @ "
1:eJxTTMoPSmNiYGAo5gYS7kWJBRmZycVO+RUQQSEkQef83IKc1AqQHCtIjgVI+GQWl0BUwn
iZQJoBTKCKFzGAwYP9mYw45exxy/3AJgcWwmIXGg+kqpgdSPgXJCZnllQWGYPBZXuEjE9mXi
rIY2x4PAa2jAmHI5hxiLNgirPiMIcVhzms2M2BOF4QSDgmFefnlJakBuRn5pUEZ1algrVA5D
mABFgcI9og7oM7BmwzmjVBpTmpxZxAhmduYnoq2GRVIA8ArVdlhg=="

PackageScope["SummaryForm"]
PackageScope["$TradQ"]

DeclareMethod[SummaryForm, SimpleSummaryForm, ContainerSummaryForm, OperatorSummaryForm];

$TradQ = False;
SimpleSummaryForm[assoc_] := If[$TradQ, deCamelCase @ assoc["Type"], NSymbol[assoc]];

ContainerSummaryForm[assoc_] := If[
	$TradQ, "Subnetwork",
	SpecializedSummary[
		NSymbol[assoc],
		Compose[HoldForm, NSymbol[assoc], UniqueSkeleton @ Length @ assoc["Nodes"]]
	]
];

PackageScope["UniqueSkeleton"]

UniqueSkeleton[n_] := UniqueSkeleton[n, RandomInteger[2^16]];

(* this is a silly hack to ensure that multiple e.g. NetChains don't show up as one
thing on a legend, because they're NOT actually the same *)
MakeBoxes[UniqueSkeleton[n_, _], form_] := MakeBoxes[Skeleton[n], form]; 

OperatorSummaryForm[assoc_] := Scope[
	subnets = GetSubNets[assoc];
	If[$TradQ, Return @ StringTrim[SimpleSummaryForm[assoc], "Net"]];
	inner = If[Length[subnets] === 1, 
		SummaryForm[First[subnets]],
		Skeleton[Length[subnets]]
	];
	Compose[HoldForm, SimpleSummaryForm[assoc], inner]
]

SummaryForm[net_, tradq_] := Block[{$TradQ = tradq}, SummaryForm[net]];

PackageScope["decapitalizeStrings"]

$ulRE = RegularExpression["\"[A-Z][a-z]"];
decapitalizeStrings[boxes_] := ReplaceAll[boxes, {
	TemplateBox[tb_, rest___] :> RuleCondition @ TemplateBox[decapitalizeStrings[tb], rest],
	s_String /; StringStartsQ[s, $ulRE] :> RuleCondition @ If[s === "\"ReLU\"", s, decapitalize[s]]
}];

decapitalize[str_String] := ToLowerCase[StringTake[str, 2]] <> StringDrop[str, 2];


PackageScope["$DummyVar"]

$DummyVar = Style["x", Italic];


PackageScope["deCamelCase"]

$camelRE = RegularExpression["[a-z][A-Z]"];
deCamelCase[str_] := 
	StringReplace[str, cam:$camelRE  :> StringInsert[
		If[$LowercaseTraditionalForm, ToLowerCase[cam], cam], 
		" ", 2]
	];

PackageScope["SpecializedSummary"]

(* currently used by ThreadingLayer and ElementwiseLayer to return a 
summary that should be used in a NetGraph legend if it is the only
layer of the given type. If it isn't, the second arg is used, and
the first arg is put as label above the vertex instead *)


PackageScope["$uninitializedColor"]

$uninitializedColor = RGBColor[0.66, 0, 0];
$uninitializedColor2 = RGBColor[0.58, 0.25, 0.25];


PackageScope["MakeLayerBoxes"]

SetHoldFirst[MakeLayerBoxes];

MakeLayerBoxes[layer:head_Symbol[assoc_Association, meta_]] := Scope[
	icon = If[InitializedNetQ[layer], 
		$genericIconBoxes, 
		OverlayBox[{
			Append[$genericIconBoxes, BaseStyle -> GrayLevel[0.65]],
			StyleBox["uninitialized", $uninitializedColor, FontSize -> 8]
		}, Alignment -> {Center, Center}]
	];
	{hypers, hidden} = LengthVarScope[infoItems[assoc]];
	OptimizedArrangeSummaryBox[head, icon, hypers, hidden]
];

MakeLayerBoxes[_] := Fail;


PackageScope["infoItems"]

infoItems[assoc_] := Scope[
	type = assoc["Type"];
	hypers = fmtSection[assoc["Parameters"], "Parameters", False];
	If[Length[hypers] === 1, AppendTo[hypers, {StyleBox["none", Gray], ""}]];
	params = fmtSection[assoc["Arrays"], "Arrays", True];
	ports = fmtSection[Join[$in /@ assoc["Inputs"], $out /@ assoc["Outputs"]], "Ports", True];
	states = fmtSection[assoc["States"], "States", True];
	hidden = ToList[params, ports, states];
	{hypers, hidden}
];

fmtSection[<||>, "Parameters", _] := fmtSection[<|"$Dummy" -> Null|>, "Parameters", False];

fmtSection[_Missing | <||>, title_, divider_] := {};

fmtSection[assoc_, title_, divider_] := Scope[
	$colorUninit = (title === "Arrays");
	list = fmtEntries @ assoc;
	If[divider, 
		frameStyle = Sequence[Frame -> {{False, False}, {False, True}}, FrameStyle -> LightGray],
		frameStyle = Sequence[];
	];
	PrependTo[list, {
		ItemBox[StyleBox[title, Bold], frameStyle],
		ItemBox[If[divider, spacerBoxes[{1,11}], ""], frameStyle]
	}];
	list
];

spacerBoxes[sz_] := StyleBox[GraphicsBox[{}, ImageSize -> sz, BaselinePosition -> Scaled[0.] -> Baseline], "CacheGraphics" -> False];

PackageScope["fmtEntries"]

fmtEntries[assoc_] := KeyValueMap[fmtEntry, assoc];

fmtEntry[k_, v_] /; StringStartsQ[k, "$"] := Nothing;

fmtEntry["InputChannels"|"Channels", v_] := Nothing;
(* %REVISIT work around the fact that Convolution, Deconvolution, Pooling, and Resize expose these parameter *)

PackageScope["$InEntryFormatting"]

$InEntryFormatting = False;

fmtEntry[k_, v_] := Scope[
	v2 = If[!StringFreeQ[k, "Dimensions"] && VectorQ[v, IntegerQ], fmtDimsList, fmtItem][v];
	k = ToBoxes[k];
	If[$colorUninit && !MatchQ[v, None | _RawArray | _DummyRawArray], k = StyleBox[k, $uninitializedColor2]];
	$InEntryFormatting = True;
	List[
		StyleBox[TemplateBox[{k, "\":\""}, "RowDefault"], "SummaryItemAnnotation"], 
		StyleBox[ToBoxes @ v2, "SummaryItem"]
	]
];


PackageScope["tensorName"]

tensorName[SizeT|NaturalT] := "tensor";
tensorName[0] = "scalar";
tensorName[1] = "vector";
tensorName[2] = "matrix";
tensorName[n_Integer] := IntegerString[n] <> "-tensor";

genTensorName[n_Integer /; n > 2] := 
	Row[{"tensor", " ", fmtNote["rank", n]}];

genTensorName[n_] := tensorName[n];


PackageScope["fmtItem"]

courierBold[e_] := Style[e, FontFamily -> "Courier", FontWeight -> Bold, FontSize -> Larger];

Clear[fmtItem];
fmtItem[<|"Type" -> t_String, ___|>] := With[{sym = $TypeToSymbol[t]}, HoldForm[sym["\[Ellipsis]"]]];

fmtPHList[n_, t_] := Tooltip[
	If[TrueQ[1 <= n <= 3], Table[$PlaceholderIcon, n], "{\[Ellipsis]}"], 
	IndefiniteTypeForm[ListT[n, t]]
];

$plural = False;
fmtPluralItem[e_] := Block[{$plural = True}, fmtItem[e]];

toPlural[s_] := If[$plural, pluralStr[s], s];

fmtItem[SizeListT[n_]] := fmtPHList[n, SizeT];
fmtItem[t:{__SizeT}] := fmtPHList[Length[t], SizeT];
fmtItem[$in[e_]] := fmtItem[e];
fmtItem[$out[e_]] := fmtItem[e]; (* couldn't find a nice icon *)
fmtItem[SequenceT[n_LengthVar, t_]] := Row[{"seq. of ", Style[FormatLengthVar[n], Italic], " ", fmtPluralItem[t]}];
fmtItem[SequenceT[2, t_NetEncoder]] := Row[{"pair of ", fmtPluralItem[t]}];
fmtItem[SequenceT[n_Integer, p:CoderP]] := Row[{"seq. of ", n, " ", fmtPluralItem[p]}];
fmtItem[e_List] := Which[
	StringVectorQ[e], Short[e, .5],
	Length[e] > 4, Short[e, 0.1], 
	True, fmtItem /@ e
];
(*
fmtItem[enc_NetEncoder] := Row[{toPlural[CoderKind[enc]], " encoded as ", fmtItem[CoderType[enc]]}];
fmtItem[dec_NetDecoder] := Row[{fmtItem[CoderType[dec]], " decoded as ", CoderKind[dec]}];
*)
fmtItem[enc_NetEncoder] := toPlural[CoderKind[enc]];
fmtItem[dec_NetDecoder] := toPlural[CoderKind[dec]];

fmtItem[RepeatedInteger[n_]] := Row[{n, ".."}];
fmtItem[_LengthVar] := Style["variable", Gray];
fmtItem[RealT] := toPlural @ "real";
fmtItem[TensorT[ListT[n_, _]]] := toPlural @ genTensorName[n];
fmtItem[e_EnumT] := Tooltip[$PlaceholderIcon, Alternatives @@ First[e]];
fmtItem[EitherT[e_]] := Alternatives @@ Map[fmtItem, e];
fmtItem[TensorT[{}, RealT]] := toPlural @ "real";
fmtItem[TensorT[{}, AtomT]] := toPlural @ "scalar";
fmtItem[TensorT[{}, i_IndexIntegerT]] := fmtItem @ i;
fmtItem[TensorT[list_List]] := fmtTensorDims[list];
fmtItem[TensorT[dims_List, i_IndexIntegerT]] := Row[{fmtTensorDims[dims], " of ", fmtPluralItem[i]}];
fmtItem[t_TensorT] := IndefiniteTypeForm[t, $plural];
fmtItem[r_RawArray] := fmtTensorDims[Dimensions[r]];
fmtItem[r_DummyRawArray] := fmtTensorDims[First[r]];
fmtItem[NaturalT] := Tooltip[$PlaceholderIcon, "non-negative integer"];
fmtItem[SizeT | PosIntegerT] := Tooltip[$PlaceholderIcon, "positive integer"];
fmtItem[IndexIntegerT[max_Integer]] := Row[{If[$plural, "indices", "index"], " ", fmtNote["range", Row[{1, "..", max}]]}];
fmtItem[IndexIntegerT[_]] := If[$plural, "indices", "index"];
fmtItem[t:True|False] := t;
fmtItem[IntegerT] := If[$plural, "integers", "integer"];
fmtItem[Nullable[t_]] := Row[{"optional", " ", fmtItem[t]}];

fmtItem[ValidatedParameter[e_]] := fmtValidatedParameter[e];

(* used only for Dropout specs, currently *)
fmtValidatedParameter[assoc_Association] := Grid[
	List[Row[RawBoxes /@ #, "  "]]& /@ KeyValueMap[fmtEntry, assoc],
	Alignment -> {Left, Automatic},
	BaselinePosition -> 1
];

fmtValidatedParameter[sf_ScalarFunctionObject] := TraditionalForm[
	Apply[
		ScalarFunctionToPureFunction[sf],
		Take[{"x","y","z"}, Length[First[sf]]]
	] /. r_Real /; IntegerQ[Rationalize[r]] :> RuleCondition @ Round[r] 
];

fmtValidatedParameter[e_] := Pane[e, {250}, BaseStyle -> {IndentMaxFraction -> 0.1, ShowStringCharacters -> True}];

fmtItem[ListT[n_Integer, t_]] /; n < 4 := ConstantArray[fmtItem[t], n];

fmtItem[l:ListT[_, t_]] := Scope[
	inner = fmtItem[t];
	If[Head[inner] =!= Tooltip, (* <- avoid saying list of <placeholder>s *)
		Row[{"list of ", inner, "s"}], (* TODO: do this properly *)
		fmtPHList @@ l
	]
];

fmtItem[e_ /; !ValidTypeQ[e]] := e;

fmtItem[img_Image] := Thumbnail[img, If[$VersionNumber >= 11, UpTo[32], 32], Padding -> None];


PackageScope["$PlaceholderIcon"]

$PlaceholderIcon = "\[DottedSquare]";
fmtItem[e_] := If[ConcreteParameterQ[e], e, $PlaceholderIcon];



fmtTensorDims[e_List] := Row[{
	toPlural @ tensorName[Length[e]], " ",
	fmtNote["size", Row[fmtDim /@ e, "\[Times]"]]
}];

fmtNote[prop_, value_] := 
	Style[Row[{"(", "\[VeryThinSpace]", prop, ":", " ", value,  "\[VeryThinSpace]", ")"}], Gray];


PackageScope["fmtDim"]

fmtDim[n_Integer] := IntegerString[n];
fmtDim[lv_LengthVar] := Style[FormatLengthVar[lv], Italic];
fmtDim[_] := $PlaceholderIcon;


PackageScope["fmtDims"]

Clear[fmtDims];
fmtDims[_] := "";
fmtDims[TensorT[_ListT]] := "";(*tensorName[n];*)
fmtDims[r_RawArray] := fmtDims[Dimensions[r]];
fmtDims[cod:CoderP] := fmtDims[CoderType[cod]];
fmtDims[TensorT[{}, t_]] := fmtDims[t];
fmtDims[RealT] := "\[DoubleStruckCapitalR]";
fmtDims[IndexIntegerT[n_Integer]] := Subscript["\[DoubleStruckCapitalN]", n];
fmtDims[TensorT[list_List, _]] := fmtDimsList[list];

PackageScope["fmtDimsList"]

fmtDimsList[{}] := Row[{"{}", Style["  (scalar)", Gray]}];
fmtDimsList[list_] := Row[fmtDim /@ list, "\[Cross]"];


PackageScope["typeInfo"]

typeInfo[key_ -> type_] := 
	infoGrid["Port", "Port", fmtKey @ key, fmtEntries @ <|"Form" -> type|>];

typeInfo[key_ -> cod:CoderP] := 
	infoGrid["Port", "Port", fmtKey @ key,
		fmtEntries @ Prepend[
			CoderData[cod],
			{"Form" -> CoderType[cod], "Type" -> CoderName[cod]}
		]
	];

PackageScope["itemInfo"]

itemInfo[key_ -> x_] := x;
itemInfo[key_ -> assoc_Association] := 
	infoGrid[
		NSymbol[assoc], 
		"Layer", RowBox[{"(", fmtKey[key], ")"}],
		showInfo[assoc]
	];

showInfo[assoc_Association] := Switch[
	assoc["Type"], 
	"Graph", List @ {ToBoxes @ netGraphPlot[assoc, False], "\[SpanFromLeft]"},
	"Chain", List @ {ToBoxes @ netChainGrid[assoc, False], "\[SpanFromLeft]"},
	_, Apply[Join] @ infoItems[assoc]
];

infoGrid[header_, type_, key_, grid_] := TagBox[
	PanelBox[
		FrameBox[
			GridBox[
				Prepend[{StyleBox[RowBox[{header, " ", key}], Bold, 12], "\[SpanFromLeft]"}] @ grid,
				GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Automatic}}},
				GridBoxSpacings -> {"Columns" -> {{1}}, "Rows" -> {0., 0.65, {Automatic}}}
			], 
			FrameStyle -> None, FrameMargins -> {{0,0},{0,5}}
		]
	], 
	Deploy, DefaultBaseStyle -> "Deploy"
];

fmtKey[key_] := If[IntStringQ[key], key, StyleBox[key, ShowStringCharacters -> True]];


PackageScope["LengthVarScope"]

SetHoldFirst[LengthVarScope];
LengthVarScope[expr_] := Block[{$lengthVarNames = <||>, $inLVScope = True}, expr];

PackageScope["FormatLengthVar"]

FormatLengthVar[lv_LengthVar] := If[!TrueQ[$inLVScope], "n",
	Subscript["n", Style[IntegerString @ CacheTo[$lengthVarNames, lv, Length[$lengthVarNames]+1], 7]]
];



PackageScope["OptimizedArrangeSummaryBox"]

(* cut straight to DynamicModuleBox becuase we're generating boxes directly for efficiency reasons but 
ArrangeSummaryBoxes ironically can't take boxes, it insists on boxifying the grid it is provided. *)

OptimizedArrangeSummaryBox[head_, icon_, closedGrid_List, openGrid_List] := Module[
	{boxes, interpretable, typedHead, leftBracket, rightBracket, closedButton, openButton},
	closedButton = DynamicBox[FEPrivate`FrontEndResource["FEBitmaps","SquarePlusIconMedium"]];
	openButton = DynamicBox[FEPrivate`FrontEndResource["FEBitmaps","SquareMinusIconMedium"]];
	typedHead = StyleBox[TagBox[SymbolName[head],"SummaryHead"], "NonInterpretableSummary"];
	leftBracket = StyleBox["[", "NonInterpretableSummary"];
	rightBracket = StyleBox["]", "NonInterpretableSummary"];
	innerBox = If[openGrid === {}, 
		makePanel @ makeGrid[icon, closedGrid, Nothing],
		Block[{UseTextFormattingQ = False}, With[
			{icon2 = If[icon === None, Nothing, icon]}, 
			{grid1 = makeGrid[icon2, closedGrid, closedButton :> Set[Typeset`open, True]],
			 grid2 = makeGrid[icon2, Join[closedGrid, openGrid], openButton :> Set[Typeset`open, False]]},
			{panel = makePanel[PaneSelectorBox[{False -> grid1, True -> grid2}, Dynamic[Typeset`open], ImageSize -> Automatic]]},
			DynamicModuleBox[{Typeset`open = False}, panel]
		]
	]];
	boxes = RowBox[{typedHead, leftBracket, innerBox, rightBracket}];
	With[{copyOut = SymbolName[head] <> "[<>]"}, 
		TagBox[
			TemplateBox[{boxes}, "CopyTag", DisplayFunction->(#1&), InterpretationFunction -> (copyOut&) ], 
			False, Selectable -> False, Editable -> False, SelectWithContents->True
		]
	]
]

(* $iconSize = {Automatic, 3.2 * CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]} *)

makePanel[contents_] := PanelBox[
	contents,
	BaselinePosition-> Baseline,
	BaseStyle -> {ShowStringCharacters -> False, NumberMarks -> False, PrintPrecision -> 3, ShowSyntaxStyles -> False}
]

ClearAll[makeGrid];

makeGrid[icon_, grid_, button_ :> expr_] :=
	makeGrid[
		icon,
		grid,
		PaneBox[
			ButtonBox[button, ButtonFunction :> expr, Appearance -> None, Evaluator -> Automatic, Method -> "Preemptive"], 
			Alignment -> {Center,Center}, 
			ImageSize -> {Automatic, 24}
		]
	];

makeGrid[icon_, grid_, button_] := 
	GridBox[
		List @ List[
			button,
			icon,
			GridBox[
				grid,
				GridBoxAlignment -> {"Columns" -> {{Left}}, "Rows" -> {{Automatic}}},
				GridBoxItemSize -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
				GridBoxSpacings -> {"Columns" -> {{2}}, "Rows" -> {{Automatic}}}
			]
		],
		GridBoxAlignment -> {"Rows" -> {{Top}}},
		GridBoxItemSize -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
		BaselinePosition -> {1, 1}
	];



PackageScope["MsgForm"]

MsgForm[f_Failure] := TextString[f];
MsgForm[path_NetPath] := NetPathString[path];
MsgForm[net_ ? ValidNetQ] := MsgForm[NData[net]];
MsgForm[assoc:<|"Type" -> _, ___|>] := NetShallowString[assoc];
MsgForm[type_ ? ValidTypeQ] := TypeString[type];
MsgForm[s_String] := "\"" <> s <> "\"";
MsgForm[s_List ? StringVectorQ] := StringJoin["\"", Riffle[s, "\",\""], "\""];
MsgForm[e_] := Shallow[e];


PackageScope["NetShallowString"]

NetShallowString[net_NetP] := NShallowString[net];

DeclareMethod[NShallowString, LayerShallowString, ContainerShallowString];

ContainerShallowString[net_] := 
	StringJoin[
		SymbolName @ NSymbol[net], "[", 
			SkeletonString @ Length @ net["Nodes"], 
			If[net["Type"] === "Graph", {",", SkeletonString @ Length @ net["Edges"]}, {}],
		"]"
	]

LayerShallowString[net_] := Scope[
	{min, max} = NProperty[net, {"MinArgCount", "MaxArgCount"}];
	If[min === 0 && max > 0, min = 1]; (* don't be stingy, e.g. CELoss *)
	args = Values @ Take[net["Parameters"], min];
	argStrings = Map[argString, args];
	argStrings = Replace[argStrings, {Shortest[a___], $Failed, ___} :> {a}];
	AppendTo[argStrings, "\[Ellipsis]"];
	SymbolName[NSymbol[net]] <> "[" <> Riffle[argStrings, ","] <> "]"
];

argString[ValidatedParameter[sym_Symbol]] := SymbolName[sym];
argString[net:<|"Type" -> _, ___|>] := NShallowString[net];
argString[e_String] := "\"" <> e <> "\"";
argString[e_Integer | e_Real] := TextString[e];
$simpleAtom = _Integer | _String | _Symbol;
argString[e:{RepeatedNull[$simpleAtom, 3]}] := StringJoin["{", Riffle[Map[argString, e], ","], "}"];
argString[e_] := $Failed;


PackageScope["SkeletonString"]

SkeletonString[n_] := StringJoin["\[LeftGuillemet]", IntegerString[n], "\[RightGuillemet]"];