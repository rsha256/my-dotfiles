Package["NeuralNetworks`"]



PackageScope["$FromMXNetName"]

$FromMXNetName = Data`UnorderedAssociation[];


PackageExport["$LayerData"]

$LayerData = Data`UnorderedAssociation[];


PackageExport["$LoadInternalLayers"]


PackageExport["DefineLayer"]

General::netdefnoparent = "There is no layer called `` to inherit from (yet)."
General::netdefmxdollar = "MXNetFunction should not contain any $XXX symbols."

$OperatorVTable := $OperatorVTable = Join[$VTable["GenericLayer"], $VTable["GenericOperator"]];

DefineLayer[name_, assoc_Association] := CatchFailure @ Scope[
	
	symbolName = name <> Lookup[assoc, "Suffix", "Layer"];

	If[NameQ["System`" <> symbolName],
		symbolName = "System`" <> symbolName,
		If[!TrueQ[$LoadInternalLayers], Return[None, Block]];
		symbolName = "NeuralNetworks`" <> symbolName
	];
	symbol = Symbol[symbolName];
	Compose[Unprotect, symbol];

	BagInsert[$NetHeads, symbol];

	If[StringQ[parent = assoc["InheritsFrom"]],
		If[!AssociationQ[parentData = $LayerData[parent]],
			ThrowFailure["netdefnoparent", parent];
		];
		parentData = parentData /. $TypeToSymbol[parent] -> symbol;
		assoc = Join[KeyDrop[parentData, {"Type", "Symbol", "Tests"}], assoc];
	];

	assoc = CoerceParam[DefineLayer, assoc, LayerDefinitionT] /. 
		With[{symbol = symbol}, HoldPattern[FailValidation[reason_]] :> FailValidation[symbol, reason]];

	(* turn "Input" -> foo into "Inputs" -> <|"Input" -> foo|> *)
	Do[
		If[assoc[key] =!= None,
			assoc[key <> "s"] = <|key -> assoc[key]|>;
		];
		KeyDropFrom[assoc, key];
		,
		{key, {"Input", "Output"}}
	];

	assoc["Type"] = name;
	assoc["Symbol"] = symbol;

	allowDynamic = SetAutomatic[assoc["AllowDynamicDimensions"], !FreeQ[assoc["Parameters"], _LengthVar]];

	If[!allowDynamic,
		ComposeTo[
			assoc["PostInferenceFunction"], 
			ApplyThrough[{#, CreateFunction[
				Hold[CheckNotDynamic][symbol, Thread @ NetPath["Inputs", Keys[assoc["Inputs"]]]]
			]}]&
		];
	];

	DesugarTypeDefinitions[assoc, {"Parameters", "Inputs", "Outputs", "Arrays", "States"}];
	SetupArgCounts[assoc];

	If[assoc["MXNet"] =!= None,
		mxname = assoc["MXNet", "Name"];
		$FromMXNetName[mxname] = name;
		aliases = assoc["MXNet", "Aliases"];
		If[ListQ[aliases],
			AssociateTo[$FromMXNetName, Thread[aliases -> name]]
		];
	];

	writer = assoc["Writer"];
	If[writer =!= None,
		assoc["Writer"] = writer;
		If[!FreeQ[writer, NetPath], ThrowFailure["netdefmxdollar"]];
	];

	If[assoc["SubNets"] =!= {},
		$VTable[name] = $OperatorVTable,
		$VTable[name] = $VTable["GenericLayer"];
	];

	summaryFunction = assoc["SummaryFunction"];
	If[summaryFunction =!= None,
		AssociateTo[$VTable[name], SummaryForm -> RightComposition[Key["Parameters"], summaryFunction]];
	];

	$LayerData[name] ^= assoc;
	$TypeToSymbol[name] = symbol;
	$SymbolToType[symbol] = name;

	SetupLayerDispatch[
		symbol, name, 
		assoc["ParameterDefaults"],
		Keys @ assoc["Inputs"], 
		Keys @ assoc["Outputs"], 
		Keys @ assoc["Arrays"]
	];

	assoc
];

LayerDefinitionT = StructT[{
	"Input" -> Defaulting @ TypeExpressionT,
	"Output" -> Defaulting @ TypeExpressionT,
	"Inputs" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"Outputs" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"States" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"Arrays" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"Parameters" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"ParameterCoercions" -> Defaulting @ AssocT[StringT, ExpressionT],
	"ParameterDefaults" -> Defaulting @ AssocT[StringT, ExpressionT],
	"InferenceRules" -> Defaulting @ ListT @ RuleT[MatchT[_NetPath], TypeExpressionT],
	"PostInferenceFunction" -> Defaulting @ ExpressionT,
	"PostConstructionFunction" -> Defaulting @ ExpressionT,
	"AuxArrays" -> Defaulting @ ListT[StringT],
	"Writer" -> Defaulting @ FunctionT,
	"MXNet" -> Defaulting @ StructT[{
		"Name" -> StringT, 
		"Parameters" -> Defaulting @ AssocT[StringT, StringT],
		"Arrays" -> Defaulting @ AssocT[StringT, StringT],
		"Reader" -> Defaulting @ FunctionT,
		"Writer" -> Defaulting @ FunctionT,
		"Skip" -> Defaulting[BooleanT, False],
		"Aliases" -> Defaulting @ ListT[StringT]
	}],
	"SubNets" -> Defaulting @ ListT[MatchT[_NetPath]],
	"IsLoss" -> Defaulting[BooleanT, False],
	"WLEquivalent" -> Defaulting[ExpressionT, Missing[]],
	"InheritsFrom" -> Defaulting[StringT, None],
	"Suffix" -> Defaulting[StringT, "Layer"],
	"SummaryFunction" -> Defaulting[FunctionT, None],
	"Tests" -> Defaulting[ListT[RuleT[ExpressionT, StringT]], {}],
	"MaxArgCount" -> Defaulting[IntegerT, Automatic],
	"MinArgCount" -> Defaulting[IntegerT, Automatic],
	"AllowDynamicDimensions" -> Defaulting[BooleanT, Automatic],
	"SourceFile" -> Defaulting[StringT, None]
}];


SetupLayerDispatch[symbol_, name_, params_, ins_, outs_, arrays_] := (
	SetCatchFailureAsMessage[symbol, s_symbol ? System`Private`HoldEntryQ, CreateLayer[name, s]];
	Options[symbol] = Join[
		Normal @ Map[toOptionValue, KeySelect[params, !StringStartsQ[#, "$"]&]] /. RuleDelayed -> Rule,
		Thread[Join[arrays, ins, outs] -> Automatic]
	];
	SetupGenericDispatch[symbol, Length[ins] == 1];
	DefineCustomBoxes[symbol, s_symbol ? System`Private`HoldNoEntryQ :> MakeLayerBoxes[s]];
	Format[s_symbol ? System`Private`HoldNoEntryQ, OutputForm] := LayerOutputForm[s];
);

toOptionValue[sym_Symbol] /; Context[sym] === "System`" := sym;
toOptionValue[sym_] := If[ConcreteParameterQ[sym], sym, FromT[sym]];
toOptionValue[RepeatedInteger[n_]] := n;


PackageScope["LayerOutputForm"]

LayerOutputForm[head_[___]] := SymbolName[head] <> "[<>]";


PackageScope["SetupArgCounts"]

SetHoldFirst[SetupArgCounts];

SetupArgCounts[data_] := Scope[

	params = data["Parameters"];
	keys = Keys[params];

	n = 1; max = 0;
	Do[
		If[!StringStartsQ[key, "$"], max = n++, Break[]],
		{key, keys}
	]; 
	n = 1; min = 0; defs = data["ParameterDefaults"];
	Do[
		If[!KeyExistsQ[defs, key] && !MatchQ[params[key], _ComputedType], min = n++, Break[]],
		{key, keys}
	];
	old = Lookup[data, {"MinArgCount", "MaxArgCount"}];
	min = Min[min, max, old /. Automatic -> Infinity];
	max = Max[min, max, old /. Automatic -> 0];
	SetAutomatic[data["MaxArgCount"], max];
	SetAutomatic[data["MinArgCount"], min];
];


PackageExport["LayerData"]

LayerData[name_String, parts___] := Slice[parts] @ $LayerData[name];
LayerData[nf_Association, parts___] := Slice[parts] @ nf;
LayerData[parts___] := Slice[parts] @ $LayerData;



PackageExport["LayerInfo"]

LayerInfo[parts___] := niceGrid[LayerData[parts]]


PackageExport["NetLayerQ"]

NetLayerQ[head_Symbol[arg_Association, _] ? System`Private`NoEntryQ] := True;
NetLayerQ[_] := False;


PackageScope["ToContainerLayers"]

ToContainerLayers[list_List] := Scope[
	layers = Map[ToLayer, list];
	AssociationThread[IntegerString[Range[Length[layers]]], layers]
];

NetGraph::notstrkey = "All layer keys must be strings."

ToContainerLayers[assoc_Association] := (
	If[!StringVectorQ[Keys[assoc]], 
		If[VectorQ[Keys[assoc], IntegerQ],
			Return @ ToContainerLayers[KeyMap[IntegerString, assoc]]
		];
		ThrowFailure["notstrkey"]
	];
	DeleteCases[Nothing] @ Map[ToLayer, assoc]
);

PackageScope["ToLayer"]

ToLayer[list_List] := NData @ ThrowOnFailure @ NetChain[list];
ToLayer[HoldPattern[Alternatives][args___]] := NData @ ThrowOnFailure @ NetParallel[args];
ToLayer[Identity] := Nothing;
ToLayer[i_Integer] := NData @ LinearLayer[i];
ToLayer[sym_Symbol /; MemberQ[$PrimitiveBinaryElementwiseFunctions, sym]] := NData @ ThreadingLayer[sym];
ToLayer[sym_Symbol /; MemberQ[$PrimitiveUnaryElementwiseFunctions, sym]] := NData @ ElementwiseLayer[sym];
ToLayer[$Raw[e_]] := e;

General::netinvnodes = "`` is not a net function."
ToLayer[e_] := If[NetLayerQ[e], NData @ e, ThrowFailure["netinvnodes", e]];


PackageScope["GetSubNets"]

GetSubNets[assoc_] := assoc @@@ NProperty[assoc, "SubNets"];


