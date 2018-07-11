Package["NeuralNetworks`"]


PackageExport["NetChain"]

SetupGenericDispatch[NetChain, True];

(ng_NetChain ? System`Private`HoldEntryQ) := 
	UseMacros @ RuleCondition @ CatchFailureAsMessage[NetChain, make[ng]];

SetHoldAll[make];

make[nc:NetChain[<|"Type" -> "Chain", ___|>]] := 
	UpgradeAndSealNet[nc];

make[nc:NetChain[<|"Type" -> "Chain", ___|>, _Association]] :=
	MaybeDowngradeAndSealNet[nc];

make[NetChain[layers_Association | layers_List, rules___Rule]] := 
	toNetChain[ToContainerLayers[layers], {rules}];

make[ng_NetChain] /; PatternFreeQ[Unevaluated[ng]] := 
	(CheckArgumentCount[ng, 1, 1]; $Failed);

make[_] := Fail;

NetChain::empty = "NetChain objects must contain at least one vertex."

toNetChain[layers_, rules_] := Scope[
	If[Length[layers] === 0, ThrowFailure["empty"]];
	ins = Normal @ layers[[All, "Inputs"]];
	outs = Normal @ layers[[All, "Outputs"]];
	$currentNodes = layers; (* for error reporting *)
	$currentNodeName = "layer";
	conns = Flatten @ MapThread[glueLayers, {Prepend[outs, None], Append[ins, None]}];
	(* handle custom type specifications *)
	inputs = ins[[1,2]];
	outputs = outs[[-1,2]];
	istates = Association @ KeyValueMap[interiorStateRules["Nodes"], layers];
	Which[
		rules === {}, 
			Null,
		ListQ[rules],
			$parseinputhead = NetChain;
			kin = Keys[inputs];
			kout = Keys[outputs];
			extra = Complement[Keys[rules], kin, kout];
			If[extra =!= {} && FreeQ[rules, $Raw], ThrowFailure["netinvcport", First[extra]]];
			Set[inputs[#1],  ParseInputSpec[#1,  inputs[#1],  #2]]& @@@ FilterRules[rules, kin];
			Set[outputs[#1], ParseOutputSpec[#1, outputs[#1], #2]]& @@@ FilterRules[rules, kout],
		AssociationQ[rules], (* only used internally by NetChainTakeDrop *)
			inputs = Lookup[rules, "Inputs", inputs];
			outputs = Lookup[rules, "Outputs", outputs];
	];
	layers = StripCoders[layers];
	CheckForTuplePorts[inputs, NetChain];
	assoc = Association[{
		"Type" -> "Chain",
		"Nodes" -> layers,
		"Edges" -> conns,
		"Inputs" -> inputs,
		"Outputs" -> outputs,
		If[istates === <||>, Nothing, "InteriorStates" -> istates]
	}];
	ConstructWithInference[NetChain, assoc]
];

NetChain::netinvcport = "`` is neither a valid input or output port for the given NetChain.";

glueLayers[None, bname_ -> b_] :=
	NetPath["Nodes", bname, "Inputs", #] -> NetPath["Inputs", #]& /@ Keys[b]; 

glueLayers[aname_ -> a_, None] :=
	NetPath["Outputs", #] -> NetPath["Nodes", aname, "Outputs", #]& /@ Keys[a];

glueLayers[aname_ -> a_, bname_ -> b_] := Scope[
	{aport, atype} = get1port[aname, "Outputs", a];
	{bport, btype} = get1port[bname, "Inputs", b];
	res = UnifyTypes[atype, btype];
	If[FailureQ[res], edgeTypeError[aport, bport, atype, btype]]; (* <- lives in NetGraph.m *)
	bport -> aport
];

General::notuport = "Layer `` should have exactly one `` port.";
get1port[name_, ptype_, <|portname_ -> porttype_|>] := {NetPath["Nodes", name, ptype, portname], porttype};
get1port[name_, ptype_, _] := ThrowFailure["notuport", name, StringDrop[ptype, -1]];


PackageExport["$NetChainInteractivity"]

$NetChainInteractivity := $NetInteractivity;

PackageScope["netChainGrid"]

netChainGrid[assoc_, tradForm_] := Scope[
	UnpackAssociation[assoc, nodes, inputs, outputs];
	$hasuninit = False; $trad = tradForm;
	rows = ToList[
		KeyValueMap[toInputRow, inputs],
		KeyValueMap[toLayerRow, nodes],
		KeyValueMap[toOutputRow, outputs]
	];
	If[$hasuninit && !tradForm, 
		AppendTo[rows, {
			Item[Style["(uninitialized)", $uninitializedColor, Small], Alignment -> Center], 
			SpanFromLeft, SpanFromLeft}
		]
	];
	grid = Grid[rows, Alignment -> Left, Spacings -> 1.1];
	If[!tradForm && $NetChainInteractivity, With[
		{gridBoxes = ToBoxes[grid], assoc2 = ReplaceRawArraysWithDummies[assoc]},
		RawBoxes @ DynamicModuleBox[
			{assoc3 = assoc2, opart, part, selected = Null},			
			DynamicBox[
				GridBox[
					List[
						List @ MouseClickBoxes[gridBoxes, 
							If[ListQ[part = MouseAnnotation[]],
								If[opart === part, 
									selected = Null; opart = Null,
									selected = Part[assoc3, Sequence @@ part]; opart = part;
								];
							]
						], 
						fmtSelected[selected, part]
					], 
					GridBoxSpacings -> {"Columns" -> {{1}}},
					GridBoxAlignment -> {"Columns" -> {{Left}}}
				], 
				TrackedSymbols :> {selected}
			],
			Initialization :> {NetChain}
		]],
		grid
	]
];

SetAttributes[MouseClickBoxes, HoldRest];
MouseClickBoxes[boxes_, code_] := TagBox[boxes, 
	EventHandlerTag[{
		"MouseClicked" :> code, Method -> "Preemptive", 
		PassEventsDown -> Automatic, PassEventsUp -> True
	}]
];


Clear[fmtSelected];

fmtSelected[Null, _] := Nothing;
fmtSelected[type_, {"Inputs"|"Outputs", name_}] := List @ ItemBox[typeInfo[name -> type], Alignment -> {Center, Center}];
fmtSelected[layer_, {"Nodes", name_}] := List @ ItemBox[itemInfo[name -> layer], Alignment -> {Center, Center}];

(* TODO: Handle last layer properly *)
toLayerRow[name_, assoc_] := 
	selector["Nodes", name] /@ {
		Style[name, Gray], 
		If[$trad, 
			SummaryForm[assoc, True],
			Style[SummaryForm[assoc], If[InitializedNetQ[assoc], Black, $hasuninit = True; $uninitializedColor]]
		] /. SpecializedSummary[_, label_] :> label,
		fmtItem @ First[assoc["Outputs"]]
	};

toInputRow[name_, type_] := 
	selector["Inputs", name] /@ {"", name, fmtInputItem @ type};

fmtInputItem[t_] := fmtItem[t];

fmtInputItem[t:TensorT[{n_}, enc_NetEncoder]] := Column[{
	fmtItem[t], 
	fmtItem[TensorT[{n}, CoderType[enc]]]}, 
	BaselinePosition -> 2
];

fmtInputItem[enc_NetEncoder] := Column[{CoderKind[enc], fmtItem[CoderType[enc]]}, BaselinePosition -> 2];


toOutputRow[name_, type_] := 
	selector["Outputs", name] /@ {"", name, fmtOutputItem @ type};

fmtOutputItem[t_] := fmtItem[t];

fmtInputItem[t:TensorT[{n_}, enc_NetEncoder]] := Column[{
	fmtItem[t], 
	fmtItem[TensorT[{n}, CoderType[enc]]]}, 
	BaselinePosition -> 2
];

fmtOutputItem[dec_NetDecoder] := CoderKind[dec];


selector[part___] := If[$NetChainInteractivity, MouseAppearance[Annotation[#, {part}, "Mouse"], "LinkHand"]&, Identity];

DefineCustomBoxes[NetChain, 
	NetChain[assoc_Association, _Association] ? System`Private`HoldNoEntryQ :> formatNetChain[assoc]
];

formatNetChain[assoc_Association] := 
	MakeCustomHeadBox["NetChain", netChainGrid[assoc, False], Automatic];

Format[HoldPattern[nc:NetChain[assoc_Association, _Association]] ? System`Private`HoldNoEntryQ, OutputForm] := 
	StringJoin[
		"NetChain[<", 
		IntegerString @ Length @ assoc["Nodes"],
		">]"
	];

PackageExport["$LowercaseTraditionalForm"]

$LowercaseTraditionalForm = True;

PackageScope["TFCase"]
TFCase[str_] := If[$LowercaseTraditionalForm, ToLowerCase[str], str];


NetChain /: MakeBoxes[
	HoldPattern[NetChain[assoc_Association, meta_Association] ? System`Private`HoldNoEntryQ],
	TraditionalForm
] := Block[{$NetChainInteractivity = False},
	If[$LowercaseTraditionalForm, decapitalizeStrings, Identity] @ ToBoxes @ netChainGrid[assoc, True]
];


PackageExport["GetNodes"]

NNSetUsage @ "
GetNodes[NetGraph[$$]] gives an association of the vertices within a NetGraph.
GetNodes[NetChain[$$]] gives an association of the layers within a NetChain.
GetNodes[net, True] will give a list of the assoc has integer keys.
"

GetNodes[HoldPattern @ NetChain[assoc_Association, _]] := 
	Map[ConstructLayer, assoc["Nodes"]];

GetNodes[net_, True] := Scope[
	nodes = GetNodes[net];
	If[DigitStringKeysQ[nodes], Values[nodes], nodes]
];


NetChain /: Take[nc_NetChain, spec_] := CatchFailureAsMessage[NetChain, NetChainTakeDrop[Take, nc, spec]];
NetChain /: Drop[nc_NetChain, spec_] := CatchFailureAsMessage[NetChain, NetChainTakeDrop[Drop, nc, spec]];
NetChain /: Normal[nc_NetChain] := GetNodes[nc, True];


NetChain::invtakespec = "Invalid `` specification."

NetChain::invlayername = "`` is not the name of layer."

$LayerNameP = _String | _Integer;

NetChainTakeDrop[f_, HoldPattern @ NetChain[assoc_Association, _], spec:$LayerNameP | {$LayerNameP, $LayerNameP}] := Scope[
	layers = assoc["Nodes"];
	first = First[layers];
	last = Last[layers];
	len = Length[layers];
	keys = Keys[layers];
	spec = spec /. str_String :> IndexOf[keys, str, ThrowFailure["invlayername", str]];
	newNodes = UnsafeQuietCheck[f[layers, spec], $Failed];
	If[FailureQ[newNodes] || Length[newNodes] == 0, ThrowFailure["invtakespec", f]];
	newSpecs = KeyTake[assoc, {"Inputs", "Outputs"}];
	If[First[newNodes] =!= First[layers], KeyDropFrom[newSpecs, "Inputs"]];
	If[Last[newNodes] =!= Last[layers], KeyDropFrom[newSpecs, "Outputs"]];
	toNetChain[newNodes, newSpecs]
];

NetChainTakeDrop[f_, _, _] := ThrowFailure["invtakespec", f];


PackageExport["DeleteLayers"]

DeleteLayers[nc_NetChain, deleteList_List] := Scope[
	data = NData[nc];
	layers = data["Nodes"];
	If[DigitStringKeysQ[layers], noKeys = True];
	layerNames = AssociationThread[#, #]& @ Keys[layers];
	deleteList = PartElse[layerNames, #, badLayer[#]]& /@ deleteList;
	KeyDropFrom[layers, deleteList];
	If[noKeys, layers = First @ RemapKeys[layers]];
	portHints = Join[
		If[MemberQ[deleteList, First @ layerNames], {}, Normal @ Inputs[nc]], 
		If[MemberQ[deleteList, Last @ layerNames], {}, Normal @ Outputs[nc]]
	];
	CatchFailureAsMessage[NetChain, toNetChain[layers, (#1 -> $Raw[#2]& @@@ portHints)]]
];

NetChain::invlayerspec = "The layer `` does not exist.";
badLayer[spec_] := ThrowFailure["invlayerspec", spec];
