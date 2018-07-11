Package["NeuralNetworks`"]



PackageExport["NetExtract"]

NetExtract[head_[assoc_Association, _] ? System`Private`HoldNoEntryQ, spec_] := 
	CatchFailureAsMessage @ If[
		VectorQ[spec, ListQ], 
		extract[#, assoc]& /@ spec,
		extract[spec, assoc]
	];

NetExtract[spec_][expr_] := NetExtract[expr, spec];


PackageExport["NetPart"]

NetPart[net_NetP, parts___] := extract[{parts}, net];


extract[spec_, assoc_] := extractOne[spec, assoc];
extract[spec_List, assoc_] := Apply[extractList, spec] @ assoc;

Clear[extractOne, extractList];
extractOne[spec:(All | _List), assoc_] := FromInternalValue /@ getElement[assoc, spec];
extractOne[pos_Integer | pos_String, assoc_] := FromInternalValue @ getElement[assoc, pos];

NetExtract::invspec = "`` is not a valid specification for NetExtract."
extractOne[spec_, _] := ThrowFailure["invspec", spec];

$msgstack = {};

extractList[][data_] := FromInternalValue @ data;

extractList[spec:(All | _List), rest___][data_] := 
	applyToElement[data, spec, Map[extractList[rest]]]

extractList[pos_, rest___][data_] := 
	applyToElement[data, pos, extractList[rest]];

applyToElement[data_, pos_, f_] := Block[
	{val = getElement[data, pos], $msgstack = Append[$msgstack, pos]},
	f @ val
]

(* TODO: make this a method, part of a container API *)
getElement[_, 0] := ThrowFailure["netnopart", 0];
getElement[assoc_, spec_] := Switch[
	assoc["Type"],
	"Graph"|"Chain", 
		If[KeyExistsQ[assoc["Inputs"], spec] || KeyExistsQ[assoc["Outputs"], spec], 
			getParam[assoc, spec],
			getNode[assoc[["Nodes"]], spec]
		],
	_String, 
		getParam[assoc, spec],
	_, 
		nopart[spec]
];

getNode[nodes_, list_List] := Map[getNode[nodes, #]&, list];

(* bit complex: if it was a NetGraph, the nodes have been re-arranged to be in topological order,
and hence we can't say what the '5th' node IF the layers were provided as an assoc. If they were
provided as a list then we're fine, of course. See 327406.*)
getNode[nodes_, n_Integer] := If[
	1 <= Abs[n] <= Length[nodes],
		Part[
			nodes, 
			If[!DigitStringKeysQ[nodes], 
				n,
				If[Positive[n], 
					IntegerString @ n,
					IntegerString @ (n + 1 + Length[nodes])
				]
			]
		],
	nopart[n]
];

getNode[nodes_, s_String] := Lookup[nodes, s, nopart[s]]

getNode[nodes_, All] := dekey @ nodes;

getNode[_, s_] := nopart[s];

nopart[spec_] := System`Private`SetNoEntry @ Missing["NotPresent", Append[$msgstack, spec]];

(* undo the associationification that happens during construction for 
pure list versions of NetGraph and NetChain *)
dekey[assoc_Association] := 
	If[DigitStringKeysQ[assoc], Values @ assoc, assoc];

dekey[e_] := e;

General::netnopart = "Part `` does not exist.";

getParam[assoc_Association, spec_] := Block[{},
	Do[	
		subassoc = assoc[key];
		If[AssociationQ[subassoc] && KeyExistsQ[subassoc, spec],
			val = subassoc[spec];
			If[key === "Arrays" && !RawArrayQ[val] && val =!= None, 
				val = Automatic];
			Return[val, Block]
		],
		{key, Keys[assoc]}
	];
	FromInternalValue @ Lookup[assoc, spec, nopart[spec]]
];

getParam[_, spec_] := nopart[spec];

PackageScope["FromInternalValue"]

FromInternalValue = MatchValues[
	ValidatedParameter[v_] := FromValidatedParameter[v]; 
	assoc_Association /; KeyExistsQ[assoc, "Type"] := ConstructLayer[assoc];
	list_List := If[PackedArrayQ[list], list, Map[FromInternalValue, list]];
	assoc_Association := Map[FromInternalValue, assoc];
	ra_RawArray := Normal[ra];
	sym_Symbol := If[Context[sym] === "System`", sym, Indeterminate];
	e_ ? AtomQ := e;
	e_Failure := e;
	e_ ? ValidTypeQ := FromT[e];
	m_Missing := m;
	Automatic
];

FromValidatedParameter = MatchValues[
	sf_ScalarFunctionObject := ScalarFunctionToPureFunction[sf];
	ca_CharacterEncodingData := Normal[ca];
	ta_TokenEncodingData := Normal[ta];
	e_ := e
];


PackageExport["NetInputs"]
PackageExport["NetOutputs"]

NNSetUsage @ "NetInputs[net$] gives an association mapping input ports of net$ to their types."
NNSetUsage @ "NetOutputs[net$] gives an association mapping input ports of net$ to their types."

NetInputs[net_NetP] := Map[FromT, net["Inputs"]];
NetOutputs[net_NetP] := Map[FromT, net["Outputs"]];


PackageExport["NetExtractArrays"]

NNSetUsage @ "
NetExtractArrays[net$] gives an association mapping the positions of all initialized arrays \
in net$ to their values.
* Arrays are returned as RawArray objects.
* The positions are in the same form as expected by NetExtract and NetReplacePart.
"

NetExtractArrays[net_NetP] := Scope[
	pos = Position[net, raw_RawArray] /. Key -> Identity;
	AssociationThread[
		FromNetPath @ pos,
		Extract[net, pos]
	]
];
