Package["NeuralNetworks`"]



PackageScope["ConstructLayer"]

ConstructLayer[assoc_Association] := ConstructLayer[assoc, $StandardMetadata];

ConstructLayer[assoc_Association, meta_Association] := 
	System`Private`ConstructNoEntry[NSymbol[assoc], assoc, meta];


PackageScope["ValidLayerAssocQ"]

ValidLayerAssocQ[assoc_ ? AssociationQ] := And[
	KeyExistsQ[$TypeToSymbol, assoc["Type"]],
	KeyExistsQ[assoc, "Inputs"],
	KeyExistsQ[assoc, "Outputs"]
];

ValidLayerAssocQ[___] := False;


PackageScope["CreateLayer"]
PackageScope["$strictLayerArgs"]

$strictLayerArgs = True;

SetHoldRest[CreateLayer];

CreateLayer[name_String, net:(_Symbol[data_Association /; ValidLayerAssocQ[data]])] := 
	UpgradeAndSealNet[net];

CreateLayer[name_String, net:(_Symbol[data_Association /; ValidLayerAssocQ[data], _Association])] := 
	MaybeDowngradeAndSealNet[net];


CreateLayer[name_String, head_Symbol[args___]] := iCreateLayer[name, head, args];

iCreateLayer[name_String, head_Symbol, Verbatim[Blank][___] | Verbatim[Pattern][_, _]] := 
	Fail;

General::netnargs = "`` arguments were provided, expected ``."

iCreateLayer[name_, head_, args___] := Scope[
	
	data = $LayerData[name];

	$interiorStates = <||>;
	{arrays, params, inputs, outputs, states} = ParseArguments[head, True, data, {args}];

	(* this is only non-trivial for operators containing nets with their own unattached states *)
	istates = Association[interiorStateRules[#1][#2, params[#2]]& @@@ data["SubNets"]];

	assoc = Association[{
		"Type" -> name,
		"Arrays" -> arrays,
		"Parameters" -> params,
		"Inputs" -> inputs,
		"Outputs" -> outputs,
		If[states === <||>, Nothing, "States" -> states],
		If[istates === <||>, Nothing, "InteriorStates" -> istates]
	}];

	{irules, pfuncs} = ReapInferenceRules[assoc];
	pcf = data["PostConstructionFunction"];
	If[pcf =!= None, AppendTo[pfuncs, pcf]];
	
	assoc = DoInference[assoc, irules, pfuncs];	

	System`Private`ConstructNoEntry[head, assoc, $StandardMetadata]
]


PackageScope["$StandardMetadata"]

$StandardMetadata := Association["Version" -> $NeuralNetworksVersionNumber];


PackageExport["NetSetMetadata"]

NetSetMetadata[NetP[data, meta], newmeta_] := 
	ConstructLayer[data, Association[meta, newmeta]]


PackageExport["NetGetMetadata"]

NetGetMetadata[NetP[data, meta]] := meta;
