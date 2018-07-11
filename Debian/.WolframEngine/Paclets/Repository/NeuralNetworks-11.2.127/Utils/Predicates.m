Package["NeuralNetworks`"]



PackageExport["FullySpecifiedNetQ"]

DeclareMethod[FullySpecifiedNetQ, LayerFullySpecifiedQ, ContainerFullySpecifiedQ];

Clear[FullySpecifiedNetQ]; (* override default Call behavior for valid-flag fast-path *)
FullySpecifiedNetQ[net_NetP] := 
	System`Private`ValidQ[net] || 
		If[Call[net, FullySpecifiedNetQ], 
			System`Private`SetValid[net]; True, 
			False
		];

FullySpecifiedNetQ[_] := False;

ContainerFullySpecifiedQ[assoc_] := 
	And[
		AllTrue[assoc["Inputs"], FullySpecifiedTypeQ],
		AllTrue[assoc["Outputs"], FullySpecifiedTypeQ],
		AllTrue[assoc["Nodes"], FullySpecifiedNetQ]
	];

LayerFullySpecifiedQ[assoc_] := ConcreteNetQ[assoc] && InitializedNetQ[assoc];


NNSetUsage @ "
FullySpecifiedNetQ[net] gives True if a net's arrays are all initialized and its parameters, inputs, and outputs have concrete types.
The Valid flag will be recursively set on the net's assocs when this is the case to make subsequent lookups free.
"

PackageExport["FindVarLengthInput"]

FindVarLengthInput[net_NetP] := Scope[
	inputs = Inputs[net];
	If[FreeQ[inputs, _LengthVar], $Failed,
		First @ SelectFirstIndex[inputs, !FreeQ[#, _LengthVar]&]]
];


PackageExport["FindUnspecifiedPath"]

FindUnspecifiedPath[net_NetP, arrayq_:True] := Scope[
	$path = NetPath[]; $uspecarrayq = arrayq;
	Catch[Call[net, ThrowUnspecifiedPart]; None]
];

DeclareMethod[
	ThrowUnspecifiedPart, 
	ThrowUnspecifiedPathInLayer, 
	ThrowUnspecifiedPathInContainer,
	ThrowUnspecifiedPathInOperator
];

ThrowUnspecifiedPathInLayer[layer_] := (
	ThrowUnspecifiedPathInField[ConcreteParameterQ, layer, "Parameters"];
	If[$uspecarrayq, ThrowUnspecifiedPathInField[InitializedArrayQ, layer, "Arrays"]];
	ThrowUnspecifiedPathInField[FullySpecifiedTypeQ, layer, "Inputs"];
	ThrowUnspecifiedPathInField[FullySpecifiedTypeQ, layer, "Outputs"];
)

ThrowUnspecifiedPathInContainer[assoc_] := (
	ScanFields["Nodes", ThrowUnspecifiedPart, assoc];
	ThrowUnspecifiedPathInField[FullySpecifiedTypeQ, assoc, "Inputs"];
	ThrowUnspecifiedPathInField[FullySpecifiedTypeQ, assoc, "Outputs"];
);

ThrowUnspecifiedPathInOperator[assoc_] := (
	ScanSubNets[ThrowUnspecifiedPart, assoc];
	ThrowUnspecifiedPathInLayer[assoc];
);

ThrowUnspecifiedPathInField[test_, layer_, field_] := 
	KeyValueScan[
		If[!test[#2] && !StringStartsQ[#1, "$"], Throw @ Join[$path, NetPath[field, #1]]]&, 
		layer[field]
	];


PackageExport["ConcreteNetQ"]

DeclareMethod[ConcreteNetQ, LayerConcreteQ, ContainerConcreteQ]

Clear[ConcreteNetQ]; (* override default Call behavior for valid-flag fast-path *)
ConcreteNetQ[net_NetP] := System`Private`ValidQ[net] || Call[net, ConcreteNetQ];
ConcreteNetQ[_] := False;

ContainerConcreteQ[assoc_] := (
	AllTrue[assoc["Nodes"], ConcreteNetQ]
)

LayerConcreteQ[assoc_] := And[
	AllTrue[assoc["Arrays"], ConcreteParameterQ],
	AllTrue[assoc["Inputs"], FullySpecifiedTypeQ],
	AllTrue[assoc["Outputs"], FullySpecifiedTypeQ],
	AllTrue[assoc["Parameters"], ConcreteParameterQ]
];
(* TODO: When we have a separate Types field, we use FullySpecifiedTypeQ there,
and for Parameters we use ConcreteParameterQ, which will check for actual integers, etc*)

NNSetUsage @ "
ConcreteNetQ[net] gives True if a net's parameters, inputs, and outputs have concrete types.
"


PackageExport["InitializedNetQ"]

DeclareMethod[InitializedNetQ, LayerInitializedQ, ContainerInitializedQ, OperatorInitializedQ];

Clear[InitializedNetQ]; (* override default Call behavior for valid-flag fast-path *)
InitializedNetQ[net_NetP] := System`Private`ValidQ[net] || Call[net, InitializedNetQ];
InitializedNetQ[_] := False;

LayerInitializedQ[assoc_] := AllTrue[assoc["Arrays"], InitializedArrayQ];

ContainerInitializedQ[assoc_] := AllTrue[assoc["Nodes"], InitializedNetQ];

OperatorInitializedQ[assoc_] := And[
	LayerInitializedQ[assoc],
	VectorQ[
		GetSubNets[assoc],
		InitializedNetQ
	]
];


PackageExport["InitializedArrayQ"]

InitializedArrayQ[ra_] := RawArrayQ[ra];
InitializedArrayQ[None] := True


NNSetUsage @ "
InitializedNetQ[net] gives True if a net's parameters are all initialized to RawArrays.
"
