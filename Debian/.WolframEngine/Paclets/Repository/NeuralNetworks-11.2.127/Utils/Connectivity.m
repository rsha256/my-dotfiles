Package["NeuralNetworks`"]



PackageScope["InitialVertices"]
PackageScope["FinalVertices"]

InitialVertices[graph_] := Pick[VertexList[graph], VertexInDegree[graph], 0];
FinalVertices[graph_] := Pick[VertexList[graph], VertexOutDegree[graph], 0];



PackageExport["LayerDependencyGraph"]

NNSetUsage @ "
LayerDependencyGraph[net$] gives a Graph, where vertices represent layers and edges represent connections between layers.
* The vertices are NetPath[$$] specifications that correspond to positions in the original net association.
* Edges p$1->p$2 exist when vertex p$2 takes as input one of the outputs of p$1.
* Containers do not appear in the dependency graph.
* The dependency graph does not represent which inputs and outputs when a layer has multiple inputs or outputs, merely \
whether there is any connection at all.
"

LayerDependencyGraph[net_NetP] := Scope[
	$path = NetPath[];
	CollectTo[{$GraphNodes, $GraphEdges}, BuildPortGraph[net]];
	$predFunc = Merge[$GraphEdges, Identity];
	virtNodes = Complement[DeepCases[$GraphEdges, _NetPath], $GraphNodes];
	$virtQ = ConstantAssociation[virtNodes, True];
	edges = Flatten @ Map[layerPredecessorEdges, $GraphNodes];
	Graph[
		$GraphNodes, Reverse[edges, 2],
		VertexLabels -> Placed["Name", Tooltip],
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
		ImageSize -> {800}
	]
];

layerPredecessorEdges[layer_] := 
	Thread @ Rule[layer, 
		Flatten @ ReplaceRepeated[
			$predFunc[layer],
			p_NetPath ? $virtQ :> Lookup[$predFunc, p, {}]
		]
	];


PackageExport["PortConnectivityGraph"]

NNSetUsage @ "
PortConnectivityGraph[net$] gives a bipartite-ish Graph, where vertices represent tensors or layers, and edges represent flow.
* The vertices are NetPath[$$] specifications that correspond to positions in the original net association.
* Vertices can represent an input or output of a layer, e.g. NetPath[[1, 'Inputs', 'Input'], or a layer itself, e.g. NetPath[1].
* Edges flow typically from inputs to layers to outputs to inputs to layers to outputs etc.
* The graph is not strictly bipartite because outputs are themselves connected to inputs.
"

PortConnectivityGraph[net_NetP] := Scope[
	$path = NetPath[];
	CollectTo[{$GraphNodes, $GraphEdges}, BuildPortGraph[net]];
	Graph[$GraphNodes, Reverse[Flatten @ $GraphEdges, 2], 
		VertexLabels -> Placed["Name", Tooltip],
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
		ImageSize -> {800}
	]
];

PackageScope["PortConnectivity"]

PortConnectivity[net_NetP] := Scope[
	$path = NetPath[];
	CollectTo[{$GraphNodes, $GraphEdges}, BuildPortGraph[net]];
	{$GraphNodes, Flatten @ $GraphEdges}
];	


PackageScope["BuildPortGraph"]

DeclareMethod[BuildPortGraph, BuildLayerPortGraph, BuildContainerPortGraph, BuildOperatorPortGraph];

BuildLayerPortGraph[node_] := (
	SowGraphNode[node];
	SowStateEdges[node];
	BagInsert[$GraphEdges, List[
		Thread[OutputPorts[node] -> $path],
		Thread[$path -> InputPorts[node]]
	]];
);

SowStateEdges[node_] := 
	If[KeyExistsQ[node, "States"],
		BagInsert[$GraphEdges, List[
			Outer[Rule, OutputPorts[node], StatePorts[node]],
			Outer[Rule, StatePorts[node], InputPorts[node]]
		]]
	];

BuildContainerPortGraph[assoc_] := (
	MapAtFields["Nodes", BuildPortGraph, assoc];
	Scan[SowPortGraphEdge, assoc["Edges"]];
)

BuildOperatorPortGraph[node_] := Scope[
	subouts = subins = subpaths = {};
	SowStateEdges[node];
	ScanSubNets[
		Function[subnode,
			BuildPortGraph[subnode];
			AppendTo[subpaths, $path];
			AppendTo[subouts, OutputPorts[subnode]];
			AppendTo[subins, InputPorts[subnode]];
		],
		node
	];
	BagInsert[$GraphEdges, List[
		Outer[Rule, OutputPorts[node], Flatten[subouts]],
		Outer[Rule, Flatten[subins], InputPorts[node]]
	]];	(* 
		our outputs depend on the subnet's outputs
		the subnets inputs depend on our inputs
	*)
];


PackageScope["SowGraphNode"]

SowGraphNode[node_] := BagInsert[$GraphNodes, $path];


PackageScope["SowPortGraphEdge"]

SowPortGraphEdge[rule:Rule[_, _List]] := Scan[SowPortGraphEdge, Thread[rule]];
SowPortGraphEdge[edge_] := BagInsert[$GraphEdges, PrefixPorts[edge]];
