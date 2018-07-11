Package["NeuralNetworks`"]



PackageExport["NetFlatten"]

NNSetUsage @ "
NetFlatten[net$] flattens a compound net into a NetGraph[$$].
* Encoders and decoders are preserved.
* Layers lose their names.
* Please double check that compound inputs like CatenateLayer or TotalLayer preserve their input order.
"

NetFlatten[net_NetP] := Scope[
	{nodes, edges} = PortConnectivity[net];
	inputs = Inputs[net]; outputs = Outputs[net];
	$nodeIDs = AssociationThread[nodes, Range[Length[nodes]]];
	$nodeIDs[NetPath[]] := Sequence[];
	$pred = Merge[edges, Replace[{e_} :> e]]; 
	keys = Union[List @@ edges];
	nodeInputs = Flatten[ReplaceList[#, edges]& /@ nodes];
	netOutputs = NetPath["Outputs", #]& /@ Keys[outputs];
	edges = Map[findOrigin[#] -> #&, Join[nodeInputs, netOutputs]];
	flatEdges = edges /. NetPath[p___, t_, n_] :> NetPort[{$nodeIDs[NetPath[p]], n}];
	hints = Normal @ Map[$Raw, Join[inputs, outputs]]; 
	NetGraph[Map[$Raw, net @@@ nodes], flatEdges]
];

findOrigin[p_] := Scope[
	Do[
		p2 = $pred[p];
		If[ListQ[p2], Return[findOrigin /@ p2]];
		If[KeyExistsQ[$nodeIDs, p2] || MissingQ[p2],  Return[p]];
		p = p2
	, 16]
];