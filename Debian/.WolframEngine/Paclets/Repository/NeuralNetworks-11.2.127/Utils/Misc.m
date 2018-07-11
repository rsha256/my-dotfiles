Package["NeuralNetworks`"]



PackageScope["BroadcastingConform"]

BroadcastingConform[arrays_] := Scope[
	dims = Map[Dimensions, arrays];
	ranks = Map[Length, dims];
	deepest = MaxIndex[ranks];
	tdim = dims[[deepest]]; trank = Length[tdim];
	newArrays = MapThread[broadcastTo[trank - #1, tdim, #2]&, {ranks, arrays}];
	offset = trank - Min[ranks];
	{offset, newArrays}
];

broadcastTo[0, _, arr_] := arr;
broadcastTo[n_, td_, arr_] := ConstantArray[arr, Take[td, n]];

	

PackageScope["RuleVectorQ"]

RuleVectorQ[list_] := MatchQ[list, {__Rule}];