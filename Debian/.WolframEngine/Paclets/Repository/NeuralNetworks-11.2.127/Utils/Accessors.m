Package["NeuralNetworks`"]



PackageExport["NetPath"]

NNSetUsage @ "
NetPath[p$1,p$2,$$] represents a hierarchical path into a network.
"

PackageScope["$TypeToSymbol"]

$TypeToSymbol = Data`UnorderedAssociation[];


PackageScope["$SymbolToType"]

$SymbolToType = Data`UnorderedAssociation[];


PackageExport["NSymbol"]

NSymbol[assoc_] := $TypeToSymbol[assoc["Type"]];


(* 
The NetP here probably slows things down a bit, especially for things like InputPorts, which
don't particularly get called on top-level entities. Still, it's nice to be general.
*)


PackageExport["NData"]

SetAttributes[NData, Listable];
NData[head_Symbol[assoc_Association, meta_]] := assoc;
NData[_] := $Unreachable;


PackageExport["NProperty"]

NProperty[net_NetP, prop_] := $LayerData[net["Type"], prop];
NProperty[net_NetP, props_List] := Lookup[$LayerData[net["Type"]], props];

PackageScope["Inputs"]
PackageScope["Outputs"]
PackageExport["RecurrentStates"]

Inputs[net_NetP] := net["Inputs"];
Outputs[net_NetP] := net["Outputs"];
RecurrentStates[net_NetP] := Lookup[net, "States", <||>];


PackageExport["InteriorStates"]

InteriorStates[net_NetP] := 
	If[KeyExistsQ[net, "States"], 
		Association[{#} -> NetPath["States", #]& /@ Keys[net["States"]]],
		Lookup[net, "InteriorStates", <||>]
	];


PackageExport["InputNames"]
PackageScope["OutputNames"]

InputNames[net_NetP] := Keys @ net["Inputs"];
OutputNames[net_NetP] := Keys @ net["Outputs"];


PackageExport["InputTypes"]
PackageScope["OutputTypes"]

InputTypes[net_NetP] := Values @ net["Inputs"];
OutputTypes[net_NetP] := Values @ net["Outputs"];


PackageScope["PortTypes"]

PortTypes[net_NetP] := {
	KeyValueMap[NetPath["Inputs", #1] -> #2&, net["Inputs"]],
	KeyValueMap[NetPath["Outputs", #1] -> #2&, net["Outputs"]]
};


PackageScope["InputPorts"]
PackageScope["OutputPorts"]
PackageScope["StatePorts"]

InputPorts[net_NetP] := Thread @ Join[$path, NetPath["Inputs", Keys @ net["Inputs"]]];
OutputPorts[net_NetP] := Thread @ Join[$path, NetPath["Outputs", Keys @ net["Outputs"]]];
StatePorts[net_NetP] := If[AssociationQ @ net["States"], Thread @ Join[$path, NetPath["States", Keys @ net["States"]]], {}];