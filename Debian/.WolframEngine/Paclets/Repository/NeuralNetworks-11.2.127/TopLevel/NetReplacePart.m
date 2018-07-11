Package["NeuralNetworks`"]



PackageScope["NetMutationHandler"]

SetHoldAllComplete[NetMutationHandler];
NetMutationHandler[Set[var_Symbol[[parts___]], value_]] := Scope[
	result = NetReplacePart[var, {parts} -> value];
	If[FailureQ[result], 
		ReissueMessage @ result; $Failed,
		var ^= result
	]
];

NetMutationHandler[_] := Language`MutationFallthrough;



PackageExport["NetReplacePart"]

(*
NNSetUsage @ "
NetReplacePart[layer$, 'array$'->value$] replaces an array in a layer with a new array.
NetReplacePart[layer$, 'input$'->spec$] attaches an encoder or type to an input.
NetReplacePart[layer$, 'output$'->spec$] attaches a decoder or type to an output.
NetReplacePart[net$, {spec$$,'array$'}->value$] replaces an array within a compound net (e.g. NetGraph).
NetReplacePart[net$, {spec$1->value$1,spec$2->value$2,$$}] makes several replacements at once.
NetReplacePart[net$, <|spec$1->value$1,$$|>] equivalent to the above.
* The compound specs are in the form used by NetExtract.
"
*)

NetReplacePart[NetP[head, assoc, meta], specs_] := CatchFailureAsMessage[head, Scope[
	$assoc = assoc;
	specs = If[AssociationQ[specs], 
		Normal[specs], 
		Flatten @ ToList @ specs
	];
	res = CatchFailure[head, 
		Scan[replacePart, specs];
		ConstructWithInference[head, $assoc, meta]
	];
	If[FailureQ[res] && SubsetQ[Union[Keys @ Inputs[assoc], Keys @ Outputs[assoc]], Keys @ specs],
		(* the Quiet is to prevent a double NetEncoder message *)
		$assoc = Quiet @ iNetReshape[assoc, specs];
		res = ConstructWithInference[head, $assoc, meta];
	];
	res
]];

General::netarg1 = "First argument should be a valid net."
NetReplacePart[_, _] := (Message[NetReplacePart::netarg1]; $Failed);

replacePart[spec_ -> value_] := Scope[
	path = ToNetPath[$assoc, spec];
	If[FailureQ[path], ThrowFailure["netnopart", spec]];
	{type, coercion} = extractTypeInfo @@ path;
	Replace[path, NetPath[part___] :> 
		Set[$assoc[part], checkValue[Take[path, -2], type, coercion @ value]]
	];
];

Clear[checkValue];
General::nocodertr = "There is no NetEncoder or NetDecoder to remove from ``."
checkValue[NetPath[io:"Inputs"|"Outputs", name_], p_, None] := Scope[
	prev = $assoc[io, name];
	If[MatchQ[prev, CoderP], 
		CoderType[prev], 
		ThrowFailure["nocodertr", NetPathString @ path]
	]
];

checkValue[NetPath["Inputs", name_], type_, value_] :=
	ParseInputSpec[name, type, value];

checkValue[NetPath["Outputs", name_], type_, value_] := 
	ParseOutputSpec[name, type, value];

checkValue[NetPath["Parameters"|"Arrays", name_], type_, value_] := 
	CoerceParam[name, value, type];

General::invsetpart = "Cannot update the specified subpart."
checkValue[_, _, _] := ThrowFailure["invsetpart"];

extractTypeInfo[part___, field:"Inputs"|"Outputs"|"Parameters"|"Arrays", last_String] := Scope[
	subnet = $assoc[part];
	extractLayerType[subnet["Type"], subnet, field, last]
];

General::nosetlayer = "Replacing entire layers is not currently supported."
extractTypeInfo[___, "Nodes", _] := ThrowFailure["nosetlayer"];

extractLayerType["Graph"|"Chain", assoc_, field_, last_] := {assoc[field, last], Identity};
extractLayerType[type_, assoc_, field_, last_] := {
	$LayerData[type, field, last], 
	If[field === "Parameters", $LayerData[type, "ParameterCoercions", last], Identity]
};

NetReplacePart::invpspec = "`` is not a valid part replacement spec."
replacePart[pspec_] := ThrowFailure["invpspec", pspec];

DeclareArgumentCount[NetReplacePart, 2, True];

