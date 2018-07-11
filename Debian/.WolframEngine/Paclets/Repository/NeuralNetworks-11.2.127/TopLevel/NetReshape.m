Package["NeuralNetworks`"]



PackageExport["NetReshape"]

NetReshape[NetP[head, net, meta], rules___Rule] := CatchFailureAsMessage[
	ConstructWithInference[head, iNetReshape[net, {rules}], meta]
]


PackageScope["iNetReshape"]

(* %REVISIT make this an official part of layer API. *)

$wipeKeys = "Dimensions"|"$Dimensions"|"$InputSize"|"$OutputSize"|"OutputSize"|"Dimensions"|
	"$OutputDimensions"|"$InputDimensions"|"$IDimensions"|"$InputShapes"|"$OutputShape"|"$Shape"|"Channels"|"$Channels"|"$InsertedDimensions";

$wipeDecoders = "Image"|"Boolean";

iNetReshape[net_Association, rules_List] := Scope[
	$net = net;
	keys = Position[$net, _Integer | {__Integer} (* 329424: packed arrays *)] /. Key[s_String] :> s;
	keys = Select[StringQ] /@ keys;
	ioRules = Cases[keys, p:{___, "Inputs"|"Outputs", _} :> (p -> TypeT)];
	paramRules = Cases[keys, p:{___, "Parameters", $wipeKeys} :> (p -> paramType[p])];
	$LastScrambledNet ^= $net = ReplacePart[$net, Join[ioRules, paramRules]];
	$net = $net /. DecoderP[kind:$wipeDecoders, param_, type_] :> NetDecoder[kind, param, AnyTensorT];
	Scan[applyPortSpec, Flatten @ ToList[rules]];
	$net
]

paramType[{most___, "Parameters", pname_}] := $LayerData[$net[most, "Type"], "Parameters", pname];

applyPortSpec[s_String -> t_] := Which[
	!MissingQ[$net["Inputs", s]],
		$net["Inputs", s] = ParseInputSpec[s, TypeT, t],
	!MissingQ[$net["Outputs", s]],
		$net["Outputs", s] = ParseOutputSpec[s, TypeT, t],
	True,
		ThrowFailure["netnopart", s]
];