Package["NeuralNetworks`"]



PackageScope["CreateEncoder"]

SetHoldAllComplete[CreateEncoder];

SetHoldAllComplete[HoldValidEncoderQ]
HoldValidEncoderQ[NetEncoder[_, _, _] ? System`Private`HoldNoEntryQ] := True;
HoldValidEncoderQ[_] := False;

CreateEncoder[NetEncoder[enc_NetEncoder ? HoldValidEncoderQ]] := enc;
CreateEncoder[NetEncoder[dec:NetDecoder[_String, _Association, _ ? ValidTypeQ]]] := DecoderToEncoder[dec];
CreateEncoder[enc:NetEncoder[_String, _Association, _ ? ValidTypeQ]] := System`Private`HoldSetNoEntry[enc];

(* Upgrade from old encoders *)
CreateEncoder[NetEncoder[name_String, <|"Parameters" -> params_, "Output" -> output_|>]] :=
	System`Private`ConstructNoEntry[NetEncoder, name, params, output /. $TensorUpdateRule];

CreateEncoder[NetEncoder[spec:{"Characters", __}]] := iCreateEncoder @@ NormalizeCharacterSpec[spec];
CreateEncoder[NetEncoder["Characters"]] := iCreateEncoder @@ NormalizeCharacterSpec[{"Characters"}];
CreateEncoder[NetEncoder[spec:{"Tokens", ___}]] := iCreateEncoder @@ NormalizeTokenSpec[spec];
CreateEncoder[NetEncoder["Tokens"]] := iCreateEncoder @@ NormalizeTokenSpec[{"Tokens"}];
CreateEncoder[NetEncoder[{type_String, args___}]] := iCreateEncoder[type, args];
CreateEncoder[NetEncoder[type_String]] := iCreateEncoder[type];

iCreateEncoder[type_String, args___] := Scope[
	
	$currentCoderHead = NetEncoder;
	$currentCoderType = type;

	NetEncoder::badtype = "`` is not a valid NetEncoder type.";
	data = Lookup[$EncoderData, type, ThrowFailure[NetEncoder::badtype, type]];
	
	params = ParseArguments[$coderFormString, False, data, {args}];

	assoc = <|"Parameters" -> params, "Output" -> data["Output"]|>;

	assoc = DoInference[assoc, data["InferenceRules"], List @ data["PostInferenceFunction"]];

	If[!FullySpecifiedTypeQ[assoc["Output"]],
		If[$DebugMode, Print[PrettyForm @ assoc]];
		NetEncoder::decnfs = "Not enough information was provided to fully specify the output of the NetEncoder.";
		ThrowFailure[NetEncoder::decnfs]
	];

	res = System`Private`ConstructNoEntry[
		NetEncoder, type, assoc["Parameters"], assoc["Output"]
	];

	If[TrueQ @ data["AcceptsLists"] @ assoc, 
		System`Private`SetValid[res]
	];

	res
];

NetEncoder::invargs = "Invalid arguments given in ``."
CreateEncoder[enc_] := ThrowFailure[NetEncoder::invargs, HoldForm @ enc];


PackageScope["DecoderToEncoder"]

DecoderToEncoder[DecoderP[kind_, assoc_, type_]] := $DecoderData[kind, "DecoderToEncoder"][assoc, type];