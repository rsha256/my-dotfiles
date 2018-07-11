Package["NeuralNetworks`"]



PackageScope["CreateDecoder"]

SetHoldAllComplete[CreateDecoder];

SetHoldAllComplete[HoldValidDecoderQ]
HoldValidDecoderQ[NetDecoder[_, _, _] ? System`Private`HoldNoEntryQ] := True;
HoldValidDecoderQ[_] := False;

CreateDecoder[NetDecoder[dec_NetDecoder ? HoldValidDecoderQ]] := dec;
CreateDecoder[dec:NetDecoder[_String, _Association, _ ? ValidTypeQ]] := System`Private`HoldSetNoEntry[dec];
CreateDecoder[NetDecoder[enc:NetEncoder[_String, _Association, _ ? ValidTypeQ]]] := EncoderToDecoder[enc];

(* Upgrade from old decoders *)
CreateDecoder[NetDecoder[name_String, <|"Parameters" -> params_, "Input" -> input_|>]] := 
	System`Private`ConstructNoEntry[NetDecoder, name, params, input /. $TensorUpdateRule];

CreateDecoder[NetDecoder[spec:{"Characters", __}]] := iCreateDecoder @@ NormalizeCharacterSpecDecoder[spec];
CreateDecoder[NetDecoder["Characters"]] := iCreateDecoder @@ NormalizeCharacterSpecDecoder[{"Characters"}];
CreateDecoder[NetDecoder[spec:{"Tokens", ___}]] := iCreateDecoder @@ NormalizeTokenSpec[spec];
CreateDecoder[NetDecoder["Tokens"]] := iCreateDecoder @@ NormalizeTokenSpec[{"Tokens"}];
CreateDecoder[NetDecoder[{type_String, args___}]] := iCreateDecoder[type, args];
CreateDecoder[NetDecoder[type_String]] := iCreateDecoder[type];

iCreateDecoder[type_String, args___] := Scope[
	
	$currentCoderHead = NetDecoder;
	$currentCoderType = type;

	NetDecoder::badtype = "`` is not a valid NetDecoder type.";
	data = Lookup[$DecoderData, type, ThrowFailure[NetDecoder::badtype, type]];
	
	params = ParseArguments[$coderFormString, False, data, {args}];

	assoc = <|"Parameters" -> params, "Input" -> data["Input"]|>;

	assoc = DoInference[assoc, data["InferenceRules"], List @ data["PostInferenceFunction"]];

	res = System`Private`ConstructNoEntry[
		NetDecoder, type, assoc["Parameters"], assoc["Input"]
	];

	res
];

NetDecoder::invargs = "Invalid arguments given in ``."
CreateDecoder[dec_] := ThrowFailure[NetDecoder::invargs, HoldForm @ dec];


PackageScope["EncoderToDecoder"]

EncoderToDecoder[EncoderP[kind_, assoc_, type_]] := $EncoderData[kind, "EncoderToDecoder"][assoc, type];