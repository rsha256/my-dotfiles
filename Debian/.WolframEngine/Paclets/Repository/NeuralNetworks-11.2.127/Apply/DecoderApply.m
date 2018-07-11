Package["NeuralNetworks`"]


PackageScope["DecoderApply"]

(* for decoders that accept lists, we must first try the non-batched version,
then if that fails try the batched version (achieved via fallthrough *)

NetDecoder::notarray = "Input to NetDecoder should be a numeric tensor."
NetDecoder::invarray = "Input to NetDecoder is not a tensor of the correct dimensions."
NetDecoder::invrank = "Input to NetDecoder is not a tensor of the correct rank."

DecoderApply[decoder_, input_, prop_] := Scope[
	If[!MachineArrayQ[input] && !MachineQ[input], ThrowFailure["notarray"]];
	ddepth = DecoderDepth[decoder];
	ddims = DecoderDimensions[decoder];
	idims = Dimensions[input];
	idepth = Length[idims];
	Which[
		ddepth === 0, 
			Which[
				idepth == 0,
					checkDims[idims, ddims];
					getDecoderFunction[decoder, prop, False] @ input,
				idepth == 1, 
					checkDims[Rest[idims], ddims];
					getDecoderFunction[decoder, prop, True] @ input,
				True,
					ThrowFailure["invarray"]
			],
		idepth === ddepth,
			checkDims[idims, ddims];
			getDecoderFunction[decoder, prop, False] @ input,
		idepth === ddepth + 1,
			checkDims[Rest[idims], ddims];
			getDecoderFunction[decoder, prop, True] @ input,
		ddepth === None,
			getDecoderFunction[decoder, prop, False] @ input,
		True,
			ThrowFailure["invrank"]
	]
];

checkDims[_, $Failed] := Null;
checkDims[idims_, ddims_] := If[!MatchQ[idims, ddims], ThrowFailure["invarray"]];

DecoderApply[___] := $Failed;


PackageScope["ToDecoderFunction"]

(* there is some uncertainty about whether ScalarT represents an encoded scalar,
as in {BatchSizeT, 1}, or a genuine scalar, e.g. {BatchSize}. Examples of the
latter are SummationLayer, and the loss layers. Example of the former is a "Boolean"
decoded output that we need to support CrossEntropyLoss["Binary"]. *)

ToDecoderFunction[ScalarT, Automatic|None, batchq_] :=
	If[batchq, Flatten, First[#,#]&];

ToDecoderFunction[dec_NetDecoder /; CoderType[dec] === ScalarT, None, batchq_] := 
	If[batchq, Flatten, First[#,#]&];	

ToDecoderFunction[dec_NetDecoder, prop_, batchq_] :=
	getDecoderFunction[dec, prop, batchq];

ToDecoderFunction[SequenceT[_, dec_NetDecoder], prop_, batchq_] :=
	Map @ ToDecoderFunction[dec, prop, batchq];

ToDecoderFunction[_, Automatic|None, _] := Identity;

ToDecoderFunction[dec_NetDecoder, batchq_] := 
	getDecoderFunction[dec, Automatic, batchq];

(* asked for a property on a non-decoded type *)
ToDecoderFunction[_, _, _] := $Failed;


getDecoderFunction[_, None, _] := Identity;

getDecoderFunction[dec_, prop_, batchq_] :=
	Cached[getDecoderFunctionCached, dec, prop, batchq];

getDecoderFunctionCached[DecoderP[name_, assoc_, type_], Automatic, batchq_] :=
	$DecoderData[name, "ToDecoderFunction"][assoc, batchq, type];

getDecoderFunctionCached[DecoderP[name_, assoc_, type_], prop_, batchq_] := 
	OnFail[
		invProp[name, assoc],
		$DecoderData[name, "ToPropertyDecoderFunction"][assoc, batchq, prop, type]
	];

NetDecoder::invprop = "NetDecoder of type `` only supports the following properties: ``."
NetDecoder::noprops = "NetDecoder of type `` does not support properties."

invProp[type_, assoc_] := Match[
	$DecoderData[type, "AvailableProperties"],
	{} :> ThrowFailure[NetDecoder::noprops, type],
	list_List :> ThrowFailure[NetDecoder::invprop, type, list]
];


