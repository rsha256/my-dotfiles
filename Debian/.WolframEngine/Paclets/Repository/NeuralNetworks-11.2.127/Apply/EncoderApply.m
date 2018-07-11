Package["NeuralNetworks`"]


PackageScope["EncoderApply"]

EncoderApply[encoder_, input_] :=
	Catch[getEncoderFunction[encoder, False] @ input, EncodeFail];

(* for encoders that accept lists, we must first try the non-batched version,
then if that fails try the batched version (achieved via fallthrough *)
EncoderApply[encoder_ ? System`Private`ValidQ, input_List] := Block[
	{$noisy, res},
	res = Catch[getEncoderFunction[encoder, False] @ input, EncodeFail]; 
	res /; res =!= $Failed
];

EncoderApply[encoder_, input_List] := Catch[
	getEncoderFunction[encoder, True] @ input,
	EncodeFail
];


PackageScope["ToEncoderFunction"]

Clear[ToEncoderFunction];

ToEncoderFunction[encoder_NetEncoder, batchq_] := 
	getEncoderFunction[encoder, batchq];

ToEncoderFunction[TensorT[{}, ttype_], batchq_] := ToEncoderFunction[ttype, batchq];
ToEncoderFunction[IndexIntegerT[max_Integer], batchq_] :=
	If[batchq, TestIndexIntegerVector[max], TestIndexInteger[max]];

(* todo: generalize this beyond just sequences *)
ToEncoderFunction[TensorT[{n_}, e:Except[_TensorT|RealT]], batchq_] := With[
	{enc = ToEncoderFunction[e, True], len = If[IntegerQ[n], n, Automatic]},
	If[batchq, 
		BatchSeqRecursiveEncoder[enc, len],
		SeqRecursiveEncoder[enc, len]
	]
];

ToEncoderFunction[_, _] := Identity;


PackageScope["SeqRecursiveEncoder"]

(* use good-ol ComplainType here *)
SeqRecursiveEncoder[f_, n_][in_] :=
	If[in === {} || !ListQ[in] || Length[in] =!= n, 
		EncodeFail["input is not a sequence of length ``", n],
		f @ in
	];

SeqRecursiveEncoder[f_, Automatic][in_] :=
	If[in === {} || !ListQ[in],
		EncodeFail["input is not a sequence"],
		f @ in
	];

PackageScope["BatchSeqRecursiveEncoder"]

BatchSeqRecursiveEncoder[f_, n_Integer][in_] :=
	If[in === {} || !VectorQ[in, ListQ] || !MatchQ[Dimensions[in, 2], {_, n}],
		EncodeFail["input is not a list of sequences of length ``", n],
		Map[f, in]
	];

BatchSeqRecursiveEncoder[f_, Automatic][in_] :=
	If[in === {} || !VectorQ[in, ListQ] || (Min[Length /@ in] === 0),
		EncodeFail["input is not a list of sequences"],
		Map[f, in]
	];


getEncoderFunction[enc_, batchq_] :=
	Cached[getEncoderFunctionCached, enc, batchq];

getEncoderFunctionCached[HoldPattern @ NetEncoder[type_, assoc_Association, _], batchq_] :=
	$EncoderData[type, "ToEncoderFunction"][assoc, batchq];


