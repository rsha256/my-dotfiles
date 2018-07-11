Package["NeuralNetworks`"]



PackageScope["NetSequenceGenerate"]

Clear[NetSequenceGenerate];

NetSequenceGenerate::notsimplenet = "Net should have one input and one output.";
NetSequenceGenerate[net_NetP, init_, n_] := CatchFailure @ Scope[
	(* assume input *)
	inputs = Inputs[net];
	outputs = Outputs[net];
	If[Length[inputs] =!= 1 || Length[outputs] =!= 1, ThrowFailure["notsimplenet"]];

	input = First @ inputs; output = First @ outputs;
	encf = ToEncoderFunction[input, False];
	decf = ToDecoderFunction[output, Automatic, False];
	
	data = encf @ init;
	bucketedExecutor = ToBucketedMXExecutor[net, {All, $EvaluationContext, False, False, False}];

	lastBucket = None;

	fsamp = makeSampler[StripCoders @ input, StripCoders @ output];

	Do[
		thisBucket = List @ RoundBucket[Length[data]];
		If[lastBucket =!= thisBucket,
			executor = GetBucketExecutor[bucketedExecutor, thisBucket];
			inputArray = First @ executor["InputArrays"];
			outputArray = First @ executor["OutputArrays"];
			lastBucket = thisBucket;
		];
		NDArraySet[inputArray, data];
		MXExecutorForward[executor];
		AppendTo[data, fsamp @ NDArrayGetFlat[outputArray]];
	, 
		n
	];

	data
];

makeSampler[SequenceT[_LengthVar, t1_], t2_] := makeSampler2[t1, t2];

NetSequenceGenerate::notseqin = "Input to net should be a sequence.";
makeSampler[_, _] := ThrowFailure["notseqin"];

makeSampler2[IndexIntegerT[n_], VectorT[n_]] := With[{r = Range[n]}, RandomChoice[# -> r]&];
makeSampler2[VectorT[n_], VectorT[n_]] := With[{m = IdentityMatrix[n]}, RandomChoice[# -> m]&];
NetSequenceGenerate::badrecur = "Cannot automatically connect output that is `` to input that is ``.";
makeSampler2[itype_, otype_] := ThrowFailure["badrecur", TypeString[otype], TypeString[itype]];