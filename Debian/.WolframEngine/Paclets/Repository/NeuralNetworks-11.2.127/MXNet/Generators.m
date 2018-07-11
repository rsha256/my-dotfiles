Package["NeuralNetworks`"]



PackageExport["$SequenceBucketingPartitions"]
$SequenceBucketingPartitions = None;


PackageScope["ConstructTrainingGenerator"]

NNSetUsage @ "
ConstructTrainingGenerator[<|name$1 -> data$1,$$|>,<|name$1 -> type$1,$$|>,batchsize$] creates a function that produces batches.
* The function maps n$ \[Function] n$th batch.
* The underlying data$i should all be of the same length.
* Each returned batch is a list of individual columns and do not incorporate the name$i.
* The order of data is randomized.
* The function is built out of various type-specific iterators from MXNetLink`Generators`.
* If any of the type$i are variable length sequences, the batches will be grouped by length.
"

ConstructTrainingGenerator[data_, types_, batchsize_] := 
	constructGroupGenerator[
		data, types, batchsize, 
		MakeBucketedRandomizedBatchPermutation[#1, #2, $SequenceBucketingPartitions]&, 
		MakeRandomizedBatchPermutation
	];


PackageScope["ConstructValidationGenerator"]

NNSetUsage @ "
ConstructValidationGenerator[<|name$1 -> data$1,$$|>,<|name$1 -> type$1,$$|>,batchsize$] creates a function that produces batches.
* The function maps n$ \[Function] n$th batch.
* The underlying data$i should all be of the same length.
* Each returned batch is a list of individual columns and do not incorporate the name$i.
* The function is built out of various type-specific iterators from MXNetLink`Generators`.
* If any of the type$i are variable length sequences, the batches will be grouped by length.
"

ConstructValidationGenerator[data_, types_, batchsize_] := Scope[
	n = Length @ First @ data;
	If[n < batchsize, data = Map[padUpTo[batchsize], data]];
	(* ^ handle case where batchsize is larger than validation set size (the excess won't contribute
	to the loss) *)
	constructGroupGenerator[
		data, types, batchsize,
		MakeBucketedBatchPermutation, None&
	]
];

padUpTo[n_][data_List] := Join[ConstantArray[First[data], n - Length[data]], data];
padUpTo[n_][_] := Panic["ValidationSetPaddingError"];

constructGroupGenerator[data_, types_, batchsize_, varbatcher_, nonvarbatcher_] := Scope[
	$batchsize = batchsize;
	n = Length @ First @ data;
	names = Keys[data];
	data = Lookup[data, names];
	types = Lookup[types, names];
	If[bucketed = ContainsVarSequenceQ[types],
		$seqpositions = $seqnames = <||>;
		$seqlens = {};
		data = MapThread[preencodeSequences, {names, data, types}];
		{$perm, buckets} = varbatcher[$seqlens, $batchsize];
	,
		$perm = nonvarbatcher[n, $batchsize];
	];
	generator = Association @ MapThread[constructGenerator, {names, data, types}];
	generator = If[bucketed, 
		BucketedGroupGenerator[generator, buckets],
		GroupGenerator[generator]
	]
];

(* TODO: Name this EncodedSequenceP *)
preencodeSequences[name_, list_, enc:(_NetEncoder ? SequenceCoderQ | SequenceT[_LengthVar, _NetEncoder])] := Scope[
	encf = ToEncoderFunction[enc, True];
	result = If[Length[list] > 1000, batchApplyEnc[name, encf, list], encf[list]];
	insertLengths[name, Length /@ result, GetLengthVarID[enc]];
	Preencoded[result]
];

preencodeSequences[name_, list_, t:TensorT[{LengthVar[id_], rest___}, _]] := Scope[
	dims = Rest @ TDimensions @ t; 
	list = Map[checkPacked[name, dims, t, #]&, list];
	insertLengths[name, Length /@ list, id];
	Preencoded[list]
];

insertLengths[name_, lens_, id_] := Scope[
	lastpos = $seqpositions[id];
	If[IntegerQ[lastpos],
		If[lens =!= $seqlens[[lastpos]],
			ThrowFailure["incseqlen", name, $seqnames[id]]
		],
		AppendTo[$seqlens, lens];
		$seqpositions[id] ^= Length[$seqlens];
		$seqnames[id] ^= name;
	];
];

preencodeSequences[name_, list_, t_] := list;

constructGenerator[name_, Preencoded[list_], _] := 
	name -> ListGenerator[list, $batchsize, $perm];

constructGenerator[name_, list_, EncoderP["Scalar"] | ScalarT] := 
	name -> PackedArrayGenerator[checkPacked[name, {}, ScalarT, list], $batchsize, $perm];

constructGenerator[name_, list_, type_] := Scope[
	encf = ToEncoderFunction[type, True];
	name -> If[encf === Identity, 
		PackedArrayGenerator[checkPacked[name, TDimensions[type], type, list], $batchsize, $perm],
		toEncodedGenerator[type, name, list, encf]
	]
];

(* we must blacklist scalar inputs because they don't play nice with
pre-encoded generators, as the final '1' dimension can't be elided in
that case.
we must also blacklist sequences (i.e. of encoders), as the windowed
NDArray needs to be transposed into the target NDArray, and that isn't
hooked up (yet) *)
toEncodedGenerator[HoldPattern[NetEncoder][_, _, ScalarT] | ScalarT | VarSequenceP[], _, list_, encf_] :=
	EncodedGenerator[list, encf, $batchsize, $perm];

PackageScope["$doPreEncoding"]
$doPreEncoding = True;

toEncodedGenerator[t_, name_, list_, encf_] /; $doPreEncoding := Scope[
	$precompName ^= name;
	res = PrecomputedEncodedGenerator[list, encf, $batchsize, $perm, $precomputationSettings];
	If[FailureQ[res] || res === $Aborted[], Abort[]];
	res
]

toEncodedGenerator[t_, name_, list_, encf_] := 
	EncodedGenerator[list, encf, $batchsize, $perm];

batchApplyEnc[name_, f_, list_] := Scope[
	bag = Bag[];
	len = Length[list];
	$precompName ^= name; precompStart[]; 
	Do[
		$precompProgress = N[n / len]; 
		StuffBag[bag, f @ Take[list, {n, UpTo[n+511]}], 1],
		{n, 1, len, 512}
	];
	precompEnd[];
	BagPart[bag, All]
];

$precomputationSettings = Association[
	"MaxMemory" -> $SystemMemory / 8, 
	"MaxTime" -> 60.0,
	"MinCallbackTime" -> 0.2, 
	"PrecomputeStart" -> precompStart, 
	"PrecomputeTick" -> precompTick, 
	"PrecomputeEnd" -> precompEnd
];

If[$Notebooks,
precompStart[] := Set[$precompCell, PrintTemporary[
	Style[Row[{"Preparing data for port \"", $precompName, "\":  ", 
		$precompProgress = 0.;
		Dynamic[ProgressIndicator[$precompProgress], UpdateInterval -> 0.1, TrackedSymbols :> {}]
	}], FontFamily -> "Verdana"]
]];
precompEnd[] := (NotebookDelete[$precompCell]; $precompCell = None; $precompName = None);
precompTick[p_] := Set[$precompProgress, p];
];

	
(* PAQ below should only be for Real PAs *)
checkPacked[name_, dims_, type_, list_] := Scope[
	If[Or[
		!PackedArrayQ[list] && !PackedArrayQ[list = ToPackedArray@N[list]],
		Rest[Dimensions[list]] =!= dims], 
		throwInvalidInDim[name, type];
	];
	list
];

(* more exotic group generators below *)

PackageScope["CustomGenerator"]
constructGroupGenerator[CustomGenerator[f_, info_], types_, batchsize_, _, _] := Scope[
	validator = ToBatchTypeTest[types, batchsize];
	encoders = Map[ToEncoderFunction[#, True]&, types];
	If[MatchQ[Values[encoders], {Identity..}], encoders = None];
	ToCustomGeneratorFunction[f, info, validator, batchsize, encoders]
];

NetTrain::invgenunsp = "Generator function did not produce data of the correct type.";

ToCustomGeneratorFunction[f_, info_, validator_, batchsize_, encoders_] := Function @ Block[{res},
	checkValid[validator, res = f[info]];
	MapIndexed[encoders[[First @ #2]][#1]&, res]
];

ToCustomGeneratorFunction[f_, info_, validator_, batchsize_, None] := Function @ Block[{res},
	checkValid[validator, res = f[info]];
	res
];


PackageScope["$LastInvalidGeneratorOutput"]
PackageScope["$LastGeneratorOutputValidator"]

$LastInvalidGeneratorOutput = None;
$LastGeneratorOutputValidator = None;

checkValid[validator_, res_] :=
	If[!validator[res], 
		$LastInvalidGeneratorOutput = res;
		$LastGeneratorOutputValidator = validator;
		ThrowFailure["invgenunsp"]
	];

constructGroupGenerator[HDF5TrainingData[_, len_, names_, colInfo_], types_, batchsize_, _, _] := Scope[
	If[ContainsVarSequenceQ[types], 
		ThrowFailure["novseqgen"];
	];
	If[Sort[names] =!= Sort[Keys[types]], 
		NetTrain::invtrainkeys = "Training data should consist of keys ``.";
		ThrowFailure["invtrainkeys", Keys[types]];
	];
	$batchsize = batchsize; $length = len;
	readers = MapThread[makeHDF5Reader, {names, colInfo, Lookup[types, names]}];
	CreateFunction[
		Hold[AssociationThread][names, readers]
	]
];

innerType[enc_NetEncoder] := CoderType[enc];
innerType[t_] := t;

makeHDF5Reader[name_, {ds_, fs_, Hold[ms_], dims_}, type_] := Scope[
	tdims = TDimensions[innerType @ type];
	NetTrain::invh5net = "Net is not suitable for training on H5 data.";
	If[FailureQ[tdims], ThrowFailure["invh5net"]];
	If[tdims =!= dims, throwInvalidInDim[name, type]];
	rank = Length[dims] + 1;
	batchDims = Prepend[$batchsize] @ dims;
	ms = HDF5Tools`h5screatesimplen[rank, batchDims];
	offset = ConstantArray[0, rank]; 
	offset[[1]] = Hold[Min][(Slot[1]-1) * $batchsize, $length - $batchsize];
	Hold[PreemptProtect] @ Hold[CompoundExpression][
		Hold[HDF5Tools`h5sselecthyperslab][fs, HDF5Tools`H5SSELECTSET, offset, {}, batchDims, {}],
		Hold[HDF5Tools`h5dreadtensorreal][ds, ms, fs, HDF5Tools`H5PDEFAULT]
	]
]

(* also used by evaluator, so needs to be General *)
General::invindim = "Data provided to port \"``\" should be ``.";
throwInvalidInDim[name_, type_] := ThrowFailure["invindim", name, TypeString @ ListT[SizeT, type]];


(******************************************************************************)
PackageScope["BucketedGroupGenerator"]

(* TODO: maybe move this into MXNetLink? *)

NNSetUsage @ "
BucketedGroupGenerator[<|key$1->gen$1,$$|>,bucketkeys$] yields a generator that yields batches from \
several sub-generators, returning them as Bucket[<|key$1->data$1,$$|>,bucketkey$].
* calling generator$[n$] uses the n$th key from bucketkeys$.
"

BucketedGroupGenerator[<|key1_ -> gen1_, key2_ -> gen2_|>, ibuckets_] := Module[
	{buckets = ibuckets},
	Function[Bucket[AssociationThread[{key1, key2}, {gen1[#], gen2[#]}], buckets[[#]]]]
];

BucketedGroupGenerator[assoc_Association, ibuckets_] := Module[
	{buckets = ibuckets},
	Function[Bucket[ApplyThrough[assoc, #], buckets[[#]]]]
];


PackageScope["ConstructBucketedGeneratorFactory"]

ConstructBucketedGeneratorFactory[inputs_, batchsize_] := Scope[
	{names, types} = KeysValues[inputs];
	withLengthCaptors[names,
		gencode = MapThread[makeGen, {Range[Length[names]], names, types}]
	];
	body = Hold[
		TempVar[buckets] = Eval[Keys[gencode]];
		{TempVar[n], TempVar[excess]} = BatchCountExcess[Length[#1], $batchsize];
		{TempVar[$perm], buckets} ^= MakeBucketedBatchPermutation[buckets, $batchsize];
		buckets ^= RoundBucket @ buckets;
		List[
			BucketedGroupGenerator[
				AssociationThread[Eval @ names, Eval @ Values[gencode]], 
				buckets
			], 
			Extract[buckets, Ordering[buckets, -1]],
			Ordering[Drop[$perm, excess]] + excess,
			n
		]
	];
	ninputs = Length[inputs];
	If[ninputs > 1, PrependTo[body, makeBatchRaggedChecker[ninputs]]];
	CreateFunction[body] /. $batchsize -> batchsize
]

Clear[makeGen];

(* i'm sure there are some common cases here that i've already figured out for
the non batched case *)

makeGen[i_, name_, enc_NetEncoder ? SequenceCoderQ] := Module[
	{encf = ToEncoderFunction[enc, True], id = GetLengthVarID[enc]},
	Rule[
		makeLengthCaptor[encf, id, i, True],
		Hold @ ListGenerator[TempVar[i], $batchsize, $perm]
	]
];

makeGen[i_, name_, t:VarSequenceP[id_]] := 
	Rule[
		makeLengthCaptor[Map[checkPacked[name, Eval[Rest @ TDimensions[t]], t, #]&], id, i, True],
		Hold @ ListGenerator[TempVar[i], $batchsize, $perm]
	];

makeGen[i_, name_, SequenceT[LengthVar[id_], type_]] := Module[
	{encf = ToEncoderFunction[type, True]},
	Rule[
		makeLengthCaptor[Map[encf], id, i, True],
		Hold @ ListGenerator[TempVar[i], $batchsize, $perm]
	]
];

makeGen[i_, name_, EncoderP["Scalar"] | ScalarT] := Rule[
	Nothing, 
	Hold @ PackedArrayGenerator[checkPacked[name, {}, ScalarT, Slot[i]], $batchsize, $perm]
];

makeGen[i_, name_, type_] := Module[
	{encf = ToEncoderFunction[type, True]},
	Rule[
		Nothing,
		If[encf === Identity, 
			Hold @ PackedArrayGenerator[
				checkPacked[name, Eval[TDimensions[type]], type, Slot[i]], 
				$batchsize, $perm
			],
			Hold @ EncodedGenerator[Slot[i], encf, $batchsize, $perm]
		]
	]
];


