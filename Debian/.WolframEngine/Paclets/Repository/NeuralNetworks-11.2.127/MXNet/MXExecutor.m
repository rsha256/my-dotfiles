Package["NeuralNetworks`"]



PackageScope["ToMXExecutor"]

NNSetUsage @ "
ToMXExecutor[MXPlan[$$], batchsize$, opts$$] takes a plan and instantiates it on a device, returning an MXExecutorData[$$] object.
The following options are supported: 
| 'Context' | 1 | the device code to use |
| 'ArrayCaching' | False | whether to use ToCachedNDArray to cache RawArrays per-context |
| 'Gradients' | False | whether to attach gradients |
| 'SharedExecutor' | None | an existing executor to share memory with |
* 'Gradients' can be an assocation of existing NDArrays, where the keys are mangled names.
* 'Gradients' can also be a list of mangled names, and GradientArrays will be created for only these.
* 'ArrayCaching' can be Hold[sym$] where sym$ is an association, and NDArrays will be cached
within the association. This is useful for training where buckets in MXTrainer need to share 
NDArrays per-NetPath, but not per-RawArray.
"

(* TODO: need to add ability to pass an existing MXExecutor as the Gradients, in which case we 
inherit the gradients from that executor *)

Options[ToMXExecutor] = {
	"Context" -> 1,
	"ArrayCaching" -> False,
	"Gradients" -> False,
	"SharedExecutor" -> None
};

ToMXExecutor[MXPlan[assoc_], batchSize_, OptionsPattern[]] := Timed @ Scope[
	UnpackOptions[context, arrayCaching, gradients, sharedExecutor];
	info = KeyTake[assoc, {"InputArrays", "OutputArrays", "SpecialArrays", "FixedArrays", "AuxilliaryArrays"}];
	(* ^ from this list, only Fixed and Aux arrays will contain actual RawArrays, the rest
	are just dimension lists *)
	argArrays = assoc["ArgumentArrays"];
	If[arrayCaching =!= False,
		$DefaultContext = context;
		cachef = arrCacheFunction[arrayCaching];
		ComposeTo[info["FixedArrays"], cachef];
		ComposeTo[info["AuxilliaryArrays"], cachef];
		ComposeTo[argArrays, cachef];
	];
	info = Normal[info];
	{gradMethod, grads} = Switch[gradients,
		_Association,  {Automatic, gradients},
		{___String},   {gradients, None},
		True,          {"Write", None},
		False,         {None,    None}
	];
	executor = MXSymbolBind[
		assoc["Symbol"], 
		argArrays, 
		Sequence @@ ReplaceAll[info, BatchSize -> batchSize],
		"GradientArrays" -> grads,
		"GradientUpdateMethod" -> gradMethod,
		"SharedExecutor" -> sharedExecutor,
		"Context" -> context
	];
	(* TODO: switch to more generic mxerr message *)
	If[FailureQ[executor], ThrowFailure["badnnexec", executor]];
	executor
];

arrCacheFunction[True] = ReplaceAll[ra_RawArray :> RuleCondition @ ToCachedNDArray[ra, context]];
arrCacheFunction[Hold[sym_]] := MapIndexed[cacheArrTo[sym]];

SetHoldFirst[cacheArrTo];
cacheArrTo[sym_][ra_, {Key[path_]}] := Lookup[sym, path, sym[path] = NDArrayCreate[ra]];

General::badnnexec = "Could not set up an execution context for given network. The error was: ``. Please contact Wolfram Research.";


PackageScope["ToBucketedMXExecutor"]

NNSetUsage @ "
ToBucketedMXExecutor[net$, {outputs$, context$, batchspec$, tmode$, gradients$}] 
The second args should be as follows:
| outputs$ | which outputs to return |
| context$ | the device to use |
| batchspec$ | whether batched or not (or a specific integer) |
| tmode$ | whether to apply dropout, etc |
| gradients$ | whether to attach gradients |
* The batchsize is currently fixed at 32 until we can figure out a better way to pick one.
* gradients$ can be False, True, or a list of mangled names for which grad should be created. 
"

General::netnullseq = "All sequences provided to net must have non-zero length."

PackageScope["$BucketRoundingExponent"]

NNSetUsage @ "
$BucketRoundingExponent controls how the bucket size is rounded, and should be a number > 1. 
* Larger exponents will produce fewer buckets but more sequence wastage.
"

$BucketRoundingExponent = 1.5;

PackageScope["RoundBucket"]
SetAttributes[RoundBucket, Listable];
RoundBucket[0] := ThrowFailure["netnullseq"];
RoundBucket[n_] := If[n <= 4, n, Floor @ Power[$BucketRoundingExponent, Ceiling @ Log[$BucketRoundingExponent, n]]];

(* for debugging *)
ToBucketedMXExecutor[net_] := ToBucketedMXExecutor[net, {All, {"CPU", 0}, False, False, False}];

ToBucketedMXExecutor[net_NetP, spec_] :=
	Cached[iToBucketedMXExecutor, net, spec];

PackageScope["$DefaultSequenceBatchSize"]
$DefaultSequenceBatchSize = 32;

(* complicated to pick automatically, because it ultimately depends on the size of the maximum
sequence we expect to come along. and we also suffer a major penalty from bigger batch sizes 
because it makes each batch more likely to be ragged when we have a large variance in 
sequence lengths *)

iToBucketedMXExecutor[net_, {outputs_, context_, batchq_, tmode_, gradients_}] := ModuleScope[
	seqIDs = MakeSeqIDLens[net, 0];
	arrayCache = Data`UnorderedAssociation[];
	info = Association[
		"Net" -> net,
		"BatchSize" -> Which[IntegerQ[batchq], batchq, TrueQ[batchq], $DefaultSequenceBatchSize, True, 1],
		"Outputs" -> outputs,
		"Context" -> context,
		"Gradients" -> gradients,
		"TMode" -> tmode,
		"SequenceIDs" -> Keys[seqIDs],
		"ArrayCache" -> Hold[arrayCache]
	];
	maxSize = Values[seqIDs];
	buckets = Data`UnorderedAssociation[];
	BucketedMXExecutor[{buckets, maxSize}, info]
];

createBucket[master_, info_, bucket_, realloc_] := Timed @ Scope[
	UnpackAssociation[info, net, outputs, batchSize, context, sequenceIDs, tMode, gradients];
	plan = ToMXPlan[net, {outputs, AssociationThread[sequenceIDs, bucket], tMode}];
	If[master =!= None && gradients =!= None, gradients = master["GradientArrays"]];
	(* ^ in training mode, pick up grads from the master, or if we're the first master, create from scratch *)
	ToMXExecutor[
		plan, batchSize, 
		"Gradients" -> gradients, "Context" -> context, 
		"SharedExecutor" -> If[realloc, None, master], 
		"ArrayCaching" -> info["ArrayCache"]
	]
];

(* TODOS:

we need a flag that tells us when we don't need to make a new plan. e.g. when
we only have things like SequenceLast or GatedRecurrentLayer etc. in that
case we can take the master executor and merely resize it down.
*)

(* TODO: maybe move this into MXNetLink? *)

PackageScope["BucketedMXExecutor"]
PackageScope["Bucket"]

SetHoldFirst[BucketedMXExecutor]

BucketedMXExecutor[_, info_][key_] := info[key];

MXExecutorMemoryInformation[BucketedMXExecutor[{buckets_, maxbucket_}, info_]] :=
	MXExecutorMemoryInformation[buckets[maxbucket]];

PackageExport["GetBucketExecutor"]

GetBucketExecutor[BucketedMXExecutor[{buckets_, maxbucket_}, info_], inbucket_] := Block[
	{bucket = RoundBucket[inbucket]},
	If[Order[bucket, maxbucket] == -1, 
		(* max exceeded, clear previous buckets and allocate a new master *)
		master = Lookup[buckets, Key[maxbucket], None];
		newmaster = createBucket[master, info, bucket, True];
		buckets = Data`UnorderedAssociation[bucket -> newmaster];
		maxbucket = bucket;
		newmaster
	,
		Lookup[
			buckets, Key @ bucket, 
			buckets[bucket] = createBucket[buckets[maxbucket], info, bucket, False]
		]
	]
]

DefineCustomBoxes[BucketedMXExecutor,
	t:BucketedMXExecutor[_, _Association] :> BucketedMXExecutorBoxes[t]
];

BucketedMXExecutorBoxes[exec:BucketedMXExecutor[{buckets_, maxsize_}, info_Association]] := Scope[
	BoxForm`ArrangeSummaryBox[
		BucketedMXExecutor,
		exec,
		None,
		{makeItem["TMode", info["TMode"]],
		 makeItem["Max size", Dynamic @ maxsize],
		 makeItem["Bucket count", Dynamic @ Length[buckets]]},
		{makeItem["Buckets", Dynamic @ Sort[Keys[buckets]]]},
		StandardForm
	]
];

makeItem[name_, value_] := BoxForm`MakeSummaryItem[{Pane[name <> ": ", {90, Automatic}], value}, StandardForm];

