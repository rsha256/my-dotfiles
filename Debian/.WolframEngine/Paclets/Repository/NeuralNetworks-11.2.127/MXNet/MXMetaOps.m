Package["NeuralNetworks`"]



(* This is where we put higher-level operations that are polymorphic over
node manifestation, e.g. that want to operate on MetaNodes *)

PackageScope["MetaNode"]

NNSetUsage @ "
MetaNode[batchwise$,timewise$,unpacked$,lnode$,maxlen$,dims$] represents a logical node in a MX graph, \
and can contain multiple physical manifestations. 
| batchwise$ | MXNode[$$] | physical and logical interpretation are the same |
| timewise$ | MXNode[$$] | physical and logical are 0-1 transposed |
| unpacked$ | {node$1, node$2, $$} | first (non-batch) logical dimension has been unpacked |
| lnode$ | MXNode[$$] | vector of dynamic dim lengths |
| maxlen$ | integer$ | max dynamic dim length |
| dims$ | {d$1,d$2,$$} | dimensions (excluding length) |
* The first 3 arguments are closure variables and so can be created and cached when a certain manifestation is demanded from layer code. 
* The last 2 arguments will be None if no dynamic dim is present.
"

PackageScope["VarMetaQ"]
PackageScope["FixedMetaQ"]

VarMetaQ[MetaNode[_, _, _, lnode_, _, _]] := (lnode =!= None);
VarMetaQ[_] := $Unreachable;

FixedMetaQ[MetaNode[_, _, _, lnode_, _, _]] := (lnode === None);
FixedMetaQ[_] := $Unreachable;


PackageScope["NewMetaNode"]

MetaNode /: MakeBoxes[MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_], StandardForm] := 
	ToBoxes[HoldForm[MetaNode][batchwise, timewise, unpacked, lnode, maxlen, dims]];

SetAttributes[SetNone, HoldAll];
SetNone[lhs_, rhs_] := If[lhs =!= None, lhs, lhs = rhs];

SetAttributes[MetaNode, HoldAll];

MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]["Batchwise"] :=
	SetNone[batchwise, 
		If[timewise =!= None, SowTranspose01[timewise],
			SowPack[unpacked, False, dims]]];

MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]["Timewise"] :=
	SetNone[timewise, 
		If[batchwise =!= None, SowTranspose01[batchwise],
			SowPack[unpacked, True, dims]]];

MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]["Packed"] :=
	If[batchwise =!= None, batchwise,
		SetNone[timewise, SowPack[unpacked, True, dims]]];

MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]["Unpacked"] := 
	SetNone[unpacked, 
		If[timewise =!= None,
			SowUnpack[timewise, maxlen, 0],
			SowUnpack[batchwise, maxlen, 1]
		]
	]

MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]["LengthNode"] := lnode;
MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]["MaxLength"] := maxlen;
MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]["Dimensions"] := dims;

_MXNode[_] := $Unreachable;


PackageScope["NewSequenceMetaNode"]

NewSequenceMetaNode[MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, _], dims_List] := 
	NewSequenceMetaNode[lnode, maxlen, dims];

NewSequenceMetaNode[lnode_, maxlen_Integer, dims_List] := ModuleScope[
	batchwise = None; timewise = None; unpacked = ConstantArray[Null, maxlen];  
	MetaNode[batchwise, timewise, unpacked, lnode, maxlen, dims]
];

NewSequenceMetaNode[___] := $Unreachable;


PackageScope["SowMetaMap"]

SowMetaMap[f_, mn:MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, idims_], dims_, batching_:Automatic] := ModuleScope[
	batchwise2 = timewise2 = unpacked2 = None;
	Which[
		batching === False,	unpacked2 = Map[f[#, 1]&, mn["Unpacked"]],
		batching === True && batchwise === None && (mn["Timewise"]; False), Null,  
		batchwise =!= None,	batchwise2 = sowFlatMap[f, batchwise, maxlen, False],
		timewise =!= None,	timewise2 = sowFlatMap[f, timewise, maxlen, True],
		unpacked =!= None,	unpacked2 = Map[f[#, 1]&, unpacked]
	];
	MetaNode[batchwise2, timewise2, unpacked2, lnode, maxlen, dims]
];

SowMetaMap[___] := $Unreachable;


PackageScope["$FlattenedNodeCache"]
sowFlatMap[f_, mxnode_, maxlen_, twise_] := Scope[
	flat = CacheTo[$FlattenedNodeCache, mxnode, SowNode["Reshape", mxnode, "shape" -> {-3, -2}]];
	flat = checkmxn @ f[flat, maxlen];
	SowNode["Reshape", flat, "shape" -> If[twise, {-4, maxlen, -1, -2}, {-4, -1, maxlen, -2}]]
];
sowFlatMap[___] := $Unreachable;

SetHoldAll[checkmxn];
checkmxn[body_] := 
	Replace[body, {
		mn_MetaNode :> mn["Batchwise"], 
		e:Except[_MXNode] :> Panic["NotMXNode", "Got `` instead of an MXNode from ``.", e, HoldForm[body]]
	}];


PackageScope["SowMetaMean"]

SowMetaMean[mn:MetaNode[batchwise_, timewise_, unpacked_, None, maxlen_, dims_]] := 
	SowNode["_DivScalar",
		Which[
			batchwise =!= None, SowSumAxis[batchwise, 1],
			timewise =!= None,  SowSumAxis[timewise, 0],
			True, SowNode["ElementWiseSum", unpacked]
		],
		"scalar" -> maxlen
	];

SowMetaMean[mn_MetaNode] := 
	SowSumAxis[SowSeqMask[mn["Timewise"], mn["LengthNode"]], 0];

SowMetaMean[_] := $Unreachable;



PackageScope["SowMetaLast"]

SowMetaLast[mn:MetaNode[batchwise_, timewise_, unpacked_, None, maxlen_, dims_]] := Which[
	unpacked =!= None, Last[unpacked],
	timewise =!= None, unflatRank[dims] @ SowNode["SequenceLast", flatRank[rank] @ timewise],
	batchwise =!= None, 
		SowReshape[
			SowNode["slice_axis", batchwise, "axis" -> 1, "begin" -> maxlen-1, "end" -> maxlen],
			dims
		]
];

SowMetaLast[mn_MetaNode] := Scope[
	dims = Last[mn];
	unflatRank[dims] @ SowNode[
		"SequenceLast", 
		{flatRank[dims] @ mn["Timewise"], mn["LengthNode"]},
		"use_sequence_length" -> "true"
	]
];

flatRank[{}] := SowInsertDim[#, 2]&;
flatRank[_] := Identity;

unflatRank[{}] := SowUReshape[#, 0]&;
unflatRank[_] := Identity;

PackageScope["SowMetaReverse"]

(* TODO: can we use backwardsliceaxis to do a reverse on batchwise? *)

SowMetaReverse[mn:MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_]] := ModuleScope[
	batchwise2 = timewise2 = unpacked2 = None;
	reverser = If[Length[dims] === 0, RankFixed[sowReverse], sowReverse];
	If[lnode === None,
		Which[
			unpacked =!= None, unpacked2 = Reverse[unpacked],
			timewise =!= None, timewise2 = reverser[timewise],
			batchwise =!= None, timewise2 = reverser[timewise = SowTranspose01[batchwise]]
		],
		timewise2 = reverser[mn["Timewise"], lnode];
	];
	MetaNode[batchwise2, timewise2, unpacked2, lnode, maxlen, dims]
];

sowReverse[in_] := SowNode["SequenceReverse", in, "use_sequence_length" -> "false"];
sowReverse[in_, len_] := SowNode["SequenceReverse", {in, len}, "use_sequence_length" -> "true"];


PackageScope["SowMetaDrop"]

SowMetaDrop[MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_], nout_Integer, leftq_] :=
	Which[
		maxlen == 1,        ThrowFailure["netseqlen"], 
		timewise =!= None,  SowTake[timewise,  If[leftq, 1, 0] + {0, nout}, 0],
		batchwise =!= None, SowTake[batchwise, If[leftq, 1, 0] + {0, nout}, 1],
		unpacked =!= None,  newUnpackedNode[If[leftq, Rest, Most] @ unpacked, None, nout, dims]
	]

newUnpackedNode[unpacked_, lnode_, maxlen_, dims_] := ModuleScope[
	batchwise = timewise = None; 
	MetaNode[batchwise, timewise, unpacked, lnode, maxlen, dims]
];

SowMetaDrop[mn:MetaNode[batchwise_, timewise_, unpacked_, lnode_, maxlen_, dims_], nout_LengthVar, leftq_] := ModuleScope[
	lnode2 = SowDerivedSequenceLengthNode[lnode, nout, -1];
	nout = maxlen-1;
	batchwise2 = timewise2 = unpacked2 = None;
	Which[
		maxlen == 1,        ThrowFailure["netseqlen"], 
		batchwise =!= None,	batchwise2 = SowTake[batchwise,  If[leftq, 1, 0] + {0,nout}, 1],
		timewise =!= None,	timewise2 = SowTake[timewise, If[leftq, 1, 0] + {0,nout}, 0],
		unpacked =!= None,	timewise2 = SowPack[If[leftq, Rest, Most] @ unpacked, True, dims]
	];
	With[{lnode2 = lnode2, maxlen2 = maxlen - 1},
		MetaNode[batchwise2, timewise2, unpacked2, lnode2, maxlen2, dims]
	]
];

SowMetaDrop[_, _, _, _] := $Unreachable;


(* so that Timewise will cause a transpose to happen -- is that what we want? 
well, we only need that because if there *is* a dynamic dimension, we'll
need to null it out before suming, and it has to be collapsed.
*)

PackageScope["MakeTimewiseMeanData"]

MakeTimewiseMeanData[inode_MetaNode ? FixedMetaQ, tnode_MetaNode] := Scope[
	{inode["Batchwise"], tnode["Batchwise"], Identity}
];

MakeTimewiseMeanData[inode_MetaNode, tnode_MetaNode] := ModuleScope[
	n = inode["MaxLength"]; lnode = inode["LengthNode"];
	inode = SowFlatten1[inode["Timewise"]];
	tnode = SowFlatten1[tnode["Timewise"]];
	{inode, tnode, SowTimewiseMean[SowUnflatten1[#, n], lnode, 1]&}
];


Language`SetMutationHandler[MetaNode, MetaNodeMutate]

SetHoldAllComplete[MetaNodeMutate];

MetaNodeMutate[Set[sym_[[part_]], mx_]] := SetMetaNodePart[sym, part, mx];

MetaNode /: (mn_MetaNode)[[part_]] := GetMetaNodePart[mn, part];
MetaNode /: Length[mn_MetaNode] := mn["MaxLength"];

SetMetaNodePart[MetaNode[_, _, unpacked_, _, _, _], part_, value_] := 
	unpacked[[part]] = value;

GetMetaNodePart[mn:MetaNode[_, _, unpacked_, _, _, _], part_] :=
	(If[unpacked === None, mn["Unpacked"]]; unpacked[[part]])


PackageScope["L12Loss"]

L12Loss[in_, target_, rank_, type_] := Scope[
	diff = SowMinus[in, target];
	mag = SowNode[If[type === "L1", "abs", "square"], diff];
	SowMean[mag, rank]
];


PackageScope["MeanLossImplementation"]

MeanLossImplementation[type_] := Scope[
	dims = GetInputDims["Input"]; len = First[dims, 0]; rank = Length[dims];
	If[IntegerQ[len], 
		loss = L12Loss[GetInput["Input"], GetInput["Target"], rank, type];
		,
		{input, target, postf} = MakeTimewiseMeanData[GetInputMetaNode["Input"], GetInputMetaNode["Target"]];
		loss = L12Loss[input, target, rank-1, type];
		loss = postf @ loss;
	];
	SetOutput["Loss", loss];
]