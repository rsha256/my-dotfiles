Package["NeuralNetworks`"]



PackageScope["SowMinus"]
SowMinus[a_] := SowNode["_RMinusScalar", a, "scalar" -> "0.0"];

PackageScope["SowOneMinus"]
SowOneMinus[a_] := SowNode["_RMinusScalar", a, "scalar" -> "1.0"];

PackageScope["SowLog"]
SowLog[a_] := SowNode["log", a];

PackageScope["SowPlusEps"]
SowPlusEps[a_] := SowNode["_PlusScalar", a, "scalar" -> "0.2220446049250313e-15"];

PackageScope["SowMix"]
SowMix[a_, b_, scale_] := SowPlus[SowHad[a, SowOneMinus[scale]], SowHad[b, scale]];

PackageScope["SowPlusScalar"]
SowPlusScalar[a_, n_] := SowNode["_PlusScalar", a, "scalar" -> N[n]];

PackageScope["SowTimesScalar"]
SowTimesScalar[a_, n_] := SowNode["_MulScalar", a, "scalar" -> N[n]];

PackageScope["SowDivideScalar"]
SowDivideScalar[a_, n_] := SowNode["_DivScalar", a, "scalar" -> N[n]];

PackageScope["SowDivide"]
SowDivide[a_, b_] := SowNode["_Div", {a, b}];

PackageScope["SowHad"]
SowHad[a_, b_] := SowNode["_Mul", {a, b}];

PackageScope["SowBHad"]
SowBHad[a_, b_] := SowNode["broadcast_mul", {a, b}];

PackageScope["SowDot"]
SowDot[x_, w_] := SowNode["FullyConnected", {x, w}, "num_hidden" -> getLen[w], "no_bias" -> True];

PackageScope["SowFC"]
SowFC[x_, w_, b_] := SowNode["FullyConnected", {x, w, b}, "num_hidden" -> getLen[w]];

getLen[id_] := First @ NodeDimensions[id];

PackageScope["SowTanh"]
SowTanh[a_] := SowNode["tanh", a];

PackageScope["SowHardTanh"]
SowHardTanh[a_] := SowPlusScalar[SowNode["_Minus", {SowRamp @ SowPlusScalar[a, "1.0"], SowRamp @ SowPlusScalar[a, "-1.0"]}], "-1.0"];

PackageScope["SowSigmoid"]
SowSigmoid[a_] := SowNode["sigmoid", a];

PackageScope["SowRamp"]
SowRamp[a_] := SowNode["relu", a];

PackageScope["SowHardSigmoid"]
SowHardSigmoid[a_] := SowTimesScalar[SowNode["_Minus", {SowRamp @ SowPlusScalar[a, "1.0"], SowRamp @ SowPlusScalar[a, "-1.0"]}], "0.5"];

PackageScope["SowPlus"]
SowPlus[a_, b_] := SowNode["_Plus", {a, b}];
SowPlus[a_, b_, c_] := SowNode["ElementWiseSum", {a, b, c}, "num_args" -> 3];

PackageScope["SowBPlus"]
SowBPlus[a_, b_] := SowNode["broadcast_add", {a, b}];

PackageScope["SowMinus"]
SowMinus[a_, b_] := SowNode["_Minus", {a, b}];

PackageScope["SowSum"]
SowSum[a_, 0] := a;
SowSum[a_, rank_] := SowNode["sum", a, "axis" -> Range[rank], "keepdims" -> "false"];

PackageScope["SowSquare"]
SowSquare[a_] := SowNode["square", a];

PackageScope["SowSqrt"]
SowSqrt[a_] := SowNode["sqrt", a];

PackageScope["SowRSqrt"]
SowRSqrt[a_] := SowNode["rsqrt", a];

PackageScope["SowL2Normalize"]
SowL2Normalize[a_] := SowNode["L2Normalization", a];

PackageScope["SowCosineDistance"]
SowCosineDistance[a_, b_, rank_] :=
	SowOneMinus @ SowSum[SowHad[SowL2Normalize[a], SowL2Normalize[b]], rank];

PackageScope["SowEuclideanDistance"]
SowEuclideanDistance[a_, b_, rank_] := 
	SowSum[SowSquare[SowMinus[a, b]], rank];

PackageScope["SowMarginLoss"]
SowMarginLoss[a_, margin_] := SowNode["relu", SowNode["_RMinusScalar", a, "scalar" -> margin]];

PackageScope["SowReshape"]
SowReshape[in_, dims___] := SowNode["Reshape", in, "shape" -> Flatten[{-1, dims}]];

PackageScope["SowUReshape"]
SowUReshape[in_, dims___] := SowNode["Reshape", in, "shape" -> Flatten[{dims}]];

PackageScope["SowTranspose"]
SowTranspose[in_, axes_:{0,2,1}] := SowNode["transpose", in, "axes" -> axes];

PackageScope["SowTransposeLast"]
SowTransposeLast[in_, 1] := in;
SowTransposeLast[in_, rank_] := SowTranspose[in, Join[{0, rank}, Range[rank-1]]];

PackageScope["SowTranspose01"]
SowTranspose01[in_] := SowNode["SwapAxis", in, "dim1" -> "0", "dim2" -> "1"];

PackageScope["SowSwapAxis"]
SowSwapAxis[in_, d1_, d2_] := If[d1 === d2, in, SowNode["SwapAxis", in, "dim1" -> d1, "dim2" -> d2]];

PackageScope["SowSumAxis"]
SowSumAxis[a_, axes_] := SowNode["sum", a, "axis" -> axes, "keepdims" -> "false"];

PackageScope["SowFlatten1"]
SowFlatten1[in_] := SowNode["Reshape", in, "shape" -> {-3,-2}];

PackageScope["SowUnflatten1"]
SowUnflatten1[in_, n_, twise_:True] := 
	SowNode["Reshape", in, "shape" -> If[twise, {-4, n, -1, -2}, {-4, -1, n, -2}]];

PackageScope["SowPack"]

SowPack[elems_List, timewise_, dims_] := Scope[
	len = Length[elems]; 
	(* %MX stupidity *)
	If[dims === {}, elems = Map[SowInsertDim[#, 1]&, elems]];
	shape = Join[If[timewise, {len, -1}, {-1, len}], dims];
	cat = SowNode["Concat", elems, "num_args" -> len, "dim" -> If[timewise, 0, 1]];
	SowNode["Reshape", cat, "shape" -> writeIntList[shape]]
];

SowPack[__] := $Unreachable;

PackageScope["SowUnpack"]
SowUnpack[node_, numOut_, axis_] := Scope[
	out = SowNode["SliceChannel", node, "axis" -> axis, "num_outputs" -> numOut, "squeeze_axis" -> "true"];
	out[[2]] = Range[numOut]-1;
	Thread[out]
]

PackageScope["SowMean"]
SowMean[node_, rank_] := SowNode["mean", node, "axis" -> Range[rank]];
SowMean[node_, 0] := node;

PackageScope["SowTake"]
SowTake[node_, {a_, b_}, axis_] := SowNode["slice_axis", node, "axis" -> axis, "begin" -> a, "end" -> b];

PackageScope["SowSeqMask"]
SowSeqMask[twise_, lnode_, mask_:"0."] := SowNode["SequenceMask", {twise, lnode}, "use_sequence_length" -> "true", "value" -> mask];

PackageScope["RankFixed"]
(* this is to fix that sequence layers insist on rank > 2 for no good reason *)
RankFixed[f_][e_, rest___] := SowRemoveDummy[f[SowInsertDummy[e], rest]];
SowInsertDummy[node_] := SowInsertDim[node, "2"];
SowRemoveDummy[node_] := SowUReshape[node, 0, 0];

PackageScope["SowInsertDim"]
SowInsertDim[in_, axis_] := SowNode["expand_dims", in, "axis" -> axis];

PackageScope["FCGate"]
FCGate[dropfn_, weight_, bias_] := Function[fcin, SowFC[dropfn[fcin], weight, bias]];

PackageScope["SowTimewiseMean"]
SowTimewiseMean[in_, lnode_MXNode, _] := SowDivide[SowSumAxis[RankFixed[SowSeqMask][in, lnode], 0], lnode];
SowTimewiseMean[in_, None, maxlen_] := SowDivideScalar[SowSumAxis[in, 0], maxlen];

PackageScope["SowBroadcastAt"]
SowBroadcastAt[in_, axis_, len_] := 
	SowNode[
		"broadcast_axis",
		SowNode["expand_dims", in, "axis" -> axis], 
		"axis" -> axis, "size" -> len
	];

PackageScope["SowBroadcastUp"]
SowBroadcastUp[in_, odim_, idim_] := Scope[
	newrank = Length[odim] - Length[idim]; 
	If[newrank === 0, Return[in]];
	newdims = Take[odim, newrank];
	expanded = SowNode["Reshape", in, "shape" -> Join[{0}, 1+newdims*0, idim]];
	SowNode["broadcast_axis", expanded, "size" -> newdims, "axis" -> Range[newrank]]
];

PackageScope["SowUniformRandom"]

SowUniformRandom[shape_List] := 
	SowSourceFixup[SowNode["uniform", {}, "low" -> "0", "high" -> "1"], shape];
	
(* Debugging ops *)

PackageScope["KillNode"]
PackageScope["ForceNode"]
KillNode[a_] := SowTimesScalar[a, "0.0"];
ForceNode[r_][a_] := SowPlusScalar[SowTimesScalar[a, "0.0"], r];


