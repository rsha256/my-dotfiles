Package["NeuralNetworks`"]



PackageScope["TDimensions"]

NNSetUsage @ "
TDimensions[type$] returns the list of dimensions of a numeric tensor type (TensorT), \
or $Failed if the rank is not fixed or the type is not a tensor type. Note: the list returned \
could contain SizeT. 
Certain pseudo-scalar values like IndexInteger, are treated as scalars (dims {}). 
"

SetHoldRest[TDimensions];

TDimensions = MatchValues[
	TensorT[dims_List, t_] := joinDims[dims, %[t]];
	TensorT[SizeListT[rank_Integer], t_] := joinDims[Table[SizeT, rank], %[t]];
	TTypeP := {};
	Nullable[t_] := %[t];
	TypeAliasP := %[ResolveAlias[type]];
	cod:CoderP := %[CoderType[cod]];
	list_List := Map[%, list];
	$Failed
];

joinDims[a_, b_] := ToList[a, b];
joinDims[_, $Failed] := $Failed;

TDimensions[t_, r_] := ReplaceAll[TDimensions[t], $Failed :> r];


PackageScope["TType"]

NNSetUsage @ "
TType[tensor$] returns the innermost type of the tensor tensor$.
* This is usually a RealT."

TType[TensorT[_ , t_]] := TType[t]
TType[t_] := t


PackageScope["TRank"]

SetHoldRest[TRank];

NNSetUsage @ "
TRank[type$] returns the rank of a numeric tensor type (TensorT), or $Failed if \
this isn't known or the type isn't a tensor type.
"

TRank = MatchValues[
	TensorT[dims_List, t_] := addRank[Length[dims], %[t]];
	TensorT[SizeListT[rank_Integer], t_] := addRank[rank, %[t]];
	TensorT[SizeListT[], t_] := If[$minRankQ, %[t], $Failed];
	TTypeP := 0;
	Nullable[t_] := %[t];
	t:TypeAliasP := %[ResolveAlias[t]];
	cod:CoderP := %[CoderType[cod]];
	$Failed
];

$minRankQ = False;
addRank[n_, $Failed] := $Failed;
addRank[n_, m_] := n + m;

TRank[t_, r_] := Replace[TRank[t], $Failed :> r];


