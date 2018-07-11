Package["NeuralNetworks`"]

failShape[fmt_String, args___] := Return[StringForm[fmt, args] /. sf_StringForm :> TextString[sf], Block];
andList[a_, b_] := StringForm["`` and ``", a, b];
andList[first_, rest__] := StringForm["``, ``", first, andList[rest]];


PackageScope["ValidateShape"]

(* This is because multiple layers share some of these shape functions, so we can't
hardcode the FailValidation call into the body of these functions. Hence we return a string,
and then use ValidateShape inside the ComputedType to call FailValidation with the right symbol
later.
*)

ValidateShape[symbol_, shape_] := ValidateShape[symbol, shape, Identity];
ValidateShape[symbol_, msg_String, f_] := FailValidation[symbol, msg];
ValidateShape[symbol_, shape_, f_] := If[FailureQ[shape], shape, f[shape]];


PackageScope["CatenateShape"]

CatenateShape[dims_, level_] := Scope[
	If[!VectorQ[dims, ListQ], Return[$Failed]];
	ranks = Length /@ dims; 
	{minrank, maxrank} = MinMax[ranks];
	If[level > minrank, failShape["catenation level `` should not exceed ``, the rank of lowest-rank input.", level, minrank]];
	dims = PadLeft[#, maxrank, Inherited]& /@ dims;
	level = level + maxrank - minrank;
	tdims = Transpose[dims];
	odims = MapIndexed[If[#2 === {level}, catmerge1[#], catmerge2[Union[#]]]&, tdims];
	odims
];

catmerge2[{i_Integer, Inherited..., SizeT..., ___LengthVar}] := i;
catmerge2[{Inherited..., SizeT..., lv___LengthVar}] := Last[{lv}, SizeT];
catmerge2[{a:Repeated[_Integer, {2,Infinity}], Shortest[___]}] := failShape["cannot catenate when off-level dimensions `` do not match.", andList[a]];

catmerge1[{i__Integer}] := Plus[i];
catmerge1[_] := SizeT;


PackageScope["PartShape"]

PartShape[{n1_Integer, nrest___}, Span[a_, b_]] := Scope[
	a = deneg[a, n1]; b = deneg[b, n1];
	If[a > b, failShape["negative range ``;;`` not supported.", a, b]];
	c = b - a + 1;
	{c, nrest}
];

PartShape[{n1_Integer, nrest___}, a_Integer] := Block[{},
	deneg[a, n1];
	{nrest}
];

deneg[i_, max_] := Which[
	Abs[i] > max, failShape["the part specification `` cannot reference positions greater than ``.", i, max],
	i < 0, max + i + 1, 
	True, i
];


PackageScope["DotShape"]

DotShape[{{a_, b_}, {b_, c_}}] := {a, c};
DotShape[{{a_, b_}, {b_}}] := {a};
DotShape[{{a_}, {a_, b_}}] := {b};
DotShape[{{a_}, {a_}}] := {};
DotShape[{a_, b_}] := Block[{}, failShape["invalid dimensions for input tensors: `` and ``.", a, b]];


PackageScope["PoolingShape"]

PoolingShape[insize_List, pad_, kern_, stride_, "valid"] := 
	Floor[(insize + 2*pad - kern)/stride] + 1;

PoolingShape[insize_List, pad_, kern_, stride_, "full"] := 
	Ceiling[(insize + 2*pad - kern)/stride] + 1;


PackageScope["ConvolutionShape"]

ConvolutionShape[insize_, pad_, kern_, stride_, dilation_] := 
	Floor[(insize + 2*pad - (dilation * (kern - 1) + 1)) / stride + 1];


PackageScope["DeconvolutionShape"]

DeconvolutionShape[insize_, pad_, kern_, stride_] := 
	stride * (insize - 1) + kern - 2 * pad;


PackageScope["FlattenShape"]

FlattenShape[idims_List, Infinity] /; FreeQ[idims, LengthVar] := {Times @@ idims};

FlattenShape[{lv_LengthVar, 1}, Infinity|1] := {lv};

FlattenShape[idims_List, level_] := Scope[
	If[MemberQ[idims, _LengthVar], 
		FailValidation[FlattenLayer, "tensors with varying dimensions are not supported."]];
	{dflatten, dpreserve} = TakeDrop[idims, level + Sign[level]];
	If[level > 0, Prepend, Append][dpreserve, Times @@ dflatten]
];

PackageScope["CheckConvolutionOrPoolingFunction"]

(* check whether an object is a list of integers *)
intListQ[x_List] := AllTrue[x, IntegerQ];
intListQ[_] := False

(* TODO: switch to using failShape *)
CheckConvolutionOrPoolingFunction[f_, rank_, isize_, ksize_, osize_, psize_] := (
	If[rank > 2 || rank < 1, FailValidation[f, "dimensionality must be either 1 or 2."]];
	If[Apply[Or, Thread[isize + 2*psize < ksize]] || Min[osize] < 1, 
		FailValidation[f, "kernel size `` plus padding size `` cannot exceed input size ``.", 
			DimsString @ ksize, DimsString @ (2*psize), DimsString @ isize
		]
	];
	(* If any output dims are < 1, then osize is SizeListT, NOT a list. 
	Solution: if isize + ksize are known, then out size is necessarily known, as 
	padding, stride, etc always have values. Thus is osize is not a list when ksize 
	and isize are known, the osize must have value < 1. Type system should deal with 
	this better in the long run...
	*)
	If[!ListQ[osize] && intListQ[isize] && ListQ[ksize], 
		FailValidation[f, "output with non-positive dimensions was inferred."]
	];
)

(*----------------------------------------------------------------------------*)

PackageScope["ReplicateShape"]

(* note: the fact that ReplicateShape (and OTHER shape functions like AggregationShop)
 returns a list of dims, rather than an actual tensor, means that we can't represent 
 partial knowledge of the output size, like the fact that the trailing or initial dimensions
 are known but the total rank is not known. refactor if this becomes a problem at some point.
*)

ReplicateShape[ListT[n_Integer, SizeT], spec_, l_] := 
	ReplicateShape[Table[SizeT, n], spec, l];

ReplicateShape[dims_List, spec_, level_] := 
	Internal`UnsafeQuietCheck[
		Flatten @ Insert[dims, spec, level], 
		failShape["level must be a non-zero integer between `` and ``.", -Length[dims] - 1, Length[dims] + 1]
	];

ReplicateShape[_, _, _] := $Failed;


PackageScope["ReplicateInverseShape"]

ReplicateInverseShape[srank_, odims_List, level_] := Scope[
	irank = Length[odims] - srank; 
	If[level < 0, level = irank + 2 + level];
	Internal`UnsafeQuietCheck[
		TakeDrop[odims, {level, level + srank - 1}],
		FailValidation[ReplicateLayer, "specification is inconsistent with rank of output."]
	]
];

ReplicateInverseShape[_, _, _] := $Failed;


PackageScope["AggregationShape"]

AggregationShape[{}, _] := "rank of input tensor should be at least 1.";

AggregationShape[dims_, specs_] := Scope[
	levels = ToLevelSet[specs, Length[dims]];
	If[StringQ[levels], Return[levels]];
	Delete[dims, List /@ levels]
]


PackageScope["ToLevelSet"]

ToLevelSet[specs_, rank_] := Scope[
	$rank = rank;
	specs = ToList[specs] //. {
		i_Integer ? Negative :> (rank + i + 1),
		i_Integer /; i > rank :> failShape["at least one level (``) exceeds rank of input (``).", i, rank]
	};
	Union @ Flatten @ Map[toLevels, specs]
];

toLevels = MatchValues[
	Span[a_, All] := levelRange[a, $rank];
	Span[All, b_] := levelRange[1, b];
	Span[a_Integer, b_Integer] := levelRange[a, b];
	All := levelRange[1, $rank];
	i_Integer := {i};
];

levelRange[a_, b_] := 
	If[a <= b, Range[a, b],
		failShape["level specification contains empty span."]
	];


PackageScope["CheckPaddingSize"]

CheckPaddingSize[layer_, padding_, kernel_] := 
	If[Or @@ Thread[padding >= kernel], 
		FailValidation[layer, "value of PaddingSize (``) cannot exceed value of KernelSize (``)", padding, kernel]
	];