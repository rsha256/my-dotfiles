Inputs: 
	$Input: SequenceT[$$InputLength, VectorT[$$InputSize]]
	$Query: SequenceT[$$QueryLength, VectorT[$$QuerySize]]

Outputs:
	$Output: SequenceT[$$QueryLength, VectorT[$$InputSize]]

Parameters:
	$ScoringNet: NormalizedT[
		NetT[<|"Input" -> VectorT[$$InputSize], "Query" -> VectorT[$$QuerySize]|>, <|"Output" -> ScalarT|>], 
		toScoringNet, "Bilinear"
	]
	$$QuerySize: SizeT
	$$InputSize: SizeT
	$$QueryLength: LengthVar[]
	$$InputLength: LengthVar[]

(* TODO: generalize to arbitrary tensors *)

toScoringNet["Bilinear"] := NetGraph[
	<|"Linear" -> LinearLayer["Biases" -> None, "Output" -> $Raw[VectorT[]]], "Dot" -> DotLayer[]|>, 
	{{NetPort["Query"], "Linear"} -> "Dot"},
	"Query" -> $Raw[VectorT[]],
	"Input" -> $Raw[VectorT[]]
];

toScoringNet["Dot"] := NetGraph[{DotLayer[]},
	{{NetPort["Query"], NetPort["Input"]} -> 1},
	"Query" -> $Raw[VectorT[]],
	"Input" -> $Raw[VectorT[]]
];

toScoringNet[net_] := net;

MinArgCount: 0

Writer: Function[
	{ilen, ilenNode} = GetDynamicDimensionInfo[#$InputLength];
	{qlen, qlenNode} = GetDynamicDimensionInfo[#$QueryLength];
	varLengthQ = ilenNode =!= None;
	query = GetInputMetaNode["Query"];
	input = GetInput["Input", "Timewise"]; (* null out possible NaNs in input *)
	If[varLengthQ, input = SowSeqMask[input, ilenNode, "0.0"]];
	input = SowFlatten1[input]; (* ilen * batch, isize *)
	(* http://stanford.edu/~lmthang/data/papers/emnlp15_attn.pdf *)
	outs = NewSequenceMetaNode[query, {#$InputSize}];
	MXDo[
		q = query[[i]]; 											(* batch, qsize *)
		q = SowBroadcastAt[q, 0, ilen];								(* ilen, batch, qsize *)
		q = SowFlatten1[q]; 										(* ilen * batch, qsize *)
		scores = SowInnerNet[<|"Input" -> input, "Query" -> q|>, {"Parameters", "ScoringNet"}, #ScoringNet, ilen];
		scores = Lookup[scores, "Output"]; 							(* ilen * batch *)
		scores = SowUnflatten1[scores, ilen]; 						(* ilen, batch *)
		(* special case: for var seq len, mask so softmax is correctly calculated *)
		If[varLengthQ, scores = RankFixed[SowSeqMask][scores, ilenNode, "-1e37"]];
		scores  = SowTranspose01[scores];							(* batch, ilen *)
		scores = SowNode["SoftmaxActivation", scores];
		scores  = SowTranspose01[scores];							(* ilen, batch *)
		scores = SowUReshape[scores, -1, 1];						(* ilen * batch, 1 *)
		attend = SowBHad[scores, input];							(* ilen * batch, isize *)
		attend = SowUReshape[attend, ilen, -1, #$InputSize]; 		(* ilen, batch, isize *)
		attend = SowSumAxis[attend, 0];								(* batch, isize *)
		outs[[i]] = attend;
	,
		{i, 1, Length[query]}
	];
	SetOutput["Output", outs];
]

Tests: {
	{"Dot", "Input" -> {2,5}, "Query" -> {3,5}} -> "3*5_GUq3wE5WHyg_XN2SsxcfCrk",
	{"Dot", "Input" -> {"Varying",13}, "Query" -> {"Varying",13}} -> "3*13_MhNY/Z2P2pA_OSqphQvIQDU",
	{"Bilinear", "Input" -> {7,4}, "Query" -> {3,5}} -> "3*4_BFKzfjQiEIY_dkc1KH/rOyA",
	{"Bilinear", "Input" -> {"Varying",2}, "Query" -> {"Varying",7}} -> "3*2_CNqgeiahgx0_dMm11nUEowo"
}
