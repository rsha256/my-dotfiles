Inputs: <||>

Output: TensorT[$Dimensions]

Parameters:
	$Dimensions: SizeListT[]
	$Distribution: ValidatedParameterT[checkDistribution, NormalDistribution[]]

$validDistributionPattern = HoldPattern @ Alternatives[
	NormalDistribution[_?NumberQ, _?Positive],
	UniformDistribution[{_?NumberQ, _?NumberQ}],
	BernoulliDistribution[_?(Between[{0,1}])]
];

checkDistribution[d_] := 
	If[MatchQ[d, $validDistributionPattern], d, 
		FailValidation["distribution should be a NormalDistribution, UniformDistribution, or BernoulliDistribution with numeric parameters"]
	];

Writer: Function[
	out = Replace[First @ #Distribution, {
		NormalDistribution[mean_, sd_] :> SowNode["normal", {}, "loc" -> mean, "scale" -> sd],
		UniformDistribution[{min_, max_}] :> SowNode["uniform", {}, "low" -> min, "high" -> max],
		BernoulliDistribution[p_] :> SowNode["bernoulli", {}, "p" -> p]
	}];
	SetOutput["Output", SowSourceFixup[out, #Dimensions]];
]
