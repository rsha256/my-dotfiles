Input: VectorT[$Dimensions]

Parameters:
	$Labels: ListT[$Dimensions, ExpressionT]
	$Dimensions: SizeT

ArrayDepth: 1

MaxArgCount: 1

DecoderToEncoder: Function[NetEncoder[{"Class", #Labels}]]

ToDecoderFunction: Function[
	toDecisionFunction[#2, #Labels]
]

ToPropertyDecoderFunction: Function @ With[
	{labels = #Labels, dims = #Dimensions},
	Replace[#3, {
		"Decision" :> toDecisionFunction[#2, labels],
		"Probabilities" :> MapB[#2, threadProbabilities[labels, dims]],
		{"Probabilities"|"Probability", class_ /; MemberQ[labels, class]} :> With[
			{index = IndexOf[labels, class]},
			If[#2, Function[input, Part[input, All, index]], Extract @ index]
		],
		"TopProbabilities" :> MapB[#2, topProbs[labels]],
		{"TopProbabilities", n_Integer /; 1 <= n <= dims} :> MapB[#2,
			Function[input, TakeLargestBy[Thread[labels -> input], Last, n]]
		],
		{"TopDecisions", n_Integer /; 1 <= n <= dims} :> MapB[#2,
			Function[input, Part[labels, Reverse @ Ordering[input, -n]]]
		],
		"RandomSample" :> MapB[#2, randomSampler[labels]],
		"Entropy" :> DEntropy,
		_ :> $Failed
	}]
]

AvailableProperties: {"Decision", "TopProbabilities", {"TopDecisions", _}, {"TopProbabilities", _Integer}, "Probabilities", {"Probabilities", _}, "RandomSample", "Entropy"}

toDecisionFunction[batchq_, labels_] :=
	If[batchq, 
		Function[input, UnsafeQuietCheck @ If[input === {}, input, Part[labels, ListMaxIndex[input]]]],
		Function[input, UnsafeQuietCheck @ If[input === {}, input, Part[labels, MaxIndex[input]]]]
	];

DEntropy = Compile[{{probs, _Real, 1}}, Module[{e = 0.},
	Do[
		If[4.440892098500626`*^-16 <= p <= 1., e -= p * Log[p]],
		{p, probs}
	];
	e
], RuntimeAttributes -> {Listable}];

randomSampler[labels_][input_] := RandomChoice[input -> labels];

topProbs[labels_][input_] := Scope[
	rules = Reverse @ SortBy[Thread[labels -> input], Last];
	Select[rules, Last /* GreaterThan[0.1 * rules[[1, 2]]]]
];

threadProbabilities[labels_, len_] :=
	Function[input, If[Length[input] =!= len, Panic[], AssociationThread[labels, input]]];