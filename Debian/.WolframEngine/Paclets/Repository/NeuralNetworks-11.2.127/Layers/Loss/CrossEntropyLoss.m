Inputs:
	$Input: TensorT[$$InputDimensions, SwitchedType[$TargetForm,
		"Binary" -> 				RealT, 
		"Probabilities"|"Index" -> 	VectorT[$$Classes],
		RealTensorT
	]]
	$Target: TensorT[$$InputDimensions, SwitchedType[$TargetForm,
		"Binary" -> 				RealT,
		"Index" -> 					IndexIntegerT[$$Classes], 
		"Probabilities" -> 			VectorT[$$Classes],
		AnyTensorT
	]]

Outputs:
	$Loss: ScalarT 

AllowDynamicDimensions: True

PostInferenceFunction: Function[
	If[$$Classes === 1,
		FailValidation["input to CrossEntropyLossLayer should be a vector of at least two elements."]
	]
]

Parameters:
	$TargetForm: ComputedType[
		EnumT[{"Binary", "Index", "Probabilities"}],
		Which[
			MatchQ[TType @ $Target, _IndexIntegerT],
				"Index",
			$Target === ScalarT,
				"Binary",
			eqNotFailed[TRank[$Target], TRank[$Input]],
				EnumT[{"Probabilities", "Binary"}],
			True,
				$Failed
		],
		{$Input, $Target}, True
	]
	$$InputDimensions: SizeListT[]
	$$Classes: SwitchedType[$TargetForm,
		"Binary" -> None,
		_String ->	SizeT,
		Nullable[SizeT]
	]

eqNotFailed[a_, b_] := a === b;
eqNotFailed[$Failed, _] := False;

MinArgCount: 0

Writer: Function[
	If[Length[#$InputDimensions] == 0 && #TargetForm =!= "Binary", 
		WriterFallthrough[];
	];
	
	input = GetInputMetaNode["Input"];
	target = GetInputMetaNode["Target"];
	sdims = Cases[#$InputDimensions, _Integer];
	srank = Length[sdims];

	{input, target, postf} = MakeTimewiseMeanData[input, target];

	If[#TargetForm === "Binary",
		loss = SowMinus @ SowPlus[
			SowHad[target, SowLog @ SowPlusEps @ input], 
			SowHad[SowOneMinus @ target, SowLog @ SowPlusEps @ SowOneMinus @ input]
		];
		If[srank > 0,
			sum = SowSum[loss, srank];
			loss = SowDivideScalar[sum, Times @@ sdims];
		];
	,
		sparse = #TargetForm === "Index";
		If[srank > 0, input = SowTransposeLast[input, srank+1]];
		If[!sparse && srank > 0, target = SowTransposeLast[target, srank+1]];
		loss = SowNode[
			"NLLLoss", {input, target}, 
			"sparse" -> sparse,
			"multi_output" -> True
		];
	];
	loss = postf[loss];
	SetOutput["Loss", loss];
]

MXNet:
	Name: "NLLLoss"
	Writer: Function[{
		"sparse" -> If[#TargetForm === "Index", "true", "false"]
	}]	

IsLoss: True

SummaryFunction: Function[
	If[$TradQ,
		SpecializedSummary[
			"Cross Entropy",
			TFCase @ Switch[#TargetForm, "Binary", "Binary Cross Entropy", "Index", "Cross Entropy", "Probabilities", "Dense Cross Entropy", _, "Cross Entropy"]
		],
		If[StringQ[#TargetForm], 
			SpecializedSummary[
				CrossEntropyLossLayer, 
				HoldForm[CrossEntropyLossLayer[#TargetForm]]
			],
			CrossEntropyLossLayer
		]
	]
]

Tests: {
	{"Index", "Input" -> 3} -> "_A+O/esckMuo_Al4TO/DUBiY",
	{"Probabilities", "Input" -> 3} -> "_Czm72Ft0JbQ_RQJYQ7V7KQA",
	{"Binary", "Input" -> {}} -> "_P2V/LF49EVU_KGjv+XzHNTw",

    (* these are higher-dimension tests *)
	{"Index", "Input" -> {3, 5}} -> "_XhH+2GaKfXw_To8bRotBq3g",
	{"Probabilities", "Input" -> {3, 5}} -> "_LBHD28h3HPE_FdGoWdDbUjY",
	{"Binary", "Input" -> 3} -> "_fdXQ2C0BeMs_IXvz0l6CXZo",
	{"Binary", "Input" -> {3, 5}} -> "_A+O/esckMuo_fRRnF06idDc",

	(* these exercise an extra transpose to work around the current wl_celoss convention *)
	{"Probabilities", "Input" -> {2, 3, 5}} -> "_QXO9IkoiJu4_bk3ynK1caS4",
	{"Index", "Input" -> {5, 3, 7}} -> "_Ub1LJNSHJ+8_CoNuJdPrMNs",
	{"Probabilities", "Input" -> {2, 3, 2, 5}} -> "_NSBlg4XV3uo_BlgyGfRbV0o",
	{"Index", "Input" -> {2, 3, 4, 7}} -> "_Czm72Ft0JbQ_VbV5FFeS8D0"

}