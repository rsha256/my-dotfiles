Input: TensorT[$$Dimensions]

Output:	TensorT[$$Dimensions]

Parameters:
	$Function: UnaryElementwiseFunctionT
	$$Dimensions: SizeListT[]

AllowDynamicDimensions: True

MinArgCount: 1

Writer: Function[
	SetOutput["Output", SowScalarFunction[First @ #Function, GetInput["Input"]]];
]

SummaryFunction: Function[
	func = First[#Function];
	name = If[$TradQ, "activation", ElementwiseLayer];
	If[!AtomQ[func] || ($TradQ && !MemberQ[{Ramp, LogisticSigmoid, Tanh}, func]),
		func2 = ScalarFunctionToPureFunction[func];
		If[LeafCount[func2] > 10, Return[name]];
		func = func2[$DummyVar] /. r_Real /; (r == Round[r]) :> Round[r];
	];
	If[$TradQ, 
		func = TraditionalForm[func /. {Ramp -> "ReLU", LogisticSigmoid -> "Sigmoid", Tanh -> "Tanh"}];
		If[SymbolQ[func], func = func[$DummyVar]];
	]; 
	SpecializedSummary[name, func]
]

MXNet: 
	Name: "Activation"
	Parameters: 
		$Function: "act_type"
	Reader: Function[
		"Function" -> $FromActivation[#["act_type"]]
	]

Tests: {
	{Ramp, "Input" -> 4} -> "4_N6dBguLAOZ8_apeZsB5KfCw",
	{LogisticSigmoid, "Input" -> {4}} -> "4_XXj3lKcHKcs_Pbd8Eqi2aO4",
	{Tanh, "Input" -> 4} -> "4_G2cnksaJGog_G9G1MQtYalM",
	{ArcTan, "Input" -> {4}} -> "4_L8KbUzwPWR8_IFA7BSjJtEM",
	{ArcTanh, "Input" -> {4}} -> "4_BLa5o91bKUg_cCE/YBx4Zmk",
	{Sin, "Input" -> {4}} -> "4_VP4C9G8SjU8_Qye/ozWc77A",
	{Sinh, "Input" -> {4}} -> "4_PE57l3hgKbo_Frzn0nueCps",
	{ArcSin, "Input" -> {4}} -> "4_GLAhRQXQUV8_MkV3ig2o+0w",
	{Cos, "Input" -> 4} -> "4_YGr8Cph+fhU_TUxB107xNYA",
	{ArcCos, "Input" -> {4}} -> "4_LnduJwGj+Ao_VdcKy09xL8A",
	{Log, "Input" -> {4}} -> "4_IqnlIB+NouU_B5TPhDUJlNM",
	{Exp, "Input" -> {4}} -> "4_Qu2YQF45zdM_IYp8MvlHXqg",
	{Sqrt, "Input" -> {4}} -> "4_HIASKVnaFgY_bG9RQqaAlH4",
	{Gamma, "Input" -> {4}} -> "4_H/bLKcsliyE_MpZ5QW+TQsw",
	{Abs,  "Input" -> 4} -> "4_N6dBguLAOZ8_apeZsB5KfCw",
	{LogGamma, "Input" -> {4}} -> "4_f5J8p2T1kUM_fBfIM9vDhVM",
	{#1+1 & , "Input" -> 4} -> "4_DXWJKbj8WcI_U3DdNX4dvsI",
	{#1*2 & , "Input" -> 4} -> "4_RmDhgFbgFq8_WmilYV1De7Y",
	{#1/2 & , "Input" -> 4} -> "4_XwjPwhZxSS4_Zyd2AyViKWU",
	{#1-2 & , "Input" -> 4} -> "4_DqtzqPKHBPA_ABIucocKrUs",
	{#1^2 & , "Input" -> 4} -> "4_MdlGgeekRbg_fuS46yPUn5Q",
	{Exp[-#1^2] & , "Input" -> 4} -> "4_TKlcbB3sGUI_RmiN6AcUf7U",
	{Min[#1, 5] & , "Input" -> 4} -> "4_N6dBguLAOZ8_apeZsB5KfCw",
	{Ramp, "Input" -> {4, 4}} -> "4*4_apeZsB5KfCw_EthWqvd3jvs",
	{Clip[#1, {0.4, 0.5}] & , "Input" -> {4}} -> "4_ccz6AwGtEF8_SJIQcRA6E7U",
	{Clip[#1, {-2.3, 0.5}] & , "Input" -> {4}} -> "4_VZ+ShuX//NU_NabzX+KgSuE",
	{Clip[3.4*Sin[#1] + 3.4 + #1/3.2 + #1^2 - Max[#1] + Min[Cos[#1] + 2.3], {-2.3, 0.5}] & , 
		"Input" -> {2}} -> "2_VYYdKBM7HxE_FbJGc4qAafQ"
}