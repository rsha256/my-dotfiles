Input: ListT[2, TensorT[$$Dimensions]]
Output: TensorT[$$Dimensions]

Parameters:
	$Function: BinaryElementwiseFunctionT
	$$Dimensions: SizeListT[]

(* this is a hack around not specifing Input: {TensorT[$$Dimensions], TensorT[$$Dimensions]},
which seems to cause trouble. for now the whole list-of-inputs case is a bit untested and weird,
so we're not going down this rabbithole until 11.2 *)
PostInferenceFunction: Function[
	If[!ListQ[$Input], Return[]];
	{l, r} = $Input;
	{lf, rf} = FullySpecifiedTypeQ /@ $Input;
	Which[
		lf && !rf, PostSet[$Input, {l, l}],
		!lf && rf, PostSet[$Input, {r, r}],
		lf && rf && FailureQ[UnifyTypes[l, r]], 
			FailValidation[ThreadingLayer, 
				"First and second input should be same shape, instead they were `` and `` respectively.", 
				MsgForm[$Input[[1]]], MsgForm[$Input[[2]]]
			];
	];
	RestartInference[];
]

Writer: Function[
	{left, right} = GetInput["Input"];
	SetOutput["Output", SowScalarFunction[First @ #Function, left, right]];
]

SummaryFunction: Function[
	func = First[#Function];
	If[SymbolQ[func], SpecializedSummary[ThreadingLayer, func], ThreadingLayer]
]

WLEquivalent: Function[
	With[{func = ScalarFunctionToPureFunction[First @ #Function]},
		Function[wlin, Quiet[Check[MapThread[func, wlin, Length[#$Dimensions]], $Failed]]]
	]
]

Tests: {
	{Plus, "Input" -> {"Real", "Real"}} -> "_AqJ2ueHiI3M_CtS5uSw8pPc",
	{Plus, "Input" -> {3, 3}} -> "3_RfcM8A5RZGc_fx9hIWnBBJk",
	{Times, "Input" -> {3, 3}} -> "3_UFuI/OphZaw_D9jZep9XXgg",
	{#1 + #2^#1 & , "Input" -> {3, 3}} -> "3_E3+NLGSR10E_UYXmVo33y6k",
	{Max, "Input" -> {{2}, {2}}} -> "2_YTB/HRE2rQ0_AvDK/KbJbRI", (* <- Max isn't Listable *)
	{Zeta, "Input" -> {3, 3}} -> "Zeta could not be symbolically evaluated as a binary scalar function."
}
