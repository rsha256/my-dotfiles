Input: TensorT[$$InputDimensions]

Output: TensorT[$$OutputDimensions]

Parameters:
	$Function: EnumT[{Mean, Min, Max, Total}]
	$Levels: ValidatedParameterT[checkAggregationSpec, 2;;]
	$$InputDimensions: SizeListT[]
	$$OutputDimensions: ComputedType[SizeListT[], ValidateShape[AggregationLayer, AggregationShape[$$InputDimensions, First @ $Levels]]]

checkAggregationSpec[spec_] := Scope[
	res = checkSpec[spec];
	If[!FreeQ[res, 0], aggSpecFail[]];
	res
];

checkSpec[e_List] := Map[checkSpecElem, e];
checkSpec[e_] := checkSpecElem[e];

checkSpecElem[s:Span[_Integer|All, _Integer|All]] := s;
checkSpecElem[All] := All;
checkSpecElem[i_Integer] := i;
checkSpecElem[_] := aggSpecFail[];
aggSpecFail[] := FailValidation[AggregationLayer, "specification should consist of an integer, a Span, All, or a list of integers or spans."];

MinArgCount: 1

Writer: Function[

	func = #Function;
	levelsToAggregate = ToLevelSet[First @ #Levels, Length[#$InputDimensions]];
	
	input = GetInput["Input"];
	out = SowNode[
		toSymbolType[#Function],
		input,
		"axis" -> levelsToAggregate
	];
	If[#Function === Mean,
		out = SowNode["_div_scalar",
			out,
			"scalar" -> Times @@ #$InputDimensions[[levelsToAggregate]]
		]
	];
	SetOutput["Output", out];
]

toSymbolType = <|
	Max -> "max_axis",
	Min -> "min_axis",
	Total -> "sum_axis",
	Mean -> "sum_axis"
|>;

Tests: {
	{Max, All, "Input" -> 4} -> "_IWqOWEDnRsY_Su2OBskfoT0",
	{Max, 1, "Input" -> 4} -> "_IWqOWEDnRsY_Su2OBskfoT0",
	{Max, {1, 3}, "Input" -> {2, 3, 4}} -> "3_NHzKAqS8WzE_BWFfB1red6w",
	{Max, 2, "Input" -> {2, 3, 4}} -> "2*4_I4AaMPCaDdI_UeVR1YhS60c",
	{Max, 1 ;; 3, "Input" -> {2, 3, 4}} -> "_a+6OH6f4oJI_Nri4ch5a/s0",
	{Max, 2, "Input" -> {2, 3, 4, 5}} -> "2*4*5_ZfjJvQTQUyA_Hvv7F0eicPE",
	{Max, 2 ;; 4, "Input" -> {2, 3, 4, 5}} -> "2_Tb+A8CskU/E_V6JFiXvji+E",
	{Max, All, "Input" -> {2, 3, 4, 5}} -> "_GPuLbfiRyzQ_Hn241y9oU3E",
	{Min, 2, "Input" -> {2, 3, 4, 5}} -> "2*4*5_KVdgVK3dUtA_Mh0tRBXZw1o",
	{Min, 2 ;; 4, "Input" -> {2, 3, 4, 5}} -> "2_GqoHJ9qF/kA_DbipFFm7GD4",
	{Min, All, "Input" -> {2, 3, 4, 5}} -> "_MXCqZlxDpmk_H8VSI6gnbYE",
	{Total, 2, "Input" -> {2, 3, 4, 5}} -> "2*4*5_W/VeLg8W3tI_CIhDl0KDlng",
	{Total, 2 ;; 4, "Input" -> {2, 3, 4, 5}} -> "2_TBBSEw0EsCs_V8N1alSIt4w",
	{Total, All, "Input" -> {2, 3, 4, 5}} -> "_EQTJ9fJWDKU_DQgy7R++flY",
	{Mean, 2, "Input" -> {2, 3, 4, 5}} -> "2*4*5_CSxuhvkFQ1A_JugqrYQktJI",
	{Mean, 2 ;; 4, "Input" -> {2, 3, 4, 5}} -> "2_COK82RpC65E_P42WvM4e1sQ",
	{Mean, All, "Input" -> {2, 3, 4, 5}} -> "_CQg8sA0YP2o_QJY9TRBT71A",
	{x, "Input" -> {2, 3, 4, 5}} -> "The value specified for Function should be either Mean,Min,Max, or Total.",
	{Max, {0, 2}, "Input" -> {2, 3, 4, 5}} -> "Validation failed for AggregationLayer: specification should consist of an integer, a Span, All, or a list of integers or spans."	
}


