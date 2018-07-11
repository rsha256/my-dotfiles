Input: TensorT[$$InputDimensions]

Output: TensorT[$OutputDimensions]

Arrays:
	$Weights: MatrixT[$$OutputSize, $$InputSize]
	$Biases: Nullable @ VectorT[$$OutputSize]

Parameters:
	$OutputDimensions: NormalizedT[SizeListT[], ToList]
	$$OutputSize: ComputedType[SizeT, Times @@ $OutputDimensions]
	$$InputSize: ComputedType[SizeT, Times @@ $$InputDimensions]
	$$InputDimensions: SizeListT[]

MinArgCount: 0

Writer: Function[
	irank = Length[#$InputDimensions]; orank = Length[#OutputDimensions];
	If[orank == 1 && irank == 1, WriterFallthrough[]];
	input = GetInput["Input"];
	If[irank =!= 1, input = SowReshape[input, #$InputSize]];
	If[#Biases === None,
		output = SowNode["FullyConnected", {input, #Weights}, "num_hidden" -> #$OutputSize, "no_bias" -> True],
		output = SowNode["FullyConnected", {input, #Weights, #Biases}, "num_hidden" -> #$OutputSize]
	];
	If[orank =!= 1, output = SowReshape[output, #OutputDimensions]];
	SetOutput["Output", output];
]	

PostInferenceFunction: Function[
	If[VectorTypeQ[$Input] && IntegerQ[$$InputSize],   PostSet[$Input,  VectorT[$$InputSize]]];
	If[VectorTypeQ[$Output] && IntegerQ[$$OutputSize], PostSet[$Output, VectorT[$$OutputSize]]];
	RestartInference[];
]

MXNet:
	Name: "FullyConnected"
	Parameters: 
		$$OutputSize: "num_hidden"
	Arrays:
		$Weights: "weight"
		$Biases: "bias"
	Writer: Function["no_bias" -> If[#2["Biases"] === None, "True", "False"]] 
	Reader: Function[{"Output" -> {Automatic}, "Input" -> {Automatic}}]

Tests: {
	{3, "Input" -> 4} -> "3_Q/Ie6ed/y2A_de2pQHdWQn4",
	{3, "Input" -> 4, "Biases" -> None} -> "3_AN0XNdKPGwE_f5VdEVlU9hY",
	{2, "Weights" -> {{0.1, 0.2}, {0.2, 0.4}}, "Biases" -> {0.1, 0.1}, "Input" -> 2} -> "2_KBSkqEK2twg_Pn2twHHtBkw",
	{"Input" -> {2, 2}, "Output" -> {3, 3}} -> "3*3_eg5mXSWE7ko_ZBMvCzkmWJA"
}