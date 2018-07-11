Input: TensorT[$$InputSize]

Output: TensorT[$OutputSize]

Parameters:
	$Specification: ValidatedParameterT[toRepSpec]
	$Level: Defaulting[IntegerT, 1]
	$OutputSize: ComputedType[SizeListT[], 
		ValidateShape[ReplicateLayer, ReplicateShape[$$InputSize, $$InsertedDimensions, $Level]], 
		{$$InputSize}, True
	]
	$$InsertedDimCount: ComputedType[SizeT, Length @ Developer`ToList @ First @ $Specification]
	$$InsertedDimensions: ComputedType[
		SizeListT[],
		First @ ReplicateInverseShape[$$InsertedDimCount, $OutputSize, $Level],
		{$OutputSize}, ListQ[$OutputSize]
	]
	$$InputSize: ComputedType[
		SizeListT[],
		Last @ ReplicateInverseShape[$$InsertedDimCount, $OutputSize, $Level], 
		{$OutputSize}, ListQ[$OutputSize]
	]

(* TODO: make inference work back to Input *)
PostConstructionFunction: Function[
	spec = Developer`ToList @ First[$Specification];
	PostSet[$$InsertedDimensions, spec /. Automatic -> SizeT];
	RestartInference[];
]

MinArgCount: 1
MaxArgCount: 2

toRepSpec[list:{Repeated[_Integer | Automatic]}] := list;
toRepSpec[other:(_Integer | Automatic)] := other;
toRepSpec[_] := FailValidation[ReplicateLayer, "specification should be an integer, Automatic, or a list of these."];

PostInferenceFunction: Function[
	If[$Specification === {}, 
		FailValidation[ReplicateLayer, "output tensor cannot be identical to the input tensor"];
	]
]

Writer: Function[
	oldDims = #$InputSize;
	newDims = Developer`ToList[#$InsertedDimensions];
	extraRank = Length[newDims];
	level = #Level;
	If[level < 0, level = Length[oldDims] + 2 + level];
	ones = ConstantArray[1, extraRank];
	reshapeDims = Flatten @ List[-1, Insert[oldDims, ones, level]];
	reshaped = SowNode["Reshape", GetInput["Input"], "shape" -> reshapeDims];
	newAxes = Range[extraRank] + level - 1;
	broadcasted = SowNode["broadcast_axis", reshaped, "axis" -> newAxes, "size" -> newDims];
	SetOutput["Output", broadcasted];	
]

Tests: {
	{1, "Input" -> {2, 3}} -> "1*2*3_SINkZe0REmI_A/KyG112bzs",
	{2, "Input" -> {2, 3}} -> "2*2*3_aUAZjJ9aN7M_M0aZQwwSKn8",
	{{4, 2}, "Input" -> 2} -> "4*2*2_fwTSrLiIOqg_L4UO8r6Zk6A",

	{3, -1, "Input" -> 4} -> "4*3_J77TsSvB1GE_G81/r7UMcNA",
	{3, -1, "Input" -> {2, 3}} -> "2*3*3_BB5FnLqEW3o_dMdnyKPcYHo",
	{3, 2, "Input" -> {4, 5}} -> "4*3*5_HgvANNtIn/Y_PIZinXe+EN8",

	{{2, 2}, -1, "Input" -> 3} -> "3*2*2_DeRXXXdr5GU_ObnZEE/vQLQ",
	{{2, 2}, 2, "Input" -> {3, 3}} -> "3*2*2*3_B3uzbRr3lFk_dUqtFC4KmGo",

	{Automatic, "Output" -> {3, 2}} -> "3*2_MAhq8kwFiII_MpLCxaABuDY",
	{Automatic, "Output" -> {3}} -> "3_FKsM10YEll4_J77TsSvB1GE",
	{{Automatic, 2}, "Output" -> {2, 2, 2}} -> "2*2*2_f/8pXwM4MhE_DtXFdZq3psQ",
	{{Automatic, Automatic}, "Output" -> {2, 2, 2}} -> "2*2*2_f/8pXwM4MhE_DtXFdZq3psQ",
	{{Automatic}, -1, "Output" -> {2, 3}} -> "2*3_bay7sk5FezM_EuF0uxGvHco",
	{{Automatic}, -2, "Output" -> {2, 3, 4}} -> "2*3*4_Ybj5SQQu4X0_QQFfykAgyAo",

	{{Automatic, Automatic}, "Input" -> {2}, "Output" -> {3, 2}} -> "Net contains inconsistent shapes.",
	{{Automatic, Automatic, Automatic}, "Output" -> {3, 2}} -> "Validation failed for ReplicateLayer: specification is inconsistent with rank of output.",
	{{Automatic}, "Input" -> {2}, "Output" -> {1, 2, 3, 4}} -> "Net contains inconsistent shapes."
}