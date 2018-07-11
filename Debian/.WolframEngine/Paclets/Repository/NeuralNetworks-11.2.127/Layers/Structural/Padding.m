Input: TensorT[$$InputSize]

Output: TensorT[$$OutputSize]

Parameters:
	$Specification: ListT[SizeT, ListT[2, NaturalT]]
	$Padding: Defaulting[EitherT[{EnumT[{"Fixed", "Reflected"}], ScalarT}], 0.]
	$Rank: ComputedType[NaturalT, Length @ $Specification]
	$$InputSize: SizeListT[$Rank]
	$$OutputSize: ComputedType[
		SizeListT[$Rank], 
		Total[$Specification, {2}] + $$InputSize
	]

MinArgCount: 1
MaxArgCount: 2

PostInferenceFunction: Function[
	If[Not[1 <= $Rank <= 4], FailValidation["only an input of rank less than 5 is currently supported."]];
	If[Head[$$InputSize] === List && $Padding == "Reflected" &&
		Apply[Or, GreaterEqual@@@Transpose[{Max/@$Specification, $$InputSize}]],
			FailValidation[PaddingLayer, "padding sizes must be smaller than the input size when using \"Reflected\" method."]
	];
]

toMXPaddingSpec[spec_?NumericQ] := "constant";
toMXPaddingSpec["Fixed"] := "edge";
toMXPaddingSpec["Reflected"] := "reflect";
toMXPaddingSpec[_] := $Failed;

(* Note: MXNet only currently supports padding the spatial dims of 2d + 3d ims. *)
Writer: Function[

	(* parse inputs *)
	padType = toMXPaddingSpec[#Padding];
	padValue = ToString @ N @ If[NumericQ @ #Padding, #Padding, 0];

	(* 1. conform 1d + 2d tensor to 3d tensor *)
	spec = #Specification;
	id = Which[
		#Rank === 1,
			spec = Join[{{0, 0}, {0, 0}, {0, 0}}, spec];
			SowNode["Reshape", GetInput["Input"], "shape" -> Join[{0}, {1, 1}, #$InputSize]],
		#Rank === 2,
			spec = Join[{{0, 0}, {0, 0}}, spec];
			SowNode["Reshape", GetInput["Input"], "shape" -> Join[{0}, {1}, #$InputSize]],
		True,
			spec = Join[{{0, 0}}, spec];
			GetInput["Input"]
	];

	(* 2. do padding assuming no padding on channel *)
	tempSpec = spec;
	tempSpec[[2]] = {0, 0};
	id = SowNode["Pad", id, 
				"pad_width" -> writeIntList[Flatten @ tempSpec],
				"mode" -> padType,
				"constant_value" -> padValue
	];

	(* 3. reshape smaller tensors back to original size *)
	If[#Rank < 3,
		id = SowNode["Reshape", id, "shape" -> Join[{0}, #$OutputSize]];
	];

	(* 4. deal with channel dim padding *)
	If[spec[[2]] =!= {0, 0},
		id = SowNode["SwapAxis", id, "dim1" -> 1, "dim2" -> 2];
		tempSpec *= 0;
		tempSpec[[3]] = spec[[2]];
		id = SowNode["Pad", id,
					"pad_width" -> writeIntList[Flatten @ tempSpec],
					"mode" -> padType,
					"constant_value" -> padValue
			];
		id = SowNode["SwapAxis", id, "dim1" -> 1, "dim2" -> 2];
	];

	SetOutput["Output", id];
]

Tests: {
	{{{1, 2}}, "Input" -> {2}, "Padding" -> "Fixed"} -> "5_O2BH5+Mv2WE_WyzUPl2tpn8",
	{{{1, 2}}, "Input" -> {2}, "Padding" -> 2.3} -> "5_cSue3BJEcuU_SKth9uZZB+Y",
{{{1, 2}}, "Input" -> {3}, "Padding" -> "Reflected"} -> "6_VCZ97/C05E8_FNmTC2kJHmE",
	{{{1, 2}, {0, 3}}, "Input" -> {2, 2}, "Padding" -> 2.3} -> "5*5_W8mlxyiv1tg_CazoKztNe4Y",
	{{{2, 0}, {1, 1}}, "Input" -> {1, 2}, "Padding" -> "Fixed"} -> "3*4_IKT5G5vbTIc_LqkZ+q4sxq4",
	{{{2, 0}, {1, 1}}, "Input" -> {3, 2}, "Padding" -> "Reflected"} -> "5*4_VXbnhPEsiWc_XU3HXY4aTgA",
	{{{0, 0}, {4, 1}, {1, 2}}, "Input" -> {1, 2, 2}, "Padding" -> "Fixed"} -> "1*7*5_WCvUqvU/mb4_ejCT2TuTbnw",
	{{{0, 0}, {4, 1}, {1, 2}, {5, 1}}, "Input" -> {1, 2, 2, 3}, "Padding" -> "Fixed"} -> "1*7*5*9_V19A93wonas_ZDAnpUNKpRE",
	{{{1, 0}, {4, 1}, {1, 2}, {5, 1}}, "Input" -> {1, 2, 2, 3}, "Padding" -> -1.2} -> "2*7*5*9_Zd7zniEgj4M_OEG2jch6+ac",
	{{{0, 0}, {0, 1}, {1, 2}, {5, 1}}, "Input" -> {1, 2, 3, 6}, "Padding" -> "Reflected"} -> "1*3*6*12_XetWwG9rAtg_Q/N6PKb4khA",

	{{{0, 0}, {0, 1}, {1, 2}, {5, 1}}, "Input" -> {1, 2, 2, 3}, "Padding" -> "Reflected"} -> "Validation failed for PaddingLayer: padding sizes must be smaller than the input size when using \"Reflected\" method."
}
