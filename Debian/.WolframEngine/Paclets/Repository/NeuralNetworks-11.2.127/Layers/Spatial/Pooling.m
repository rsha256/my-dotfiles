Input: ChannelT[$Channels, TensorT[$$InputSize]]

Output: ChannelT[$Channels, TensorT[$$OutputSize]]

Parameters:
	$KernelSize:     ArbSizeListT[$Dimensionality, PosIntegerT, None]
	$Stride:         ArbSizeListT[$Dimensionality, PosIntegerT, 1]
	$PaddingSize:    ArbSizeListT[$Dimensionality, NaturalT,    0]
	$Function:       Defaulting[PoolingFunctionT, Max]
	$Dimensionality: NaturalT
	$Channels:       SizeT
	$$InputSize:     SizeListT[$Dimensionality]
	$$OutputSize:    ComputedType[SizeListT[$Dimensionality],
		If[$$MXGlobalPool,
			ConstantArray[1, Length @ $$InputSize],
			PoolingShape[$$InputSize, $PaddingSize, $KernelSize, $Stride, $$MXPoolingConvention]
		]
	]
	$$MXPoolingConvention: Defaulting[EnumT[{"valid", "full"}], "valid"]
	$$MXGlobalPool: Defaulting[BooleanT, False]	

MinArgCount: 1 

PostConstructionFunction: Function[
	CheckPaddingSize[PoolingLayer, $PaddingSize, $KernelSize]
]

PostInferenceFunction: Function[
	CheckConvolutionOrPoolingFunction[PoolingLayer, $Dimensionality, $$InputSize, $KernelSize, $$OutputSize, $PaddingSize]
]

Writer: Function[
	Switch[#Dimensionality,
		2,
			WriterFallthrough[],
	 	1,
	 		input = SowReshape[GetInput["Input"], #Channels, First @ #$InputSize, 1];
			out = SowNode["Pooling", 
				input,
				"kernel" -> pad1 @ #KernelSize,
				"pad" -> pad0 @ #PaddingSize,
				"stride" -> pad1 @ #Stride,
				"pool_type" -> $ToPooling[#Function],
				"pooling_convention" -> #$MXPoolingConvention,
				"global_pool" -> #$MXGlobalPool
			];
			SetOutput["Output", SowReshape[out, #Channels, First @ #$OutputSize]]
	]
]

pad1[x_] := Append[x, 1];
pad0[x_] := Append[x, 0];

MXNet:
	Name: "Pooling"
	Parameters:
		$KernelSize: "kernel"
		$PaddingSize: "pad"
		$Stride: "stride"
		$Function: "pool_type"
		$$MXGlobalPool: "global_pool"
		$$MXPoolingConvention: "pooling_convention"

Tests: {
	{2, "Input" -> {2, 4, 4}} -> "2*3*3_ATYPBv07Hs8_Vdq2yW67/PY",
	{2, "PaddingSize" -> 2, "Input" -> {2, 4, 4}} -> "Validation failed for PoolingLayer: value of PaddingSize ({2, 2}) cannot exceed value of KernelSize ({2, 2})",
	{2, "Function" -> Mean, "Input" -> {2, 4, 4}} -> "2*3*3_JhX0kbpKO+8_G4GQnTKoDus",
	{{2, 1}, "Input" -> {2, 4, 4}} -> "2*3*4_XfU+DxhvLsI_d+76Z3xB+IU",
	{2, "Stride" -> 2, "Input" -> {2, 4, 4}} -> "2*2*2_C96JdXhPPc0_LujfV2xlsXM",

	(* 1-d case *)
	{3, "Input" -> {1, 9}} -> "1*7_DwN+i+t8HKQ_U1eyO5fisj0",
	{3, "Input" -> {1, 9}, "Stride" -> 3} -> "1*3_EO8rW4iATf4_Pk50/gKW0Nk"
}

