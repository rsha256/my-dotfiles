Input: ChannelT[$Channels, TensorT[$$InputSize]]

Output: ChannelT[$Channels, TensorT[$$OutputSize]]

Parameters:
	$Specification: ValidatedParameterT[parseResizeSpec]
	$Resampling: Defaulting[EnumT[{"Linear", "Nearest"}], "Linear"]
	$Dimensionality: ComputedType[SizeT, Length @ First @ $Specification]
	$Channels: SizeT
	$$InputSize: SizeListT[$Dimensionality]
	$$OutputSize: ComputedType[
		SizeListT[$Dimensionality], 
		checkSize @ MapThread[computeSize, {$$InputSize, First @ $Specification}]
	]

MinArgCount: 1
MaxArgCount: 1

PostConstructionFunction: Function[
	If[$Resampling === "Nearest" && !MatchQ[First@$Specification, {Repeated[Scaled[n_Integer]]}],
		FailValidation[ResizeLayer, "layer specification should be `` when using \"Resampling\" -> \"Nearest\".", Table[Scaled["n"], $Dimensionality]]
	];	
]

checkSize[n_List] /; Min[n] <= 1 := FailValidation[ResizeLayer, "layer would produce an output of size ``, which is not supported.", Row[n, "\[Times]"]];
checkSize[n_] := n;

computeSize[n_Integer, m_Integer] := m;
computeSize[n_Integer, Scaled[r_]] := Ceiling[N[r] * n];
computeSize[n_Integer, All] := n;

resizeP = (_Integer ? Positive) | Scaled[_ ? Positive] | All;
parseResizeSpec[spec_] := 
	If[MatchQ[spec, {Repeated[resizeP,2]}], spec,
		FailValidation[ResizeLayer, "specification should be a list containing integers, Scaled[r], or All."]
	];

$IdentityAffine = RawArray["Real32", {1, 0, 0, 0, 1, 0}];

makeBilinearKernel[c_, n_] := makeBilinearKernel[n] = RawArray["Real32", ConstantArray[1.0, {c, 1, n, n}]];

Writer: Function[

	input = GetInput["Input"];
	outsize = #$OutputSize;
	oneDim = #Dimensionality == 1;

	Switch[#Resampling,

		"Linear",

			(* note about 1D case:
			we're using the horrible hack of using the SpatialTransformer to
			do the bilinear resampling, but unfortunately it encontours a numeric
			error when given a height of 1. So we SowReshape a 1D input to 2D with
			height 1, broadcast up to a height of 3, do the resample, then crop
			the extra 2 rows away, then SowReshape back. Nasty stuff.
			*)
			If[oneDim, 
				AppendTo[outsize, 3];
				input = SowReshape[input, #Channels, First @ #$InputSize, 1];
				input = SowNode["broadcast_axis", input, "axis" -> {3}, "size" -> {3}];
			];
			(* this was a potential speed optimization to use the older UpSampling
			layer when the scales where both the same integer, but it is crashing, and 
			in addition we need to write a proper bilinear kernel generator, see 
			example/fcn-xs/init_fcnxs.py in MXNet. *)	
		(*	scale = #$OutputSize / #$InputSize;
			If[VectorQ[scale, IntegerQ] && Apply[Equal, scale],
				scale = First[scale];
				kernel = SowFixedArray @ makeBilinearKernel[#Channels, 2*scale - Mod[scale, 2]];
				out = SowNode["UpSampling", 
					{input, kernel},
					"scale" -> scale,
					"num_args" -> "1",
					"num_filter" -> "1",
					"sample_type" -> "bilinear"
				];
			,*)
			affine = SowBatchified[SowFixedArray[$IdentityAffine]];
			out = SowNode["SpatialTransformer", 
				{input, affine},
				"target_shape" -> outsize,
				"transform_type" -> "affine",
				"sampler_type" -> "bilinear"
			];
			If[oneDim, 
				out = SowNode["Crop", out, "h_w" -> {First[outsize], 1}, "center_crop" -> True, "num_args" -> 1];
				out = SowReshape[out, #Channels, First[outsize]];
			];
			SetOutput["Output", out];,

		"Nearest",
			(* now assuming #Specification is a List of Scaled[n] *)
			scale = #Specification[[1, 1, 1]];

			(* note about 1D case:
			another horrible hack, as UpSampling wants a 3-tensor no matter what.
			Hence we reshape to a 3-tensor, rescale, then crop and reshape back. 
			More nasty stuff.
			*)
			If[oneDim,
				input = SowReshape[input, #Channels, First @ #$InputSize, 1]
			];
			out = SowNode[
				"UpSampling",
				input,
				"scale" -> scale,
				"sample_type" -> "nearest",
				"num_args" -> 1
			];
			If[oneDim, 
				out = SowNode["Crop", out, "h_w" -> {First[outsize], 1}, "center_crop" -> True, "num_args" -> 1];
				out = SowReshape[out, #Channels, First[outsize]];
			];
			SetOutput["Output", out];
	]

]

Tests: {
	{{10}, "Resampling" -> "Linear", "Input" -> {2, 3}} -> "2*10_U7X4OME3/9o_YntLvIMJ2W8",
	{{Scaled[3.5]}, "Resampling" -> "Linear", "Input" -> {2, 3}} -> "2*11_Keaw9nMPp3s_NrtutZLS6iA",
	{{All}, "Resampling" -> "Linear", "Input" -> {2, 3}} -> "2*3_XNJtjBjJ1rk_Luzvstmun4A",
	{{10, 4}, "Resampling" -> "Linear", "Input" -> {1, 2, 3}} -> "1*10*4_Wl0K/n+4G1M_XhKv4PPrLNE",
	{{10, Scaled[2.5]}, "Resampling" -> "Linear", "Input" -> {1, 2, 3}} -> "1*10*8_PM5IIQoXLN0_W2EmKtyxnH8",
	{{10, All}, "Resampling" -> "Linear", "Input" -> {1, 2, 3}} -> "1*10*3_AFI3J+LJ40o_YqE2SPY6oAE",
	{{-1}, "Resampling" -> "Linear", "Input" -> {2, 3}} -> "Validation failed for ResizeLayer: specification should be a list containing integers, Scaled[r], or All.",
	{{Scaled[2]}, "Resampling" -> "Nearest", "Input" -> {2, 3}} -> "2*6_MaILbyfgI4M_EgMq2fuMxxU",
	{{Scaled[2], Scaled[2]}, "Resampling" -> "Nearest", "Input" -> {1, 2, 3}} -> "1*4*6_NVYKBlcoO8s_Ue59keoE9OQ",
	{{10}, "Resampling" -> "Nearest", "Input" -> {2, 3}} -> "Validation failed for ResizeLayer: layer specification should be {Scaled[n]} when using \"Resampling\" -> \"Nearest\".",
	{{Scaled[4], 2}, "Resampling" -> "Nearest", "Input" -> {1, 2, 3}} -> "Validation failed for ResizeLayer: layer specification should be {Scaled[n], Scaled[n]} when using \"Resampling\" -> \"Nearest\"."
}