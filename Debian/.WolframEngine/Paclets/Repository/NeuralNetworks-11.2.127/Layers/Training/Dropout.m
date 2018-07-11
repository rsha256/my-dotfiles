Input: RealTensorT

Output: $Input

AllowDynamicDimensions: True

Parameters:
	$DropoutProbability: Defaulting[IntervalScalarT[0., 1.], 0.5]
	$Method: Defaulting[EnumT[{"Dropout", "AlphaDropout"}], "Dropout"]

Writer: Function[
	rate = #DropoutProbability;
	in = GetInput["Input"];
	Switch[#Method,
		"Dropout",
			out = SowNode["Dropout", in, "p" -> rate],
		(* Should be consistent with: https://github.com/bioinf-jku/SNNs/blob/master/selu.py *)
		"AlphaDropout",
			If[!$TMode,
				out = in,
                alpha = 1.6732632423543772848170429916717;
                scale = 1.0507009873554804934193349852946;
				out = SowNode["Dropout", in, "p" -> rate];
				mask = out;
				mask[[2]] = 1; (* Note: mask is scaled by 1 / (1 - DropoutProbability) *)
				(* rescale *)
				mask = SowPlusScalar[mask, -1];
				mask = SowTimesScalar[mask, alpha * scale];
				out = SowPlus[out, mask];
				const = Sqrt[(1 - rate) / (1 + alpha^2 * rate * scale^2)];
				out = SowTimesScalar[out, const];
			];
	];
	SetOutput["Output", out];
]

MXNet:
	Name: "Dropout"
	Parameters:
		$DropoutProbability: "p"
