Input: $$Shape

Output: $$Shape

Parameters:
	$Momentum: Defaulting[ScalarT, 0.9]
	$Epsilon: Defaulting[ScalarT, 0.001]
	$Channels: SizeT
	$$Shape: EitherT[{VectorT[$Channels], TensorT[{$Channels, SizeT, SizeT}]}]

Arrays:
	$Gamma: VectorT[$Channels]
	$Beta: VectorT[$Channels]
	$MovingMean: VectorT[$Channels]
	$MovingVariance: VectorT[$Channels]

AuxArrays: {"MovingMean", "MovingVariance"}

MXNet:
	Name: "BatchNorm"
	Parameters: 
		$Epsilon: "eps"
		$Momentum: "momentum"
	Writer: Function[{
		"fix_gamma" -> "false", 
		"use_global_stats" -> "false"
	}]
	Arrays:
		$Gamma: "gamma"
		$Beta: "beta"
		$MovingMean: "moving_mean"
		$MovingVariance: "moving_var"

Tests: {
	{"Input" -> 3} -> "3_AIvAa0t3efE_dSrbrONjgxw",
	{"Input" -> {3, 2, 2}} -> "3*2*2_DrEFVSHl9bc_UCreN7cY4sQ"
}