Input: VectorT[$$InputSize]

Output: VectorT[$Size]

Arrays:
	$Weights: MatrixT[$Size, $$InputSize]
	$Biases: Nullable @ VectorT[$Size]

Parameters:
	$Size: SizeT
	$$InputSize: SizeT

MinArgCount: 0

MXNet:
	Name: "FullyConnected"
	Parameters: 
		$Size: "num_hidden"
	Arrays:
		$Weights: "weight"
		$Biases: "bias"
	Writer: Function["no_bias" -> If[#2["Biases"] === None, "True", "False"]] 

Tests: {
	{3, "Input" -> 4} -> "3_Q/Ie6ed/y2A_de2pQHdWQn4",
	{3, "Input" -> 4, "Biases" -> None} -> "3_AN0XNdKPGwE_f5VdEVlU9hY"
}