Inputs: 
	$Input: TensorT[$$Dimensions]
	$Target: TensorT[$$Dimensions]

Outputs:
	$Loss: ScalarT 

Parameters:
	$Margin: Defaulting[ScalarT, 0.5]
	$Norm: Defaulting[EnumT[{"L1","L2"}], "L2"]
	$$Dimensions: SizeListT[]

Writer: Function[
	input = GetInput["Input"];
	target = GetInput["Target"];
	loss = SowNode["MeanLoss", {input, target}, "loss_type" -> #Norm];
	loss = SowNode["_MinusScalar", loss, "scalar" -> #Margin];
	loss = SowNode["relu", loss];
	SetOutput["Loss", loss];
]

IsLoss: True

Tests: {
}

