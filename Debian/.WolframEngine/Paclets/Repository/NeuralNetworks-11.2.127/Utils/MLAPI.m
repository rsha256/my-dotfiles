Package["NeuralNetworks`"]



PackageExport["MLAPIEvaluate"]

MLAPIEvaluate[net_, input_] :=
	Log[unwrapBoole @ NetApply[net, input, All -> None] + $MinMachineNumber]

unwrapBoole[l_List /; VectorQ[l, RealQ]] := 
	Transpose[{1-l, l}];

unwrapBoole[e_] := e;


PackageExport["MLAPIOutputClasses"]

MLAPIOutputClasses[net_] := 
	Match[
		OutputTypes[net],
		{DecoderP["Class", assoc_]} :> assoc["Labels"],
		{DecoderP["Boolean"]} :> {False, True},
		$Failed
	];


PackageExport["MLAPIInputTypes"]

MLAPIInputTypes[net_] := Catch[
	KeyValueMap[
		<|"Name" -> #1, "Type" -> mltype @ #2|>&,
		Inputs[net]
	]
];

mltype[EncoderP[type_, assoc_]] := 
	$EncoderData[type, "MLType"][assoc];

mltype[ScalarT] := "Numerical";

mltype[TensorT[{n_Integer}]] := {"NumericalVector", n};

mltype[t_TensorT] := {"NumericalTensor", TDimensions[t]};

mltype[PosIntegerT] := "PositiveInteger";

mltype[IntegerT] := "Integer";

mltype[_] := Throw[$Failed];

