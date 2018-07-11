Inputs:
	$Left: TensorT[$$Dimensions]	
	$Right: TensorT[$$Dimensions]

Parameters:
	$Function: Defaulting @ EnumT[{EuclideanDistance, CosineDistance}]
	$$Dimensions: SizeListT[]

Outputs:
	$Output: ScalarT

Writer: Function[
	func = Switch[#Function,
		CosineDistance, SowCosineDistance,
		EuclideanDistance, SowEuclideanDistance
	];
	SetOutput["Output", func[GetInput["Left"], GetInput["Right"], Length[#$Dimensions]]]
]