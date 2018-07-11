Input: $$InputShapes

Output: $$OutputShape

Parameters:
	$Level: Defaulting[PosIntegerT, 1]
	$$InputShapes: ListT[$$InputCount, TensorT[SizeListT[]]]
	$$InputCount: SizeT
	$$OutputShape: ComputedType[
		RealTensorT,
		ValidateShape[CatenateLayer, 
			CatenateShape[TDimensions /@ $Input, $Level],
			TensorT
		]
	]

Writer: Function[
	level = #Level;
	odim = TDimensions[#$OutputShape]; orank = Length[odim];
	idims = TDimensions /@ #$InputShapes; iranks = Length /@ idims;
	If[level === 1 && SameQ @@ iranks, 
		If[!FreeQ[idims[[All, 1]], _LengthVar],
			FailValidation[CatenateLayer, "cannot catenate on level 1 as one or more of the input tensors is \"Varying\" on that level."];
		];
		WriterFallthrough[]
	];
	idims = ToMaxLengths[idims]; odim = ToMaxLengths[odim];
	nodes = MapThread[SowBroadcastUp[#1, odim, #2]&, {GetInput["Input"], idims}];
	tlevel = level + orank - Min[iranks];
	out = SowNode["Concat", nodes, "dim" -> tlevel, "num_args" -> Length[nodes]];
	SetOutput["Output", out];
]

(* this PIF makes sure that the first dim is unified on. we should actually
be doing a more general thing here, unifying ALL the dims, taking into account
broadcasting as well, so that shape info can flow in through one input and
out another input. but for now we want to just make sure that if you
are catenating two sequences, they are forced to be the same length. 

In[20]:= CatenateLayer[2,"Input"->{{"Varying",2},{"Varying",2}}]//Inputs
Out[20]= <|Input->{TensorT[{LengthVar[2025028587],2},RealT],TensorT[{LengthVar[2025028587],2},RealT]}|>

*)

PostInferenceFunction: Function[
	inputs = $$InputShapes;
	dims = TDimensions /@ inputs;

	(* only the first dim of each input can be varying, but if we're catenating at that dim,
	they can't be varying, so there are no varying dims, so we have nothing to do *)
	If[$Length == 1, Return[]];

	(* if there are no dynamic dims, nothing to do *)
	If[!VectorQ[dims, ListQ] || FreeQ[dims, _LengthVar], Return[]];

	(* take first dimension of inputs, ignoring broadcasted inputs *)
	fdims = MaximalBy[dims, Length][[All, 1]];

	(* find the fixed and variable such first dims *)
	vars = Cases[fdims, _LengthVar];
	ints = Cases[fdims, _Integer];

	If[Length[ints] > 0,
		(* at least one fixed dim: they must all be that dim *)
		If[!Apply[SameQ, ints], FailValidation[CatenateLayer,
			"all inputs must be tensors of rank \[GreaterEqual] 1 with whose dimensions differ only on the catenation level. Instead, the input dimensions were ``.",
			TextString @ Row[FromT[TensorT[#]]& /@ dims, ", "]
		]];
		final = First[ints];
	,	(* we are guarenteed at least one var, becuase there is one SOMEWHERE, and it 
		can't be hiding in broadcast dims *)
		(* now we unify with output's first dim, if any *)
		fodim = First[TDimensions[$$OutputShape], $Failed];
		final = If[
			IntegerQ[fodim] || fodim === SizeT, fodim,
			If[MatchQ[fodim, _LengthVar], AppendTo[vars, fodim]];
			LengthVar @ Max @ vars[[All, 1]]
		];
	];

	newDims = dims /. _LengthVar -> final;
	newInputs = MapThread[UnifyTypes[TensorT[#, AtomT], #2]&, {newDims, inputs}];
	PostSet[$$InputShapes, newInputs];
	RestartInference[];
]

AllowDynamicDimensions: True

MXNet:
	Name: "Concat"
	Parameters:
		$$InputCount: "num_args"

WLEquivalent: Function[ExpandedCatenate[#Level]]

ExpandedCatenate[level_][inputs_] := Scope[
	{offset, arrays} = BroadcastingConform[inputs];
	Join @@ Append[arrays, level + offset]
];
