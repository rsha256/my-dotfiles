Package["NeuralNetworks`"]



PackageExport["EqualRatio"]

EqualRatio[a_List, b_List] :=
	Mean@MapThread[If[#1===#2,1.,0.]&, {a, b}];


PackageExport["MemberRatio"]

MemberRatio[sets_List, elems_List] :=
	Mean@MapThread[If[MemberQ[#1,#2],1.,0.]&, {sets, elems}]


PackageExport["FalsePositiveRatio"]

FalsePositiveRatio[predicted_List, actual_List] :=
	Mean@MapThread[If[#1 === True && #2 === False, 1.,0.]&, {predicted, actual}]


PackageExport["FalseNegativeRatio"]

FalsePositiveRatio[predicted_List, actual_List] :=
	Mean@MapThread[If[#1 === False && #2 === True, 1.,0.]&, {predicted, actual}]


PackageExport["ClassRecall"]

ClassRecall[predicted_List, actual_List] := 
	ClassPrecision @ ConfusionMatrix[actual, predicted];

ClassRecall[{sa_SparseArray, labels_List}] := 
	ClassPrecision[{Transpose[sa], labels}];


PackageExport["ClassPrecision"]

ClassPrecision[predicted_List, actual_List] := 
	ClassPrecision @ ConfusionMatrix[predicted, actual];

ClassPrecision[{sa_SparseArray, labels_List}] := 
	Association @ MapThread[
		#1 -> (#2 / #3)&, 
		{labels, N@Diagonal[sa], Total[sa]}
	];


PackageExport["ClassFScore"]

ClassFScore[predicted_List, actual_List] :=
	ClassFScore @ ConfusionMatrix[predicted, actual];

ClassFScore[{sa_SparseArray, labels_List}] := 
	Association @ MapThread[
		#1 -> (2. * #2 / #3)&, 
		{labels, Diagonal[sa], Total[sa] + Total[Transpose[sa]]}
	];


PackageExport["ConfusionMatrix"]

ConfusionMatrix[predicted_List, actual_List] := Scope[
	labels = Union[predicted, actual];
	toInteger = Dispatch[Thread[labels -> Range[Length[labels]]]];
	predictedi = Replace[predicted, toInteger, {1}];
	actuali = Replace[actual, toInteger, {1}];
	{SparseArray[Rule @@@ Tally[Transpose[{actuali, predictedi}]]], labels}
]


PackageExport["ConfusionMatrixPlot"]

ConfusionMatrixPlot[a_List, b_List] := Scope[
	{matrix, labels} = ConfusionMatrix[a, b];
	ticks = Transpose[{Range @ Length[labels], labels}];
	yticks = ticks;
	xticks = MapAt[Rotate[#,Pi/3]&, ticks, {All, 2}];
	MatrixPlot[matrix, 
		ImageSize -> Min[(60 * Length[labels]), 150],
		FrameTicks -> {{None, yticks}, {xticks, None}},
		FrameLabel -> {"actual", "predicted"}]
];


PackageExport["ConfusionCases"]

ConfusionCases[predicted_, true_] := Block[{i = 0},
	rules = MapThread[
		Function[i++; If[#1 =!= #2, {#1, #2} -> i, Nothing]],
		{predicted, true}
	];
	SortBy[Merge[rules, Identity], -Length[#]&]
];

ConfusionCases[predicted_, true_, inputs_] :=
	Map[Part[inputs, #]&, ConfusionCases[predicted, true]];


PackageExport["ClassificationAccuracy"]

ClassificationAccuracy[predicted_, true_] := 
	Count[
		MapThread[#1 === #2&, {predicted, true}],
		True
	] / N@Length@true

