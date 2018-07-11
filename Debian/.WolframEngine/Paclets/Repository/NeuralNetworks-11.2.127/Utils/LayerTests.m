Package["NeuralNetworks`"]



PackageScope["CreateLayerTest"]

General::typenotspec = "In test of ``, port `` has a type that is not fully specified:\n``"
General::laytestevalfailed = "Layer `` failed when evaluated on `` input with dimensions ``. 
Layer InputForm available as $LastTestLayerInputForm. Layer available as $LastTestLayer. Input available as $LastTestInput and $LastTestBatchInput."
General::laytestinconsistent = "Layer `` had numerical inconsistency between batch and singleton evaluation."

SetHoldAll[CreateLayerTest];

CreateLayerTest[head_Symbol[args___]] := CreateLayerTest[head, args];

CreateLayerTest[head_, args___] := CatchFailure @ Scope[
	{res, v1, v2} = getTestOutput[head, args];
	istring = With[{res2 = res}, ToString[Unevaluated[{args} -> res2], InputForm]];
	CopyToClipboard[istring];
	{{args} -> res, nform /@ v1, nform /@ v2}
];

nform[assoc_Association] := Map[nform, assoc];
nform[l_List /; MachineArrayQ[l] || VectorQ[Flatten[l], NumberQ]] := ColorArrayForm[l];
nform[e_] := e;

$diffThreshold = 1./1000000;

getTestOutput[head_, args___] := Scope[
	$head = head;
	GeneralUtilities`PackageScope`$DisableCatchFailureAsMessage = True;
	$LastTestLayerInputForm ^= With[{h = head}, HoldForm[h[args]]];
	$LastTestLayer ^= layer = createInitializedLayer[head, args];
	If[FailureQ[layer], 
		res = tostr[layer, False];
		{res, None, None}
	,
		inputs = Inputs[layer];
		{in, inBatch} = createRandomInputData[inputs];
		Label[batchHadShortElem];
		$LastTestInput ^= in; $LastTestBatchInput ^= inBatch;
		out = EvaluateChecked @ layer[in, TargetDevice -> $LayerTestDevice];
		If[FailureQ[out], ThrowFailure["laytestevalfailed", $LastTestLayerInputForm, "single", cleverDims[in]]];
		outBatch = layer[inBatch, TargetDevice -> $LayerTestDevice];
		If[FailureQ[outBatch], 
			If[outBatch[["MessageName"]] === "netseqlen", inBatch = Take[inBatch, 2]; Goto[batchHadShortElem]];
			ThrowFailure["laytestevalfailed", $LastTestLayerInputForm, "batch", cleverDims[inBatch]];
		];
		If[(diff = numericDifference[out, firstOutBatch = getFirst[outBatch]]) > $diffThreshold && head =!= ConstantArrayLayer,
			ThrowFailure["laytestinconsistent", $LastTestLayerInputForm];
		];
		res = StringJoin[tostr[out, False], "_", tostr[outBatch, True]];
		$LastTest ^= {res, in -> out, inBatch -> outBatch}
	]
];

cleverDims[dims_Association] := Map[cleverDims, dims];
cleverDims[s_String] := String;
cleverDims[l_List /; !PackedArrayQ[l] && Length[l] <= 3] := Map[cleverDims, l];
cleverDims[e_List] := Dimensions[e];


PackageScope["$LastTest"]
PackageScope["$LastTestLayer"]
PackageScope["$LastTestLayerInputForm"]
PackageScope["$LastTestInput"]
PackageScope["$LastTestBatchInput"]
PackageScope["$TestHashFunction"]

$TestHashFunction = RoundRelative /* Base64Hash;

$LastTestLayerInputForm = $LastTest = $LastTestLayer = $LastTestInput = $LastTestBatchInput = None;

numericDifference[a_, b_] := If[Dimensions[a] =!= Dimensions[b], Infinity, Max[Abs[a - b]]];

getFirst[assoc_Association] := Map[First, assoc];
getFirst[list_List] := First[list];

tostr[f_Failure, _] := TextString[f];
tostr[e_, batchq_] := If[batchq, "", StringRiffle[Dimensions[e],"*"] <> "_"] <> $TestHashFunction[e];

PackageScope["RoundRelative"]

SetAttributes[RoundRelative, Listable];
RoundRelative[n_ ? NumericQ] := 
	If[Abs[n] < 10^-6, 0., Block[{man,exp}, 
		{man,exp} = MantissaExponent[N[n]];
		Round[man, .01] * 10^exp]
	];
RoundRelative[n_] := n;

createRandomInputData[<|"Input" -> list_List|>] := Scope[
	SeedRandom[54321];
	val0 = Map[rand0, list];
	val1 = Map[rand1, list];
	val2 = Map[rand2, list];
	val3 = Map[rand3, list];
	{val0, Transpose[{val0, val1, val2, val3}]}
];

createRandomInputData[types_] := Scope @ BlockRandom[ 
	KeyValueScan[
		If[!FullySpecifiedTypeQ[#2], ThrowFailure["typenotspec", $head, #1, #2]]&,
		types
	];
	SeedRandom[54321];
	val0 = Map[rand0, types];
	val1 = Map[rand1, types];
	val2 = Map[rand2, types];
	val3 = Map[rand3, types];
	single = val0; batch = AssociationTranspose[{val0, val1, val2, val3}];
	res = {single, batch};
	If[Length[types] === 1, First /@ res, res]
];

(* ensure ragged sequence lengths in the batch *)
rand0[t_] := RandomInstance[t /. _LengthVar -> 3];
rand1[t_] := RandomInstance[t /. _LengthVar -> 4];
rand2[t_] := RandomInstance[t /. _LengthVar -> 1];
rand3[t_] := RandomInstance[t /. _LengthVar -> 2];

createInitializedLayer[head_, args___] := Scope @ BlockRandom[
	SeedRandom[12321];
	args2 = MapSequence[ReleaseHold, args];
	net = head[args2];
	If[FailureQ[net], Return[net]];
	SeedRandom[12345];
	NetInitialize[net, Method -> {"Random", "Weights" -> 1., "Biases" -> 1.}]
];


PackageExport["RunLayerTests"]
PackageExport["RunLayerTestsSQA"]

Clear[RunLayerTests];

$LayerTestDevice = "CPU";

Options[RunLayerTests] = Options[RunLayerTestsSQA] = {
	TargetDevice -> "CPU"
};

failFunc = Function[
	Print[$LastTestLayerInputForm, " test failed: ", Inactive[Unequal][paster @ #1, #2]]
];

General::laytesthashneq = "Layer `` when run on $LastTestInput and $LastTestBatchInput produced `` instead of ``.
Layer InputForm available as $LastTestLayerInputForm. Layer available as $LastTestLayer."

throwFailFunc = Function[
	ThrowFailure["laytesthashneq", $LastTestLayerInputForm, #1, #2];
];

RunLayerTestsSQA[opts:OptionsPattern[]] := Scope[
	failFunc = throwFailFunc;
	RunLayerTests[opts]
];

RunLayerTests[opts:OptionsPattern[]] := RunLayerTests[$NetHeads, opts];

RunLayerTests[heads_List | heads_Symbol, OptionsPattern[]] := CatchFailure @ Scope[
	$testCount = 0;
	$lastFailRules ^= <||>;
	$LayerTestDevice = OptionValue[TargetDevice];
	Scan[iRunTest, ToList[heads]];
	$testCount
];

iRunTest[sym_Symbol] := Scope[
	tests = $LayerData[$SymbolToType[sym], "Tests"];
	$testHead = sym;
	Scan[runTest, tests];
];

runTest[{args___} -> result_] := Scope[
	out = First @ getTestOutput[$testHead, args];
	If[out =!= result, 
		file = $LayerData[$SymbolToType[$testHead], "SourceFile"];
		If[!KeyExistsQ[$lastFailRules, file], $lastFailRules[file] = {}];
		AppendTo[$lastFailRules[file], result -> out];
		failFunc[out, result]
	];
	$testCount++
];

paster[e_] := MouseAppearance[
	EventHandler[e, "MouseClicked" :> CopyToClipboard[ToString[e, InputForm]]],
	"LinkHand"
];


PackageScope["UpdateLayerTests"]

$lastFailRules = <||>

UpdateLayerTests[] := (
	KeyValueScan[updateFile, $lastFailRules];
	$lastFailRules = <||>;
);

UpdateLayerTests::update = "Updating `` tests in ``."

updateFile[file_, rules_] := Scope[
	str = FileString[file];
	str2 = StringReplace[str, rules];
	If[str =!= str2, 
		Print["Updating ", Length[rules], " tests in ", file];
		WriteString[file, str2];
		Close[file];
	,
		Print["Couldn't find targets in ", file];
	];

];

