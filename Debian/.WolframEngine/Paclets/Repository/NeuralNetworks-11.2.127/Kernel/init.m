BeginPackage["NeuralNetworks`"]

Begin["NeuralNetworks`Bootstrap`Private`"];

(* load dependancies *)
TakeDrop (* This causes loading of Language`PairFunctions` *)
Quiet @ Needs["GeneralUtilities`"];
Quiet @ Needs["MXNetLink`"];

(* clear protected symbols *)

Unprotect[ValidationSet];
Clear[ValidationSet];

syms = PacletExportedSymbols["NeuralNetworks"];
Unprotect @@ syms;
ClearAll @@ syms;

(* obtain files to load *)

$basePath = DirectoryName[$InputFileName, 2];
subPath[p__] := FileNameJoin[{$basePath, p}];
$files = FileNames["*.m", $basePath, Infinity];
$ignoreFiles = Flatten @ {
	FileNames["*.m", subPath /@ {"Layers","Encoders","Decoders"}, Infinity],
	{subPath["Kernel", "init.m"], subPath["PacletInfo.m"]}
};
$files = Complement[$files, $ignoreFiles];
utilsPath = $PathnameSeparator <> "Utils" <> $PathnameSeparator;
sortOrder[str_] := {-StringCount[str, "Initial.m"], -StringCount[str, "Types.m"], -StringCount[str, "Utils"], -StringCount[str, "Types"], str};
$files = SortBy[$files, sortOrder];

(* scan for scoped and exported symbols *)

$lines = StringSplit[StringTrim @ FindList[$files, {"PackageScope", "PackageExport"}], {"[", "]", "\""}]
$private = Cases[$lines, {"PackageScope", _, name_} :> name];
$public =  Cases[$lines, {"PackageExport", _, name_} :> name];
$public = Complement[$public, Names["System`*"]];

(* create symbols in the right context *)

createInContext[context_, names_] := Block[{$ContextPath = {}, $Context = context}, ToExpression[names, InputForm, Hold]];

createInContext["NeuralNetworks`", $public];
createInContext["NeuralNetworks`Private`", $private];

(* load files *)

$contexts = {"System`", "Developer`", "Internal`", "GeneralUtilities`", "MXNetLink`", "NeuralNetworks`", "NeuralNetworks`Private`"};
Block[{$ContextPath = $contexts}, 

$outcome = Null; SetAttributes[try, HoldAll];
try[body_] := If[!FailureQ[$outcome], $outcome = body];

loadFile[file_] := Block[
	{$Context = "NeuralNetworks`Private`" <> FileBaseName[file] <> "`"},
	contents = FileString[file];
	If[!StringStartsQ[contents, "Package[\"NeuralNetworks`\"]"], Return[]];
	contents = StringDrop[contents, 27];
	Check[
		ToExpression[contents, InputForm],
		Print["Message occurred during file: ", file];
	];
];

try @ CatchFailure[General, Scan[loadFile, $files]];
try @ NeuralNetworks`LoadEncoderDefinitions[];
try @ NeuralNetworks`LoadDecoderDefinitions[];
try @ NeuralNetworks`LoadLayerDefinitions[];

];

NeuralNetworks`Private`DeclareContainer["Graph", NetGraph];
NeuralNetworks`Private`DeclareContainer["Chain", NetChain];

Clear[DotPlusLayer];
DotPlusLayer[args___] := LinearLayer[args, "Input" -> {Automatic}, "Output" -> {Automatic}];

Protect @@ syms;

NeuralNetworks`$NetHeads = Internal`BagPart[NeuralNetworks`$NetHeads, All];

End[];

EndPackage[];

NeuralNetworks`Bootstrap`Private`$outcome