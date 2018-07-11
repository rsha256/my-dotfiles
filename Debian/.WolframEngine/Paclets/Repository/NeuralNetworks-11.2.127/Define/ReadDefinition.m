Package["NeuralNetworks`"]



PackageScope["ReadDefinitionFile"]

$ConfigContextPath = {
	"NeuralNetworks`", 
	"NeuralNetworks`Private`",  
	"GeneralUtilities`", 
	"Developer`", 
	"Internal`", 
	"DummyContext`",
	"System`"
};

General::defsyntax = "Syntax error in definition file ``.";
General::defnexists = "Definition file `` does not exist.";
General::defevalmsg = "Messages occured while loading definition file ``.";

ReadDefinitionFile[file_String, baseContext_String] := CatchFailure @ Scope[
	If[!FileExistsQ[file], ThrowFailure["defnexists", file]];
	name = FileBaseName[file];
	string = FileString[file];
	keystack = {};
	string = StringReplace[string, {
		StartOfLine ~~ tabs:"\t"... ~~ Repeated["$", {0,1}] ~~ word:(Repeated["$",{0,1}] ~~ LetterCharacter..) ~~ ":" :> (
			keystack = Append[Take[keystack, StringLength[tabs]], word];
			ToString[K @@ keystack, InputForm] <> ";"
		)
	}];
	$data = <||>;
	Block[{
		$Context = baseContext <> name <> "`",
		$ContextPath = $ConfigContextPath},
		DummyContext`$Input; DummyContext`$Output;
		Check[
			statements = ToExpression[string, InputForm, Hold],
			ThrowFailure["defsyntax", file]
		];
		Scan[proc, statements];
		$data = $data /. f_Function :> RuleCondition @ ScopePureFunction[f] /. {
			DummyContext`$Input -> NetPath["Inputs", "Input"],
			DummyContext`$Output -> NetPath["Outputs", "Output"]
		};
		rewrites = FlatMap[
			Function[field, Map[toPortRule[field, #]&, Keys @ Lookup[$data, field, {}]]],
			{"Inputs", "Outputs", "Arrays", "Parameters", "States"}
		];
	];
	$data["SourceFile"] = file;
	$data = $data /. rewrites /. NetPath[p___][q___] :> NetPath[p, q];
	Clear @@ All1[rewrites];
	$data
];

toPortRule[field_, symname_] := ToExpression["$" <> symname] -> NetPath[field, symname];


SetHoldAll[proc];

proc[ce_CompoundExpression] := ce;
proc[K[args___]; Null] := $data[args] = <||>;
proc[K[args___]; value_] := $data[args] = value;
proc[Null] := Null;
proc[expr_] := Panic["UnexpectedExpression", "The expression `` was not expected.", HoldForm[expr]];


PackageScope["DesugarTypeDefinitions"]

SetAttributes[DesugarTypeDefinitions, HoldFirst];

DesugarTypeDefinitions::failure = "Bootstrap inference step failed on desugared definitions: ``";

DesugarTypeDefinitions[assoc_, fields_] := Block[{irules},
	$irules = assoc["InferenceRules"];
	$subnets = {}; 
	$coercions = $defaults = <||>;
	$sid = 0;
	assoc = assoc /. c:ComputedType[_, _] :> RuleCondition @ Append[c, findDeep[c, _NetPath]];
	assoc = assoc /. LengthVar[] :> RuleCondition @ LengthVar[$sid++];
	Do[
		assoc[field] = desugarDefs[NetPath[field], assoc[field]],
		{field, fields}
	];
	assoc = CatchFailure[
		DesugarTypeDefinitions, 
		DoInference[assoc, Cases[$irules, _Rule], {}]
	];
	If[FailureQ[assoc], 
		Print[PrettyForm @ $irules];
		ThrowFailure[DesugarTypeDefinitions::failure, TextString @ assoc];
	];
	assoc["ParameterCoercions"] = $coercions;
	assoc["ParameterDefaults"] = $defaults;
	assoc["SubNets"] = $subnets;
	assoc["InferenceRules"] = Replace[$irules, $runtimeRule[r_] :> r, {1}];
];

Clear[desugarDefs];

desugarDefs[path_, assoc_Association] := 
	IMap[
		desugarDefs[Append[path, #1], #2]&,
		assoc
	]

desugarDefs[path_, t:NetT[ins_, outs_]] := (
	AppendTo[$subnets, path];
	ScanThread[appendIORules[path], {{"Inputs", "Outputs"}, {ins, outs}}];
	t /. _NetPath :> ExpressionT
);

(* we wrap the rules in $runtimeRule, these will be ignored during bootstrap inference,
because the inner net doesn't exist at that point *)
appendIORules[path_][field_, assoc_] :=
	KeyValueScan[
		AppendTo[$irules, $runtimeRule[Join[path, NetPath[field, #1]] -> #2]]&, 
		assoc
	];

desugarDefs[path_, Defaulting[t_, d_]] := (
	$defaults[Last @ path] := d;
	desugarDefs[path, t]
)

desugarDefs[path_, ValidatedParameterT[fn_, def_:Null]] := (
	$coercions[Last @ path] = fn /* ValidatedParameter;
	If[Unevaluated[def] =!= Null, $defaults[Last @ path] := def];
	ExpressionT
);


desugarDefs[path_, NormalizedT[type_, func_, def_:Null]] := (
	$coercions[Last @ path] = func;
	If[Unevaluated[def] =!= Null, $defaults[Last @ path] := def];
	desugarDefs[path, type]
);

desugarDefs[path_, type_ /; FreeQ[type, NetPath | TypeReplace]] := type;

desugarDefs[path_, type_] := (
	AppendTo[$irules, path -> type]; 
	Replace[type, {
		tr_TypeReplace :> tr[[2,-1,2]],
		_ :> TypeT
	}]
);


PackageExport["LoadLayerDefinitions"]

$baseDir = ParentDirectory @ DirectoryName[$InputFileName];
getDefinitionDir[name_] := FileNameJoin[{$baseDir, name}];

PackageScope["$LayerDefinitionsPath"]

$LayerDefinitionsPath = getDefinitionDir["Layers"];

LoadLayerDefinitions[name_:All] := CatchFailure[Replace[Map[
	loadDefinition[#, "Layers`", DefineLayer]&, 
	FileNames[If[name === All, "*.m", name <> ".m"], $LayerDefinitionsPath, 2]
], Hold[h_] :> h, {1}];]


PackageExport["LoadEncoderDefinitions"]

$EncoderDefinitionsPath = getDefinitionDir["Encoders"];

LoadEncoderDefinitions[] := CatchFailure @ 
	Scan[
		loadDefinition[#, "Encoders`", DefineEncoder]&, 
		FileNames["*.m", $EncoderDefinitionsPath]
	];


PackageExport["LoadDecoderDefinitions"]

$DecoderDefinitionsPath = getDefinitionDir["Decoders"];

LoadDecoderDefinitions[] := CatchFailure @ 
	Scan[
		loadDefinition[#, "Decoders`", DefineDecoder]&, 
		FileNames["*.m", $DecoderDefinitionsPath]
	];

General::invnetdeffile = "Definition in file `` is invalid.\nThe failed definition is available as $FailedDefinition. The error was:\n``";
General::unreaddeffile = "Couldn't read definition in file ``.";

PackageScope["$FailedDefinition"]

$FailedDefinition = None;

PackageScope["$LoadDefinitionTimings"]

$LoadDefinitionTimings = <||>;

$parseTimes = 0;

loadDefinition[file_, context_, func_] := ModuleScope[
	$LoadDefinitionTimings[FileNameTake[file, -2]] = First @ AbsoluteTiming[
		$parseTimes += First @ AbsoluteTiming[def = ReadDefinitionFile[file, "NeuralNetworks`" <> context]];
		If[FailureQ[def], ThrowFailure["unreaddeffile", file]];
		(* we must defer layers that are InheritsFrom until their parents have been
		loaded *)
		appf = If[KeyExistsQ[def, "InheritsFrom"], Hold, Identity]; 
		res = appf[checkRes[file, func, def]]
	];
	res
];

checkRes[file_, func_, def_] := Scope[
	name = FileBaseName[file];
	res = func[name, def];
	If[FailureQ[res], 
		$FailedDefinition ^= def;
		ThrowFailure["invnetdeffile", file, TextString @ res]
	];
];
