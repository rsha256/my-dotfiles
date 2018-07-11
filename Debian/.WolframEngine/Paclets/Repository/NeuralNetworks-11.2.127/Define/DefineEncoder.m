Package["NeuralNetworks`"]



PackageExport["$EncoderData"]

$EncoderData = Data`UnorderedAssociation[];


PackageExport["NetEncoder"]


PackageScope["CoderKind"]

CoderKind[HoldPattern @ NetEncoder[name_, _, _]] := 
	Replace[$EncoderData[name, "Kind"], Automatic :> ToLowerCase[name]];


PackageScope["CoderData"]

CoderData[HoldPattern @ NetEncoder[_, data_, _]] := data;


PackageScope["CoderName"]

CoderName[HoldPattern @ NetEncoder[name_, _, _]] := name;


PackageExport["CoderType"]

CoderType[HoldPattern @ NetEncoder[_, _, type_]] := type;


PackageScope["SequenceCoderQ"]

SequenceCoderQ[HoldPattern @ NetEncoder[name_, _, type_]] := !FreeQ[type, _LengthVar];


PackageScope["AcceptsListsQ"]

AcceptsListsQ[enc_NetEncoder] := System`Private`ValidQ[enc];


PackageScope["EncodeFail"]

General::invencin = "Invalid input, ``.";

$noisy = True;
EncodeFail[msg_] := (
	If[$noisy, InheritedMessage["invencin", fromStringForm[msg]]]; 
	Throw[$Failed, EncodeFail]
);

EncodeFail[msg_String, args__] := EncodeFail[StringForm[msg, args]];


PackageScope["EncoderRandomInstance"]

EncoderRandomInstance[NetEncoder[name_, assoc_, _]] := 
	$EncoderData[name, "RandomInstance"][assoc];


PackageExport["DefineEncoder"]

DefineEncoder[name_, assoc_] := CatchFailure @ Scope[

	assoc = CoerceParam[DefineEncoder, assoc, EncoderDefinitionT];

	DesugarTypeDefinitions[assoc, {"Parameters", "Output"}];
	SetupArgCounts[assoc];

	$EncoderData[name] = assoc;
];

EncoderDefinitionT = StructT[{
	"Output" -> TypeExpressionT,
	"Parameters" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"ParameterCoercions" -> Defaulting @ AssocT[StringT, ExpressionT],
	"ParameterDefaults" -> Defaulting @ AssocT[StringT, ExpressionT],	
	"InferenceRules" -> Defaulting @ ListT @ RuleT[MatchT[_NetPath], TypeExpressionT],
	"PostInferenceFunction" -> Defaulting @ ExpressionT,
	"ToEncoderFunction" -> FunctionT,
	"AcceptsLists" -> Defaulting[FunctionT, False&],
	"MLType" -> FunctionT,
	"RandomInstance" -> Defaulting[FunctionT, Panic["NoRandomInstanceDefined"]&],
	"Kind" -> Defaulting[StringT, Automatic],
	"EncoderToDecoder" -> Defaulting[FunctionT, None&],
	"MaxArgCount" -> Defaulting[IntegerT, Automatic],
	"MinArgCount" -> Defaulting[IntegerT, Automatic],
	"SourceFile" -> Defaulting[StringT, None],
	"HiddenFields" -> Defaulting[ListT[StringT], {}]
}];

(enc_NetEncoder ? System`Private`HoldEntryQ) := UseMacros @ RuleCondition @ CatchFailureAsMessage[NetEncoder, CreateEncoder[enc]];
(enc_NetEncoder ? System`Private`NoEntryQ)[input_] := CatchFailureAsMessage @ EncoderApply[enc, input];
NetEncoder /: Normal[(enc_NetEncoder ? System`Private`NoEntryQ)] := CoderExtract[enc, All];
NetEncoder /: Part[(enc_NetEncoder ? System`Private`NoEntryQ), part_] := CoderExtract[enc, part];

DefineCustomBoxes[NetEncoder, enc_NetEncoder ? System`Private`HoldNoEntryQ :> MakeEncoderBoxes[enc]];

MakeEncoderBoxes[HoldPattern @ NetEncoder[kind_, assoc_, type_]] := Scope[
	hidden = $EncoderData[kind, "HiddenFields"];
	entries = Append["Output" -> type] @ Prepend["Type" -> kind] @ KeyDrop[hidden] @ assoc;
	OptimizedArrangeSummaryBox[
		NetEncoder, Nothing,
		fmtEntries[entries],
		{}
	]
];

MakeEncoderBoxes[_] := $Failed;