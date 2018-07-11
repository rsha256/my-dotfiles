Package["NeuralNetworks`"]



PackageExport["$DecoderData"]

$DecoderData = Data`UnorderedAssociation[];


PackageExport["NetDecoder"]


PackageScope["CoderKind"]

CoderKind[HoldPattern @ NetDecoder[name_, assoc_, type_]] := 
	Replace[$DecoderData[name, "Kind"], {
		Automatic :> ToLowerCase[name], 
		f_Function :> f[assoc, type]
	}];


PackageScope["CoderData"]

CoderData[HoldPattern @ NetDecoder[_, data_, _]] := data;


PackageScope["CoderName"]

CoderName[HoldPattern @ NetDecoder[name_, _, _]] := name;


PackageExport["CoderType"]

CoderType[HoldPattern @ NetDecoder[_, _, type_]] := type;


PackageScope["SequenceCoderQ"]

SequenceCoderQ[HoldPattern @ NetDecoder[name_, _, type_]] := !FreeQ[type, _LengthVar];
SequenceCoderQ[_] := False;


PackageExport["DecoderDepth"]

DecoderDepth[HoldPattern @ NetDecoder[name_, assoc_, type_]] := Replace[
	$DecoderData[name, "ArrayDepth"],
	f_Function :> f[assoc, type]
];


PackageExport["DecoderDimensions"]

DecoderDimensions[HoldPattern @ NetDecoder[_, _, type_]] := TDimensions[type] /. SizeT -> _;


PackageExport["DefineDecoder"]

DefineDecoder[name_, assoc_] := CatchFailure @ Scope[

	assoc = CoerceParam[DefineDecoder, assoc, DecoderDefinitionT];

	DesugarTypeDefinitions[assoc, {"Parameters", "Input"}];
	SetupArgCounts[assoc];

	$DecoderData[name] = assoc;
];

DecoderDefinitionT = StructT[{
	"Input" -> TypeExpressionT,
	"Parameters" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"ParameterCoercions" -> Defaulting @ AssocT[StringT, ExpressionT],
	"ParameterDefaults" -> Defaulting @ AssocT[StringT, ExpressionT],	
	"InferenceRules" -> Defaulting @ ListT @ RuleT[MatchT[_NetPath], TypeExpressionT],
	"PostInferenceFunction" -> Defaulting @ ExpressionT,
	"ToDecoderFunction" -> FunctionT,
	"ToPropertyDecoderFunction" -> Defaulting[FunctionT, $Failed&],
	"AvailableProperties" -> Defaulting[ListT[ExpressionT], {}],
	"ArrayDepth" -> Nullable[EitherT[{IntegerT,FunctionT}]],
	"Kind" -> Defaulting[EitherT[{StringT, FunctionT}], Automatic],
	"DecoderToEncoder" -> Defaulting[FunctionT, None&],
	"MaxArgCount" -> Defaulting[IntegerT, Automatic],
	"MinArgCount" -> Defaulting[IntegerT, Automatic],
	"SourceFile" -> Defaulting[StringT, None]
}];


(dec_NetDecoder ? System`Private`HoldEntryQ) := UseMacros @ RuleCondition @ CatchFailureAsMessage[NetDecoder, CreateDecoder[dec]];
(dec_NetDecoder ? System`Private`NoEntryQ)[input_] := CatchFailureAsMessage @ DecoderApply[dec, input, Automatic];
(dec_NetDecoder ? System`Private`NoEntryQ)[input_, prop_] := CatchFailureAsMessage @ DecoderApply[dec, input, prop];
NetDecoder /: Normal[(dec_NetDecoder ? System`Private`NoEntryQ)] := CoderExtract[dec, All];
NetDecoder /: Part[(dec_NetDecoder ? System`Private`NoEntryQ), part_] := CoderExtract[dec, part];
DefineCustomBoxes[NetDecoder, dec_NetDecoder ? System`Private`HoldNoEntryQ :> MakeDecoderBoxes[dec]];

MakeDecoderBoxes[HoldPattern @ NetDecoder[type_, assoc_, _]] :=
	OptimizedArrangeSummaryBox[
		NetDecoder, Nothing, 
		fmtEntries[Prepend["Type" -> type] @ assoc],
		{}
	]

MakeDecoderBoxes[_] := $Failed;