Package["NeuralNetworks`"]



(* Type aliases provide a mechanism to declare new types that are really just
names for other types, e.g. ColorSpaceT = EnumT[{"RGB", "HSV", ...}]. It's up
to APIs like Coerce and so on to hook into this mechanism.
*)


PackageScope["$TypeAliasPatterns"]

$TypeAliasPatterns = Alternatives[];
$InvalidTypeAlias = _ :> Panic["InvalidTypeAliasUsage"];
$TypeAliasRules = {$InvalidTypeAlias};

PackageScope["DeclareTypeAlias"]

DeclareTypeAlias[rule:RuleDelayed[new_, old_]] := (
	$TypeAliasRules[[-1]] = rule;
	AppendTo[$TypeAliasRules, $InvalidTypeAlias];
	AppendTo[$TypeAliasPatterns, new];
);
DeclareTypeAlias[_] := Panic["InvalidAlias"];

PackageScope["TypeAliasP"]
PackageScope["ResolveAlias"]

TypeAliasP := _ ? (MatchQ[$TypeAliasPatterns]);
ResolveAlias[p_] := Replace[p, $TypeAliasRules];

PackageExport["TypeT"]
PackageExport["TypeExpressionT"]

NNSetUsage @ "TypeT represents another type, like IntegerT or TensorT[$$]."
NNSetUsage @ "TypeExpressionT represents a type expression, which may involve types that reference ports."


PackageExport["ValidatedParameter"]

NNSetUsage @ "
ValidatedParameter[expr$] represents a user-provided spec that has been validated by a ValidatedParameterT.
"

PackageExport["ValidatedParameterT"]

NNSetUsage @ "
ValidatedParameterT[f$] represents an expression that should be constructed from user input by f$, then wrapped in ValidatedParameter.
ValidatedParameterT[f$, d$] specifies the default value should be ValidatedParameter[d$].
"


PackageExport["NormalizedT"]

NNSetUsage @ "
NormalizedT[t$, f$] represents an expression of type t$ that is constructed from user input by f$.
NormalizedT[t$, f$, d$] specifies the default value should be given by f$[d$].
* NormalizedT is not actually a type, it is sugar that gets stripped off and used to populate the ParameterCoercions field.
"


PackageScope["RepeatedInteger"]

NNSetUsage @ "RepeatedInteger[n$] is an integer repeated as many times as contextually necessary."


PackageScope["ArbSizeListT"]

NNSetUsage @ "
ArbSizeListT[rank$, def$] is a shortcut for a fairly complex NormalizedT that handles kernel, padding, \
etc. sizes for conv and pool layers. It remains symbolic if not given as a list and will later \
expand to the right rank. 
* rank$ should be the parameter that actaully stores the rank.
* def$ is an integer to be used by default (repeated contextually), or None.
"

ArbSizeListT[rank_, type_, def_] := NormalizedT[ListT[rank, type], ToIntTuple, RepeatedInteger[def]];
ArbSizeListT[rank_, type_, None] := NormalizedT[ListT[rank, type], ToIntTuple];


PackageExport["PosIntegerT"]
PackageExport["IntegerT"]
PackageExport["NaturalT"]
PackageExport["RealT"]
PackageExport["AtomT"]
PackageExport["IndexIntegerT"]
PackageExport["IntervalScalarT"]

NNSetUsage @ "PosIntegerT represents a positive integer."
NNSetUsage @ "IntegerT represents an integer."
NNSetUsage @ "NaturalT represents a non-negative integer."
NNSetUsage @ "IndexIntegerT[max$] represents an integer in the range [1,max$]."
NNSetUsage @ "IntervalScalarT[min$,max$] represents a real number in the range [min$,max$]."
NNSetUsage @ "AtomT represents any of the types accepted as the element of a TensorT.
* For now, these are only RealT and IndexIntegerT."


PackageExport["ChannelT"]

ChannelT[n_, t_] := TensorT[{n}, t];


PackageExport["TensorT"]

NNSetUsage @ "
TensorT[dims$] represents a real-valued tensor of dimensions dims$.
TensorT[dims$,type$] represents a tensor of dimensions dims$ whose elements are of type type$.
* The dims$ can be:
| SizeListT[n$] | n$ unknown dimensions |
| {d$1,$$,d$n} | explicit dimensions, each being an integer or SizeT |
* The type$ can be:
| RealT | floating point value |
| IndexIntegerT[n$] | integers in [1,n] |
| TensorT[$$] | a sub-tensor |
* TensorT[{a$$}, TensorT[{b$$}, type$]] automatically flattens to TensorT[{a$,b$},type$]
"

TensorT[{}, t:(_TensorT | _NetEncoder | _NetDecoder)] := t;
TensorT[dims1_List, TensorT[ListT[n_Integer, _], type_]] := TensorT[Join[dims1, ConstantArray[SizeT, n]], type];
TensorT[dims1_List, TensorT[dims2_List, type_]] := TensorT[Join[dims1, dims2], type];
TensorT[ListT[n1_, SizeT], TensorT[ListT[n2_, SizeT], t_]] /; (Head[n1] === Head[n2]) := 
	TensorT[ListT[If[IntegerQ[n1], n1 + n2, NaturalT], SizeT], t];

PackageScope["TRoll"]
PackageScope["TUnroll"]

TRoll[a_, b___] := TensorT[Replace[a, {i_Integer :> {i}, All -> SizeListT[]}], TRoll[b]];
TRoll[b_] := b;

TUnroll[TensorT[d_List, p:TTypeP]] := {d, p}; (* <- fast path *)
TUnroll[$Failed] := $Failed;
TUnroll[t_] := unroll[t] //. {L___, a_List, b_List, R___} :> {L, Join[a, b], R};

unroll[TensorT[t1_, t2_]] := Prepend[
	unroll[t2],
	Replace[t1, ListT[n_, z_] :> If[IntegerQ[n], Table[z, n], All]]
];

unroll[t_] := {t};

PackageScope["TToNormalForm"]
PackageScope["TFromNormalForm"]

(* NormalForm is {dimsOnLeft, hasArbitraryGap, dimsOnRight, innerType} *)

midP = All | _RepeatedInteger;
TToNormalForm[t_TensorT] := Match[TUnroll[t],
	{a_List, it_} :> {a, False, {}, it},
	{a_List, All, it_} :> {a, True, {}, it},
	{All, it_} :> {{}, True, {}, it},
	{All, b_List, it_} :> {{}, True, b, it},
	{a_List, All, b_List, it_} :> {a, True, b, it},
	{a_List, r_RepeatedInteger, it_} :> {a, r, {}, it},
	z_ /; !FreeQ[z, RepeatedInteger] :> (Print[z]; Panic["UnsupportedRepeatedIntegerRole", "``", z]),
	{$Failed, False, $Failed, $Failed}
];


TFromNormalForm[{a_List, False, _, it_}] := TensorT[a, it];
TFromNormalForm[{a_List, True, {}, it_}] := TensorT[a, TensorT[SizeListT[], it]];
TFromNormalForm[{a_List, True, b_List, it_}] := TensorT[a, TensorT[SizeListT[], TensorT[b, it]]];
TFromNormalForm[e:{_, rp_RepeatedInteger, _, _}] := TFromNormalForm[e /. rp -> True] /. SizeListT[] -> rp;


PackageExport["ScalarT"]

NNSetUsage @ "ScalarT represents a 0-rank tensor, and is equivalent to TensorT[{}, RealT]."

ScalarT = TensorT[{}, RealT];


PackageExport["NetT"]

NNSetUsage @ "NetT[<|'inname$'->intype$,$$|>, <|'outname$'->outtype$,$$|>] represents a net that has a specific set of inputs and outputs of specific types."

PackageExport["RuleT"]
PackageExport["FunctionT"]
PackageExport["MatchT"]
PackageExport["StructT"]
PackageExport["AssocT"]
PackageExport["EnumT"]
PackageExport["StringT"]
PackageExport["BooleanT"]
PackageExport["SizeT"]

NNSetUsage @ "RuleT[lhs$, rhs$] represents a rule whose LHS is of type lhs$ and whose RHS is of type $rhs."
NNSetUsage @ "FunctionT represents an expression with head Function."
NNSetUsage @ "MatchT[patt$] represents an expression which matches patt$."
NNSetUsage @ "StructT[{key$1->type$1,$$}] represents an association with keys key$i whose values match type$i."
NNSetUsage @ "AssocT[ktype$,vtype$] represents an association whose keys have type ktype$ and values have type vtype$."
NNSetUsage @ "EnumT[{val$1,val$2,$$}] represents an expression whose values are one of the literal expressions val$i."
NNSetUsage @ "StringT represents a string."
NNSetUsage @ "BooleanT represents the symbols True and False."
NNSetUsage @ "SizeT represents a positive integer used as a 'size' of some sort."

PackageScope["ComputedType"]

NNSetUsage @ "
ComputedType[type$,expr$,{dep$1,$$}] represents a value of form type$ that is computed by expr$, and depends on ports dep$i.
ComputedType[type$,expr$,{dep$1,$$},trigger$] only attempts to compute expr$ when trigger$ is satisfied.
"

PackageScope["SwitchedType"]

NNSetUsage @ "
SwitchedType[port$,value$1->output$1,value$2->output$2,$$] allows a port or parameter to be one of several \
different types depending on the value of another port. Backward inference still works on the chosen type,
unlike with ComputedType (because CT strongly evaluates its body, replacing all NetPaths before unifying).
SwitchedType[port$,rules$$,basetype$] specifies that the base type of the associated port should be \
basetype$ (the default base type is TypeT).
"

SwitchedType[port_, rules___Rule] := 
	TypeReplace[port, {rules, _ -> TypeT}];

SwitchedType[port_, rules___Rule, fallback:Except[_Rule]] := 
	TypeReplace[port, {rules, _ -> fallback}];


PackageScope["TypeReplace"]


PackageExport["EitherT"]

NNSetUsage @ "EitherT[{type$1,type$2,$$}] represents a value of one of the type$i."


PackageExport["ListT"]

NNSetUsage @ "
ListT[type$] represents a list of any length, whose values are of type type$.
ListT[n$, type$] represents a list of length n$.
"

ListT[type_] := ListT[NaturalT, type];


PackageExport["ArrayCasesT"]

NNSetUsage @ "
ArrayCasesT[type$] represents a type$ or a list of rules mapping layer specs to type$.
ArrayCasesT[type$, default$] gives the default value for the whole net or for those layers that don't match.
"



PackageExport["SizeListT"]

NNSetUsage @ "
SizeListT[] represents a list of positive integers.
SizeListT[n$] represents a list of n$ positive integers.
SizeListT[$$] evaluates to a ListT.
"

SizeListT[n_] := ListT[n, SizeT];
SizeListT[] := ListT[NaturalT, SizeT];


PackageExport["SequenceT"]
PackageExport["LengthVar"]

NNSetUsage @ "
LengthVar[id$] represents a unique variable representing an unknown sequence.
"

PackageScope["NewLengthVar"]

NNSetUsage @ "
NewLengthVar[] makes a LengthVar[$$] containing a random id.
"

NewLengthVar[] := LengthVar[RandomInteger[2^31]]


PackageScope["GetLengthVarID"]

GetLengthVarID[VarSequenceP[id_]] := id;
GetLengthVarID[coder:CoderP] := GetLengthVarID[CoderType[coder]];
GetLengthVarID[_] := $Failed;


PackageScope["MakeVarSeq"]

MakeVarSeq[t_] := SequenceT[NewLengthVar[], t];


PackageExport["MatrixT"]

NNSetUsage @ "
MatrixT[rows$,cols$] evaluates to TensorT[{rows$,cols$}, RealT].
"

MatrixT[] := TensorT[{SizeT, SizeT}, RealT];
MatrixT[rows_, cols_] := TensorT[{rows, cols}, RealT];


PackageExport["VectorT"]

NNSetUsage @ "
VectorT[n$] evaluates to TensorT[{n}, RealT].
VectorT[] evaluates to TensorT[{SizeT}, RealT].
"

VectorT[] := TensorT[{SizeT}, RealT];
VectorT[size_] := TensorT[{size}, RealT];


PackageExport["ImageT"]

NNSetUsage @ "
ImageT[] represents an image.
ImageT[dims$] represents an image of dimensions dims$.
ImageT[dims$,cspace$] represents an image with color space cspace$.
"

ImageT[] := ImageT[SizeListT[2], ColorSpaceT]; 


PackageExport["ColorSpaceT"]

NNSetUsage @ "ColorSpaceT represents a color space string ('RGB', 'Grayscale', etc.)."

PackageScope["$ColorSpaces"]

$ColorSpaces = {"Grayscale", "RGB", "CMYK", "HSB", "XYZ", "LAB", "LCH", "LUV", Automatic};
DeclareTypeAlias[ColorSpaceT :> EnumT[$ColorSpaces]];


PackageExport["UnaryElementwiseFunctionT"]

NNSetUsage @ "UnaryElementwiseFunctionT is actually a shortcut for a ValidatedParameterT that
checks the input is one of the $PrimitiveUnaryElementwiseFunctions, or a one-input ScalarFunction."

UnaryElementwiseFunctionT = ValidatedParameterT[ToUnaryElementwiseFunction]


PackageExport["BinaryElementwiseFunctionT"]

NNSetUsage @ "BinaryElementwiseFunctionT is actually a shortcut for a ValidatedParameterT that 
checks the input is one of the $PrimitiveBinaryElementwiseFunctions, or a two-input ScalarFunction."

BinaryElementwiseFunctionT = ValidatedParameterT[ToBinaryElementwiseFunction]


PackageScope["PoolingFunctionT"]

NNSetUsage @ "PoolingFunctionT represents a pooling function for PoolingLayer (one of Mean, Max, Total)."

$PoolingFunctions = {Mean, Max, Total}
DeclareTypeAlias[PoolingFunctionT :> EnumT[$PoolingFunctions]];


PackageExport["ExpressionT"]

NNSetUsage @ "ExpressionT represents any expression."


PackageExport["DistributionT"]

NNSetUsage @ "DistributionT represents a univariate distribution."


PackageExport["Defaulting"]

NNSetUsage @ "Defaulting[type$,value$] represents an expression of type type$ that defaults to value$."

Defaulting[ValidatedParameterT[f_], v_] := ValidatedParameterT[f, v];
Defaulting[t_ListT] := Defaulting[t, {}];
Defaulting[t_AssocT] := Defaulting[t, <||>];
Defaulting[t_EnumT] := Defaulting[t, t[[1,1]]];
Defaulting[t_Nullable] := Defaulting[t, None];
Defaulting[t_] := Defaulting[t, None];


PackageExport["Nullable"]

NNSetUsage @ "Nullable[type$] represents a type type$ that can also be None."


PackageExport["EncodedType"]
PackageExport["DecodedType"]

NNSetUsage @ "EncodedType[encoder$,type$] is deprecated."
NNSetUsage @ "DecodedType[decoder$,type$] is deprecated."


PackageScope["ComplainType"]

General::invindata1 = "Data supplied to port \"``\" was ``, but expected ``.";
General::invindata2 = "Data supplied to port \"``\" was not ``.";

(* TODO: split this into batched and non-batched versions, the batched version
should search in the input data to find the element that didn't match and report it.
also deal with sequences etc. *)
ComplainType[name_, type_][input_] := Scope[
	dims = MachineArrayDimensions[input];
	If[FailureQ[dims], 
		ThrowFailure["invindata2", name, TypeString[type]],
		ThrowFailure["invindata1", name, TypeString[TensorT[dims]], TypeString[type]]
	]
];


PackageScope["SequenceOf"]

PackageScope["ToT"]

NNSetUsage @ "
ToT[spec$] converts a user-provided spec into an internal type.
ToT[spec$, mode$] will create encoders or decoders from strings.
* If mode$ is NetEncoder, encoders will be created, otherwise decoders.
"

$iomode = None;
ToT[t_, mode_:None] := Block[{$iomode = mode}, toT[t]];

toT = MatchValues[
	n_Integer := VectorT[n];
	n:{Repeated["Varying", {0,1}], RepeatedNull[_Integer|Automatic|_LengthVar]} := toTensor[n, RealT];
	{n:(_Integer|Automatic|"Varying"), p:CoderP} := toTensor[{n}, %[p]];
	"Varying" := TensorT[{NewLengthVar[]}, RealT];
	"Real" := ScalarT;
	"Integer" := IndexIntegerT[SizeT];
	enc_NetEncoder := If[$iomode === NetEncoder, enc, $Failed];
	dec_NetDecoder := If[$iomode === NetDecoder, dec, $Failed];
	HoldPattern[Restricted["Integer"|Integer, n_Integer]] /; Positive[n] := IndexIntegerT[n];
	HoldPattern[(RepeatingElement|SequenceOf)[el_]] := SequenceT[LengthVar[0], %[el]];
	spec_String := If[$iomode === None, $Failed, checkF @ $iomode[spec]];
	$Raw[t_] := t;
	Automatic := TypeT;
	t_ ? ValidTypeQ := t;
	$Failed
];

toTensor[_, $Failed] := $Failed;
toTensor[spec_, inner_] := TensorT[
	ReplaceAll[spec, {Automatic -> SizeT, "Varying" :> NewLengthVar[]}],
	inner
];

checkF[f_Failure] := ThrowRawFailure[f];
checkF[e_] := e;

PackageScope["FromT"]

NNSetUsage @ "
FromT[spec$] converts an internal type back into its equivalent user-providable spec.
"

FromT = MatchValues[
	VectorT[n_Integer] := n;
	ScalarT := "Real";
	IndexIntegerT[SizeT] := "Integer";
	TensorT[t_List, p:CoderP] := Append[fromTensor[t], p];
	TensorT[t_List, _] := fromTensor[t];
	enc_NetEncoder := enc;
	dec_NetDecoder := dec;
	IndexIntegerT[n_Integer] := Restricted["Integer", n];
	Automatic
]

fromTensor[dims_List] := ReplaceAll[dims, {SizeT -> Automatic, _LengthVar -> "Varying"}];
