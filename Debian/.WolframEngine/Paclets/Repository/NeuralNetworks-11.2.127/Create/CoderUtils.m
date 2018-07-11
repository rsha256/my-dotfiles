Package["NeuralNetworks`"]


teCache[lang_,ic_] :=
	Replace[
		If[lang=="English", Union[WordList["Stopwords", Language -> "English"], WordList["CommonWords", Language -> "English"]],
			WordList[Language -> lang]
		],
		e_List :> (teCache[lang,ic] = ToTokenEncodingData[{e, IgnoreCase-> ic }])
	];


$validLanguages = {"English", "Spanish", "French", "German"};


PackageScope["NormalizeCharacterSpec"]
PackageScope["NormalizeCharacterSpecDecoder"]

(*
we need to duplicate the IgnoreCase information into the first positional argument, which is the
value of $Encoding, so that ToCharacterEncodingData knows how to do its job. We must also convert
IgnoreCase into a string, because the actual parameter key (used only for display purposes) is a string.
*)
NormalizeCharacterSpec = MatchValues[
	{"Characters", IgnoreCase -> ic_} :=
		% @ {"Characters", Automatic, IgnoreCase -> ic};
	{"Characters", spec_, IgnoreCase -> ic_} :=
		% @ {"Characters", spec, "Index", IgnoreCase -> ic};
	{"Characters", args___} :=
		% @ {"Characters", args, IgnoreCase -> False};
	{"Characters", spec_, form_String, IgnoreCase -> ic_} :=
		{"Characters", Append[ToList[spec], IgnoreCase -> ic], form, "IgnoreCase" -> ic};
	other_ := other;
];
NormalizeCharacterSpecDecoder := NormalizeCharacterSpec[#][[{1, 2, 4}]] &

PackageScope["ParseCharacterEncodingSpec"]

ParseCharacterEncodingSpec[c_CharacterEncodingData] := c;
ParseCharacterEncodingSpec[{c_CharacterEncodingData, ("IgnoreCase"|IgnoreCase -> _)}] := c;
(* ^ only happens when doing EncoderToDecoder of a premade coder *)

ParseCharacterEncodingSpec[spec_] :=
	MXNetLink`ToCharacterEncodingData[spec, stripIC[spec]];

stripIC[e_] := Replace[e /. (IgnoreCase -> _) -> Nothing, {o_} :> o];


PackageScope["NormalizeTokenSpec"]

NormalizeTokenSpec = MatchValues[
	{"Tokens", tokens_, args___, IgnoreCase|"IgnoreCase"-> ic_} :=
		{"Tokens", {tokens, IgnoreCase-> ic}, args, IgnoreCase-> ic};
	{"Tokens", IgnoreCase|"IgnoreCase"-> ic_} :=
		{"Tokens", {"English", IgnoreCase-> ic}, IgnoreCase-> ic};
	{"Tokens", args___} :=
		% @ {"Tokens", args, IgnoreCase -> True};
	other_ := other;
];

PackageScope["ToTokenEncodingData"]
PackageExport["TokenEncodingData"]

$TokenEncodingDataCache = <||>;

ToTokenEncodingData[{ta_TokenEncodingData, IgnoreCase -> (ic:True|False)}] := ta;

ToTokenEncodingData[{lang_String, IgnoreCase -> (ic:True|False)}] /; MemberQ[$validLanguages, lang] := 
	Replace[teCache[lang, ic], {
		ta_TokenEncodingData :> ta,
		_ :> FailCoder["Could not obtain a word list for ``.", lang]
	}];	

ToTokenEncodingData[{lang_String, IgnoreCase -> (ic:True|False)}] /; MemberQ[$icidLanguages, lang] := teCache[lang,ic];

ToTokenEncodingData[{tokens_List ? StringVectorQ, IgnoreCase -> True}] := ToTokenEncodingData[{ ToLowerCase @ tokens, IgnoreCase -> False }];
ToTokenEncodingData[{tokens_List ? StringVectorQ, IgnoreCase -> False}] := Scope[
	uniqueTokens= DeleteDuplicates @ tokens;
	TokenEncodingData[
		1,
		If[ByteCount[uniqueTokens] < 256, uniqueTokens, CompressToByteArray[uniqueTokens]], 
		Length[uniqueTokens]
	]
];

ToTokenEncodingData[{_, IgnoreCase -> (ic:True|False)}] := FailCoder[
	"Tokens should be a list of strings or one of ``.", 
	$validLanguages];
ToTokenEncodingData[___] := FailCoder["Input arguments are not valid. Check your syntax"];


TokenEncodingData /: MakeBoxes[TokenEncodingData[1, data_, len_], StandardForm] :=
	ToBoxes @ Style[Skeleton[Row[{len, " strings"}]], ShowStringCharacters -> False];

TokenEncodingData /: Normal[TokenEncodingData[1, data_, _]] := 
	If[ByteArrayQ[data], UncompressFromByteArray[data], data];

TokenEncodingData /: Length[TokenEncodingData[1, _, len_]] := len;


PackageScope["TokenEncode"]
PackageScope["MakeTokenDictionary"]

MakeTokenDictionary[tokens_List] := 
	Data`UnorderedAssociation @ MapIndexed[
		Hash[#1] -> First[#2]&, 
		tokens
	];

TokenEncode[dict_, patt_, casefunc_][input_] := Scope[
	If[!StringQ[input], EncodeFail["input was not a string"]];
	inputTokens = Map[Hash, TokenizeIntoWords[casefunc @ input, patt]];
	Lookup[dict, inputTokens, Length[dict]+1]
];

(* SplitPattern option accepts the same argument type as StringSplit: StringMatchQ and/or rule(s) of conversion *)
PackageScope["StringPatternOrRulesQ"]
StringPatternOrRulesQ[e_] := StringPatternQ @ Cases[ToList[e], Except[(_?GeneralUtilities`StringPatternQ -> _String) | (_?GeneralUtilities`StringPatternQ :> _)]];

PackageScope["TokenizeIntoWords"]
PackageScope["$DefaultTokenSplitPattern"]

(* Note: using {WordBoundary, x:PunctuationCharacter :> x} as default gives a bad display in the Notebook *)
$DefaultTokenSplitPattern = WordBoundary; 
TokenizeIntoWords[input_, patt_] :=
	DeleteCases[StringTrim @ StringSplit[input, patt], ""];


PackageScope["$currentCoderHead"]
PackageScope["$currentCoderType"]
PackageScope["$coderFormString"]

PackageScope["FailCoder"]

$coderFormString := StringForm["``[{\"``\", \[Ellipsis]}]", $currentCoderHead, $currentCoderType];
General::invcdrarg = "Invalid argument for ``: ``"; 


FailCoder[reason_] := ThrowFailure["invcdrarg", $coderFormString, fromStringForm[reason]];
FailCoder[reason_String, args__] := FailCoder @ StringForm[reason, args];


PackageScope["CoderParamTypes"]

CoderParamTypes[NetEncoder[kind_, ___]] := $EncoderData[kind, "Parameters"]
CoderParamTypes[NetDecoder[kind_, ___]] := $DecoderData[kind, "Parameters"]


PackageScope["CoderExtract"]

CoderExtract[coder_, All] := Scope[
	ktypes = CoderParamTypes[coder];
	IMap[extractAs[ktypes[#1], #2]&, CoderData[coder]]
];

extractAs[ListT[_, ExpressionT], value_] := value;
extractAs[_, value_] := FromInternalValue[value];

CoderExtract[coder_, part_] := If[StringQ[part], 
	extractAs[
		CoderParamTypes[coder][part], 
		CoderData[coder][part]
	],
	ThrowFailure["pspec1", part]
];


PackageScope["ListMaxIndex"]

ListMaxIndex = Compile[{{values, _Real, 2}},
	Map[If[Length[#] === 0, 0, First @ Ordering[#, -1]]&, values]
];

PackageScope["MaxIndex"]

MaxIndex[input_] := First @ Ordering[input, -1]


PackageScope["DepthWrapper"]

DepthWrapper[f_][in_] := Switch[ArrayDepth[in], 1, f[{in}], 2, f[in], _, $Failed];

