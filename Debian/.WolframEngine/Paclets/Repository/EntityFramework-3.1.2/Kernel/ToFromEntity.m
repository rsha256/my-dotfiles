(* Mathematica package *)

System`ToEntity;
System`FromEntity;

(Unprotect[#];Clear[#];)& /@ {
	ToEntity,
	FromEntity
}

Begin["EntityFramework`EntityTransformations`"];

$tag = "EFETCatchThrowFlag";
$iTEHeld = True;

(*================ Utility functions =======================*)

CanonicalForm[g_] := Module[{h = System`CanonicalGraph[g]},
    AdjacencyList[h, #] & /@ VertexList[h]
]

initGraphData[] := Module[{},
	Set[$GDInit,True];
	GraphData[]; (* to load the definition of $GraphCanonicalForms *)
]


assoc := assoc = If[ValueQ[DataPaclets`GraphDataDump`$GraphCanonicalForms32],
	Association[
   Thread[If[$SystemWordLength === 64, DataPaclets`GraphDataDump`$GraphCanonicalForms64,
DataPaclets`GraphDataDump`$GraphCanonicalForms32] -> 
     DataPaclets`GraphDataDump`$GraphAll]],
 Association[
   Thread[DataPaclets`GraphDataDump`$GraphCanonicalForms -> 
     DataPaclets`GraphDataDump`$GraphAll]]
]

SetAttributes[ToEntityColorData, HoldFirst]

ToEntityColorData[RGBColor[r_, g_, b_]] :=  Entity["Color", {"RGB", {r, g, b}}]
ToEntityColorData[RGBColor[r_, g_, b_, \[Alpha]_]] := Entity["Color", {"RGB", {r, g, b}}]
ToEntityColorData[CMYKColor[c_, m_, y_, k_]] :=  Entity["Color", {"CMYK", {c, m, y, k}}]
ToEntityColorData[XYZColor[X_, Y_, Z_]] :=  Entity["Color", {"XYZ", {X, Y, Z}}]
ToEntityColorData[LABColor[L_, a_, b_]] :=  Entity["Color", {"CIE1976Lab", 100*{L, a, b}}]
ToEntityColorData[LUVColor[L_, u_, v_]] :=  Entity["Color", {"Luv", 100*{L, u, v}}]
ToEntityColorData[GrayLevel[frac_]]:=Entity["Color",{"GrayLevel",{frac}}]
ToEntityColorData[Hue[h_]] := Entity["Color", {"HSB", {h, 1, 1}}]
ToEntityColorData[Hue[h_, s_, b_, \[Alpha]_]] := Entity["Color", {"HSB", {h, s, b}}]
ToEntityColorData[Red] = Entity["Color",{"WolframLanguage","Red"}]
ToEntityColorData[Green] = Entity["Color",{"WolframLanguage","Green"}]
ToEntityColorData[Blue] = Entity["Color",{"WolframLanguage","Blue"}]
ToEntityColorData[Black] = Entity["Color",{"WolframLanguage","Black"}]
ToEntityColorData[White] = Entity["Color",{"WolframLanguage","White"}]
ToEntityColorData[Gray] = Entity["Color",{"WolframLanguage","Gray"}]
ToEntityColorData[Cyan] = Entity["Color",{"WolframLanguage","Cyan"}]
ToEntityColorData[Magenta] = Entity["Color",{"WolframLanguage","Magenta"}]
ToEntityColorData[Yellow] = Entity["Color",{"WolframLanguage","Yellow"}]
ToEntityColorData[Brown] = Entity["Color",{"WolframLanguage","Brown"}]
ToEntityColorData[Orange] = Entity["Color",{"WolframLanguage","Orange"}]
ToEntityColorData[Pink] = Entity["Color",{"WolframLanguage","Pink"}]
ToEntityColorData[Purple] = Entity["Color",{"WolframLanguage","Purple"}]

(**LCH color is currently (as of Apr 1,2014) not supported in Alpha.Will be a TODO item for Meng Lu.**)
(*ToEntityColorData[LCHColor[L_, c_, h_]] := $Failed*)
ToEntityColorData[other_] := (Message[ToEntity::noentp, other, "Color"]; $Failed)

EntityColorToWL[{"RGB", c: {r_,g_,b_} /; r>1 || g>1 || b>1}] := RGBColor@@(c/255)
EntityColorToWL[{"RGB", c: {_?(0<=#<=1&), _?(0<=#<=1&), _?(0<=#<=1&)}}] := RGBColor@@c
EntityColorToWL[{"CMYK", c: {c_?(0<=#<=1&), m_?(0<=#<=1&), y_?(0<=#<=1&), k_?(0<=#<=1&)}}] := CMYKColor@@c
EntityColorToWL[{"XYZ", c: {_?(0<=#<=1&), _?(0<=#<=1&), _?(0<=#<=1&)}}] := XYZColor@@c
EntityColorToWL[{"CIE1976Lab", c: {L_?(0<=#<=100&), a_?(-127<=#<=128&), b_?(-127<=#<=128&)}}] := LABColor@@(c/100)
EntityColorToWL[{"Luv", c: {L_?(0<=#<=100&), u_?(-100<=#<=100&), v_?(-100<=#<=100&)}}] := LUVColor@@(c/100)
EntityColorToWL[{"GrayLevel", {frac_?(0<=#<=1&)}}] := GrayLevel[frac]
EntityColorToWL[{"HSB", c: {_?(0<=#<=1&), _?(0<=#<=1&), _?(0<=#<=1&)}}] := Hue@@c
EntityColorToWL[{"Mathematica",c_String}] := Symbol[c]
EntityColorToWL[{"WolframLanguage",c_String}] := Symbol[c]
EntityColorToWL[{scheme:("HTML"|"Legacy"|"Crayola"),name_}] := ColorData[scheme][name]

EntityColorToWL[{"Hex", name_String /; StringMatchQ[name, RegularExpression["#[a-fA-F0-9]+"]]}] := With[
    {hex = StringReplace[name, "#" -> ""]},
    RGBColor @@ (IntegerDigits[FromDigits[hex, 16], 256, 3]/255)
]

EntityWLToWL[s_Symbol] := s
EntityWLToWL[s_String] := ToExpression[s]
EntityWLToWL[___] := "ActuallyFailed" (*don't use $Failed because Entity["WolframLanguageSymbol", $Failed] is actually a thing *)
(*

Alternatively, it might be interesting to convert a hex number 
(in the sub set of 216 "Web safe color" c.f. http://websafecolors.info/) 
to ColorData["WebSafe"][index]:

hexToWebSafeColorIndex[
    hex_String /; StringMatchQ[hex, RegularExpression["#[a-fA-F0-9]+"]]
]:= Module[
    {r, g, b, webSafeColorIndex},
    {r, g, b} = IntegerDigits[FromDigits[hex, 16], 256, 3];
    {r, g, b} = 5 - {r, g, b}/51;
    webSafeColorIndex = r * 6^2 + g * 6 + b + 1;
    If[IntegerQ[r]&&IntegerQ[g]&&IntegerQ[b], webSafeColorIndex, $Failed]
]
 
EntityColorToWL[{"Hex", name_}] := With[
    {index = hexToWebSafeColorIndex[name]},
    If[IntegerQ[index], ColorData["WebSafe"][index], $Failed]
];

*)


EntityColorToWL[input:{_String,_}] := With[
    {r=EntityValue[Entity["Color",input],"Value"]/.HoldForm->Identity},
    If[MatchQ[r,RGBColor[__]|CMYKColor[__]], r, $Failed]
]

EntityColorToWL[__] := $Failed

SetAttributes[symbolQ, HoldFirst];

symbolQ[(Hold|HoldComplete)[expr_]] := symbolQ[expr]
symbolQ[expr_] := With[{form = expr},
	And[
		AtomQ[form], 
		MatchQ[form, _Symbol],
		SymbolName[form] === SymbolName[expr]]
]

makeMFDPatternEntityPairs[] := Module[
	{ents = EntityValue["MathematicalFunction", "Entities"], p0, p1, p2, p3, p4, p5, p6, p7, p8, p9},
	
	(* fundamental argument patterns *)
	p0 = ReplaceRepeated[
		Transpose[{EntityValue[ents, "ArgumentPattern"], ents}],
		Inactive[head_][vars___] :> HoldPattern[head[vars]]
	];
	(* context-free arqument patterns for non-System` symbols *)
	p1 = Cases[
		p0,
		{Verbatim[HoldPattern][s_Symbol?(Context[#] =!= "System`" &)[args___]], ent_} :> {HoldPattern[_Symbol?(And[symbolQ[#], SymbolName[#] === SymbolName[s]] &)[args]], ent},
		{0, Infinity}, Heads -> True
	];
	(* arg patterns with PatternTests removed *)
	p2 = Cases[
		Join[p0, p1],
		{Verbatim[HoldPattern][head_[most___, Verbatim[PatternTest][pat_, test_], rest___]], ent_},
		{0, Infinity}
	] //. Verbatim[PatternTest][pat_, test_] -> pat;
	(* arg patterns with PatternTests AND head specifiers removed *)
	p3 = Select[p0, MemberQ[#, Verbatim[Blank][Except[Null]], Infinity, Heads -> True] &] //. {
		Verbatim[Blank][_] -> Blank[],
		Verbatim[PatternTest][pat_, test_] -> pat,
		Verbatim[Alternatives][first_, rest___] -> first
	};
	(* arg patterns with explicit (integer) values for some arguments removed *)
	p4 = MapAt[
		ReplaceAll[#, _Integer -> Blank[]] &, 
		Select[ReplaceRepeated[Join[p0, p1], Verbatim[PatternTest][pat_, test_] :> pat], 
			MemberQ[#, _[__, _Integer, ___] | _[___, _Integer, __], Infinity] &
		], {All, 1}];
	(* patterns based on head with any arg structure *)
	p5 = Join[p0, p1] //. Verbatim[HoldPattern][head_[___]] :> HoldPattern[head[___]];
	(* bare head patterns *)
	p6 = p0 //. Verbatim[HoldPattern][head_[___]] :> HoldPattern[head];
	p7 = p1 //. {Verbatim[HoldPattern][Verbatim[PatternTest][pat_, test_][args___]], ent_} :> {HoldPattern[PatternTest[pat, test]], ent};
	(* bare head string patterns *)
	p8 = p0 //. Verbatim[HoldPattern][head_[___]] :> HoldPattern[head // ToString // Evaluate];
	p9 = Cases[p0, {Verbatim[HoldPattern][s_Symbol?(Context[#] =!= "System`" &)[args___]], ent_} :> {HoldPattern[SymbolName[s] // Evaluate], ent}, {0, Infinity}, Heads -> True];
	
	Set[MFDPatternEntityPairs, Join[p0, p1, p2, p3, p4, p5, p6, p7, p8, p9] // Sort // Reverse // DeleteDuplicates]
]

MFDPatternEntityPairs := makeMFDPatternEntityPairs[]

WLSymbolNames := WLSymbolNames = EntityValue["WolframLanguageSymbol", "EntityCanonicalNames"]

(*================ Main functions =======================*)
SetAttributes[ToEntity, HoldFirst]

ToEntity[args___] := With[{res=Catch[iToEntity[args],$tag]},
	res/;res=!=$Failed]
	
SetAttributes[iToEntity, HoldFirst]

iToEntity[e:Entity[type_String,_], type_String] := e

iToEntity[g_Graph] := iToEntityGraphHandler[g]

iToEntity[char_String]/;SameQ[StringLength[char],1] := iToEntityCharacterHandler[char]

iToEntity[e:(_RGBColor | _CMYKColor | _XYZColor | _LABColor | _LUVColor | _GrayLevel)] := ToEntityColorData[e]
iToEntity[e:(Red | Green | Blue | Black | White | Gray | Cyan | Magenta | Yellow | Brown | Orange | Pink | Purple)] := ToEntityColorData[e]

iToEntity[e_Entity] := e
(*check context before passing on to avoid snarfing inputs like x = CompleteGraph[4]; ToEntity[x] *)
iToEntity[s_Symbol] /; MemberQ[WLSymbolNames, wlSymbol2String[s]] := iToEntityWolframLanguageSymbolHandler[s]
(*default to WolframLanguageSymbol interpretation in single-arg case; prevents steals by MFD*)
iToEntity[s_String] /; MemberQ[WLSymbolNames, s] := iToEntityWolframLanguageSymbolHandler[s]

(* this next bit is a little funky ...*)
iToEntity[other_] := Module[{},
	initializeMFDPatterns[];
	removeiToEntityDownValue[];
	resetiToEntityDownValue[];
	iToEntity[other]
]
(* the above function initializes the misc patterns from MathematicalFunction, remove it's ownd DownValue,     		*)
(* and then resets a new DownValue with the same patttern to handle generic failures, and finally calls iToEntity.	*)
(* This is done so that the arbitrary list of supported MathematicalFunctions can be caught in both their Held, 	*)
(* and unheld forms. In practice this leaves us with just the DownValue created by resetiToEntityDownValue.			*)

removeiToEntityDownValue[] := Unset[iToEntity[Pattern[other, Blank[]]]];
resetiToEntityDownValue[] := SetDelayed[iToEntity[Pattern[other, Blank[]]], 
	(Message[ToEntity::noent, other]; $Failed)
]

iToEntity[g_, "Graph"] := iToEntityGraphHandler[g]
iToEntity[char_, "Character"] := iToEntityCharacterHandler[char]
iToEntity[e_, "Color"] := ToEntityColorData[e]
iToEntity[expr_, "MathematicalFunction"] := iToEntityMathematicalFunction[expr]
iToEntity[s_, "WolframLanguageSymbol"] := iToEntityWolframLanguageSymbolHandler[s]

iToEntity[other_,args___] /; TrueQ[$iTEHeld] := Block[{$iTEHeld = False},
	(*release hold if input is unmatched*)
	With[{e = other}, iToEntity[e, args]]
]

iToEntity[e_, type_] := (Message[ToEntity::noentp, e, type]; $Failed)

iToEntity[args___] := (System`Private`Arguments[FromEntity[args], {1, 2}]; $Failed)

needsLegacyHashQ[] := And[
	$SystemWordLength === 32, 
	$VersionNumber === 10.1,
	$ReleaseNumber === 1
	
]

getGraphHash[g_] := With[{cf = Quiet @ Check[CanonicalForm[g], $Failed]},
	If[cf === $Failed, Return[cf]];
	If[needsLegacyHashQ[],
		Module[{so=SystemOptions["HashOptions"], res},
			SetSystemOptions["HashOptions" -> {"HashArrayLengthThreshold" -> 32, "HashByteLengthThreshold" -> 128}];
			res = Hash[cf];
			SetSystemOptions[so];
			res
		],
		Hash[cf]
	]
]

iToEntityGraphHandler[g_Graph] := Module[{},
	If[Not[TrueQ[$GDInit]],initGraphData[]];
	With[{res = assoc[getGraphHash[g]]}, 
		If[MatchQ[res, _Missing|$Failed], 
			$Failed, 
			Entity["Graph", res]
		]
	]
]

iToEntityGraphHandler[other_] /; TrueQ[$iTEHeld] := Block[{$iTEHeld = False},(*release hold if input is unmatched*)
 With[{e = other}, iToEntityGraphHandler[e]]
]

iToEntityGraphHandler[other_] := (Message[ToEntity::noentp, other, "Graph"]; $Failed)

iToEntityCharacterHandler[char_String] /;SameQ[StringLength[char],1] := Module[{},
	If[MatchQ[#,{_Integer}],Entity["Character",First[#]],Message[ToEntity::noent,char];$Failed]&[ToCharacterCode[char]]
]

iToEntityCharacterHandler[other_] /; TrueQ[$iTEHeld] := Block[{$iTEHeld = False},(*release hold if input is unmatched*)
 With[{e = other}, iToEntityCharacterHandler[e]]
]

iToEntityCharacterHandler[other_] := (Message[ToEntity::noentp, other, "Character"]; $Failed)

initializeMFDPatterns[] := With[{pats = MFDPatternEntityPairs},
	If[MatchQ[pats, {{_, _Entity}..}],
		Map[makeMFDownValues, pats];
		Set[initializeMFDPatterns[], True];(* only has to initialize once *)
	]
]

SetAttributes[iMathematicalFunctionLookup, HoldFirst];

iMathematicalFunctionLookup[__] = $Failed

transparentMFDWrappers = Verbatim /@ {Hold, HoldForm, HoldComplete, HoldPattern, Defer, Unevaluated}
SetAttributes[iToEntityWolframLanguageSymbolHandler, HoldAllComplete];
iToEntityWolframLanguageSymbolHandler[s_Symbol] /; MemberQ[WLSymbolNames, wlSymbol2String[s]] := With[{sas = wlSymbol2String[s]},
	Entity["WolframLanguageSymbol", sas]
]

iToEntityWolframLanguageSymbolHandler[s_String] /; MemberQ[WLSymbolNames, s] := Entity["WolframLanguageSymbol", s]

iToEntityWolframLanguageSymbolHandler[other_] := (Message[ToEntity::noentp, other, "WolframLanguageSymbol"]; $Failed)

SetAttributes[wlSymbol2String, HoldAllComplete];
wlSymbol2String[s_] := Fold[StringDrop, ToString[HoldComplete[s]], {13, -1}]
makeMFDownValues[{pat_, ent_}] := Module[
	{
		inactivatedPat= pat /. Verbatim[HoldPattern][head_[args___]] :> HoldPattern[Inactive[head][args]]
	},
	Set[iMathematicalFunctionLookup[pat], ent];
	Set[iMathematicalFunctionLookup[#[pat]], ent]& /@ transparentMFDWrappers;
	Set[iMathematicalFunctionLookup[Evaluate[inactivatedPat]], ent];
	If[ ! (MatchQ[pat, Verbatim[HoldPattern][head_Symbol]] || MatchQ[pat, Verbatim[HoldPattern][head_String?(MemberQ[WLSymbolNames, #]&)]]),
		Set[iToEntity[pat], ent];
		Set[iToEntity[#[pat]], ent]& /@ transparentMFDWrappers;
		Set[iToEntity[Evaluate[inactivatedPat]], ent]
	]
]

SetAttributes[iToEntityMathematicalFunction, HoldAll];

iToEntityMathematicalFunction[expr_] := Module[{result},
	initializeMFDPatterns[];
	result = iMathematicalFunctionLookup[expr];
	If[result === $Failed,
		With[{r = expr}, result = iMathematicalFunctionLookup[r]];
		If[ result === $Failed,
			Message[ToEntity::noentp, expr, "MathematicalFunction"];
			$Failed,
			result
		],
		result
	]
]

(********************** FromEntity **************************)
FromEntity[args___] := With[{res=Catch[iFromEntity[args],$tag]},
	res/;res=!=$Failed]

iFromEntity[e:Entity["Graph", gname_]] := Module[{},
	If[Not[TrueQ[$GDInit]],initGraphData[]];
	If[DataPaclets`GraphDataDump`MyGraphQ[gname],
		GraphData[gname, "Graph"],
		With[{r=EntityValue[e,"Graph"]},
			If[GraphQ[r],
				r,
				Message[FromEntity::norep,e];$Failed
			]]
	]
]

iFromEntity[e:Entity["Character",char_Integer]] := Module[{},Quiet[
	Check[FromCharacterCode[char],Message[FromEntity::norep,e];$Failed,{FromCharacterCode::notunicode}],
	{FromCharacterCode::notunicode}]
]

iFromEntity[e:Entity["Color",colordata_]] := Module[{r=EntityColorToWL[colordata]},
	If[UnsameQ[r,$Failed],
		r,
		Message[FromEntity::norep,e];$Failed
	]
]

iFromEntity[e:Entity["WolframLanguageSymbol", s_]] :=  Module[{r=EntityWLToWL[s]},
	If[UnsameQ[r,"ActuallyFailed"],
		r,
		Message[FromEntity::norep,e];$Failed
	]
]

iFromEntity[e:Entity["MathematicalFunction", funcHead_]] := Module[
	{
		i, j, numBlanks, result,
		argPattern = EntityValue[e, "ArgumentPattern"] //. {
			Verbatim[Blank][_] -> Blank[],
			Verbatim[PatternTest][pat_, test_] -> pat,
			Verbatim[Alternatives][args___] :> First[{args}]
		}
	},
	If[MatchQ[argPattern, _Missing], Message[FromEntity::norep,e]; Return[$Failed]];
	numBlanks = Position[argPattern, _Blank] // Length;
	If[ Length[argPattern] === 1, Return[argPattern[[0, 1]]] ];
	result = argPattern;
	For[j = 1, j <= numBlanks, j++,
		i = 0;
		result = result /. (Verbatim[Blank][] :> Slot[j] /; i++ == 0)
	];
	result /. Inactive[head_][args___] :> Function[head[args]]
]

iFromEntity[e_Entity] := Module[{},
	Message[FromEntity::norep,e];$Failed
	]
	
iFromEntity[e:Except[_Entity]] := Module[{},
	Message[FromEntity::notent,e];$Failed
]

iFromEntity[args___] := Module[{},
	ArgumentCountQ[FromEntity,Length[{args}],1,1];
	$Failed
]

SetAttributes[{
	ToEntity,
	FromEntity
},{ReadProtected,Protected}]

Internal`SetValueNoTrack[{$iTEHeld}, True]

End[];