BeginPackage["CloudObject`"]

Needs["URLUtilities`"]

System`URLDispatcher

Begin["`Private`"]

Unprotect[URLDispatcher]
ClearAll[URLDispatcher]

$RuleListPattern = {RepeatedNull[_Rule|_RuleDelayed]};
$URLBuildPattern = _Association|_String|$RuleListPattern|_RuleDelayed|_Rule;

pathJoin[path_?StringQ] := path;
pathJoin[path_List] := FileNameJoin[path, OperatingSystem -> "Unix"];

URLDispatcher /: GenerateHTTPResponse[URLDispatcher[rules:{(_Rule|_RuleDelayed)...}]] := First @ StringCases[
	HTTPRequestData["DispatchPathString"],
	Append[
		Map[
			fixRule,
			rules
		],
		___ :> GenerateHTTPResponse[HTTPErrorResponse[404]]
	],
	1
]

fixRule[(Rule|RuleDelayed)[patt_, payload_]] := RuleDelayed[
	StartOfString ~~ Longest[patt] ~~ u___,
	Block[
		{$HTTPRequest = Append[$HTTPRequest, "DispatchPathString" -> u]},
		GenerateHTTPResponse[payload]
	]
]

URLDispatcher[rules_Association] := URLDispatcher[Normal[rules]]

URLDispatcher[rules:_Rule|_RuleDelayed] := URLDispatcher[{rules}]

End[]

EndPackage[]