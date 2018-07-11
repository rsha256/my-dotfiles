BeginPackage["CloudObject`"]

System`CloudEvaluate;
System`CloudFunction;

Begin["`Private`"]

(* CloudFunction *)
Unprotect[CloudFunction];

CloudFunction[obj_CloudObject, rest___][args___] := CloudFunction[Get[obj], rest][args]
CloudFunction[fn_, head_:Function[##]][args___] := CloudEvaluate[fn[args], head]

CloudFunction[args___] := (ArgumentCountQ[CloudFunction, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False)

SetAttributes[CloudFunction, {ReadProtected}];
Protect[CloudFunction];

(* CloudEvaluate *)

Unprotect[CloudEvaluate];

Options[CloudEvaluate] = {CloudBase -> Automatic, IncludeDefinitions -> True};

CloudEvaluate[expr_, head_:Function[##], opts:OptionsPattern[]] /; TrueQ[$CloudEvaluation] := head @@ {expr}

CloudEvaluate[expr_, opts:OptionsPattern[]] := CloudEvaluate[expr, Function[##], opts]

CloudEvaluate[expr_, head_, opts:OptionsPattern[]] :=
    Block[ {$CloudBase = handleCBase[OptionValue[CloudBase]]},
        Replace[
            execute[
                $CloudBase, 
                "POST", 
                {"evaluations"}, 
                Body -> If[TrueQ[OptionValue[IncludeDefinitions]],                	 
                			exprToStringBytesIncludingDefinitions[Unevaluated[expr]], 
                			exprToStringBytesNotIncludingDefinitions[Unevaluated[expr]]
                		]
            ], {
                HTTPError[404, ___] :> LegacyCloudFunction[expr &, head][],
                result:{_String, _List} :> handleSuccessfulEvaluation[result, head],
                (* TODO msghd should be CloudFunction if this is being called by CloudFunction *)
                result_ :> (checkError[result, CloudEvaluate];
                            $Failed) 
            }
        ]
    ]

handleSuccessfulEvaluation[{_, bytes_List}, rest___] :=
    handleSuccessfulEvaluation[FromCharacterCode[bytes, "UTF-8"], rest]

handleSuccessfulEvaluation[responseText_String, head_:Function[##]] := 
	With[{response = ToExpression[responseText, InputForm]},
		Scan[ReleaseHold, Lookup[response, "MessagesExpressions", {}]];
		Extract[response, "Result", head]
	]

internalCloudEvaluate[args___] := 
    Block[{$IncludedContexts = {"CloudObject"}},
        CloudEvaluate[args]
    ]

SetAttributes[{CloudEvaluate, internalCloudEvaluate}, {HoldFirst, ReadProtected}];
Protect[CloudEvaluate];

(* LegacyCloudFunction and LegacyCloudEvaluate are the implementation of
	CloudFunction and CloudEvaluate based on on-demand APIFunctions. They are
	present so that Mathematica 10.3 will continue to function until the new
	/evaluations API is deployed, at which time they can be removed.
*)
(* LegacyCloudFunction *)

If[TrueQ[$CloudEvaluation],
	LegacyCloudFunction[fn_, headwrap_][args___] := headwrap[fn[args]],
(* Else calling from outside the cloud *)
	(* CloudFunction stores an expression as an APIFunction in the cloud and executes it (in the cloud). *)
	LegacyCloudFunction[expr_, headwrap_][args___] :=
	    Module[{co},
	        Block[{formalargs}, 
	            co = iCloudPut[APIFunction[{"args" -> "String"},
	                ResponseForm[ExportForm[expr @@ ToExpression[#args], "WL"], "WL"] &
	            ], CloudObject[], expressionMimeType["CloudEvaluation"], IncludeDefinitions -> True, IconRules -> {}];
	            If[co === $Failed, Return[$Failed]];
	            (* TODO: This cleanup could happen asynchronously. *)
	            cleanup[
	            	co,
	            	getAPIResult[
	            		co, 
	            		{"args" -> URLEncode[Block[{$ContextPath={"System`"}, $Context="System`"}, ToString[{args}, InputForm]]]},
	            		headwrap
	            	]
	            ]
	        ]
	    ];
]

getAPIResult[obj_CloudObject, arguments_ : {}, headwrap_ : Identity] :=
    Module[{body, result},
        body = StringJoin[#1 <> "=" <> #2& @@@ arguments];
        (* TODO: Remove view parameter in favor of _view when that change is deployed to production. *)
        result = responseToExpr @ execute[obj, "POST", "objects",
            Parameters -> {"view" -> "API", "_view" -> "API", "exportform" -> "WL", "responseform" -> "WL"},
            Body -> body, Type -> "application/x-www-form-urlencoded"
        ];
        ReleaseHold /@ result["MessagesExpressions"];
        If[result === $Failed, Return[$Failed]];
        If[KeyExistsQ[result, "Result"],
            ToExpression[result["Result"], InputForm, headwrap],
        	(* else *)
        	Null
        ]
    ]

LegacyCloudFunction[args___] := (ArgumentCountQ[CloudFunction, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False)

End[]

EndPackage[]
