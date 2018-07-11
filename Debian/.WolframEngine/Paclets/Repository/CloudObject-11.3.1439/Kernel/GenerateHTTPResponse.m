BeginPackage["CloudObject`"]

System`GenerateHTTPResponse;
System`ResponseForm;

(* Exported symbols that are used in other paclets such as Forms to set global states for GenerateHTTPResponse *)

CloudObject`$ExportFormat = "WL"
CloudObject`$ResponseForm = None;
CloudObject`$ResponseKeys = All;
CloudObject`$CachePersistence = Inherited;

Begin["`Private`"]

(* Developer riccardod, carlob *)

Unprotect[GenerateHTTPResponse]
SetAttributes[GenerateHTTPResponse, HoldAllComplete]

Unprotect[{AutoRefreshed, Delayed, ExportForm, ResponseForm, HTTPRedirect, HTTPResponse, HTTPErrorResponse}]


(* Delayed export spec parser *)

SetAttributes[export, HoldAllComplete];

export[body_, fmt:Except[_?ListQ]] := export[body, {fmt, Inherited}];
export[body_, {fmt_?StringQ, rules:Repeated[_Rule|_RuleDelayed]}] := export[body, {{fmt, rules}, Inherited}];
export[body_, {None|_Missing|False, rfmt_}] := export[body, {rfmt}];
export[body_, {fmt:Except[_?ListQ], rfmt_}] := export[ExportForm[body, fmt], {rfmt}];
export[body_, {{fmt__}, rfmt_}] := export[ExportForm[body, fmt], {rfmt}];
export[body_, {rfmt:Except[_?ListQ]}] := ResponseForm[body, rfmt];
export[body_, {{rfmt__}}] := ResponseForm[body, rfmt];

(* $Failed should display a nice error page *)

Unprotect[Failure]
Failure /: GenerateHTTPResponse[f:Failure[_String, _?AssociationQ]] := HTTPResponse[
    f["Message"],
    <|"StatusCode" -> 500, "ContentType" -> "text/plain"|>
]
Protect[Failure]

AutoRefreshed /: GenerateHTTPResponse[AutoRefreshed[body_, tspec:Except[_Rule|_RuleDelayed]:3600, fmt:Except[_Rule|_RuleDelayed]:"WL", ___]] := 
    GenerateHTTPResponse[Delayed[body, fmt]]

Delayed /: GenerateHTTPResponse[Delayed[body_, format:Except[_Rule|_RuleDelayed]:Automatic, opt:OptionsPattern[Delayed]]] :=
    Block[
        {$CachePersistence = OptionValue[Delayed, {opt}, CachePersistence]},
        With[
            {response = GenerateHTTPResponse[export[body, format]]},
            Replace[
                {OptionValue[Delayed, {opt}, UpdateInterval], response}, {
                    {Infinity|None|Automatic|False|_Missing, expr_} :> 
                        expr,
                    {refresh_, res:HTTPResponse[inner_, meta_, rest___]} :> (
                        HTTPResponse[
                            inner,
                            Append[
                                meta, 
                                "Headers" -> Join[
                                    res["Headers"], 
                                    {"Refresh" -> ToString[refresh]}
                                ]
                            ],
                            rest
                        ]
                    ),
                    _ :> $Failed (* This should never happen*)
                }
            ]
        ]
    ]


   
(* HTTPRedirect to HTTPResponse *)

HTTPErrorResponse /: GenerateHTTPResponse[res:HTTPErrorResponse[code_Integer]] := 
    HTTPResponse[
        TemplateApply[
            File["Templates/HTTPErrorResponse.html"], <|
                "StatusCode" -> code, 
                "StatusCodeDescription" -> res["StatusCodeDescription"]
            |>
        ],
        <|"StatusCode" -> code|>
    ]

HTTPRedirect /: GenerateHTTPResponse[HTTPRedirect[url:_CloudObject|_URL, rest___]] :=
    GenerateHTTPResponse @ HTTPRedirect[First[url], rest];

HTTPRedirect /: GenerateHTTPResponse[HTTPRedirect[url_?StringQ, meta:_Rule|_RuleDelayed|{RepeatedNull[_Rule|_RuleDelayed]}]] := 
    GenerateHTTPResponse @ HTTPRedirect[url, <|meta|>];

HTTPRedirect /: GenerateHTTPResponse[HTTPRedirect[url_, code_Integer?IntegerQ]] := 
    GenerateHTTPResponse @ HTTPRedirect[url, <|"StatusCode" -> code|>];

HTTPRedirect /: GenerateHTTPResponse[HTTPRedirect[url:_?StringQ:"/", meta:_?AssociationQ:<||>]] := 
    HTTPResponse[
        "Redirecting...",
        Append[
            meta, {
                "Headers" -> Join[
                    Replace[
                        Lookup[meta, "Headers", None], {
                            None|Null|Missing|_Missing -> {},
                            a_Association?AssociationQ :> Normal[Delete[a, "Location"]],
                            l_List :> DeleteCases[l, (Rule|RuleDelayed)["Location", _], {1}],
                            (Rule|RuleDelayed)["Location", _] -> Sequence[],
                            any_ :> {any}
                        }
                    ], 
                    {"Location" -> url}
                ],
                "StatusCode" -> Lookup[meta, "StatusCode", 302]
            }
        ]
    ];

(*  A valid HTTPResponse will just stay as is *)

(*  An HTTPResponse with something inside will continue the recursion, 
    but metadata will won and will be preserved.
    Sample Usage: 
        HTTPResponse[ExportForm[<|"Success" -> False|>, "JSON"], "StatusCode" -> 500] 

    It must reset any ExportForm was defined previously
        This is done by Block *)

HTTPResponse /: GenerateHTTPResponse[res:HTTPResponse[_, meta_?AssociationQ, ___]] :=
    Block[
        {$ExportFormat = None, $ResponseForm = None, $ResponseKeys = All}, 
		res["Encode"]
    ]

(*  ExportForm is using Block.
    Nested ExportForm will override each other until the last iteration is done.
    The inner ExportForm with this system always wins. *)

ExportForm /: GenerateHTTPResponse[ExportForm[body_, Inherited|{Inherited, ___}, ___]] := 
    GenerateHTTPResponse[body]

ExportForm /: GenerateHTTPResponse[ExportForm[body_, opt__]] := Block[
    {$ExportFormat = opt},
    GenerateHTTPResponse[body]
]

makeExportForm[expr_] := 
    Replace[
        exportFormToByteArray[expr, $ExportFormat], {
            {body_String?StringQ, meta_} :>
                HTTPResponse[ToCharacterCode[body], meta],
            {body:$BodyPattern, meta_} :>
                HTTPResponse[body, meta],
            {body:_HTTPResponse|_ExportForm|_ResponseForm, meta_} :>
                GenerateHTTPResponse[body, meta],
            _ :> 
                HTTPResponse[
                    StringJoin[
                        "Failed to export to ", 
                        ToString[First[Flatten[{$ExportFormat}]]],
                        " for input:\n", 
                        ToString[expr, InputForm]
                    ], <|
                        "ContentType" -> "text/plain", 
                        "Headers" -> {"Content-Disposition" -> "inline"},
                        "StatusCode" -> 500
                    |>
                ]
        }
    ]

makeCachePersistence[res:HTTPResponse[body_, meta_, opts___]] /; $CachePersistence =!= Inherited := Replace[
    OptionValue[HTTPResponse, {opts}, CachePersistence], {
        Inherited :> HTTPResponse[
            body, 
            meta, 
            Sequence @@ FilterRules[{opts}, Except[CachePersistence]],
            CachePersistence -> $CachePersistence
        ],
        val_ :> res
    }
]
makeCachePersistence[res_] := res


(* Starting response form logic *)

SetAttributes[ResponseForm, HoldFirst];

responseJSON = ReplaceAll[{
    None|_Missing -> Null,
    any:_HoldForm|_Hold|_MessageName :> Block[{}, ToString[Unevaluated[any], InputForm] /; True] 
}]

responseXML[result_] := 
    XMLElement["evaluation-data", 
        Map[
            #1 -> ToString[result[#1]] &, {
            "Success", 
            "FailureType", 
            "StatusCode", 
            "InputString", 
            "Timing", 
            "AbsoluteTiming"
            }
        ], {
            XMLElement[
                "Result", {}, 
                {result["Result"]}
            ],
            XMLElement[
                "OutputLog", {}, 
                Map[XMLElement["OutputLogEvent", {}, {ToString[#, InputForm]}] &, result["OutputLog"]]
            ],
            XMLElement[
                "Messages", {}, 
                Map[XMLElement["Message", {}, {ToString[#, InputForm]}] &, result["Messages"]]
            ],
            XMLElement[
                "MessagesText", {}, 
                Map[XMLElement["MessagesText", {}, {#}] &, result["MessagesText"]]
            ],
            XMLElement[
                "MessagesExpressions", {}, 
                Map[XMLElement["MessageExpression", {}, {ToString[#, InputForm]}] &, result["MessagesExpressions"]]
            ]
        }
    ]

makeResponseForm[evalData_] /; MatchQ[$ResponseForm, None|Inherited|_Missing|False] := evalData["Result"]
makeResponseForm[evalData_] := GenerateHTTPResponse @ Replace[{  
    (* Uppercasing the format for a quick match in Replace *)
    If[StringQ[$ResponseForm], ToUpperCase[$ResponseForm], $ResponseForm],
    (* Quick and dirty implementation of third arg of ResponseForm *)
    If[$ResponseKeys === All, Identity, KeyTake[$ResponseKeys]] @ <|
        "StatusCode" -> evalData["Result"]["StatusCode"],
        "Success" -> evalData["Result"]["StatusCode"] < 400 && evalData["Success"],
        (* Removing extra keys *)
        KeyDrop[evalData, {"Result", "Success", "Messages", "MessagesText", "MessagesExpressions"}],
        (* Joining messages with existing messages *)
        Thread[
            Rule[
                {"Messages", "MessagesText", "MessagesExpressions"},
                MapThread[
                    Join,
                    Lookup[
                        {evalData, CloudObject`$EvaluationParameters}, 
                        {"Messages", "MessagesText", "MessagesExpressions"}, 
                        {}
                    ]
                ]
            ]
        ],
        (* Summing Timings *)
        Thread[
            Rule[
                {"Timing", "AbsoluteTiming"},
                MapThread[
                    Plus,
                    Lookup[
                        {evalData, CloudObject`$EvaluationParameters}, 
                        {"Timing", "AbsoluteTiming"}, 
                        0
                    ]
                ]
            ]
        ],
        (* In case the result is binary then we encode it base64. *)
        "Result" -> If[
            evalData["Result"]["BinaryFormatQ"],
            ExportString[evalData["Result"]["Body"], {"Base64", "String"}],
            evalData["Result"]["Body"]
        ],
        "ResultMeta" -> evalData["Result"]["Meta"]
    |>}, {
        {"XML", result_} :> 
            HTTPResponse[
                ExportForm[responseXML[Evaluate[result]], "XML"],
                <|"StatusCode" -> evalData["Result"]["StatusCode"]|>
            ],
        {"JSON"|"RawJSON", result_} :> 
            HTTPResponse[
                ExportForm[responseJSON[Evaluate[result]], "JSON"],
                <|"StatusCode" -> evalData["Result"]["StatusCode"]|>
            ],
        {None|Null|_Missing|Automatic|"WL"|"STRING"|"TEXT", result_} :> 
            HTTPResponse[
                ExportForm[result, "WL"],
                <|"StatusCode" -> evalData["Result"]["StatusCode"]|>
            ],
        {"HTML"|"HTMLFRAGMENT", result_} :> 
            HTTPResponse[
                ExportForm[
                    ResponseForm[
                        result["Result"], 
                        $ResponseForm, 
                        $ResponseKeys, 
                        <|"EvaluationData" -> result|>
                    ], 
                    "HTMLFragment", 
                    "FullDocument" -> True
                ],
                <|"StatusCode" -> evalData["Result"]["StatusCode"]|>
            ],
        {_?StringQ, result_} :> 
            HTTPResponse[
                "ResponseForm " <> $ResponseForm <> " is not JSON, WL, XML, or HTML", <|
                    "ContentType" -> "text/plain", 
                    "StatusCode" -> 500,
                    "Headers" -> {"Content-Disposition" -> "inline"}
                |>
            ],
        {any_, result_} :> 
            HTTPResponse[
                ExportForm[result, any],
                <|"StatusCode" -> evalData["Result"]["StatusCode"]|>
            ]
    }
]

(*  ResponseForm is only setting a global variable that the user at some point requested it 
    At the end of the recursion, after exiting from Block it will be applyed *)

$innerCall = False;

ResponseForm /: GenerateHTTPResponse[ResponseForm[expr_, format_:"WL", keys_:All]] := (
    $ResponseForm = Replace[format, Inherited :> $ResponseForm];
    $ResponseKeys = Replace[keys, Inherited :> $ResponseKeys];
    GenerateHTTPResponse[expr]
)

(* this is the very last call *)
GenerateHTTPResponse[obj_] /; TrueQ[$innerCall] := makeCachePersistence @ makeExportForm[obj]
GenerateHTTPResponse[held_, req_:Inherited] /; ! TrueQ[$innerCall] := 
    With[
        {expr = held, request = Replace[
            req, {
                Automatic|Inherited :> Replace[$HTTPRequest, {a_?AssociationQ :> a, _ :> <||>}],
                None|_Missing :> <||>,
                any:_String|_URL :> URLParse[any, All],
                any:{RepeatedNull[_Rule|_RuleDelayed]}|_Rule|_RuleDelayed :> <|any|>,
                any_?AssociationQ :> any,
                _ :> (
                    Message[GenerateHTTPResponse::nvldrequest, req];
                    <||>
                )
            }
        ]},
        Block[{
            $EvaluationEnvironment = "WebAPI",
            $innerCall    = True,
            $ResponseForm = None,
            $ResponseKeys = All,
            $CachePersistence = Inherited,
            $exportFormDefault = "HTML",
            $exportFormSoundDefault = "HTML",
            $HTTPRequest = request
            },
            Internal`InheritedBlock[
                {GenerateHTTPResponse},
                ClearAttributes[GenerateHTTPResponse, HoldAllComplete];
                makeResponseForm @ EvaluationData[
                    GenerateHTTPResponse[expr, request]
                ]
            ]
        ]
    ]    

(* Two argument version *)

GenerateHTTPResponse[expr_, _] /; $innerCall :=
    GenerateHTTPResponse[expr]

(* no args are creating a blank response *)

GenerateHTTPResponse[] := HTTPResponse[]

GenerateHTTPResponse[args__] := (
    ArgumentCountQ[GenerateHTTPResponse, Length[{args}], 1, 2];
    $Failed
)

Unprotect[{AutoRefreshed, Delayed, ExportForm, ResponseForm, HTTPRedirect, HTTPResponse, HTTPErrorResponse}]


End[]

EndPackage[]
