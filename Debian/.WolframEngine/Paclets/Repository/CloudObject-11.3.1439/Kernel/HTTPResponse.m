(* ::Package:: *)

BeginPackage["CloudObject`"]

System`HTTPResponse
System`HTTPRequest
System`HTTPErrorResponse

Begin["`Private`"]

(*  Developer riccardod, carlob.

    official xml is took from wikipedia and linked here
    http://www.iana.org/assignments/http-status-codes/http-status-codes.xml
    to update them run this code:

    $StatusCodes = Association @ Cases[
        Import["http://www.iana.org/assignments/http-status-codes/http-status-codes.xml", "XML"], 
        {___, XMLElement["value", _, {val_}], ___, XMLElement["description", _, {desc:Except["Unassigned"]}], ___} 
            :> {FromDigits[val] -> desc}, 
        Infinity
    ] *)

toStatusCodeDescription := toStatusCodeDescription = Replace @ Dispatch[{
    100 -> "Continue",
    101 -> "Switching Protocols",
    102 -> "Processing",
    200 -> "OK",
    201 -> "Created",
    202 -> "Accepted",
    203 -> "Non-Authoritative Information",
    204 -> "No Content",
    205 -> "Reset Content",
    206 -> "Partial Content",
    207 -> "Multi-Status",
    208 -> "Already Reported",
    226 -> "IM Used",
    300 -> "Multiple Choices",
    301 -> "Moved Permanently",
    302 -> "Found",
    303 -> "See Other",
    304 -> "Not Modified",
    305 -> "Use Proxy",
    307 -> "Temporary Redirect",
    308 -> "Permanent Redirect",
    400 -> "Bad Request",
    401 -> "Unauthorized",
    402 -> "Payment Required",
    403 -> "Forbidden",
    404 -> "Not Found",
    405 -> "Method Not Allowed",
    406 -> "Not Acceptable",
    407 -> "Proxy Authentication Required",
    408 -> "Request Timeout",
    409 -> "Conflict",
    410 -> "Gone",
    411 -> "Length Required",
    412 -> "Precondition Failed",
    413 -> "Payload Too Large",
    414 -> "URI Too Long",
    415 -> "Unsupported Media Type",
    416 -> "Range Not Satisfiable",
    417 -> "Expectation Failed",
    421 -> "Misdirected Request",
    422 -> "Unprocessable Entity",
    423 -> "Locked",
    424 -> "Failed Dependency",
    426 -> "Upgrade Required",
    428 -> "Precondition Required",
    429 -> "Too Many Requests",
    431 -> "Request Header Fields Too Large",
    500 -> "Internal Server Error",
    501 -> "Not Implemented",
    502 -> "Bad Gateway",
    503 -> "Service Unavailable",
    504 -> "Gateway Timeout",
    505 -> "HTTP Version Not Supported",
    506 -> "Variant Also Negotiates",
    507 -> "Insufficient Storage",
    508 -> "Loop Detected",
    510 -> "Not Extended",
    511 -> "Network Authentication Required",
    _   -> Missing["Undefined"]
}];

toStatusCode = Replace[{
    status_Integer /; 100 <= status < 600 :> status,
    "PageNotFound"|"NotFound" -> 404,
    "PermissionDenied"|"Forbidden" -> 403,
    "ServerError"|"InternalServerError" -> 500,
    s_?StringQ /; StringMatchQ[s, DigitCharacter..] :> FromDigits[s],
    status_ :> (Message[HTTPResponse::nvldstatus, status];500)
}]

$rulePattern  = {RepeatedNull[_Rule|_RuleDelayed]}|_Rule|_RuleDelayed
$assocPattern = $rulePattern|_?AssociationQ;
$urlPattern   = $assocPattern|_CloudObject|_URL|_String;

$BodyPattern  = HoldPattern[_String?StringQ|None|{___Integer}|_ByteArray?ByteArrayQ]
$httpPattern  = HTTPResponse[$BodyPattern, _?AssociationQ, ___]|HTTPRequest[$urlPattern, _?AssociationQ, ___]

$invalidResponse := Failure[
    "EncodingError", <|
        "MessageTemplate" :> "Failed to encode HTTPResponse"
    |>
]

getFromHeaders[res:(HTTPResponse|HTTPRequest)[_, meta_?AssociationQ, ___], name_, header_, default_:None] :=
    Lookup[
        meta, 
        name, 
        Lookup[
            HTTPProperty[res, "RawLowerHeaders"], 
            header, 
            default
        ]
    ]    

toContentTypeWithEncoding[res_HTTPResponse] := 
    toContentTypeWithEncoding[res, "text/html", HTTPProperty[res, CharacterEncoding], True]
toContentTypeWithEncoding[res_HTTPRequest] := 
    toContentTypeWithEncoding[
        res, 
        Replace[
            HTTPProperty[res, "RawBody"], {
                rules:$assocPattern :> If[
                    FreeQ[rules, _Association|_File, {2, Infinity}],
                    "application/x-www-form-urlencoded",
                    "multipart/form-data"
                ],
                _List|_String|_ByteArray :> "text/plain",
                _ :> None
            }
        ],
        HTTPProperty[res, CharacterEncoding]
    ]
toContentTypeWithEncoding[res:$httpPattern, cttp_:"text/html", rest___] := 
    toContentTypeWithEncoding[
        Evaluate @ getFromHeaders[res, "ContentType", "content-type"],
        cttp,
        rest
    ]

toContentTypeWithEncoding[s_String, cttp_:"text/html", charset_:"UTF-8", compare_:False] := 
    First[
        StringCases[
            s, {
                StringExpression[
                    StartOfString,
                    mime__,
                    ";",
                    WhitespaceCharacter...,
                    "charset=",
                    enc__,
                    Repeated[";", {0, 1}]
                ] :> Hold[
                    mime, 
                    If[
                        And[
                            TrueQ[compare],
                            StringQ[charset],
                            ! toCharacterEncoding[charset, charset] === toCharacterEncoding[enc, enc]
                        ],
                        Message[HTTPResponse::chrmismatch, enc, charset];
                        enc,
                        enc
                    ]
                ],
                StringExpression[StartOfString,"text/", ___] :> Hold[s, toCharset[charset]]
            },
            IgnoreCase -> True
        ],
        {s, None}
    ]

(* since I'm using a variable as pattern, this set should happen after downvalues *)
SetAttributes[{getFromHeaders, toContentTypeWithEncoding}, HoldAll]

toContentTypeWithEncoding[_, cttp_:"text/html", charset_:"UTF-8", ___] := 
    Hold[cttp, toCharset[charset]]

toCharacterEncoding[enc_, charset_:"UTF-8"] := 
    With[
        {msg := (Message[HTTPResponse::nvldenc, enc, charset];charset)},
        If[
            MatchQ[enc, _String],
            ToCharacterEncoding[enc, msg],
            msg
        ]
    ]

toCharset[enc_String, default_:None] := ToCharset[enc, default]
toCharset[_, default_:None] := default

SetAttributes[checkBodyBytes, HoldRest]

checkBodyBytes[content_] := checkBodyBytes[content, content]
checkBodyBytes[{}|_ByteArray, default_] := default
checkBodyBytes[content_List, default_] := 
    If[
        IntervalMemberQ[
            Interval[{0, 255}], 
            Interval[MinMax[content]]
        ],
        default,
        Message[HTTPResponse::encfailed]; $Failed
    ]

urlParse[key_, val:None|Automatic|_String] := key -> val
urlParse[key_, val_] := Replace[
    URLParse[<|key -> val|>, key], {
        $Failed :> (Message[HTTPResponse::nvldcache, key -> val, Nothing]; Nothing),
        any_ :> key -> any
    }
]

normalizeCachePersistence = Replace @ Dispatch @ {
    Method :> 
        normalizeCachePersistence @ {Automatic, {Method}},

    any:Automatic|None|Infinity|Inherited|True|False|_?NumberQ|"Minute"|"Hour"|"Day"|"Week"|"Month"|"Year"|HoldPattern[_TimeObject|_DateObject|_Quantity] :> 
        normalizeCachePersistence @ {any, {}},
    s_String :> 
        normalizeCachePersistence @ {Automatic, {s}},
    a_Association?AssociationQ :> 
        normalizeCachePersistence @ {Automatic, a},
    {HoldPattern[date_DateObject], keys_} :> 
        normalizeCachePersistence @ {UnixTime[date] - UnixTime[], keys},
    {HoldPattern[time_TimeObject], keys_} :> 
        normalizeCachePersistence @ {ReplacePart[DateObject[], 2 -> time], keys},
    rules:{RepeatedNull[_Rule|_RuleDelayed|_String|Method]}|_Rule|_RuleDelayed|_String|Method :> 
        normalizeCachePersistence @ {Automatic, rules},
    {time_, rule:_Rule|_RuleDelayed|_String|Method} :> 
        normalizeCachePersistence @ {time, {rule}},
    {None|Inherited|False, keys_} :> 
        normalizeCachePersistence @ {0, keys},
    {Automatic|True, keys_} :> 
        normalizeCachePersistence @ {Infinity, keys},
    {HoldPattern[q_Quantity], rest___} :> 
        normalizeCachePersistence @ {
            Replace[
                QuantityMagnitude[q, "Seconds"], 
                Except[_Integer|_Real] :> (
                    Message[HTTPResponse::nvldcache, q, None]; 
                    None
                )
            ], 
            rest
        },
    {"Minute", rest___} :> 
        normalizeCachePersistence @ {60, rest},
    {"Hour",   rest___} :> 
        normalizeCachePersistence @ {3600, rest},
    {"Day",    rest___} :> 
        normalizeCachePersistence @ {86400, rest},
    {"Week",   rest___} :> 
        normalizeCachePersistence @ {604800, rest},
    {"Month",  rest___} :> 
        normalizeCachePersistence @ {2592000, rest},
    {"Year",   rest___} :> 
        normalizeCachePersistence @ {31536000, rest},
    {time:Infinity|_?NumberQ, keys:{RepeatedNull[_Rule|_RuleDelayed|_String|Method]}} :> 
        (* this is the ONLY valid pattern *)
        {N[time], Association @ Replace[
            keys, {
                _["PathString"|"Path", s_]   :> urlParse["PathString", s],
                _["Query"|"QueryString", s_] :> urlParse["QueryString", s],
                "Path"  -> "PathString"  -> Automatic,
                "Query" -> "QueryString" -> Automatic,
                Method  -> "Method"      -> Automatic,
                _[Method, v:_String|None|Automatic] :> "Method" -> v,
                s:_String|Method :> s -> Automatic, 
                r:_[_String|Method, _String|None|Automatic] :> r,
                any_ :> (
                    Message[HTTPResponse::nvldcache, any, Nothing]; 
                    Nothing
                ) 
            },
            {1}]
        },
    {time_, keys_Association?AssociationQ} :> 
        normalizeCachePersistence @ {time, Normal[keys]}, 
    {time_, keys:Repeated[_Rule|_RuleDelayed|_String|Method, {2, Infinity}]} :> 
        normalizeCachePersistence @ {time, {keys}},
    any_ :> (
        Message[HTTPResponse::nvldcache, any, None]; 
        normalizeCachePersistence @ None
    )
}

Options[HTTPResponse] = {
    CharacterEncoding -> Automatic, 
    CachePersistence -> Inherited
}

outer:HTTPResponse[inner:HTTPResponse[body_, innermeta_?AssociationQ, inneropt___], meta_?AssociationQ, opt___] :=
    HTTPResponse[
        body, <|
            meta, 
            innermeta,
            "Headers" -> <|
                HTTPProperty[outer, "RawHeaders"], 
                HTTPProperty[inner, "RawHeaders"]
            |>,
            "Cookies" -> <|
                HTTPProperty[outer, "Cookies"],    
                HTTPProperty[inner, "Cookies"]
            |>
        |>, 
        inneropt,
        opt
    ]

HTTPResponse[body_:None] := HTTPResponse[body, <||>]

HTTPResponse[body_, meta:_Rule|_RuleDelayed, rest___] :=
    If[
        KeyExistsQ[Options[HTTPResponse], First[meta]],
        HTTPResponse[body, <||>, meta, rest],
        HTTPResponse[body, <|meta|>, rest]
    ]

HTTPResponse[body_, meta:$rulePattern, rest___] :=
    HTTPResponse[body, <|meta|>, rest]

HTTPResponse[body_, None|_Missing, rest___] := HTTPResponse[body, <||>, rest]
HTTPResponse[""|{}, rest___] := HTTPResponse[None, rest]

HTTPResponse[body:{__Integer}, rest___] := 
    HTTPResponse[ByteArray[body], rest]

res:HTTPResponse[body_ByteArray, ___] /; Not[ByteArrayQ[body]] := 
    (Message[HTTPResponse::encfailed]; $invalidResponse)

res:HTTPResponse[_String, ___] := 
    HTTPProperty[res, "Encode"]

res_HTTPResponse[prop_:All] := 
    With[{value = HTTPProperty[res, prop]}, value /; value =!= $Failed]

SetAttributes[HTTPProperty, HoldFirst]
HTTPProperty[res_, All|"PropertyAssociation"] := 
    HTTPProperty[res, DeleteCases[HTTPProperty[res, "Properties"], "BodyBytes"|"Body"]]

HTTPProperty[res_, prop_Alternatives] := 
    Replace[
        Scan[
            Replace[HTTPProperty[res, #], val:Except[None|_Missing|Null] :> Return[val]] &,
            prop
        ],
        Null -> None
    ]

HTTPProperty[res_, prop_List] := 
    AssociationThread[prop -> Replace[Map[Hold[res, #] &, prop], $httpRules, {1}]]

HTTPProperty[res_, prop_] := Replace[Hold[res, prop], $httpRules]

$httpRules := $httpRules = Dispatch @ Apply[
    Function[{lhs, rhs}, Hold @@ lhs :> rhs, HoldAllComplete], {
    {HTTPResponse[$BodyPattern, _?AssociationQ, ___], "Properties"} :> {
        "Body",
        "BodyByteArray",
        "BodyBytes",
        "CharacterEncoding",
        "ContentType",
        "Headers",
        "StatusCode",
        "StatusCodeDescription",
        "Version"
    },
    {HTTPRequest[$urlPattern, _?AssociationQ, ___], "Properties"} :> 
        Union @ Join[{
            Method,
            "Body",
            "BodyByteArray",
            "BodyBytes",
            "ContentType",
            "Cookies",
            "FormRules",
            "Headers",
            "URL",
            "UserAgent"
            },
            URLParse[] (* this is returning all properties of URLParse *)
        ],
    {_, "Properties"} :> {},
    {res_HTTPResponse, "Meta"} :> 
        res[{"StatusCode", "ContentType", "Headers"}],
    {HTTPResponse[body_, meta_?AssociationQ, ___], "StatusCode"} :> 
        If[
            MatchQ[body, $BodyPattern],
            toStatusCode @ Lookup[meta, "StatusCode", 200],
            500
        ],
    {res_HTTPResponse, "StatusCodeDescription"} :> 
        toStatusCodeDescription[HTTPProperty[res, "StatusCode"]],
    {res:$httpPattern, "CharacterEncoding"|"Charset"} :> 
        Last @ toContentTypeWithEncoding[res],
    {res:$httpPattern, "ContentType"|"Content-Type"} :> 
        Replace[
            List @@ toContentTypeWithEncoding[res], {
                {m_String, enc_String} :> StringJoin[m, ";charset=", enc],
                {m_, _} :> m
            }
        ],
    {res:$httpPattern, "UserAgent"|"User-Agent"} :> 
        getFromHeaders[
            res, 
            "UserAgent", 
            "user-agent", 
            Replace[res, {_HTTPRequest :> "Wolfram HTTPClient "<>ToString[$VersionNumber], _ :> None}]
        ],
    {res:HTTPResponse[body:$BodyPattern, meta_?AssociationQ, opt:OptionsPattern[]], "Encode"} :>
        With[
            {result = HTTPProperty[res, "BodyByteArray"]},
            If[
                MatchQ[result, $BodyPattern],
                HTTPResponse[
                    result,
                    Append[meta, "ContentType" -> HTTPProperty[res, "ContentType"]],
                    CharacterEncoding -> HTTPProperty[res, CharacterEncoding],
                    Sequence @@ FilterRules[
                        {opt}, 
                        Except[CharacterEncoding]
                    ]
                ],
                $invalidResponse          
            ]        
        ],
    {res:HTTPResponse[body_, meta_?AssociationQ, opt___], "Encode"} :>
        HTTPResponse[GenerateHTTPResponse[body], meta, opt]["Encode"],

    {res:HTTPRequest[$urlPattern, _?AssociationQ, ___], "FormRules"} :> 
        Replace[
            Replace[
                HTTPProperty[res, "RawBody"], {
                    a_Association :> Normal[a],
                    rules:_Rule|_RuleDelayed :> {rules},
                    rules:$rulePattern :> rules,
                    _ :> None
                }
            ],
            rules_List :> Flatten[
                Apply[
                    Outer[Rule, ##, 1] &,
                    Replace[
                        Replace[
                            Rule @@@ rules, {
                                _[a_List, b:$assocPattern] :> Rule[a,   {<|b|>}],
                                _[a_List, b_List]          :> Rule[a,   b],
                                _[a_List, b_]              :> Rule[a,   {b}], 
                                _[a_,     b:$assocPattern] :> Rule[{a}, {<|b|>}],
                                _[a_,     b_List]          :> Rule[{a}, b],
                                _[a_,     b_]              :> Rule[{a}, {b}] 
                            },
                            {1}
                        ],
                        s_Integer :> ToString[s],
                        {3}
                    ],
                    {1}
                ]
            ]
        ],

    {head_[_, meta_?AssociationQ, ___], "Version"} :> 
        Lookup[meta, "Version", "1.1"],

    {head_[_, meta_?AssociationQ, ___], "RawHeaders"} :> 
        With[
            (* Normalizing to a list of rules *)
            {headers = Rule @@@ Replace[
                Lookup[meta, "Headers", {}], {
                    {s_?StringQ, f_?StringQ} :> {s -> f},
                    h:{___List} :> h,
                    h_?AssociationQ :> Normal[h],
                    h:_Rule|_RuleDelayed :> {h},
                    h:$rulePattern :> h,
                    h_ :> (Message[head::nvldheaders, h];{})
                }
            ]},
            If[
                Length[headers] > 0,
                (* 
                This is making developer life much easier, we are doing ToLowerCase of the lhs because 
                Each header field consists of a name followed by a colon (":") and the field value. Field names are case-insensitive.
                
                RFC 2616 - "Hypertext Transfer Protocol -- HTTP/1.1", Section 4.2, "Message Headers":
                https://www.w3.org/Protocols/rfc2616/rfc2616.html
                *)
                Replace[
                    headers, { 
                        h_[l_String, r_String] :> h[l, r],
                        h_[l_String, date_DateObject] :> h[l, DateString[
                            TimeZoneConvert[date, "GMT"], {
                                "DayNameShort", ", ", 
                                "DayShort", " ", 
                                "MonthNameShort", " ", 
                                "Year", " ",  "Time", " GMT"
                            }
                        ]],                        
                        h_[l_String, r_] :> h[l, TextString[r]]
                    },
                    {1}
                ],  
                headers
            ]
        ],
    {res:$httpPattern, "RawLowerHeaders"} :> Replace[
            HTTPProperty[res, "RawHeaders"], {
                {} :> {},
                headers_ :> MapAt[ToLowerCase, headers, {All, 1}]
            }
        ],
    {res:$httpPattern, "CompleteHeaders"} :> 
        Block[{
            headers = HTTPProperty[res, "RawHeaders"],
            keys   := keys   = ToLowerCase[headers[[All, 1]]],
            cttp   := cttp   = HTTPProperty[res, "ContentType"],
            uagent := uagent = HTTPProperty[res, "UserAgent"]
            },
            headers = If[
                MemberQ[keys, "content-type"] || ! StringQ[cttp],
                headers,
                Append[headers, "Content-Type" -> cttp]
            ];
            If[
                MemberQ[keys, "user-agent"] || ! StringQ[uagent],
                headers,
                Append[headers, "User-Agent" -> uagent]
            ]
        ],
    {res:$httpPattern, "Headers"} :> Replace[
            HTTPProperty[res, "CompleteHeaders"], {
                {} :> {},
                headers_ :> MapAt[ToLowerCase, headers, {All, 1}]
            }
        ],
    {HTTPRequest[_, meta_?AssociationQ, ___], "Cookies"} :> 
        Lookup[meta, "Cookies", Automatic],
    {_[_, meta_?AssociationQ, ___], "Cookies"} :> 
        Lookup[meta, "Cookies", {}],
    {res:HTTPResponse[_, _, opts___], CachePersistence} :> 
        normalizeCachePersistence @ OptionValue[HTTPResponse, {opts}, CachePersistence],
    {res:head_[_, meta_?AssociationQ, opts___], CharacterEncoding} :> 
        Replace[
            OptionValue[head, {opts}, CharacterEncoding], {
                None|_Missing :> None,
                Automatic :> With[
                    {cttp = toContentTypeWithEncoding[
                        res, 
                        "text/html", 
                        (* this is needed because bytes returned from URLFetch usually roundtrip with ISO8859-1 *)
                        Replace[HTTPProperty[res, "RawBody"], {_String :> "UTF-8", _ :> "ISO8859-1"}]
                    ]},
                    First @ StringCases[
                        First[cttp], {
                            StartOfString ~~ "application/json"|"text/xml" ~~ EndOfString :> "UTF-8",
                            StartOfString ~~ "text/" ~~ ___ :> toCharacterEncoding @ Last[cttp],
                            __ :> None
                        },
                        IgnoreCase -> True
                    ]
                ],
                s_ :> toCharacterEncoding[s]
            }
        ],
    {HTTPResponse[body:$BodyPattern, ___], "RawBody"} :>
        body,
    {HTTPRequest[_, meta_?AssociationQ, ___], "RawBody"} :>
        Lookup[meta, "Body", None],        
    {res:$httpPattern, "Body"} :> 
        Replace[
            HTTPProperty[res, "RawBody"], {
                None|{}|"" -> "",
                content:_List|_ByteArray :> checkBodyBytes[
                    content,
                    FromCharacterCode[
                        Normal[content],
                        Replace[
                            HTTPProperty[res, CharacterEncoding|"Charset"], {
                                s_String :> toCharacterEncoding[s, "ISO8859-1"], 
                                _ :> "ISO8859-1"
                            }
                        ]
                    ]
                ],
                content_?StringQ  :> Replace[
                    HTTPProperty[res, CharacterEncoding|"Charset"], {
                        enc_?StringQ :> Replace[
                            Quiet[FromCharacterCode[checkBodyBytes @ ToCharacterCode[content, enc]]], {
                                final_String :> final,
                                _ :> (Message[HTTPResponse::encfailed]; $Failed)
                            }
                        ],
                        _ :> content
                    }
                ]
            }
        ],
    {res:$httpPattern, "BodyBytes"} :> 
        Developer`ToPackedArray @ Replace[
            HTTPProperty[res, "RawBody"], {
                None -> {},
                content_ByteArray :> Normal[content],
                content_List      :> checkBodyBytes @ content,
                content_?StringQ  :> checkBodyBytes @ Replace[
                    HTTPProperty[res, CharacterEncoding], {
                        enc_?StringQ :> ToCharacterCode[content, enc],
                        _            :> ToCharacterCode[content, "ISO8859-1"]
                    }
                ]
            }
        ],
    {res:$httpPattern, "BodyByteArray"} :> 
        Replace[
            HTTPProperty[res, "RawBody"], {
                (* ByteArray[{}] is broken due to https://bugs.wolfram.com/show?number=310692 *)
                None              -> ByteArray[{}],
                content_ByteArray?ByteArrayQ :> content,
                content_List      :> Replace[ByteArray[content], Except[_?ByteArrayQ] :> (Message[HTTPResponse::encfailed]; $Failed)],
                content_?StringQ  :> Replace[HTTPProperty[res, "BodyBytes"], r:Except[$Failed] :> ByteArray[r]],
                _ :> (Message[HTTPResponse::encfailed]; $Failed)
            }
        ],
    {res:$httpPattern, "BinaryFormatQ"} :>
        ! MemberQ[{
                "c", "csv", "dif", "dot", "eps", "expressionml", "fasta", "fastq", 
                "graph6", "graphlet", "graphml", "gxl", "harwellboeing", "html", 
                "jvx", "kml", "leda", "mathml", "maya", "mol", "mol2", "mtx", "nb", 
                "nexus", "obj", "package", "pajek", "pdb", "pov", "rtf", "sdf", 
                "sparse6", "string", "svg", "table", "tex", "text", "tgf", "tsv", 
                "uue", "vrml", "wl", "x3d", "xbm", "xhtml", "xhtmlmathml", "xml", 
                "xyz", "rawhtml", "json"
            },
            ToLowerCase[mimetypeToFormat[First @ toContentTypeWithEncoding[res]]]
        ],
    {HTTPRequest[_, meta_?AssociationQ, ___], prop:"Username"|"Password"} :> 
        Lookup[meta, prop, ""],
    {res:HTTPRequest[$urlPattern, meta_?AssociationQ, ___], Method|"Method"} :>
        Replace[
            First[DeleteCases[Lookup[meta, {Method, "Method"}], _Missing], Automatic], {
                None|Automatic :> If[MatchQ[HTTPProperty[res, "RawBody"], None|_Missing|""], "GET", "POST"], 
                method_String :> ToUpperCase[method],
                method_ :> (Message[HTTPRequest::nvldmethod, method]; "GET")
            }
        ],    
    {HTTPRequest[url:$urlPattern, meta_?AssociationQ, ___], "URLString"} :>
        With[
            {todo = Complement[{
                "Scheme",
                "User",
                "Domain",
                "Port",
                "PathString",
                "QueryString",
                "Fragment"
            },
            Replace[Keys[meta], {"Path" -> "PathString", "Query" -> "QueryString"}, {1}]
            ]},
            URLUtilities`URLCorrect @ KeyDrop[
                <|
                    meta, 
                    Thread[Replace[todo, {"PathString" -> "Path", "QueryString" -> "Query"}, {1}] -> URLParse[url, todo]]
                |>,
                {"User", "Username", "Password"}
            ]
        ],
    {req:HTTPRequest[url:$urlPattern, _?AssociationQ, ___], "URL"} :>
        Replace[
            url, {
                any:_URL|_CloudObject :> Head[any][HTTPProperty[req, "URLString"]],
                _ :> HTTPProperty[req, "URLString"]
            }
        ],
    {HTTPRequest[url_?AssociationQ, meta_?AssociationQ, ___], key:(Alternatives@@URLParse[])} :>
        URLParse[Join[url, meta], key],
    {HTTPRequest[url:$urlPattern, meta_?AssociationQ, ___], key:(Alternatives@@URLParse[])} :>
        URLParse[Join[URLParse[url], meta], key],
    {_[_, meta_?AssociationQ, ___], prop_} /; KeyExistQ[meta, prop] :> 
        meta[prop],
    {head_[args___], prop_} :> (
        Message[
            head::notprop, 
            prop, 
            With[
                {inner = "\[LeftGuillemet]" <> ToString[Length[Hold[args]]] <> "\[RightGuillemet]"}, 
                HoldForm[head[inner]]]
            ]; 
            $Failed
        )
    }, 
    {1}
]


makeBoxIcon[text_String, color_] := 
    Graphics[{
        color,
        Disk[], 
        Text[
            Style[text, Directive[13 - StringLength[text], White, Bold]], 
            Scaled[{.53, .48}]
        ]}, 
        ImageSize -> Dynamic[{
            Automatic, 
            (3 * CurrentValue["FontCapHeight"]) / AbsoluteCurrentValue[Magnification]
        }],
        Background -> None
    ]
makeBoxIcon[s_, rest___] := makeBoxIcon[ToString[s], rest]

HTTPResponse /: MakeBoxes[
    res:HTTPResponse[_?StringQ|None|_List|_ByteArray, meta_?AssociationQ, ___], 
    StandardForm
    ] := 
    With[{
        (* Evaluation leak in MakeBoxes should not happen ever. I added a Quiet, but this should be handled in a different way. *)
        status  = Quiet @ HTTPProperty[res, "StatusCode"], 
        cttp    = Quiet @ HTTPProperty[res, "ContentType"]
        },
        BoxForm`ArrangeSummaryBox[
            HTTPResponse,
            res,
            makeBoxIcon[
                status,
                Which[
                    200 <= status < 300, Darker[Green],
                    300 <= status < 400, Darker[Cyan],
                    400 <= status < 500, Orange,
                    500 <= status < 600, Red,
                    True, Black
                ]
            ],{
                BoxForm`MakeSummaryItem[{"Status: ", toStatusCodeDescription[status]}, StandardForm],
                BoxForm`MakeSummaryItem[{"Content type: ", cttp}, StandardForm]
            },
            {},
            StandardForm
        ]
    ]

HTTPErrorResponse[] := HTTPErrorResponse[500]
HTTPErrorResponse[status_?StringQ, rest___] := HTTPErrorResponse[toStatusCode[status], rest]
HTTPErrorResponse[status_Integer, ___]["StatusCodeDescription"] := 
    toStatusCodeDescription[status];

Options[HTTPRequest] := Options[HTTPRequest] = Options[URLRead]

HTTPRequest[url_?AssociationQ]    := HTTPRequest[KeyTake[url, URLParse[]], KeyDrop[url, URLParse[]]]
HTTPRequest[url_] := HTTPRequest[url, <||>]
HTTPRequest[url:$rulePattern, rest___] := HTTPRequest[<|url|>, rest]
HTTPRequest[url_, meta:_Rule|_RuleDelayed, rest___] :=
    If[
        KeyExistsQ[Options[HTTPRequest], First[meta]],
        HTTPRequest[url, <||>, meta, rest],
        HTTPRequest[url, <|meta|>, rest]
    ]
HTTPRequest[url_, meta:$rulePattern, rest___] := HTTPRequest[url, <|meta|>, rest]

res_HTTPRequest[prop:"URL"|"URLString"] := 
    HTTPProperty[res, prop]

res_HTTPRequest[prop_:All] := 
    With[{value = HTTPProperty[res, prop]}, value /; value =!= $Failed]

HTTPRequest /: MakeBoxes[
    response:HTTPRequest[_?AssociationQ|_URL|_CloudObject|_String, _?AssociationQ, ___], 
    StandardForm
    ] := With[{
        (* Evaluation leak in MakeBoxes should not happen ever. I added a Quiet, but this should be handled in a different way. *)
        method  = Quiet @ response["Method"], 
        cttp    = Quiet @ response["ContentType"],
        url     = Quiet @ response["URLString"]
        },

    BoxForm`ArrangeSummaryBox[
        HTTPRequest,
        response,
        makeBoxIcon[
            Replace[
                method, 
                {"DELETE" -> "DEL", any_ /; StringLength[any] > 6 :> StringTake[any, 6]}
            ],
            Switch[
                method,
                "GET"|"HEAD", Darker[Green],
                "POST"|"PUT", Orange,
                "DELETE", Red,
                _, Black
            ]
        ],
        {
            BoxForm`MakeSummaryItem[{"URL: ", Hyperlink[Short[url], url]}, StandardForm],
            BoxForm`MakeSummaryItem[{"ContentType: ", cttp}, StandardForm]
        },
        {},
        StandardForm
    ]
]

(* Handling Import *)

HTTPRequest /: Import[req:HTTPRequest[$urlPattern, _?AssociationQ, ___], rest___] :=
    Replace[
        URLRead[req], 
        res_HTTPResponse :> Import[res, rest]
    ] 
HTTPResponse /: Import[res:HTTPResponse[$BodyPattern, _?AssociationQ, ___], f_:Automatic, opt:RepeatedNull[_Rule|_RuleDelayed]] :=
    Replace[
        res["Body"], 
        body_String :> 
            If[f === Automatic && First[toContentTypeWithEncoding[res]] === "application/json",
                importFromJSON[body],
                ImportString[body, f, opt]
            ]
    ]


    

End[]

EndPackage[]
