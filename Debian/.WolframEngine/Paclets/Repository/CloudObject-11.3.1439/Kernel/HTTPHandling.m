BeginPackage["CloudObject`"]

System`HTTPRequestData
Hold[CloudObject`$EvaluationParameters]
Hold[System`$HTTPRequest]
Hold[System`$RequesterAddress]

Begin["`Private`"]

CloudObject`$EvaluationParameters = <| |> (* default in desktop environment *)

Unprotect[{$HTTPRequest, HTTPRequestData}];
ClearAll[$HTTPRequest];
ClearAll[HTTPRequestData];

(* 
 properties provided by cloud 
 "Fragment" isn't supported because the cloud server doesn't know it,
   cf. http://stackoverflow.com/questions/13503213/how-to-get-the-url-fragment-identifier-from-httpservletrequest
 "User" isn't supported because the HttpServletRequest reports the OAuth, not a username that's explicitly
   in the URL.  In fact, the username isn't even visible in HttpServletRequest.getRequestURL() when
   visiting http://me@domain/path  So the best we could do is to make "User" equivalent to
   $RequesterWolframID (see also $WolframID).
 "Password" is completely unavailable, and that's surely a good thing
 *)

$fundHttpRequestData = <|
    "Domain"            -> None,
    "Scheme"            -> None,
    "Method"            -> "GET",
    "Headers"           -> {},
    "Parameters"        -> None,
    "MultipartElements" -> None,
    "Port"              -> None,
    "BodyByteArray"     -> None,
    "Body"              -> None, (* actually recursive, but with a per-request CharacterEncoding *)
    "SessionID"         -> None
|>;

(* properties derived from $rawHttpRequestData properties via URLParse *)
$httpRequestDataViaURLParse = <|
    (* doesn't actually require the consituent components, as weird as that sounds *)
    "AbsolutePath"     :> URLParse[$HTTPRequest, "AbsolutePath"],
    "AbsoluteDomain"   :> URLParse[$HTTPRequest, "AbsoluteDomain"],
    "Path"             :> URLParse[$HTTPRequest, "Path"],
    "Query"            :> URLParse[$HTTPRequest, "Query"],
    "PathString"       :> URLParse[$HTTPRequest, "PathString"],
    "QueryString"      :> URLParse[$HTTPRequest, "QueryString"]
|>;

(* properties stored elsewhere from $HTTPRequest *)
$httpRequestDataViaGlobals = <|
    "RequesterAddress" :> $RequesterAddress,
    "FormRules"        :> makeFormRules @@ HTTPRequestData[{
        "Parameters", 
        "Query", 
        "MultipartElements"
    }],
    "DispatchPathString" :> HTTPRequestData["PathString"]
|>;

(* backwards-compatible properties that were renamed.  This can also be used
   to define synonyms.  These fields do not appear in "Properties". *)
$httpRequestDataViaOldNames = <|
    "MultipartContent" :> HTTPRequestData["MultipartElements"],
    "BodyData"         :> HTTPRequestData["Body"]
|>;    

$httpRequestDataViaRecursion = <|
    "BodyBytes"        :> Normal@HTTPRequestData["BodyByteArray"]
|>;

(* the complete list of HTTPRequestData["Properties"] when $HTTPRequest is an association *)
$allHttpRequestData = Join[
    $fundHttpRequestData,
    $httpRequestDataViaURLParse,
    $httpRequestDataViaGlobals,
    $httpRequestDataViaOldNames,
    $httpRequestDataViaRecursion
];

$httpRequestDataProperties = Complement[Keys[$allHttpRequestData], Keys[$httpRequestDataViaOldNames]]

$HTTPRequest = None; (* default in desktop environment *)

completeHTTPRequestData[] :=
    If[ AssociationQ[$HTTPRequest],
        (* the RuleDelayed's in $allHttpRequestData means that the rhs's
           don't evaluate, even if you try to use part extraction.  Thus
           Join[
               $allHttpRequestData[$httpRequestDataProperties (* or missing properties *)],
               $HTTPRequest
           ]
           doesn't work. *)
        AssociationThread[$httpRequestDataProperties, HTTPRequestData[$httpRequestDataProperties]]
        ,
        $HTTPRequest
    ]

HTTPRequestData[] := completeHTTPRequestData[];

HTTPRequestData["Properties"] := 
    Replace[$HTTPRequest, {
        assoc_?AssociationQ :> $httpRequestDataProperties,
        _ :> {}
    }]

(* computeHTTPRequestData is listable to level 1 *)
computeHTTPRequestData[prop_] :=
    Module[{success = True},
        Lookup[
            $allHttpRequestData,
            prop,
            If[ success,
                Message[HTTPRequestData::notprop, findBadProp[prop], HTTPRequestData];
                success = False;
            ]
        ]
    ];
(* the first time copmuteHTTPRequestData[_List] fails to find a property,
   it calls findBadProp to find the first invalid property in the list. *)
findBadProp[arg_List] := SelectFirst[arg, !KeyExistsQ[$allHttpRequestData, #]&, System`Private`SystemAssert[Row[{arg, " should have at least one invalid HTTPRequestData property"}]]];
findBadProp[arg_] := arg; (* it wasn't a list, so arg is the bad property *)

(* used by makeFormRules to delete the right number of copies of elements of
   the second list from the first.  This works by counting the number of
   each element in the first list, subtracting off the corresponding elements
   of the other lists, and then reconstructing the right number of remaining
   elements. *)
multisetComplement[a_List, {}...] := a;
multisetComplement[{}, ___] := {};
multisetComplement[a_List, b__List] :=
  Apply[Join,
    Apply[ConstantArray,
      Map[{#[[1, 1]], Clip[Total[#[[All, 2]]], {0, Infinity}]} &,
        GatherBy[
          Join[
            Tally[a],
            Apply[{#1, -#2} &, Tally[Join[b]], {1}]
          ],
          First
        ]
      ],
     {1}
    ]
  ]

(* make FormRules by:
    * taking the complement of Parameters and Query
    * adding MultipartElements but as "param" -> content, not param -> allInfoAboutFile
*)
makeFormRules[params_, queryParams_, multipart_] :=
    Join[
        multisetComplement[
            Replace[params, Except[_?ListQ] :> {}],
            Replace[queryParams, Except[_?ListQ] :> {}]
        ]
        ,
        makeMultipartFormRules[multipart]
    ] 

makeMultipartFormRules[multipart_?ListQ] :=
    Function[part,
        With[{partInfo = Association[part[[2]]]},
            If[ AssociationQ[partInfo],
                Which[
                    KeyExistsQ[partInfo, "ContentString"]
                    && (!KeyExistsQ[partInfo, "OriginalFileName"]
                        || MatchQ[Lookup[partInfo, "OriginalFileName"], None | Null | ""]
                        || TrueQ[Lookup[partInfo, "InMemory"]]),
                    part[[1]] -> Lookup[partInfo, "ContentString"] (* TODO: is this correct or should we make a temporary file? *)
                    ,
                    KeyExistsQ[partInfo, "FileName"],
                    part[[1]] -> File[Lookup[partInfo, "FileName"]]
                    ,
                    KeyExistsQ[partInfo, "ContentString"],
                    (* take care to not prematurely evaluate any Import.
                       TODO: as above, do we really want to drop the info that it was an uploaded file? *)
                    Extract[partInfo, "ContentString", Function[c, part[[1]] :> c, {HoldAllComplete}]]
                    ,
                    True,
                    Sequence@@{} (* drop it -- invalid *)
                ]
                ,
                Sequence@@{} (* drop it -- invalid *)
            ]
        ]
    ] /@ multipart;
makeMultipartFormRules[_ (* e.g. None *)] = {}


HTTPRequestData[argIn_] := 
    Module[{success = True, res, arg = translateHTTPRequestData[argIn]},
        Replace[$HTTPRequest, {
            assoc_?AssociationQ :>
                (res = Lookup[assoc, arg, (success=False; HTTPRequestDataMissing)];
                 If[ !success,
                     success = True; (* assume again *)
                     With[{pos = If[ ListQ[arg],
                                     Flatten@Position[res, HTTPRequestDataMissing, {1}, Heads->False],
                                     Sequence@@{} (* i.e. Part[expr, pos] === expr, even when an atom *)
                                   ]},
                         System`Private`SystemAssert[(ListQ[res] && Length[res] === Length[arg])
                                                     || (!ListQ[res] && !ListQ[arg])];
                         res[[pos]] =
                             Check[
                                 computeHTTPRequestData[arg[[pos]]],
                                 success = False; HTTPRequestDataMissing,
                                 {HTTPRequestData::notprop}
                             ]
                     ]
                 ])
             ,
             _ :> (
                Message[HTTPRequestData::notprop, arg, HTTPRequestData];
                success = False
            )
        }];
        res /; success
    ] 

checkNumberOfArguments:HTTPRequestData[___] :=
    $Failed /; System`Private`Arguments[checkNumberOfArguments, {0, 1}]

translateHTTPRequestData["BaseRequestURL"] = "PathString";
translateHTTPRequestData["HTTPMethod"] = translateHTTPRequestData[Method] = "Method";
translateHTTPRequestData["RequestHeaderRules"] = "Headers";
translateHTTPRequestData["ParameterRules"] = "Parameters";
translateHTTPRequestData[prop_Symbol] := SymbolName[prop]; (* courtesy, general case of Method, similar to Option names *)
translateHTTPRequestData[prop_List] := Replace[prop, p:Except[_List]:>translateHTTPRequestData[p], {1}]; (* don't recurse into additional (invalid) lists *)
translateHTTPRequestData[prop_] := prop;

SetAttributes[{HTTPRequestData}, {Protected, ReadProtected}];

End[]

EndPackage[]
