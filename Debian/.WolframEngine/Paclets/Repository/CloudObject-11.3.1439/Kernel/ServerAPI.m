(* ::Package:: *)

(* Mathematica package *)
BeginPackage["CloudObject`"]

Begin["`Private`"]

handleErrorDetails["max-viewers", extra_, head_] := Message[head::maxviewers, extra]
handleErrorDetails["unknown-user", extra_, head_] := Message[head::userunknown, extra]
handleErrorDetails["user-not-found", extra_, head_] := Message[head::invusr, extra]
handleErrorDetails[code_, extra_, head_] := Message[head::notparam]

(*TODO: handle various error codes*)
checkError[response_, msghd_Symbol:CloudObject] :=
    If[response === {$Failed, $Failed},
        True,
        With[{res = MatchQ[response, HTTPError[_Integer, ___]]},
            If[res,
                Switch[response,
                    HTTPError[400 | 412, {__Rule}, ___],
                        (* When the server returns a 400 or 412, it sometimes returns JSON data in the response body,
                         giving more details about the error. *)
                        handleErrorDetails[Lookup[response[[2]], "errorCode"], Lookup[response[[2]], "extraData"], msghd],
                    HTTPError[400, ___], Message[msghd::notparam],
                    HTTPError[401, ___], Message[msghd::notauth],(*might need a different message here*)
                    HTTPError[403, ___], Message[msghd::notperm],
                    HTTPError[404, ___], Message[msghd::cloudnf], (* TODO need a CloudObject to pass here *)
                    HTTPError[405, ___], Message[msghd::notmethod],
                    HTTPError[412, ___], Message[msghd::cloudprecondition],
                    HTTPError[429, ___], Message[msghd::rejreq],
                    HTTPError[500, ___], Message[msghd::srverr],
                    HTTPError[503, ___], Message[msghd::unavailable],
                    _, Message[msghd::cloudunknown]
                ]
            ];
            res
        ]
    ]

checkUUID[uuid_, obj_CloudObject] :=
    ( checkUUIDHelper[uuid, obj]; $Failed )

checkUUIDForExecute[uuid_, obj_CloudObject] :=
    ( checkUUIDHelper[uuid, obj]; {$Failed, $Failed} )

checkUUIDHelper[uuid_, obj_CloudObject] :=
    Switch[uuid,
        (* not found *)
        None, Message[CloudObject::cloudnf, obj],
        (* unauthenticated or not permitted. getCloudAndUUID returns the error message *)
        $Failed, Null,
        (* anything else *)
        _, Message[CloudObject::srverr, obj]
    ]

fetchURL[url_, elements_, options___] /; And[TrueQ[$CloudEvaluation], $CloudBase =!= $EvaluationCloudBase] := 
	URLFetch[url, elements, options]
fetchURL[url_, elements_, options___] := authenticatedURLFetch[url, elements, options]

(* The asynchronous version simply ignores the requested elements. It does not return anything, it just sets off the request. *)
fetchURLAsync[url_, elements_, options___] := (
    If[TrueQ[$CloudConnected], authenticatedURLFetchAsynchronous, URLFetchAsynchronous][url, Null, options];
    {200, {}, {}}
)

contentDisplay[list:{_Integer...}] := FromCharacterCode[list]
contentDisplay[value_String] := value
contentDisplay[expr_] := ToString[expr, InputForm]

preprocessContent[content_List] := FromCharacterCode[content, "UTF-8"]
preprocessContent[content_] := content

preprocessErrorContent[content_, "application/json"] :=
    Module[{data},
        data = importFromJSON[preprocessContent[content]];
        log["Error content: `1`", data, DebugLevel->2];
        data
    ]
preprocessErrorContent[content_, type_] := preprocessContent[content]

callServer[url_, mimetype_: "text/plain", httpVerb_: "GET", body_: "", async_ : False] :=
    Module[{response, status, headers, content, callFunction, finalURL, contentType, 
    	localBody = Replace[body, {} -> ""](* work around for bug-342303 *)},
        log["Calling remote server: `1` `2` with MIME type `3`", httpVerb, url, mimetype];
        log["Decoded URL: `1`", URLDecode[url], DebugLevel->2];
        log["Request content: `1`", contentDisplay[localBody], DebugLevel->2];
        finalURL = url;
        callFunction = If[async, fetchURLAsync, fetchURL];
        response = callFunction[finalURL, {"StatusCode", "Headers", "ContentData"},
           "Method"->httpVerb,
           "Headers"->{
               "Content-Type"->mimetype,
               "Accept"->"application/vnd.wolfram.v1"
           },
           "Body"->localBody,
           "VerifyPeer"->False,
           "DisplayProxyDialog" -> False
        ];
        If[MatchQ[response,{_,_,_}], {status, headers, content} = response, If[MatchQ[response, _HTTPError], Return[response], Return[HTTPError[404]]]];
        log["Response status: `1`", status];
        If[headers =!= {},
           log["Response headers: `1`", headers, DebugLevel->2];
        ];
        log["Response content: `1`", contentDisplay[content], DebugLevel->2];
        contentType = contentTypeCheck[Lookup[Rule @@@ headers, "Content-Type"]];
        If[Not[And[status >= 200, status < 300]],
           content = preprocessErrorContent[content, contentType];
           Return[HTTPError[status, content, contentType]]
        ];
        {contentType, content}
    ]

getUUID[cloud_, path_] := Module[{pathString, uuid},
    pathString = JoinURL[path];
    uuid = responseToString @ execute[cloud, "GET", {"files"}, Parameters -> {"path" -> pathString}];
    log["UUID for path `1`: `2`", pathString, uuid];
    If[uuid === "", None, uuid]
]

getCloud[uri_] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        cloud
    ]

getCloudAndUUID[obj : CloudObject[uri_String, ___]] := getCloudAndUUID[uri]
    
getCloudAndUUID[uri_String] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[uuid === None,
            uuid = getUUID[cloud, {user, path}],
        (* uuid set, check for path inside it (file inside an unnamed directory) *)
            If[extraPath =!= {},
                uuid = getUUID[cloud, {uuid, extraPath}]
            ]
        ];
        {cloud, uuid}
    ]

getCloudAndUUIDOrPath[CloudObject[uri_, ___]] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[extraPath === {},
            {cloud, uuid, If[path === None, None, Join[{user}, path]]},
        (* else: *)
            If[uuid === None,
            (* this will not actually happen, because extraPath is only set when uuid is set *)
                {cloud, None, Join[{user}, path, extraPath]},
            (* else *)
                {cloud, None, Join[{uuid}, extraPath]}
            ]
        ]
    ]

getCloudAndPathList[obj_CloudObject] :=
    Module[{cloud, uuid, path},
        {cloud, uuid, path} = getCloudAndUUIDOrPath[obj];
        {cloud, If[path === None, {uuid}, path]}
    ]

Options[execute] = {Parameters -> {}, Body -> {}, Type -> "text/plain", UseUUID -> True, Asynchronous -> False};

(* perform the execute locally, we are already in the cloud *)
Options[executeInCloud] = Options[execute];
executeInCloud[cloud_String, method_String, path_List : {}, OptionsPattern[]] :=
    Module[{parameters, mimetype = OptionValue[Type], body = OptionValue[Body],
        bodyString},

        parameters = OptionValue[Parameters];

        log["Calling server `1` `2` with MIME type `3`, parameters `4`", method,
            JoinURL[path], mimetype, ToString[parameters, InputForm],
            DebugLevel -> 2];
        If[body =!= {},
            log["Content: `1`", body, DebugLevel->2];
        ];
        (* string is more efficient than a list of bytes in the Java server *)
        bodyString = If[ListQ[body], FromCharacterCode[body], body];

        $lastExecuteResult = CloudSystem`Private`writeCallPacketService[
            CloudSystem`CloudObject`DoCloudOperation[method, path, parameters,
                mimetype, bodyString
            ]
        ];
        log["Call packet result: `1`", ToString[$lastExecuteResult, InputForm], DebugLevel -> 2];

        Replace[
            $lastExecuteResult,
            {
                {type_, File[resultFile_String]} :>
                    {contentTypeCheck[type], BinaryReadList[resultFile]},
                {type_, result_String} :> {contentTypeCheck[type], ToCharacterCode[result]},
                {type_, result_List} :> {contentTypeCheck[type], result},
                err:HTTPError[status_Integer?Positive, content_, type_] :>
                    HTTPError[status, preprocessErrorContent[content, type], type],
                err:HTTPError[_Integer?Positive] :> err,
                _ :> HTTPError[500]
            }
        ]
    ]

(* make an HTTP request to perform the execute *)
Options[executeRemotely] = Options[execute];
executeRemotely[cloud_String, method_String, path_List : {}, OptionsPattern[]] := Module[{url},
    url = JoinURL[{cloud, path}] <> JoinURLSearch[OptionValue[Parameters]];
    callServer[url, OptionValue[Type], method, OptionValue[Body], OptionValue[Asynchronous]]
]

execute[cloud_String, method_String, path_List : {}, opts:OptionsPattern[]] :=
    If[TrueQ[System`$CloudEvaluation] && (cloud === $EvaluationCloudBase),
        executeInCloud[cloud, method, path, opts]
        ,
        executeRemotely[cloud, method, path, opts]
    ]

execute[obj_CloudObject, method : _String | Automatic : "GET", api_String : "files", subpath_List : {}, options : OptionsPattern[]] :=
    Module[{cloud, uuid, path, methodToUse, parameters, newOptions, optionBody, bodyIsCloudObject},
        If[OptionValue[UseUUID] === True,
            {cloud, uuid} = getCloudAndUUID[obj];
            If[!UUIDQ[uuid], Return[checkUUIDForExecute[uuid, obj]]];
            log["Executing on UUID `1`", uuid];
            execute[cloud, method, {api, uuid, subpath}, options],
        (* else *)
            {cloud, uuid, path} = getCloudAndUUIDOrPath[obj];
            (* uuid can be None *)
            If[uuid === $Failed, Return[{$Failed, $Failed}]];
            If[method === Automatic,
                If[uuid === None,
                    methodToUse = "POST",
                    methodToUse = "PUT"
                ]
            ];
            parameters = OptionValue[Parameters];
            optionBody = OptionValue[Body];
            bodyIsCloudObject = Head[optionBody] === CloudObject;
            If[bodyIsCloudObject,
                Module[{srccloud, srcuuid},
                    {srccloud, srcuuid} = getCloudAndUUID[optionBody];
                    If[!UUIDQ[srcuuid], Return[checkUUIDForExecute[srcuuid, optionBody]]];
                    parameters = Join[parameters, {"copyContentFrom" -> srcuuid}];
                ]
            ];
            newOptions = Join[If[bodyIsCloudObject, {Body -> {}}, {}], {options}];
            If[uuid === None,
                execute[cloud, methodToUse, {api, subpath}, Parameters -> Join[parameters, {"path" -> JoinURL[path]}], newOptions],
                execute[cloud, methodToUse, {api, uuid, subpath}, Parameters -> parameters, newOptions]
            ]
        ]
    ]

contentTypeCheck[type_] := Replace[type, Except[_String] -> "text/plain"]

responseToString[{type_, content_List}, head_] := FromCharacterCode[content, "UTF-8"]
responseToString[{type_, content_String}, head_] := content
responseToString[response_, head_] := $Failed /; checkError[response, head]
responseToString[response_] := responseToString[response, CloudObject]

responseToExpr[response_] := Replace[responseToString[response], r_String :> ToExpression[r]]

responseToStringList[response_, head_] := StringSplit[responseToString[response, head], "\n"]
responseToStringList[response_, head_] := $Failed /; checkError[response, head]

dumpBinary[filename_, contents_] := Module[{file},
    file = OpenWrite[filename, BinaryFormat -> True];
    BinaryWrite[file, contents];
    Close[file];
]

responseToFile[{type_, content_List}, head_:CloudObject] := Module[{tempfilename},
    tempfilename = CreateTemporary[];
    dumpBinary[tempfilename, content];
    {tempfilename, type}
]
responseToFile[response_, head_:CloudObject] := {$Failed, $Failed} /; checkError[response, head]

responseCheck[response_, head_, result_] :=
    If[checkError[response, head],
        $Failed,
        result
    ]
responseCheck[response_, head_ : CloudObject] := responseCheck[response, head, Null]

End[]

EndPackage[]
