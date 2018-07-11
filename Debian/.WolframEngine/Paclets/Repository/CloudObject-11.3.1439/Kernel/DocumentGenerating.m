(* ::Package:: *)

(* Mathematica package *)
BeginPackage["CloudObject`"];

System`DocumentGenerator;
System`DocumentGeneratorInformation::usage = "DocumentGeneratorInformation[CloudObject] returns information about a generator.
DocumentGeneratorInformation[CloudObject, property] returns the value of the specified property.";
System`DocumentGeneratorInformationData;
System`DocumentGenerators::usage = "DocumentGenerators[] returns a list of the user's document generators, as cloud objects.";

(* Option symbols *)
System`DeliveryFunction;
System`GeneratorDescription;
System`EpilogFunction;
System`GeneratorHistoryLength;
System`GeneratorOutputType;

System`GeneratedDocumentBinding;

Begin["`Private`"];


Unprotect[DocumentGenerator, DocumentGenerators, DocumentGeneratorInformation, CloudObject];


Unprotect[DocumentGenerator, CloudObject];
SetAttributes[DocumentGenerator, {HoldAll, ReadProtected}];

simpleLog[args___] := Print[
    DateString[{"DateShort", " ", "Time"}],
    "\t", 
    Apply[Sequence, Replace[{args}, s_StringForm :> ToString[s], {1}]]
];

$docGenMimeTypes = {"application/vnd.wolfram.bundle.document-generator"};
(* Slow! *)
documentGeneratorQ[co_CloudObject] := 
    With[{mime = Quiet[Check[CloudObjectInformation[co, "MIMEType"], $Failed]]},
    	MemberQ[$docGenMimeTypes, mime]
    ];
documentGeneratorQ[_] := False;

Options[DocumentGenerator] = Sort @ Join[{
        GeneratorDescription -> None,
        EpilogFunction -> None,
        GeneratorHistoryLength -> Infinity,
        DeliveryFunction -> None,
        
        GeneratorOutputType -> "StaticPage",
        Permissions -> Automatic (* Applies to generated documents *)
    },
    Options[ScheduledTask]
];

driverTypeQ[x_] := MatchQ[x, Alternatives @@ {
    None,
    _File, Delayed[_File],
    _CloudObject, Delayed[_CloudObject],
    _Association (*, Delayed[_Association] *),
    _Function
}];

templateTypeQ[x_] := MatchQ[x, Alternatives @@ {
    _File, Delayed[_File],
    _CloudObject, Delayed[_CloudObject],
    _TemplateObject (* Delayed[_TemplateObject] *)
}];

epilogTypeQ[x_] := MatchQ[x, Alternatives @@ {
    None,
    _File, Delayed[_File],
    _CloudObject, Delayed[_CloudObject],
    _Function,
    _Missing (* Not supplied in options *)
}];

(* Upload local files from a desktop client, take care of as many cases as possible
 * on the server.
 *)
resolveDocGenResource[res_] := Check[
    Replace[res, {
        File[f_] :> CloudObject`Private`deleteable@CopyFile[FindFile[f], CloudObject[]],
        p:CloudObject`Private`preexisting[_] :> p,
        t:Except[_Delayed|_CloudObject|None] :> CloudObject`Private`deleteable@CloudPut[t, IncludeDefinitions -> True]
    }],
    Throw[$Failed, $tag]
];

toBeCopiedQ[_File] := True;
toBeCopiedQ[_CloudObject] := True;
toBeCopiedQ[CloudObject`Private`deleteable[_]] := True;
toBeCopiedQ[CloudObject`Private`preexisting[_]] := False;
toBeCopiedQ[_] := False;

$deliveryFormats = Join[ToUpperCase[$ExportFormats], ToLowerCase[$ExportFormats]];(*{"NB", "PDF", "CDF", Null, None};*)
$outputFormats = {"CloudCDF", "PDF", "CDF", "StaticPage"};

validateDocGenArg["options", {{}, r_}] := True;
validateDocGenArg["options", {unknown_List, r_}] := (Message[DocumentGenerator::optx, First[unknown], r]; False);
(* No way to evaluate desktop file at runtime: *)
validateDocGenArg[a:"template"|"driver"|"epilog", Delayed[f_File]] := If[Not[TrueQ[$CloudEvaluation]],
    Message[DocumentGenerator::badarg, f, a]; False,
    True
];
validateDocGenArg[a:"template"|"driver"|"epilog", preexisting[p_]] := validateDocGenArg[a, p];
validateDocGenArg["template", _?templateTypeQ] := True;
validateDocGenArg["driver", _?driverTypeQ] := True;
validateDocGenArg["epilog", _?epilogTypeQ] := True;
validateDocGenArg["notification", Automatic|All|None] := True;
validateDocGenArg["notification", _String -> Automatic|All] := True;
validateDocGenArg["notification", {___String}] := True;
validateDocGenArg["notification", {({__String} -> (Automatic|All|None|_Function|_List))..}] := True;
validateDocGenArg["notification", _Function] := True;
validateDocGenArg["notification", a__] := (Message[DocumentGenerator::badarg, a, NotificationFunction]; False);
validateDocGenArg["delivery", Automatic|None] := True;
validateDocGenArg["delivery", Alternatives @@ $deliveryFormats] := True;
validateDocGenArg["delivery", {___String}] := True;
validateDocGenArg["delivery", {__String} -> Alternatives @@ $deliveryFormats | Automatic| _Function] := True;
validateDocGenArg["delivery", {({__String} -> Alternatives @@ $deliveryFormats | Automatic| _Function)..}] := True;
validateDocGenArg["delivery", _String -> Alternatives @@ $deliveryFormats | Automatic] := True;    (* non-email channel delivery *)
validateDocGenArg["delivery", _Function] := True;
validateDocGenArg["delivery", a__] := (Message[DocumentGenerator::badarg, a, DeliveryFunction]; False);
validateDocGenArg["outputformat", Alternatives @@ $outputFormats] := True;
validateDocGenArg["outputformat", f_] := (Message[DocumentGenerator::badarg, f, GeneratorOutputType]; False);
validateDocGenArg["historylength", _Integer?(# >= 1 &)] := True;
validateDocGenArg["historylength", Infinity|DirectedInfinity] := True;
validateDocGenArg["historylength", h_] := (Message[DocumentGenerator::badarg, h, GeneratorHistoryLength]; False);
(* Better time zone validation in ST path, this is almost pointless. *)
validateDocGenArg["timezone", _String] := True;
validateDocGenArg["timezone", _?NumericQ] := True;
validateDocGenArg["timezone", Automatic] := True;
validateDocGenArg["timezone", HoldPattern[Entity["TimeZone", _String]]] := True;
validateDocGenArg["timezone", t_] := (Message[DocumentGenerator::badarg, t, TimeZone]; False);
validateDocGenArg["autoremove", True|False] := True;
validateDocGenArg["autoremove", a_] := (Message[DocumentGenerator::badarg, a, AutoRemove]; False);
validateDocGenArg[__] := False;


(* No driver *)
DocumentGenerator /: CloudDeploy[DocumentGenerator[t_?templateTypeQ, sched_, o:OptionsPattern[]], args___] :=
    CloudDeploy[DocumentGenerator[t, None, sched, o], args];
    
DocumentGenerator /: CloudDeploy[r:DocumentGenerator[t_?templateTypeQ, d_?driverTypeQ, sched_, o:OptionsPattern[]],
    co_CloudObject, oD:OptionsPattern[]] :=
    Catch[iCloudDeployDocumentGenerator[DocumentGenerator[t, d, sched, o], co, oD], $tag]

iCloudDeployDocumentGenerator[
    r:DocumentGenerator[templateRaw_?templateTypeQ, driverRaw_?driverTypeQ, sched_, o:OptionsPattern[]],
    co:CloudObject[uri_String, ___],
    oD:OptionsPattern[]
] := Module[
    {cloud, uuid, name,
    	runImmediately, cronned,
    	opts = Flatten[{o}],
    	params, rJson, taskJson,
    	templateMed = templateRaw, driverMed = driverRaw, templateRes, driverRes, epilogRes = None,
    	updating = False, existingOpts = {}, existingTaskOpts = {}, endpoint
    },
    {cloud, uuid, name} = Replace[getCloudAndUUIDOrPath[co], None -> Null, {1}];
    name = Replace[name, 
    	{n : {"user-" ~~ $CloudUserUUID, __} :> FileNameJoin[Rest[n], OperatingSystem -> "Unix"],
        n : {$UserURLBase, __} :> FileNameJoin[Rest[n], OperatingSystem -> "Unix"],
        n_List :> FileNameJoin[n, OperatingSystem -> "Unix"]}];

    {runImmediately, cronned} = Which[
        nowQ[sched],
        {True, resolveTimespec[None]},
        
        True,
        {False, resolveTimespec[sched]}
    ];

    If[MatchQ[cronned, $Failed],
        Message[ScheduledTask::sched, sched];
        Throw[$Failed, $tag]
    ];


    If[FileExistsQ[co] && MemberQ[$docGenMimeTypes, CloudObjectInformation[co, "MIMEType"]],
        updating = True;
        (* The api expects a relative path for renaming, so updating with the same name
         * moves the report to a new subdirectory. There is no way to change the name from WL. *)
        name = Replace[name, n_String :> FileNameTake[n]]; 
        Check[With[{dgInfo = CloudObject`Private`iCloudDocumentGeneratorInformation[co]}, 
                (* Dig up DG-relevant task options (TimeZone etc.) *)
                existingOpts = Replace[
                    FilterRules[
                        Join[Normal@dgInfo, Normal@dgInfo["Task"]],
                        Options[DocumentGenerator]
                    ],
                    Rule[EpilogFunction, e:Except[None]] :> Rule[EpilogFunction, preexisting[e]],
                    {1}
                ];
                existingTaskOpts = FilterRules[Normal@dgInfo["Task"], Options[ScheduledTask]];

                If[MatchQ[driverRaw, _CloudObject] && GetUUID[driverRaw] === Lookup[dgInfo, "DriverUUID"],
                    driverMed = preexisting[driverRaw];
                ];
                If[MatchQ[templateRaw, _CloudObject] && GetUUID[templateRaw] === Lookup[dgInfo, "TemplateUUID"],
                    templateMed = preexisting[templateRaw];
                ]
            ],
            Throw[$Failed, $tag]
        ]
    ];
    opts = Flatten[Join[opts, existingOpts, Options[DocumentGenerator]]];

    If[And @@ # === False, Throw[$Failed, $tag]] & @ MapThread[
        validateDocGenArg[#1, #2] &,
        {
            {"options", "template", "driver", "delivery", "epilog", "historylength", "notification", "outputformat",
                "timezone", "autoremove"},
            {{FilterRules[opts, Except[Options[DocumentGenerator]]], r}, templateMed, driverMed,
                Lookup[opts, DeliveryFunction], Lookup[opts, EpilogFunction],
                Lookup[opts, GeneratorHistoryLength], Lookup[opts, NotificationFunction], Lookup[opts, GeneratorOutputType],
                Lookup[opts, TimeZone], Lookup[opts, AutoRemove]}
        }
    ];
    templateRes = resolveDocGenResource[templateMed];
    driverRes = resolveDocGenResource[driverMed];
    epilogRes = resolveDocGenResource[Lookup[opts, EpilogFunction]];
    (* Print["resolved ress:", {templateRes, driverRes, epilogRes}]; *)
    
    (* To correctly support generator updating, must be careful to pass
     * only explicitly provided options. *)
    opts = Flatten[Join[Replace[opts, {
            Rule[TimeZone, Automatic] :> Rule[TimeZone, $TimeZone],
            Rule[EpilogFunction, e_] :> {
                Rule["EpilogSrcUUID", Last@safeCloudAndUUIDFetch[epilogRes, DocumentGenerator]],
                Rule["CopyEpilog", toBeCopiedQ[epilogRes]]
            }
        }, 1], 
        {
            Rule["TemplateSrcUUID", Last@safeCloudAndUUIDFetch[templateRes, DocumentGenerator]],
            Rule["CopyTemplate", toBeCopiedQ[templateRes]],
            Rule["DriverSrcUUID", Last@safeCloudAndUUIDFetch[driverRes, DocumentGenerator]],
            Rule["CopyDriver", toBeCopiedQ[driverRes]],
            Rule["Name", name],
            Rule["UUID", uuid]
        }
    ], 1];
    (* Print@opts; *)

    params = {"properties" -> exportToJSON[unpresentifyDocGenMeta[FilterRules[opts, Except[Options[ScheduledTask]]]], "Compact" -> True],
        "path" -> Replace[name, Null -> ""],
        "task" -> (taskJson = With[{expr = uuid, timespec = sched},
            generateTaskJson[ScheduledTask[expr, (* unused *) timespec], {name, Null}, cronned,
                (* "TaskType" will be taken care of on the server *)
                FilterRules[opts, Options[ScheduledTask]],
                existingTaskOpts
            ]
        ])
    };
    (* Print@taskJson; *)
    (* Print[params]; *)

    endpoint = If[TrueQ[updating], {"reports", Last@safeCloudAndUUIDFetch[co, DocumentGenerator]}, {"reports"}]; 
    With[{mh = DocumentGenerator},
        rJson = Replace[execute[$CloudBase, "POST", endpoint, Parameters -> params], {
            {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , HTTPError[400, ___] :> (Message[mh::argu]; Throw[$Failed, $tag])
            , HTTPError[403, ___] :> (Message[ScheduledTask::restr]; Throw[$Failed, $tag])
            , HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
            , other_ :> (Message[mh::srverr]; Message[mh::crea]; Throw[$Failed, $tag])
        }];
    ];
    (* Print[rJson]; *)
    
    If[TrueQ[runImmediately], RunScheduledTask @@ {co}];

    (* Cleanup *)
    Cases[{templateRes, driverRes, epilogRes},
        CloudObject`Private`deleteable[del_CloudObject] :> Quiet[DeleteFile[del]]
    ];
    
    co
]

iCloudDeployDocumentGenerator[DocumentGenerator[args___, o:OptionsPattern[]], ___] := 
    (ArgumentCountQ[DocumentGenerator, Length[Hold[args]], 3, 3]; $Failed)
(* iCloudDeployDocumentGenerator[___] := $Failed *)



Options[iCloudRunDocumentGenerator] = {
    GeneratedDocumentBinding -> Automatic
};

iCloudRunDocumentGenerator[obj_CloudObject, mh_:RunScheduledTask, o:OptionsPattern[]] := 
    (iCloudRunDocumentGenerator[safeCloudAndUUIDFetch[obj, mh], mh, o]; obj)
iCloudRunDocumentGenerator[{cloud_String, uuid_String}, mh_:RunScheduledTask, o:OptionsPattern[]] := Module[
    {json, params},

    params = Replace[OptionValue[GeneratedDocumentBinding], {
        Automatic|Null|None -> {},
        a_Association :> Normal[a],
        x:Except[{___Rule}] :> (Message[DocumentGenerator::badarg, x, GeneratedDocumentBinding]; Throw[$Failed, $tag]) 
    }];

    json = Replace[execute[cloud, "POST", {"reports", uuid, "execute"}, Parameters -> params], {
        HTTPError[400, content_, ___] :> ( (* object inactive *)
            mess = ToExpression[Lookup[importFromJSON[content], "error"], InputForm, Hold];
            ReleaseHold[Replace[mess, slug_ :> Message[slug, cloudObjectFromUUID[uuid]], 1]];
            Throw[$Failed, $tag]
        )
        , HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, obj]; Throw[$Failed, $tag])
    }];
    uuid
];

(* iCloudRunDocumentGenerator[r_, OptionsPattern[]] := handleGeneratingResponse[$Failed] *)


(*
 * Lower-level synchronous evaluation, bypasses task notification etc.
 * Two implementations are provided here, one that goes through the custom API endpoint and one
 * that uses CloudEvaluate. The custom API would have the advantage of unifying all code paths,
 * except that the CloudObjectCallPacketHandler doesn't currently have enough context to do general kernel 
 * evaluations. So the API version doesn't work in cloud notebooks. The CloudEvaluate version
 * is hooked up for CloudObject clients, and the endpoint is maintained for web clients.
 *)
Options[iCloudEvaluateDocumentGeneratorAPI] = Options[iCloudEvaluateDocumentGeneratorCE] = 
Join[Options[iCloudRunDocumentGenerator], {
    "LoggingFunction" -> None
}];

iCloudEvaluateDocumentGeneratorAPI[obj_CloudObject, mh_:EvaluateScheduledTask, o:OptionsPattern[]] := 
    (iCloudEvaluateDocumentGeneratorAPI[safeCloudAndUUIDFetch[obj, mh], mh, o]; obj)
iCloudEvaluateDocumentGeneratorAPI[{cloud_String, uuid_String}, mh_:EvaluateScheduledTask, o:OptionsPattern[]] := Module[
    {json, params},

    params = Replace[OptionValue[GeneratedDocumentBinding], {
        Automatic|Null|None -> {},
        a_Association :> Normal[a],
        x:Except[{___Rule}] :> (Message[DocumentGenerator::badarg, x, GeneratedDocumentBinding]; Throw[$Failed, $tag]) 
    }];

    json = Replace[execute[cloud, "POST", {"reports", uuid, "evaluate"}, Parameters -> params], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, obj]; Throw[$Failed, $tag])
    }];
    
    json
];


(* This interface uses direct WL invocation, so Java will not have been able to set a time zone abbreviation. *)
iCloudEvaluateDocumentGeneratorCE[r_CloudObject, dest_CloudObject, o:OptionsPattern[]] := Block[
    {binding = OptionValue[GeneratedDocumentBinding], rLog, pres},

    With[{lf = OptionValue["LoggingFunction"]},
        rLog = Replace[lf, {Automatic -> simpleLog, f_Function :> (f[Now, ##] &), _ -> Identity}]
    ];

    (* Make option binding available to report object at run time. Cleaner way to do this? *)
    SetOptions[r, MetaInformation -> "__Binding" -> Replace[binding, {Automatic -> Missing[Undefined]}]];
    pres = If[TrueQ[$CloudEvaluation], Identity, internalCloudEvaluate][
        Module[{res, jobObj},
            jobObj = CloudSystem`Scheduling`Private`fetchReportDetails[r];
            res = CloudSystem`DocumentGenerating`EvaluateCloudDocumentGenerator[jobObj, "LoggingFunction" -> rLog, o];
            If[res === $Failed, Return[$Failed]];
            rLog@StringForm["Resource `` generated", res];
            (* Generate presentable *)
            Check[Switch[Lookup[jobObj, GeneratorOutputType],
                (* TODO: not sure that this relies on stable behavior of CloudDeploy *)
                "StaticPage", CloudDeploy[Import[res], dest],
                "CDF", CloudExport[Import[res], "CDF", dest],
                "PDF", CloudExport[Import[res], "PDF", dest],
                _, CloudExport[Import[res], "CloudCDF", dest]
            ], rLog@StringForm["Error generating presentable output from ``", res]; $Failed]
        ]
    ];

    If[pres === $Failed,
        Throw[$Failed, $tag]
    ];

    pres
]

(* iCloudEvaluateDocumentGenerator[co_, OptionsPattern[]] := handleGeneratingResponse[$Failed] *)

(* RemoveScheduledTask *) 
iCloudRemoveDocumentGenerator[obj_CloudObject, mh_:RemoveScheduledTask, o:OptionsPattern[]] := (iCloudRemoveDocumentGenerator[safeCloudAndUUIDFetch[obj, mh], mh, o]; obj)
iCloudRemoveDocumentGenerator[{cloud_String, uuid_String}, mh_:RemoveScheduledTask, o:OptionsPattern[]] := Module[
    {json},
    json = Replace[execute[cloud, "DELETE", {"reports", uuid}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr]; Message[ScheduledTask::norm, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];
    uuid
];

(* iCloudRemoveDocumentGenerator[co_, OptionsPattern[]] := (Message[DocumentGenerator::norm]; $Failed) *)


(* DocumentGeneratorInformation *)
SetAttributes[DocumentGeneratorInformation, {ReadProtected}];

DocumentGeneratorInformation::noprop = "`1` is not a property returned by DocumentGeneratorInformation.";

(* Convert DeliveryFunction from flat list *)
gatherDeliveryFunction[n:{{__}...}] := SortBy[
    Sort[Lookup[#, "email"]] -> ToExpression[Lookup[#[[1, -1]], "format"]] & /@ GatherBy[n, Lookup["format"]],
    Last
];


(* Output is a flat list digestible by the web ui. *)
denormalizeDeliveryFunction[dfRaw_] := Module[
    {df = Replace[dfRaw, {
        Null|None -> {},
        Automatic -> {{$CloudUserID} -> Automatic},
        s_String -> {{$CloudUserID} -> s},
        {u__String} :> {{u} -> Automatic},
        Rule[{u__String}, f_] :> {{u} -> f},
        f_Function :> {{$CloudUserID} -> f},
        (* non-mail channels *)
        Rule[s_String, form_] :> {Rule[{$CloudUserID}, {s, form}]}
    }]},
    
    (* Naked addresses etc. *)
    df = Replace[
        df,
        {
            (addr_ -> form_) :> Flatten[{addr}] -> Replace[form, {None -> Null, s_ :> ToString[InputForm[s]]}]
        },
        {1}
    ];
    
    With[{pairs = DeleteDuplicates @ Flatten[(Thread[List @@ #1, List, 1] &) /@ df, 1]},
        {"email" -> First@#, "format" -> Last@#} & /@ pairs
    ]
]


$docGenNormalizationRules = {
    Rule[tag:"CreationDate"|"LastModificationDate"|"LastRunDate", 0|Null] :> Rule[tag, None],
    Rule[tag:"CreationDate"|"LastModificationDate"|"LastRunDate", t_?NumericQ] :> Rule[tag, FromUnixTime[Round[t/1000]]],
    Rule["CurrentDocument", uuid_String] :> {
        Rule["CurrentDocumentUUID", uuid],
        Rule["CurrentDocument", cloudObjectFromUUID[uuid]]
    },
    Rule[DeliveryFunction, mo_] :> Rule[DeliveryFunction, gatherDeliveryFunction[mo]],
    Rule[de:"Driver"|EpilogFunction, Null] :> Rule[de, None],
    Rule["Driver", uuid_String] :> {
        Rule["DriverUUID", uuid],
        Rule["Driver", cloudObjectFromUUID[uuid]]
    },
    Rule["DriverSrc", uuid_String] :> {
        Rule["DriverSrcUUID", uuid],
        Rule["DriverSrc", cloudObjectFromUUID[uuid]]
    },
    Rule[EpilogFunction, uuid_String] :> {
        Rule[EpilogFunction, uuid],
        Rule[EpilogFunction, cloudObjectFromUUID[uuid]]
    }, 
    Rule["EpilogFunctionSrc", uuid_String] :> {
        Rule["EpilogFunctionSrc", uuid],
        Rule["EpilogFunctionSrc", cloudObjectFromUUID[uuid]]
    }, 
    Rule["Name", Null] -> Rule["Name", None],
    Rule[GeneratorDescription, Null] -> Rule[GeneratorDescription, None],
    Rule["GeneratedDocumentHistory", uuid_String] :> {
        Rule["GeneratedDocumentHistoryUUID", uuid],
        Rule["GeneratedDocumentHistory", cloudObjectFromUUID[uuid]]
    },
    Rule[GeneratorHistoryLength, -1] -> Rule[GeneratorHistoryLength, Infinity],
    Rule["Log", uuid_String] :> {
        Rule["LogUUID", uuid],
        Rule["Log", cloudObjectFromUUID[uuid]]
    },
    Rule["CurrentOutput", uuid_String] :> {
        Rule["CurrentOutputUUID", uuid],
        Rule["CurrentOutput", cloudObjectFromUUID[uuid]]
    },
    Rule[Permissions, s_String] :> Rule[Permissions, ToExpression[s]],
    Rule["Task", expr_] :> Rule["Task", taskMetaToWL[expr]],
    Rule["Template", uuid_String] :> {
        Rule["TemplateUUID", uuid],
        Rule["Template", cloudObjectFromUUID[uuid]]
    },
    Rule["TemplateSrc", uuid_String] :> {
        Rule["TemplateSrcUUID", uuid],
        Rule["TemplateSrc", cloudObjectFromUUID[uuid]]
    },
    Rule["UUID", uuid_String] :> {
        Rule["UUID", uuid],
        Rule["Directory", cloudObjectFromUUID[uuid]]
    }
};


$docGenDenormalizationRules = {
    (* Make sure the time is not an integer here, to work around bug 289879. *)
    Rule["archiveLength", Infinity|DirectedInfinity] -> Rule["archiveLength", -1],
    Rule[tag:"creationDate"|"lastModificationDate"|"lastRunDate", t:(_DateObject|_?NumericQ)] :> 
        Rule[tag, ToString@Round[1000*ToUnixTime[t]]],
    Rule["outputPermissions", expr_] :> Rule["outputPermissions", ToString[InputForm[expr]]],
    Rule["recipients", df_] :> Rule["recipients", denormalizeDeliveryFunction[df]],
    Rule[lhs_, None] :> Rule[lhs, Null]
};


$docGenMetaToWLKeyMap = Association[
    "archiveId" -> "GeneratedDocumentHistory",
    "archiveLength" -> GeneratorHistoryLength,
    "archivePath" -> "GeneratedDocumentHistoryPath",
    "active" -> "Active",
    "creationDate" -> "CreationDate",
    "copyDriver" -> "CopyDriver",
    "copyEpilog" -> "CopyEpilog",
    "copyTemplate" -> "CopyTemplate",
    "currentId" -> "CurrentDocument",
    "currentPath" -> "CurrentDocumentPath",
    "driverId" -> "DriverSrcUUID", 
    "driverPath" -> "DriverPath",
    "epilogId" -> "EpilogSrcUUID", 
    "epilogPath" -> "EpilogPath", 
    "lastModificationDate" -> "LastModificationDate", 
    "lastRunDate" -> "LastRunDate",
    "logId" -> "Log",
    "logPath" -> "LogPath",
    "operationPath" -> "OperationPath",
    "outputFormat" -> GeneratorOutputType,
    "outputId" -> "CurrentOutput",
    "outputPath" -> "CurrentOutputPath",
    "outputPermissions" -> Permissions,
    "owner" -> "Owner",
    "recipients" -> DeliveryFunction,
    "reportDescription" -> GeneratorDescription,
    "reportHistory" -> "ReportHistory",
    "reportName" -> "Name", 
    "reportPath" -> "DirectoryPath",
    "schedule" -> "Task",
    "taskId" -> "TaskUUID",
    "templateId" -> "TemplateSrcUUID",
    "templatePath" -> "TemplatePath",
    "uuid" -> "UUID",
    "workingDriverId" -> "Driver",
    "workingEpilogId" -> EpilogFunction,
    "workingTemplateId" -> "Template"
];

$WLToDocGenMetaKeyMap = Association[Reverse /@ Normal[$docGenMetaToWLKeyMap]];

$presentableDocGenInfoKeys = Key /@ List[
    "CreationDate",
    "CurrentOutput",
    DeliveryFunction,
    "Directory",
    "Driver", 
    EpilogFunction,
    GeneratorDescription,
    "GeneratedDocumentHistory",
    GeneratorHistoryLength,
    GeneratorOutputType,
    "LastModificationDate", 
    (* "LastRunDate", *)
    "Log",
    "Name", 
    "Owner",
    Permissions,
    "Task",
    "Template", 
    "UUID"
];


$outgoingDocGenMetaKeys = Key /@ List[
    "archiveLength",
    "copyDriver",
    "copyEpilog",
    "copyTemplate",
    "deliveryFunction",
    "driverId", 
    "epilogId",
    "outputFormat",
    "outputPermissions",
    "recipients",
    "reportDescription",
    "reportName", 
    "templateId",
    "uuid"
];


docGenMetaToWL[raw_List] := Module[
    {med, well},
    (* Replace json keys with WL symbols/strings *)
    med = DeleteCases[
        Replace[raw, Rule[lhs_, rhs_] :> Rule[$docGenMetaToWLKeyMap[lhs], rhs], 1],
        Rule[Missing[__], _]
    ];
    
    well = Association @@ Flatten[Replace[med, $docGenNormalizationRules, 1], 1] 
]


WLToDocGenMeta[System`DocumentGeneratorInformationData[a_Association]] := WLToDocGenMeta[Normal[a]];
WLToDocGenMeta[a_Association] := WLToDocGenMeta[Normal[a]];
WLToDocGenMeta[raw:OptionsPattern[]] := Module[
    {med, well},

    (* Replace WL symbols and strings with json keys *)
    med = DeleteDuplicates[DeleteCases[
        Replace[raw, Rule[lhs_, rhs_] :> Rule[$WLToDocGenMetaKeyMap[lhs], rhs], 1],
        Rule[Missing[__], _]
    ], First[#1] === First[#2] &];
    
    well = Replace[med, $docGenDenormalizationRules, 1];
    well
]


unpresentifyDocGenMeta[raw_] := With[{med = WLToDocGenMeta[raw]},
    DeleteCases[
        Normal[Apply[Association, med][[$outgoingDocGenMetaKeys]]],
        Rule[_, Missing[__]]
    ]
];


presentifyDocGenMeta[med_Association] := Module[
    {well = med},
    If[KeyExistsQ[well, "Task"],
        well["Task"] = System`ScheduledTaskInformationData[presentifyTaskMeta[well["Task"]]];
    ];
    well[[$presentableDocGenInfoKeys]]
]


DocumentGeneratorInformation[obj_CloudObject] := Replace[
    Catch[iCloudDocumentGeneratorInformation[obj], $tag], {
    assoc_Association :> System`DocumentGeneratorInformationData[presentifyDocGenMeta @ assoc]
}]

DocumentGeneratorInformation[obj_CloudObject, property_] := Replace[
    Catch[iCloudDocumentGeneratorInformation[obj], $tag], {
    assoc_Association :> If[KeyExistsQ[assoc, property],
        presentifyDocGenMeta[assoc][property],
        (* else *)
        Message[DocumentGeneratorInformation::noprop, property];
        $Failed
    ]
}]

iCloudDocumentGeneratorInformation[obj_CloudObject, mh_:DocumentGeneratorInformation] := 
    iCloudDocumentGeneratorInformation[safeCloudAndUUIDFetch[obj, mh], mh]
iCloudDocumentGeneratorInformation[{cloud_String, uuid_String}, mh_:DocumentGeneratorInformation] := Module[
    {raw, json},
    With[{msghd = mh},

        json = Replace[execute[cloud, "GET", {"reports", uuid}], {
            HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
            , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , other_ :> (Message[msghd::srverr]; Throw[$Failed, $tag])
        }];

        Check[raw = Lookup[importFromJSON[json], "report"],
            Message[mh::srverr];
            Throw[$Failed, $tag]
        ]
    ];

    docGenMetaToWL[raw]
];


SetAttributes[DocumentGenerators, {ReadProtected}];
DocumentGenerators[o:OptionsPattern[]] := Module[
    {res = Catch[iCloudDocumentGenerators[o], $tag]},
    res
]

iCloudDocumentGenerators[o:OptionsPattern[]] := Module[
    {raw, med, json, msghd = DocumentGenerators, opts = Flatten[{o}]},

    With[{mh = msghd},
        json = Replace[execute[$CloudBase, "GET", {"reports"}], {
            {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , other_ :> (Message[mh::srverr]; Throw[$Failed, $tag])
        }];

        Check[raw = Lookup[importFromJSON[json], "reports", {Missing[]}],
            Message[mh::srverr];
            Throw[$Failed, $tag]
        ];
    ];
    
    If[Length[raw] == 0, Return[{}]];
    
    med[1] = Lookup[raw, {"uuid", "schedule", "visible"}, Missing[]];
    (* This sorts by next run date, soonest first. *)
    med[2] = Reverse @ Quiet @ SortBy[DeleteCases[med[1], {___, Missing[], ___}], Lookup[Last[#], "nextTimestamp"] &];
    med[3] = If[TrueQ[Lookup[opts, "IncludeInvisible", False]],
        med[2],
        Select[med[2], #[[3]] =!= False &]
    ];

    cloudObjectFromUUID /@ med[3][[All, 1]]
]


Protect[DocumentGenerator, DocumentGenerators, DocumentGeneratorInformation, CloudObject];

End[]

EndPackage[]
