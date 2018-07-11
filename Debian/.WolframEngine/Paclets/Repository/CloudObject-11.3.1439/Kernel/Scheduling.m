(* ::Package:: *)

(* Mathematica package *)
BeginPackage["CloudObject`"];

System`ScheduledTask;
System`EvaluateScheduledTask;
System`NextScheduledTaskTime;
System`ScheduledTaskActiveQ;
System`ScheduledTaskInformation::usage = "ScheduledTaskInformation[CloudObject] returns information about a task.
ScheduledTaskInformation[CloudObject, property] returns the value of the specified property.";
System`ScheduledTaskInformationData;
System`ContinuousTask;
System`AbortScheduledTask;
System`AutoRefreshed;
System`TaskAbort;
Tasks`TaskEvaluate;
System`TaskExecute;

(* Option symbols *)
System`NotificationFunction;
System`IncludeGeneratorTasks;
System`RestartInterval;

Begin["`Private`"];

Unprotect[ScheduledTask, ContinuousTask, CloudObject];
SetAttributes[ScheduledTask, {HoldAll, ReadProtected}];

(* MySQL-specific; must match CalendarUtilities *)
$neverRunDate = 253402300799000;
$taskMimeTypes = {"application/vnd.wolfram.expression.task"};

Options[ScheduledTask] = Options[ContinuousTask] = Sort@{
    NotificationFunction -> Automatic,
    TimeZone -> Automatic,
    AutoRemove -> False
};

ScheduledTask /: CloudDeploy[task_ScheduledTask, co_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudDeployScheduledTask[task, co], $tag]}, 
    res
]

SetAttributes[ContinuousTask, {HoldAll, ReadProtected}];
Options[ContinuousTask] = {
    NotificationFunction -> Automatic,
    RestartInterval -> Automatic,
    TimeZone -> Automatic
};

(* Continuous tasks must be pausable, so don't use None for timespec. *)
ContinuousTask /: CloudDeploy[ContinuousTask[expr_, o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := 
    continuousTaskStaging[ContinuousTask[expr, 365*24*3600, o], co, oCD]

ContinuousTask /: CloudDeploy[ct:ContinuousTask[expr_, end:Except[HoldPattern[_Quantity]], o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := 
    continuousTaskStaging[ContinuousTask[expr, {Now, 365*24*3600, DateObject[end]}, o], co, oCD]

ContinuousTask /: CloudDeploy[ct:ContinuousTask[expr_, tspan:HoldPattern[_Quantity], o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := 
    continuousTaskStaging[ContinuousTask[expr, {Now, 365*24*3600, Now + tspan}, o], co, oCD]

continuousTaskStaging[ContinuousTask[expr_, tspec_, o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := With[
    {options = Join[Flatten[{o}], {"Continuous" -> True}, Options[ContinuousTask]]}, 
    Catch[iCloudDeployScheduledTask[ScheduledTask[expr, tspec, options], co, oCD], $tag]
];


iCloudDeployScheduledTask[st:ScheduledTask[expr_, sched_, o:OptionsPattern[]], obj:CloudObject[uri_String, ___], iO:OptionsPattern[]] := Module[
    {
        cloud, uuid, name,
        continuous = TrueQ[Lookup[Flatten[{o}], "Continuous", False]],
        runImmediately = False,
        cronned,
        params, taskJson, rJson,
        updating = False, existingOpts = {}, endpoint
    },
    {cloud, uuid, name} = Replace[getCloudAndUUIDOrPath[obj], None -> Null, {1}];
    name = Replace[name, {
        n : {"user-" ~~ $CloudUserUUID, __} :> FileNameJoin[Rest[n], OperatingSystem -> "Unix"],
        n : {$UserURLBase, __} :> FileNameJoin[Rest[n], OperatingSystem -> "Unix"],
        n_List :> FileNameJoin[n, OperatingSystem -> "Unix"]
    }];

    If[scheduledTaskQ[obj],
        updating = True;
        (* The api expects a relative path for renaming, so updating with the same name
         * moves the task to a new subdirectory. There is no way to change the name from WL. *)
        name = Replace[name, n_String :> FileNameTake[n]]; 
        Check[With[{taskInfo = CloudObject`Private`iCloudScheduledTaskInformation[obj]}, 
                existingOpts = FilterRules[Normal[taskInfo], Options[ScheduledTask]];
            ],
            Throw[$Failed, $tag]
        ]
    ];

    {runImmediately, cronned} = Which[
        continuous,
        {True, resolveTimespec[ReleaseHold[sched]]},
        
        nowQ[sched],
        {True, resolveTimespec[None]},
        
        True,
        {False, resolveTimespec[ReleaseHold[sched]]}
    ];
    
    If[MatchQ[cronned, $Failed],
        Message[ScheduledTask::sched, sched];
        Throw[$Failed, $tag]
    ];

	taskJson = generateTaskJson[st, {name, uuid}, cronned, Flatten[{o}], existingOpts];
	If[taskJson === $Failed,
			Throw[$Failed, $tag]
	];
    params = {"task" -> taskJson};
    (* Print@params; *)

    endpoint = If[TrueQ[updating], {"tasks", Last@safeCloudAndUUIDFetch[obj, ScheduledTask]}, {"tasks"}];
    With[{mh = ScheduledTask},
        rJson = Replace[
        	execute[cloud, "POST", endpoint, 
    	        Body -> ToCharacterCode[exportToJSON[params], "UTF-8"]
        	], {
            {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , HTTPError[400, ___] :> (Message[mh::argu]; Throw[$Failed, $tag])
            , HTTPError[403, content_, ___] :> (
                    ToExpression[Lookup[importFromJSON[content], "error", "ScheduledTask::restr"], InputForm, Message];
                    Throw[$Failed, $tag]
                )
            , other_ :> (Message[mh::srverr]; Message[mh::crea]; Throw[$Failed, $tag])
        }];
    ];
    (* Print[rJson]; *)
    
    (* Continuous tasks track their end dates in MetaInformation, to bypass Quartz modification of the field. *)
    If[continuous, SetOptions[obj, MetaInformation -> "__ContinuousEndDate" -> Last[cronned]]];

    If[TrueQ[runImmediately], RunScheduledTask @@ {obj}];
    obj
]


resolveTaskExpr[st:ScheduledTask[expr_, __]] := Replace[Unevaluated[expr], {
	(* Local file, cloud; client supplies path, validated on server *)
    f_File :> {"file", AbsoluteFileName[First[f]]} /; TrueQ[$CloudEvaluation]
    (* Local file, desktop; client supplies uuid *)
    , f_File :> With[{o = Check[CopyFile[f, CloudObject[], "MIMEType" -> "application/vnd.wolfram.wl"], Throw[$Failed, $tag]]},
    	Message[ScheduledTask::copied, First[f], o];
    	{"file", Last@safeCloudAndUUIDFetch[o]}
    ] /; Not[TrueQ[$CloudEvaluation]]
    (* Existing cloud object; client supplies uuid *)
    , o_CloudObject :> {"file", Last@safeCloudAndUUIDFetch[o]}
    (* Arbitrary expression *) 
	, e_ :> {"script", exprToStringIncludingDefinitions[Unevaluated[expr]]}
}];


generateTaskJson[st:ScheduledTask[expr_, __], {name:(_String|Null), uuid:(_String|Null)}, {start_, stdSched_, end_}, 
	o:{___?OptionQ}, existingOpts:{___?OptionQ}] := Module[
	{opts, taskJson, cont, type, strexpr},
	
	cont = Lookup[o, "Continuous", False];
	
	{type, strexpr} = resolveTaskExpr[st];
	
    opts = Flatten[
    	Replace[Join[
	        {
	            "Continuous" -> cont
	            , "StartDate" -> start
	            (* If a continuous task has an end date, the first run will set the task to Completed and the end date will revert to None.
	             * Continuous tasks track their end dates in MetaInformation. *)
	            , "EndDate" -> If[cont, None, end]
	            , "CronSchedule" -> With[{s = First[stdSched]}, If[StringQ[s], s, Null]]
	            , "RepeatCount" -> Replace[Last[stdSched], r_Integer :> r - 1]
	            , "RepeatInterval" -> With[{s = First[stdSched]}, If[IntegerQ[s], s, Null]]
	            , "Name" -> name
	            , "UUID" -> uuid
	        },
	        o,
	        existingOpts,
	        {
	            "TaskType" -> type
	            , RestartInterval -> Null
	            , "Expression" -> strexpr
	        },
	        Options[ScheduledTask]
	    ], {
	        Rule[TimeZone, Automatic] :> Rule[TimeZone, $TimeZone],
            Rule[TimeZone, id_String] :> With[{tz = Entity["TimeZone", id]},
            	{Rule[TimeZone, QuantityMagnitude[tz["OffsetFromUTC"], "Hours"]], Rule["TimeZoneFullName", tz["Name"]]}
            ],
            Rule[TimeZone, tz:Entity["TimeZone", _String]] :> 
            	{Rule[TimeZone, QuantityMagnitude[tz["OffsetFromUTC"], "Hours"]], Rule["TimeZoneFullName", tz["Name"]]},
            Rule[TimeZone, tz_?NumberQ] :> 
                {Rule[TimeZone, tz], Rule["TimeZoneFullName", Null]}
	    }, 1],
	    1
	];
    (* Print[opts]; *)

    validateTaskArg["options", {}] := True;
    validateTaskArg["options", unknown_List] := (Message[ScheduledTask::optx, First[unknown], st]; False);
    validateTaskArg["notification", Automatic|All|None] := True;
    validateTaskArg["notification", _String -> Automatic|All] := True;
    validateTaskArg["notification", {___String}] := True;
    validateTaskArg["notification", {({__String} -> (Automatic|All|None|_Function|_List))..}] := True;
    validateTaskArg["notification", _Function] := True;
    validateTaskArg["notification", a__] := (Message[ScheduledTask::badarg, a, NotificationFunction]; False);
    validateTaskArg["restart", Automatic] := True;
    validateTaskArg["restart", Null|None] := True;
    validateTaskArg["restart", _Function] := True;
    validateTaskArg["restart", HoldPattern[_Quantity]] := True;
    validateTaskArg["restart", r_?NumericQ /; r >= 0] := True;
    validateTaskArg["restart", r__] := (Message[ScheduledTask::badarg, r, RestartInterval]; False);
    validateTaskArg["timezone", tz_?NumberQ] := True;
    validateTaskArg["timezone", tz__] := (Message[ScheduledTask::badarg, First@Cases[tz, {"TimeZone", s_String} :> s, Infinity], TimeZone]; False);

    validateTaskArg[__] := False;

    (* Client-side validation *)
    If[And @@ # === False, Throw[$Failed, $tag]] & @ MapThread[
        validateTaskArg[#1, #2] &,
        {
            {"options", "notification", "restart", "timezone"},
            {
                FilterRules[o, Except[Join[Options[ScheduledTask], Options[ContinuousTask],
                    {"Visible" -> True, "Continuous" -> False}]]],
                Lookup[opts, NotificationFunction],
                Lookup[opts, RestartInterval, Null],
                Lookup[opts, TimeZone]
            }
        }
    ];

    taskJson = exportToJSON[unpresentifyTaskMeta[opts], "Compact" -> True];
    taskJson
]


iCloudDeployScheduledTask[ScheduledTask[args___], ___] := 
    (ArgumentCountQ[ScheduledTask, Length[DeleteCases[{args}, _Rule|_RuleDelayed, Infinity]], 2, 2]; $Failed)
iCloudDeployScheduledTask[___] := $Failed


toSchedule[n_String] := Module[{cron},
    cron = StringSplit[n];
    If[Length@cron < 3 || Length@cron > 7, Return[$Failed]];
    cron = Replace[
        cron,
        {
            (*{s_, m_, h_, dom_, m_, dow_, y_}:>{s, m, h, dom, m, dow, y},
            {s_, m_, h_, dom_, m_, dow_}:>{s, m, h, dom, m, dow, "*"},
            {h_, dom_, m_, dow_, y_}:>{"*", "*", h, dom, m, dow, y},
            {h_, dom_, m_, dow_}:>{"*", "*", h, dom, m, dow, "*"}*)
            
            
            {s_, m_, h_, dom_, mo_, dow_, y_}:>{s, m, h, dom, mo, dow, y}, (* quartz expression *)
            {m_, h_, dom_, mo_, dow_, y_}:>{"*", m, h, dom, mo, dow, y}, (* classic cron with optional year *)
            {m_, h_, dom_, mo_, dow_}:>{"*", m, h, dom, mo, dow, "*"}, (* classic cron *)
            {h_, dom_, mo_, dow_}:>{"*", "*", h, dom, mo, dow, "*"}
        
        
        }
    ];
    StringJoin[Riffle[ToUpperCase@cron, " "]]
]

toSchedule[___] := $Failed

current[spec_] := DateString[DateList[], spec]
(* We can remove this and instead use dowToCron *)
currentDOW[] := With[{date = DateList[]}, Which[
    DayMatchQ[date, Sunday], "1",
    DayMatchQ[date, Monday], "2",
    DayMatchQ[date, Tuesday], "3",
    DayMatchQ[date, Wednesday], "4",
    DayMatchQ[date, Thursday], "5",
    DayMatchQ[date, Friday], "6",
    DayMatchQ[date, Saturday], "7",
    True, "*"
]]

(* this really needs to get fixed... the first section of each cron expression*)
$TimeSpecs = {
   "Hourly" :> StringJoin[current[{"MinuteShort"}]," * * * ? *"],
   "Daily" :> StringJoin[current[{"MinuteShort", " ", "HourShort"}], " * * ? *"],
   "Weekly" :> StringJoin[current[{"MinuteShort", " ", "HourShort"}], " ? * ", currentDOW[], " *"],
   "Monthly" :> StringJoin[current[{"MinuteShort"," ", "HourShort", " ", "DayShort"}], " * ? *"],
   "Yearly" :> StringJoin[current[{"MinuteShort", " ", "HourShort", " ", "DayShort", " ", "MonthShort"}], " ? *"]
};

(*validateTimeSpec[{n_Integer?Positive, di:(_Integer|_DirectedInfinity)}] := {n, di}
validateTimeSpec[__] := $Failed
validateCronSpec[{cron_, di_DirectedInfinity}] := toSchedule[cron]*)

$AvailableTimeSpecs = First /@ $TimeSpecs;

resolveTimespec[expr_] := With[{cronned = timespec2Cron[expr]},
	If[MemberQ[cronned, $Failed, Infinity], $Failed, cronned]
]
(* Dispatch *)
timespec2Cron[ts:{_DateObject, _DateObject|_Integer}] := (Message[ScheduledTask::ambig, ts]; $Failed)

timespec2Cron[spec_] := timespec2Cron[{Null, spec, Null}]
timespec2Cron[{start:_DateObject|None|Null, spec_}] := timespec2Cron[{start, spec, Null}]
timespec2Cron[{spec_, end:_DateObject|None|Null}] := timespec2Cron[{Null, spec, end}]
timespec2Cron[{start:_DateObject|None|Null, spec:Except[_List], end:_DateObject|None|Null}] := {start, {handleSpec[spec], Infinity}, end}
timespec2Cron[{start:_DateObject|None|Null, {spec_}, end:_DateObject|None|Null}] := {start, {handleSpec[spec], 1}, end}
timespec2Cron[{start:_DateObject|None|Null, {spec_, rc:_Integer?Positive|_DirectedInfinity}, end:_DateObject|None|Null}] := {start, {handleSpec[spec], rc}, end}
timespec2Cron[__] := $Failed;

(* CRON Output *)
handleSpec[spec_String] /; MemberQ[$AvailableTimeSpecs, spec] := spec /. $TimeSpecs
handleSpec[spec_DateObject] := With[{cronned = DateObjectToCronSpecification[spec]}, 
    If[StringQ[cronned], cronned, $Failed]
]
handleSpec[spec_String] := With[{cronned = toSchedule[spec]}, 
    If[StringQ[cronned], cronned, $Failed]
]

(* Interval Output *)
handleSpec[HoldPattern[q_Quantity]] := If[CompatibleUnitQ[q, "Seconds"], 
    QuantityMagnitude[UnitConvert[q, "Seconds"]],
    $Failed
]
handleSpec[n_Integer?Positive] := n

(* None/dummy *)
handleSpec[None|Null] := Null;

(* fallthrough *)
handleSpec[__] := $Failed;


(* Now? *)
SetAttributes[nowQ, HoldAll];
nowQ[Now] := True;
nowQ[{Now}] := True;
nowQ[{Now, count_}] := MatchQ[ReleaseHold[count], _Integer|Infinity|DirectedInfinity];
nowQ[{start_, Now, end_}] := MatchQ[ReleaseHold[start], _DateObject] && MatchQ[ReleaseHold[end], _DateObject];
nowQ[{start_DateObject, Now}] := MatchQ[ReleaseHold[start], _DateObject];
nowQ[{Now, end_DateObject}] := MatchQ[ReleaseHold[end], _DateObject];
nowQ[___] := False;

(*
    StopScheduledTask (pre-11.2)
*)

CloudObject /: StopScheduledTask[co_CloudObject, OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudStopScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        Message[DocumentGenerator::notask, co];
        i
    ] 
]

CloudObject /: StopScheduledTask[task_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudStopScheduledTask[task], $tag]},
    res
]

iCloudStopScheduledTask[obj_CloudObject, mh_:StopScheduledTask] := (iCloudStopScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudStopScheduledTask[{cloud_String, uuid_String}, mh_:StopScheduledTask] := Module[
    {json},
    json = Replace[execute[cloud, "POST", {"tasks", uuid, "pause"}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, obj]; Message[ScheduledTask::nostop, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];
    uuid
];

(*iCloudStopScheduledTask[st_,OptionsPattern[]] := (Message[ScheduledTask::nostop,st];$Failed)*)

(*
    11.2+. The same code with replaced StopScheduledTask --> TaskSuspend
*)

CloudObject /: TaskSuspend[co_CloudObject, OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudStopScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        Message[DocumentGenerator::notask, co];
        i
    ] 
]

CloudObject /: TaskSuspend[task_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudStopScheduledTask[task, TaskSuspend], $tag]},
    res
]


(*
    StartScheduledTask (pre-11.2)
*)

CloudObject /: StartScheduledTask[co_CloudObject, o:OptionsPattern[]]  /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudStartScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        Message[DocumentGenerator::notask, co];
        i
    ]
]

CloudObject /: StartScheduledTask[task_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudStartScheduledTask[task], $tag]}, res
]

iCloudStartScheduledTask[obj_CloudObject, mh_:StartScheduledTask] := (iCloudStartScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudStartScheduledTask[{cloud_String, uuid_String}, mh_:StartScheduledTask] := Module[
    {json},
    json = Replace[execute[cloud, "POST", {"tasks", uuid, "resume"}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, cloudObjectFromUUID[uuid]]; Message[ScheduledTask::nostart]; Throw[$Failed, $tag])
    }];
    uuid
];

(*iCloudResumeScheduledTask[st_, OptionsPattern[]] := handleSchedulingResponse[$Failed]*)

(*
    11.2+. The same code with replaced StartScheduledTask --> TaskResume
*)

CloudObject /: TaskResume[co_CloudObject, o:OptionsPattern[]]  /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudStartScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        Message[DocumentGenerator::notask, co];
        i
    ]
]

CloudObject /: TaskResume[task_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudStartScheduledTask[task, TaskResume], $tag]}, res
]


(*
 * Equivalent to "Run now" in web interface.
 *)
CloudObject /: RunScheduledTask[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudRunDocumentGenerator[co, RunScheduledTask, o], $tag]},
    res
]

CloudObject /: RunScheduledTask[co_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudRunScheduledTask[co], $tag]},
    res
]

iCloudRunScheduledTask[obj_CloudObject, mh_:RunScheduledTask] := (iCloudRunScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudRunScheduledTask[{cloud_String, uuid_String}, mh_:RunScheduledTask] := Module[
    {json, mess},
    json = Replace[execute[cloud, "POST", {"tasks", uuid, "execute"}], {
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

(*iCloudRunScheduledTask[st_,OptionsPattern[]] := handleSchedulingResponse[$Failed]*)


(*
    11.2+. The same code with replaced RunScheduledTask --> TaskExecute
*)

CloudObject /: TaskExecute[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudRunDocumentGenerator[co, RunScheduledTask, o], $tag]},
    res
]

CloudObject /: TaskExecute[co_CloudObject, OptionsPattern[]] := With[
    {res = Catch[iCloudRunScheduledTask[co, TaskExecute], $tag]},
    res
]


(*
    RemoveScheduledTask
*)

CloudObject /: RemoveScheduledTask[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudRemoveDocumentGenerator[co, RemoveScheduledTask, o], $tag]},
    res
]

$autoRefreshedMimeTypes = {"application/vnd.wolfram.bundle.autorefreshed"};
(* Slow! *)
autoRefreshedQ[co_CloudObject] := 
    With[{mime = Quiet[Check[CloudObjectInformation[co, "MIMEType"], $Failed]]},
    	MemberQ[$autoRefreshedMimeTypes, mime]
    ];
autoRefreshedQ[_] := False;

CloudObject /: RemoveScheduledTask[ar_CloudObject, o:OptionsPattern[]] /; Quiet[autoRefreshedQ[ar]] := With[
    {res = Catch[iCloudRemoveAutoRefreshed[ar, RemoveScheduledTask, o], $tag]},
    res
]

iCloudRemoveAutoRefreshed[obj_CloudObject, mh_:RemoveScheduledTask, o:OptionsPattern[]] := (iCloudRemoveAutoRefreshed[safeCloudAndUUIDFetch[obj, mh], mh, o]; obj)
iCloudRemoveAutoRefreshed[{cloud_String, uuid_String}, mh_:RemoveScheduledTask, o:OptionsPattern[]] := Module[
    {json},
    json = Replace[execute[cloud, "DELETE", {"tasks", uuid, "autorefreshed"}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr]; Message[ScheduledTask::norm, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];
    uuid
];

scheduledTaskQ[co_CloudObject] := 
    With[{mime = Quiet[Check[CloudObjectInformation[co, "MIMEType"], $Failed]]},
    	MemberQ[$taskMimeTypes, mime]
    ];
scheduledTaskQ[_] := False;

CloudObject /: RemoveScheduledTask[task_CloudObject, o:OptionsPattern[]] := With[
    {res = Catch[iCloudRemoveScheduledTask[task, RemoveScheduledTask, o], $tag]},
    res
]

iCloudRemoveScheduledTask[obj_CloudObject, mh_:RemoveScheduledTask, o:OptionsPattern[]] := (iCloudRemoveScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh, o]; obj)
iCloudRemoveScheduledTask[{cloud_String, uuid_String}, mh_:RemoveScheduledTask, o:OptionsPattern[]] := Module[
    {json},
    json = Replace[execute[cloud, "DELETE", {"tasks", uuid}], {
        HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr]; Message[ScheduledTask::norm, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];
    uuid
];

iCloudRemoveScheduledTask[st_, OptionsPattern[]] := (Message[ScheduledTask::norm, st]; $Failed)


(*
    11.2+. The same code with replaced RemoveScheduledTask --> TaskRemove
*)


CloudObject /: TaskRemove[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudRemoveDocumentGenerator[co, TaskRemove, o], $tag]},
    res
]


CloudObject /: TaskRemove[ar_CloudObject, o:OptionsPattern[]] /; Quiet[autoRefreshedQ[ar]] := With[
    {res = Catch[iCloudRemoveAutoRefreshed[ar, TaskRemove, o], $tag]},
    res
]


CloudObject /: TaskRemove[task_CloudObject, o:OptionsPattern[]] := With[
    {res = Catch[iCloudRemoveScheduledTask[task, TaskRemove, o], $tag]},
    res
]


(*
    EvaluateScheduledTask (pre-11.2)
*)
Unprotect[EvaluateScheduledTask];

CloudObject /: EvaluateScheduledTask[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudEvaluateDocumentGeneratorCE[co, CloudObject[], o], $tag]},
    res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject, uri_String, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudEvaluateDocumentGeneratorCE[co, CloudObject[uri], o], $tag]},
    res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject, dest_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudEvaluateDocumentGeneratorCE[co, dest, o], $tag]},
    res
]

CloudObject /: EvaluateScheduledTask[co_CloudObject] := Module[
    {expr, i},
    
    Check[
        i = ScheduledTaskInformation[co],
        Return[$Failed]
    ];
    i = First[i];
    
    Switch[i["TaskType"],
    	"file", Replace[i["Expression"], {s_String :> Get[cloudObjectFromUUID[s]]}],
    	_, 
		(* Pre-1.22 tasks store their code in the cloud object and have execution expressions like
		 *      "EvaluateScheduledTask[CloudObject[\"http://www.wolframcloud.\com/objects/user-74e17eb9-8669-4795-b270-032b6ad916af/task\\"]]"
		 * In 1.22+ this results in recursion if evaluated naively, so check for expressions of this form. 
		 *)
        expr = Replace[i["Expression"], x_String :> ToExpression[x, InputForm, Hold]];
	    Last @ List @ ReleaseHold @ Replace[expr, {
	    	Hold[EvaluateScheduledTask[co2_CloudObject]] :> Replace[
	    		CloudGet[co2],
	    		ScheduledTask[code_, ___] :> (code)
	    	] /; CloudObjectInformation[co, "UUID"] === CloudObjectInformation[co2, "UUID"]
	    }]
    ]
]

(*
   11.2+ Repeat the code for EvaluateScheduledTask replacing it with TaskEvaluate
*)

Unprotect[TaskEvaluate];

CloudObject /: TaskEvaluate[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudEvaluateDocumentGeneratorCE[co, CloudObject[], o], $tag]},
    res
]

CloudObject /: TaskEvaluate[co_CloudObject, uri_String, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudEvaluateDocumentGeneratorCE[co, CloudObject[uri], o], $tag]},
    res
]

CloudObject /: TaskEvaluate[co_CloudObject, dest_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := Module[
    {res = Catch[iCloudEvaluateDocumentGeneratorCE[co, dest, o], $tag]},
    res
]

CloudObject /: TaskEvaluate[co_CloudObject] := Module[
    {expr, i},
    
    Check[
        i = ScheduledTaskInformation[co],
        Return[$Failed]
    ];
    i = First[i];
    
    Switch[i["TaskType"],
    	"file", Replace[i["Expression"], {s_String :> Get[cloudObjectFromUUID[s]]}],
    	_, 
		(* Pre-1.22 tasks store their code in the cloud object and have execution expressions like
		 *      "TaskEvaluate[CloudObject[\"http://www.wolframcloud.\com/objects/user-74e17eb9-8669-4795-b270-032b6ad916af/task\\"]]"
		 * In 1.22+ this results in recursion if evaluated naively, so check for expressions of this form. 
		 *)
        expr = Replace[i["Expression"], x_String :> ToExpression[x, InputForm, Hold]];
	    Last @ List @ ReleaseHold @ Replace[expr, {
	    	Hold[TaskEvaluate[co2_CloudObject]] :> Replace[
	    		CloudGet[co2],
	    		ScheduledTask[code_, ___] :> (code)
	    	] /; CloudObjectInformation[co, "UUID"] === CloudObjectInformation[co2, "UUID"]
	    }]
    ]
]



(*
 * Hybrid task listing
 *)
Unprotect[ScheduledTasks];
SetAttributes[ScheduledTasks, {ReadProtected}];
Options[ScheduledTasks] = Options[iCloudScheduledTasks] = {
    IncludeGeneratorTasks -> False
};
$cloudScheduledTasksFlag = True;
ScheduledTasks[o:OptionsPattern[]] /; TrueQ[And[$CloudConnected, $cloudScheduledTasksFlag]] := Block[
    {$cloudScheduledTasksFlag = False},
    Join[ScheduledTasks[], Catch[iCloudScheduledTasks[o], $tag]]
]

iCloudScheduledTasks[o:OptionsPattern[]] := Module[
    {raw, med, json, msghd = ScheduledTasks, opts = Flatten[{o}]},

    With[{mh = msghd},
        json = Replace[execute[$CloudBase, "GET", {"tasks"}], {
            {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , other_ :> (Message[mh::srverr]; Throw[$Failed, $tag])
        }];

        Check[raw = Lookup[importFromJSON[json], "tasks", {Missing[]}],
            Message[mh::srverr];
            Throw[$Failed, $tag]
        ];
    ];
    
    If[Length[raw] == 0, Return[{}]];
    
    med[1] = Lookup[raw, {"nextTimestamp", "taskType", "uuid", "visible"}, Missing[]];

    (* This sorts by next run date, soonest first. *)
    med[2] = Reverse @ SortBy[DeleteCases[med[1], {___, Missing[], ___}], First];
    med[3] = If[TrueQ[Lookup[opts, "IncludeInvisible", False]],
    	med[2],
    	Select[med[2], #[[4]] =!= False &]
    ];
    med[4] = If[TrueQ[Lookup[opts, IncludeGeneratorTasks, False]],
    	med[3],
    	Select[med[3], #[[2]] =!= "document-generator" &]
    ];
    cloudObjectFromUUID /@ med[4][[All, 3]]
]


(* ScheduledTaskInformation *)
Unprotect[ScheduledTaskInformation, ScheduledTaskInformationData];
SetAttributes[ScheduledTaskInformation, {ReadProtected}];

ScheduledTaskInformation::noprop = "`1` is not a property returned by ScheduledTaskInformation.";


gatherNotificationFunction[n:{{__}...}] := SortBy[
    Sort[Lookup[#, "email"]] -> ToExpression[Lookup[#[[1, -1]], "condition"]] & /@ GatherBy[n, Lookup["condition"]],
    Last
];


(* Output is a flat list digestible by the web ui. *)
denormalizeNotificationFunction[notifRaw_] := Module[
    {notif = Replace[notifRaw, {
        Null | None -> {},
        All -> {{$CloudUserID} -> All},
        Automatic -> {{$CloudUserID} -> Automatic},
        f_Function :> {{$CloudUserID} -> f},
        {u__String} :> {{u} -> Automatic},
        (* non-mail channels *)
        Rule[s_String, cond : Automatic | All] :> {Rule[{$CloudUserID}, {s, cond}]}
    }]},
    
    notif = Replace[
        notif,
        (addr_ -> cond_) :> Flatten[{addr}] -> Replace[cond, {None -> Null, ns:Except[_String] :> ToString[InputForm[ns]]}],
        {1}
    ];
    
    With[{pairs = DeleteDuplicates @ Flatten[(Thread[List @@ #1, List, 1] &) /@ notif, 1]},
        {"email" -> First@#, "condition" -> Last@#} & /@ pairs
    ]
]


$taskInfoNormalizationRules = {
    Rule[tag:"CreationDate"|"StartDate"|"EndDate"|"LastRunDate"|"NextRunDate", t_] /; t >= $neverRunDate :> 
        Rule[tag, None],
    Rule[tag:"CreationDate"|"StartDate"|"EndDate"|"LastRunDate"|"NextRunDate", Null|0] :> 
        Rule[tag, None],
    Rule[tag:"CreationDate"|"StartDate"|"EndDate"|"LastRunDate"|"NextRunDate", t_?NumericQ] :> 
        Rule[tag, FromUnixTime[Round[t/1000]]],
    Rule["Log", uuid_String] :> {Rule["Log", cloudObjectFromUUID[uuid]], Rule["LogUUID", uuid]},
    Rule[NotificationFunction, notif_] :> Rule[NotificationFunction, gatherNotificationFunction[notif]],
    Rule["Name", Null] -> Rule["Name", None],
    Rule[RestartInterval, r_] :> Rule[RestartInterval, ToExpression[r]],
    Rule["RepeatInterval", t_?NumericQ] :> Rule["RepeatInterval", Round[t/1000]],
    Rule["RepeatCount", -1|Null] :> Rule["RepeatCount", Infinity]
};


$taskInfoDenormalizationRules = {
	(* Make sure the time is not an integer here, to work around bug 289879. *)
    Rule[tag:"creationTimestamp"|"startTimestamp"|"endTimestamp"|"lastTimestamp"|"nextTimestamp", t:(_DateObject|_?NumericQ)] :> 
        Rule[tag, ToString@Round[1000*UnixTime[t]]],
    Rule[tag:"creationTimestamp"|"startTimestamp"|"endTimestamp"|"lastTimestamp"|"nextTimestamp", t:None] :> 
        Rule[tag, Null],
    Rule["name", None] -> Rule["name", Null],
    Rule["notificatees", notif_] :> Rule["notificatees", denormalizeNotificationFunction[notif]],
    Rule["restartInterval", r_] :> Rule["restartInterval", ToString[InputForm[r]]],
    Rule["repeatInterval", t_?NumericQ] :> Rule["repeatInterval", Round[1000*t]],
    Rule["repeatCount", Infinity|DirectedInfinity] -> Rule["repeatCount", -1]
};


$taskMetaToWLKeyMap = Association[
    "active" -> "Active", 
    "autoRemove" -> AutoRemove, 
    "completed" -> "Completed", 
    "continuous" -> "Continuous", 
    "cronSchedule" -> "CronSchedule", 
    "endTimestamp" -> "EndDate",
    "executions" -> "Executions",
    "failures" -> "Failures", 
    "lastTimestamp" -> "LastRunDate", 
    "log" -> "Log", 
    "name" -> "Name", 
    "nextTimestamp" -> "NextRunDate", 
    "notificatees" -> NotificationFunction,
    "owner" -> "Owner", 
    "paused" -> "Paused",
    "quartzTablePrefix" -> "QuartzTablePrefix", 
    "remainingTriggers" -> "RemainingTriggers",
    "repeatCount" -> "RepeatCount", 
    "repeatInterval" -> "RepeatInterval", 
    "restartInterval" -> RestartInterval, 
    "startTimestamp" -> "StartDate", 
    "creationTimestamp" -> "CreationDate", 
    "status" -> "Status", 
    "taskData" -> "Expression", 
    "taskType" -> "TaskType", 
    "timeZoneOffsetHrs" -> TimeZone, 
    "timeZone" -> "TimeZoneFullName", 
    "timeZoneAbbr" -> "TimeZoneAbbreviation",
    "uuid" -> "UUID",
    "visible" -> "Visible"
];

$WLToTaskMetaKeyMap = Association[Reverse /@ Normal[$taskMetaToWLKeyMap]];

$presentableTaskInfoKeys = Key /@ List[
    AutoRemove, 
    "Completed",
    "Continuous",
    "CreationDate", 
    "CronSchedule", 
    "EndDate", 
    "Executions",
    "Expression", 
    "Failures",
    "LastRunDate", 
    "Log", 
    "Name", 
    "NextRunDate", 
    NotificationFunction,
    "Owner",
    "Paused",
    "RepeatCount", 
    "RepeatInterval", 
    RestartInterval, 
    "StartDate", 
    "Status", 
    "TaskType",
    TimeZone, 
    "UUID",
    "Visible"
];


$outgoingTaskMetaKeys = Key /@ List[
    (* "active", *) 
    "autoRemove", 
    "completed", 
    "continuous", 
    "cronSchedule", 
    "endTimestamp", 
    "lastTimestamp", 
    (* "log", *) 
    "name", 
    "nextTimestamp", 
    "notificatees", 
    "paused", 
    "repeatCount", 
    "repeatInterval", 
    "restartInterval", 
    "startTimestamp", 
    "status", 
    "taskData", 
    "taskType", 
    "timeZone", 
    "timeZoneOffsetHrs",
    (* "timeZoneAbbr", *)
    "uuid"
];


taskMetaToWL[raw_List] := Module[
    {med, well},
    If[MatchQ[Lookup[raw, "visible"], _],
        (* Replace json keys with WL symbols/strings *)
        med = DeleteCases[
            Replace[raw, Rule[lhs_, rhs_] :> Rule[$taskMetaToWLKeyMap[lhs], rhs], 1],
            Rule[Missing[__], _]
        ];
        well = Association @@ Flatten[Replace[med, $taskInfoNormalizationRules, 1], 1];
        well[TimeZone] = Quiet@Replace[well["TimeZoneFullName"], {
        	name_String :> Entity["TimeZone", name] /; MemberQ[EntityList["TimeZone"], Entity["TimeZone", name]],
        	_ -> well[TimeZone]
        }];
        If[well["Continuous"],
        	well["EndDate"] = Lookup[
                Lookup[Options[cloudObjectFromUUID[well["UUID"]], MetaInformation -> "__ContinuousEndDate"], MetaInformation],
                "__ContinuousEndDate"
	        ];
        	well["Completed"] = TrueQ[Now > well["EndDate"]];
        	well["RepeatInterval"] = Null;
        ];
        KeySort @ well,
        (* else *)
        Null
    ]
]


WLToTaskMeta[System`ScheduledTaskInformationData[a_Association]] := WLToTaskMeta[Normal[a]];
WLToTaskMeta[a_Association] := WLToTaskMeta[Normal[a]];
WLToTaskMeta[raw:OptionsPattern[]] := Module[
    {med, well},

    (* Replace WL symbols and strings with json keys *)
    med = DeleteDuplicates[DeleteCases[
        Replace[raw, Rule[lhs_, rhs_] :> Rule[$WLToTaskMetaKeyMap[lhs], rhs], 1],
        Rule[Missing[__], _]
    ], First[#1] === First[#2] &];
    
    well = Replace[med, $taskInfoDenormalizationRules, 1];
    well
]


unpresentifyTaskMeta[raw_] := With[{med = WLToTaskMeta[raw]},
    DeleteCases[
        Normal[Apply[Association, med][[$outgoingTaskMetaKeys]]],
        Rule[_, Missing[__]]
    ]
];


presentifyTaskMeta[med_Association] := med[[$presentableTaskInfoKeys]];


ScheduledTaskInformation[obj_CloudObject] := Replace[
    Catch[iCloudScheduledTaskInformation[obj], $tag], {
    assoc_Association :> System`ScheduledTaskInformationData[presentifyTaskMeta @ assoc]
}]

ScheduledTaskInformation[obj_CloudObject, property_] := Replace[
    Catch[iCloudScheduledTaskInformation[obj], $tag], {
    assoc_Association :> If[KeyExistsQ[assoc, property],
        presentifyTaskMeta[assoc][property],
        (* else *)
        Message[ScheduledTaskInformation::noprop, property];
        $Failed
    ]
}]

iCloudScheduledTaskInformation[obj_CloudObject] := Module[
    {raw, cloud, uuid, json, msghd = ScheduledTaskInformation, mess},
    With[{mh = msghd},
        {cloud, uuid} = safeCloudAndUUIDFetch[obj, mh];

        json = Replace[execute[cloud, "GET", {"tasks", uuid}], {
            HTTPError[404, ___] :> (Message[ScheduledTask::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
            , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
            , HTTPError[410, ___] :> (mess = ToExpression[Lookup[importFromJSON[content], "error"], InputForm, Hold];
                ReleaseHold[Replace[mess, slug_ :> Message[slug, cloudObjectFromUUID[uuid]], 1]];
            )
            , other_ :> (Message[mh::srverr]; Throw[$Failed, $tag])
        }];

        Check[raw = Lookup[importFromJSON[json], "task"],
            Message[mh::srverr];
            Throw[$Failed, $tag]
        ]
    ];

    taskMetaToWL[raw]
];

CloudObject /: ScheduledTaskActiveQ[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := With[
    {i = Catch[iCloudDocumentGeneratorInformation[co], $tag]},
    If[MatchQ[i, _Association],
        i["Active"] && Not[i["Task"]["Paused"]] && Not[i["Task"]["Completed"]],
        (* else *)
        i
    ] 
]

CloudObject /: ScheduledTaskActiveQ[co_CloudObject] := With[
    {i = Catch[iCloudScheduledTaskInformation[co], $tag]},
    If[MatchQ[i, _Association],
        i["Active"] && Not[i["Paused"]] && Not[i["Completed"]],
        (* else *)
        i
    ]
]


CloudObject /: NextScheduledTaskTime[co_CloudObject, o:OptionsPattern[]] /; Quiet[documentGeneratorQ[co]] := With[
    {i = Catch[iCloudDocumentGeneratorInformation[co], $tag]},
    If[MatchQ[i, _Association],
        If[TrueQ[Lookup[i["Task"], "Visible"]],
            Lookup[i[["Task"]], "NextRunDate", Message[ScheduledTask::nonext, co]; $Failed],
            (* else *)
            None
        ],
        (* else *)
        i
    ] 
]


CloudObject /: NextScheduledTaskTime[task_CloudObject] := With[
    {i = Catch[iCloudScheduledTaskInformation[task], $tag]},
    If[MatchQ[i, _Association],
        Lookup[i, "NextRunDate", Message[ScheduledTask::nonext, co]; $Failed],
        (* else *)
        i
    ] 
]

(*iCloudNextScheduledTaskTime[st_, OptionsPattern[]] := (Message[ScheduledTask::nonext, st]; $Failed)*)


(*
    AbortScheduledTask (pre-11.2)
*)

Unprotect[AbortScheduledTask];
SetAttributes[AbortScheduledTask, {ReadProtected}];

CloudObject /: AbortScheduledTask[co_CloudObject] /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudAbortScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        i
    ] 
]

CloudObject /: AbortScheduledTask[task_CloudObject] := With[
    {res = Catch[iCloudAbortScheduledTask[task], $tag]},
    res
]

iCloudAbortScheduledTask[obj_CloudObject, mh_:ScheduledTask] := (iCloudAbortScheduledTask[safeCloudAndUUIDFetch[obj, mh], mh]; obj)
iCloudAbortScheduledTask[{cloud_String, uuid_String}, mh_:ScheduledTask] := Module[
    {raw, json},

    json = Replace[execute[cloud, "POST", {"tasks", uuid, "abort"}], {
        (* HTTPError[400, ___] :> (Message[mh::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag]) *)
        HTTPError[404, ___] :> (Message[mh::tasknf, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
        , HTTPError[409, ___] :> (Message[mh::norun, cloudObjectFromUUID[uuid]]; Return[obj])
        , {_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content])
        , other_ :> (Message[mh::srverr, cloudObjectFromUUID[uuid]]; Throw[$Failed, $tag])
    }];

    Check[raw = Lookup[importFromJSON[json], "status"],
        Message[mh::srverr];
        Throw[$Failed, $tag]
    ];
    
    uuid
];


(*
    11.2+ AbortScheduledTask --> TaskAbort
*)

CloudObject /: TaskAbort[co_CloudObject] /; Quiet[documentGeneratorQ[co]] := Module[
    {i = System`DocumentGeneratorInformation[co, "Task"]},
    If[Head[i] === ScheduledTaskInformationData,
        Catch[iCloudAbortScheduledTask[{$CloudBase, Lookup[i[[1]], "UUID"]}], $tag];
        co,
        (* else *)
        i
    ] 
]

CloudObject /: TaskAbort[task_CloudObject] := With[
    {res = Catch[iCloudAbortScheduledTask[task], $tag]},
    res
]


(* AutoRefreshed *)

Unprotect[AutoRefreshed];
SetAttributes[AutoRefreshed, {HoldFirst, ReadProtected}];
Options[AutoRefreshed] = {
};

AutoRefreshed /: CloudDeploy[
    ct:AutoRefreshed[
        expr_, 
        tspec:Except[_Rule|_RuleDelayed]:3600, 
        fmt:Except[_Rule|_RuleDelayed]:"WL", 
        o:OptionsPattern[AutoRefreshed]
    ], 
    co_CloudObject, 
    oCD:OptionsPattern[CloudDeploy]
    ] := 
    Catch[autoRefreshedStaging[AutoRefreshed[expr, tspec, fmt, o], co, oCD], $tag]


(*
 * This function hijacks the InstantAPIServer internals to provide the documented support
 * for export and response forms.
 *) 
SetAttributes[exploy, HoldFirst];
Options[exploy] = Options[CloudDeploy];

exploy[expr_, fmt_, dest:CloudObject[uri_, destOpts:OptionsPattern[CloudObject]], o:OptionsPattern[]] := 
    Block[
        {CloudObject`$EvaluationParameters = <||>},
        (* This will basically use Delayed to render. *)
        Replace[
            Map[GenerateHTTPResponse[AutoRefreshed[expr, None, fmt]], {"Body", "ContentType"}],
            {body_, contentType_} :> 
                CloudObject`Private`writeObject[
                    dest, 
                    ToCharacterCode[body], 
                    contentType,
                    OptionValue[Permissions],
                    OptionValue[CloudObject, {destOpts}, IconRules],
                    Unevaluated[expr],
                    OptionValue[CloudObject, {destOpts}, MetaInformation],
                    {},
                    AutoRefreshed
                ]    
        ]
    ]

 
autoRefreshedStaging[AutoRefreshed[expr_, tspec_, fmt_, o:OptionsPattern[]], co_CloudObject, oCD:OptionsPattern[]] := Module[
    {taskObj, taskUuid, contentObj, uuid, perm},
    (* Create bundle (directory) *)
    responseCheck[
        Replace[execute[co, Automatic, UseUUID -> False, Type -> "application/vnd.wolfram.bundle.autorefreshed"], {
            HTTPError[400, ___] :> (Message[CreateDirectory::filex, co]; Throw[$Failed, $tag])
        }],
        AutoRefreshed,
        co
    ];

    Check[
        uuid = GetUUID[co];

        (* (This works for unnamed bundles too) *)
        contentObj = FileNameJoin[{co, uuid <> "-content"}];
        (* To get task obj into unnamed bundle, Put placeholder first *)
        taskObj = FileNameJoin[{co, uuid <> "-task"}];
        CloudPut[1, taskObj];
        taskUuid = GetUUID[taskObj];
        taskObj = cloudObjectFromUUID[taskUuid];

        perm = OptionValue[CloudDeploy, {oCD}, Permissions];

        With[{dest = contentObj, f = CloudObject`Private`exploy, p = perm},
            (* Uncomment expr if you want a synchronous (cloud) evaluation to take place here, not in service kernel *)
            f[ (*expr*) "Object pending evaluation", fmt, dest, Permissions -> p]
        ];

        (* Deploy generating task *)
        With[{taskOpts = Sequence @@ Join[FilterRules[Flatten[{o}], Options[ScheduledTask]], {"Visible" -> False}],
            dest = contentObj, permOpt = RuleDelayed[Permissions, Lookup[Options[co, Permissions], Permissions]]},
            iCloudDeployScheduledTask[
                ScheduledTask[
                    CloudObject`Private`exploy[expr, fmt, dest, permOpt],
                    tspec,
                    taskOpts
                ],
                taskObj,
                oCD
            ]
        ];

        (* Track contents in bundle meta *)
        SetOptions[co, {
            MetaInformation -> Join[
                {"__Content" -> GetUUID[contentObj], "__Task" -> taskUuid},
                Flatten[{OptionValue[CloudDeploy, {oCD}, MetaInformation]}]
            ]
            ,
            Permissions -> perm
            (* bad interaction between icon rules and bundle, possibly weird mime type: IconRules -> OptionValue[CloudDeploy, {oCD}, IconRules] *)
        }];
        (* Fill in content! *)
        RunScheduledTask @@ {taskObj};
        ,
        (* Remove files for object if failure occurs *)
        Quiet[RemoveScheduledTask[taskObj];
            DeleteDirectory[co, DeleteContents->True]];
        Throw[$Failed, $tag]
    ];

    co
];


(* etc. *)
GetNameFromURI[uri_String] := With[{split = StringSplit[uri, "/"]},
    If[Length[split] < 2, Message[ScheduledTask::nouri, uri]; Throw[$Failed, $tag], Last[split]]
]

GetUUID[obj_CloudObject] := Module[{res}, If[MatchQ[res = getCloudAndUUID[obj], {_, id_String}], Last[res], Throw[$Failed, $tag]]]
GetUUID[obj_String] := GetUUID[CloudObject[obj]]
GetUUID[___] := Throw[$Failed, $tag]

safeCloudAndUUIDFetch[CloudObject`Private`deleteable[obj_CloudObject], mh_:ScheduledTask] := safeCloudAndUUIDFetch[obj, mh];
safeCloudAndUUIDFetch[CloudObject`Private`preexisting[obj_CloudObject], mh_:ScheduledTask] := safeCloudAndUUIDFetch[obj, mh];
safeCloudAndUUIDFetch[obj_CloudObject, mh_:ScheduledTask] := Replace[getCloudAndUUID[obj], {
    {_, None} :> (Message[mh::cloudnf, obj]; Throw[$Failed, $tag])
}];
safeCloudAndUUIDFetch[None, mh_] := {$CloudBase, Null};
safeCloudAndUUIDFetch[___] := Throw[$Failed, $tag];


Protect[ScheduledTask, CloudObject, ScheduledTasks, EvaluateScheduledTask, ScheduledTaskInformation, ScheduledTaskInformationData,
    ContinuousTask, AbortScheduledTask, AutoRefreshed, Tasks`TaskEvaluate, TaskExecute, TaskAbort];

$Flag = False;


(* begin helper functions for timespec2cron[DateObject] *)
DatePatternQ[list_List] := MatchQ[list, {_?DatePatternElementQ ..}]

$DaysOfTheWeek = {Sunday, Monday, Tuesday, Wednesday, Thursday, 
   Friday, Saturday};
DatePatternElementQ[_?NumberQ] := True
DatePatternElementQ[Verbatim[Blank[]]] := True
DatePatternElementQ[day_Symbol] := MemberQ[$DaysOfTheWeek, day]
DatePatternElementQ[
  Verbatim[Alternatives][_?DatePatternElementQ ..]] := True
DatePatternElementQ[___] := False

QuartzValueQ[year_Integer, {1}] := TrueQ[1970 <= year <= 2099]
QuartzValueQ[month_Integer, {2}] := TrueQ[1 <= month <= 12]
QuartzValueQ[dayofmonth_Integer, {3}] := TrueQ[1 <= dayofmonth <= 31]
QuartzValueQ[hour_Integer, {4}] := TrueQ[0 <= hour <= 23]
QuartzValueQ[minute_Integer, {5}] := TrueQ[0 <= minute <= 59]
QuartzValueQ[seconds_Integer, {6}] := 
 TrueQ[0 <= seconds <= 59](*probably need numberQ*)

QuartzValueQ[___] := False


Clear[ElementToCron]
ElementToCron[element_?DatePatternElementQ, n : Except[{3}]] := 
  Which[QuartzValueQ[element, n], ToString[Ceiling[element]], 
   MatchQ[element, Verbatim[Blank[]]], "*", 
   MatchQ[element, 
    Verbatim[Alternatives][
     Repeated[PatternTest[Blank[], QuartzValueQ[#, n] &]]]], 
   StringJoin[
    Riffle[ToString[Ceiling[#]] & /@ (List @@ element), ","]], True, 
   Throw[$Failed, $tag]];

(*DOM[] is a wrapper for "day of the month" and DOW[] is for "day of \
the week"*)

ElementToCron[day_?DatePatternElementQ, n : {3}] := 
 Which[QuartzValueQ[day, n], DOM[ToString[Ceiling[day]]], 
  MatchQ[day, Verbatim[Blank[]]], DOM["*"], 
  MemberQ[$DaysOfTheWeek, Verbatim[day]], 
  DOW[ToString[First[Flatten[Position[$DaysOfTheWeek, day]]]]], 
  MatchQ[day, 
   Verbatim[Alternatives][
    Repeated[PatternTest[Blank[], MemberQ[$DaysOfTheWeek, #] &]]]], 
  DOW[StringJoin[
    Riffle[ToString[
        First[Flatten[Position[$DaysOfTheWeek, #]]]] & /@ (List @@ 
        day), ","]]], 
  MatchQ[day, 
   Verbatim[Alternatives][
    Repeated[PatternTest[Blank[], QuartzValueQ[#, n] &]]]], 
  DOM[StringJoin[
    Riffle[ToString[Ceiling[#]] & /@ (List @@ day), ","]]], True, 
  Throw[$Failed, $tag]]

ElementToCron[___] := Throw[$Failed, $tag]

currentTime[spec_] := DateString[DateList[], spec]

(* should probably get minutes and seconds in here as well 
Changed the name here
*)
$current := {currentTime["MonthShort"], currentTime["DayShort"], 
  currentTime["HourShort"], currentTime["MinuteShort"], 
  currentTime["SecondShort"]}

Clear[PadAppropriately];
PadAppropriately[list_List] := 
 Join[list, Take[$current, {Length[list], -1}]]


Clear[OrderForDOM, OrderForDOW];
OrderForDOM[list_List] := 
 Insert[Reverse[PadAppropriately[list]], "?", 6]

OrderForDOW[list_List] := 
 Insert[Part[PadAppropriately[list], {6, 5, 4, 2, 3, 1}], "?", 4]

DateObjectToCronSpecification[
  HoldPattern[dObj_DateObject?DateObjectQ], 
  target_: $TimeZone] := 
 With[{d = DateAndTime`DateObjectToDateList[dObj, target]}, 
  StringJoin[
   Riffle[Join[
     Reverse[ToString[Ceiling[#]] & /@ Rest[d]], {"?"}, {ToString[
       First[d]]}], " "]]]

DateObjectToCronSpecification[
  HoldPattern[
   dObj : DateObject[_List, 
      TimeObject[time_List, ___?OptionQ], ___?OptionQ]?DateObjectQ], 
  target_: $TimeZone] := 
 With[{d = DateAndTime`DateObjectToDateList[dObj, target]}, 
  StringJoin[
   Riffle[Join[
     ToString[Ceiling[#]] & /@ 
      Reverse[Rest[d]], {"?"}, {ToString[First[d]]}], " "]]]

DateObjectToCronSpecification[DateObject[l_?DatePatternQ]] := 
 If[0 < Length[l] < 7, 
  Catch[With[{cron = MapIndexed[ElementToCron, l]}, 
    StringJoin[
     Riffle[If[FreeQ[cron, DOW], OrderForDOM[cron], 
        OrderForDOW[cron]] /. {DOM[d_] :> d, DOW[d_] :> d}, 
      " "]]], $tag], $Failed]
      
DateObjectToCronSpecification[HoldPattern[dObj_DateObject?DateObjectQ]] := DateObjectToCronSpecification[dObj, $TimeZone]

DateObjectToCronSpecification[___] := $Failed


End[]

EndPackage[]
