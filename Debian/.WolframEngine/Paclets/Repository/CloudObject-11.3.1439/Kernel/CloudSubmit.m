(* Mathematica package *)
BeginPackage["CloudObject`"];

System`CloudSubmit;
System`MessageObject;


Begin["`Private`"];


(*
   User may provide handlers for these
*)
$CloudSubmitHandlerNames = {
	"TaskStarted", "TaskFinished", "TaskRemoved", "TaskStatusChanged",
	"PrintOutputGenerated", "MessageGenerated", "FailureOccurred", "ResultReceived",
	"UnhandledEvent"
}

(*
   Which keys to use in an association, which is the argument of event handlers 
*)
$CloudSubmitKeyNames = {
	(* meta properties *)
	"TaskUUID", "Task", "TaskType", "EvaluationExpression", "EventName",
	(* set by events *)
	"TaskStatus", "PrintOutput", "MessageOutput", "Failure", "EvaluationResult"
}

If[ValueQ[Internal`$DebugBuild] && Internal`$DebugBuild,
	$CloudSubmitKeyNames = Join[$CloudSubmitKeyNames, {
		(* cloud objects *)
		"RemoteObject" (* task *),
		"RemoteDataObject" (* data *)
	}]
]

$DefaultCloudSubmitKeyNames = $CloudSubmitKeyNames

(* Polling method *)
$DefaultUpdateInterval = 1;

(*
    <|...|> - event data field and function to use to update it
    {...} - which user-level events to activate. The user may have supplied handlers for these.
*)
$CloudTaskEventActions = <|
	"CloudTaskStarted" -> {
		<|
			"TaskStatus" -> makeTaskStatusRunning
		|>,
		{"TaskStarted", "TaskStatusChanged"}
	},
	"CloudTaskFinished" -> {
		<|
			"TaskStatus" -> makeTaskStatusFinished
		|>,
		{"TaskFinished", "TaskStatusChanged", "ResultReceived"}
	},
	"CloudTaskRemoved" -> {
		<|
			"TaskStatus" -> makeTaskStatusRemoved
		|>,
		{"TaskRemoved", "TaskStatusChanged"}
	},
	"CloudMessageGenerated" -> {
		<|
			"MessageOutput" -> makeIdentity
		|>,
		{"MessageGenerated"}
	},
	"CloudPrintOutputGenerated" -> {
		<|
			"PrintOutput" -> makeIdentity
		|>,
		{"PrintOutputGenerated"}
	},
	"CloudFailure" -> {
		<|
			"Failure" -> makeFailure
		|>,
		{"FailureOccurred"}	
	}
|>

(************************************************************)

(*
    Generic function to verify symbol's options
*)
CheckAndGetOptions[name_Symbol, opts_List] := 
	Block[
		{allopts, tmp = opts, rest}
		,
		allopts = Association[Options[name]];
		If[tmp != {},
			tmp = Association[tmp];
			rest = KeyDrop[tmp, Keys[allopts]];
		];
		If[Length[rest] == 0,
			AppendTo[allopts, tmp];
			allopts			
			,
			(* Unknown opition(s) *)
			Message[MessageName[name, "optx"], First[Keys[rest]], name];
	        $Failed
		]
]

CheckAndGetOptions[___] := $Failed

(*
   Given handlers as an association, which is specified by a user, pick only those relevant
   for the caller and return them. Generate a message and return $Failed, if unknown entries are found.
*)
filterHandlerFunctions[caller_, hfuns_, taskEventNames_] := Block[
	{defaults, handlerEventNames, extra}
	,
	If[AssociationQ[hfuns]
		,
		extra = KeyDrop[hfuns, taskEventNames];
		If[Length[extra] != 0,
			Message[MessageName[caller, "invhf"], First[Keys[extra]]]];
		KeyTake[hfuns, taskEventNames]
		,
		Message[MessageName[caller, "invak"], hfuns];
		$Failed
	]
]


iFilterKeyNanes[caller_, hkeys_, taskKeyNames_] := Block[
	{handlerKeyNames = hkeys, extra}
	,
    If[!ContainsAll[taskKeyNames, handlerKeyNames],
		extra = Complement[handlerKeyNames, taskKeyNames];
		Message[MessageName[caller, "invhk"], First[extra]];
		handlerKeyNames = Intersection[handlerKeyNames, taskKeyNames];
    ];
    If[Length[handlerKeyNames] > 0,
    	handlerKeyNames,
    	{}
    ]	
]


filterKeyNames[caller_, hkeys_, taskKeyNames_] := Which[
	hkeys === Automatic,
		Automatic (* will use default values *),
	hkeys === All,
		taskKeyNames,
	StringQ[hkeys],
		iFilterKeyNanes[caller, {hkeys}, taskKeyNames],
	ListQ[hkeys],
		iFilterKeyNanes[caller, hkeys, taskKeyNames],
	True,
		Message[MessageName[caller, "nslist"], hkeys];
        $Failed
]



(************************************************************)

(* Evaluated in the Cloud *)


appendToObject[arg_, name_String, obj_] := Module[
	{tmp, val, counter}
	,
	tmp = System`CloudGet[obj];
	If[AssociationQ[tmp],
		val = Lookup[tmp, name, {}];
		If[MissingQ[val],
			val = {}];
		If[!ListQ[val],
			val = {val}];
		AppendTo[val, arg];
		counter = Lookup[tmp, "Counter", 0];
		AssociateTo[tmp, {name -> val, "Counter" -> counter + 1}];
		System`CloudPut[tmp, obj]
	]
]


printHandler[obj_][arg_] := (appendToObject[arg, "Print", obj]; (* do not issue the message *) False)


messageHandler[obj_][arg_] := Module[
	{msg, visible}
	,
	visible = Extract[arg, {2}];
	If[TrueQ[visible]
		,
		(*
			arg is Hold[Message[MessageName[symbol, tag], HoldForm[param]...], True|False] 
			msg is Hold[{symbol::tag, HoldForm[param]...}]
		*)
		msg = Apply[List, Extract[arg, {1}, Hold], {1}];
		
		(* 
			symbol is HoldForm[sym]; held because it can self-evaluate (e.g. $RecursionLimit)
			tag is a string
			params is a list {HoldForm[param]...}
		*)		
		appendToObject[
			With[{
					sym = Extract[msg, {1, 1, 1}, HoldForm],
					tag = Extract[msg, {1, 1, 2}],
					prm = Rest[msg[[1]]]				
				}
				,
				System`MessageObject[<|
					"MessageSymbol" -> sym,
					"MessageTag" -> tag,
					"MessageParameters" -> prm,
					"MessageTemplate" -> ReleaseHold[Quiet[MessageName[sym, tag]]]
				|>]
			],
			"Messages",
			obj
		]
	];
	(* do not issue the message *)
	False
]


updateObject[obj_, name_, val_] := Module[
	{tmp, counter}
	,
	tmp = System`CloudGet[obj];
	
	If[AssociationQ[tmp],
		counter = Lookup[tmp, "Counter", 0];
		AssociateTo[tmp, {name -> val, "Counter" -> counter + 1}];
		System`CloudPut[tmp, obj]
	]
]


SetAttributes[doEvaluation, HoldFirst]

doEvaluation[expr_, obj_] := Module[
	{res}
	,
	updateObject[obj, "TaskStatus", "Running"];
	(* 
	   use Message.Veto and Wolfram.System.Print.Veto
	   in kernel 11.2 and later to quiet Message and Print
	 *)
	If[$VersionNumber >= 11.2,
		Internal`AddHandler["Message.Veto", messageHandler[obj]];
		Internal`AddHandler["Wolfram.System.Print.Veto", printHandler[obj]]
		,
		Internal`AddHandler["Message", messageHandler[obj]];
		Internal`AddHandler["Wolfram.System.Print", printHandler[obj]]
	];		
	(* actual evaluation *)
	res = expr;
	updateObject[obj, "EvaluationResult", res];
	(* *)
	If[$VersionNumber >= 11.2,
		Internal`RemoveHandler["Wolfram.System.Print.Veto", printHandler[obj]];
		Internal`RemoveHandler["Message.Veto", messageHandler[obj]]
		,
		Internal`RemoveHandler["Wolfram.System.Print", printHandler[obj]];
		Internal`RemoveHandler["Message", messageHandler[obj]]
	];		
	updateObject[obj, "TaskStatus", "Finished"];
	res
]


(************************************************************)

(* Evaluated in the local kernel *)

makeIdentity[dataID_, name_, newData_] := 
	Tasks`SetTaskEventValue[dataID, name, newData]

makeTaskStatusRunning[dataID_, _, _] :=
	Tasks`SetTaskEventValue[dataID, "TaskStatus", "Running"]

makeTaskStatusFinished[dataID_, _, _] :=
	Tasks`SetTaskEventValue[dataID, "TaskStatus", "Finished"]

makeTaskStatusRemoved[dataID_, _, _] :=
	Tasks`SetTaskEventValue[dataID, "TaskStatus", "Removed"]

makeFailure[dataID_, _, _] :=
	Tasks`SetTaskEventValue[dataID, "Failure", None] (* TODO *)


activateEvents[localDataID_, localData_, remoteData_, eventFuncs_] := Module[
	{
		localMessages, remoteMessages, newMessages,
		localText, remoteText, newText
	}
	,
	localMessages = Lookup[localData, "Messages", {}];
	remoteMessages = Lookup[remoteData, "Messages", {}];

	If[Length[localMessages] =!= Length[remoteMessages],
		newMessages = Complement[remoteMessages, localMessages];
		
		Map[
			Tasks`Package`handleEvent["CloudMessageGenerated", localDataID, #, eventFuncs]&, 
			newMessages
		]
	];
	
	localText = Lookup[localData, "Print", {}];
	remoteText = Lookup[remoteData, "Print", {}];
	If[Length[localText] =!= Length[remoteText],
		newText = Complement[remoteText, localText];
		Map[
			Tasks`Package`handleEvent["CloudPrintOutputGenerated", localDataID, #, eventFuncs]&,
			newText
		]
	]
]


deleteObject[obj_] := DeleteFile[obj]

makeReadHandler[obj_, countIn_, handlerKeys_, eventFuncs_, origExprHeld_, mainObjIn_] := Module[
	{
		localCount = countIn, remoteCount,
		localData = <||>,
		firstTime = True,
		localDataID = None,
		taskID = None,
		mainObj = mainObjIn
	},
	If[!AssociationQ[localData], Throw[$Failed]];

	Function[{}
		,
		Module[
			{remoteData, taskStatus, res, tmp}
			,
			res = Catch[
				If[localDataID === None,
					localDataID = Tasks`GetTaskEventDataID[System`$CurrentTask];
					If[localDataID === $Failed, Throw[$Failed]]
				];
				If[taskID === None,
					taskID = Tasks`GetTaskID[$CurrentTask];
					If[taskID === $Failed, Throw[$Failed]]
				];

				If[MemberQ[handlerKeys, "RemoteObject"],
					Tasks`SetTaskEventValue[localDataID, "RemoteObject", mainObj]
				];

				If[MemberQ[handlerKeys, "RemoteDataObject"],
					Tasks`SetTaskEventValue[localDataID, "RemoteDataObject", obj]
				];

				remoteData = Quiet[CloudGet[obj]];

				(*
					If cloud object does not exist, e.g. has been deleted,
					then the task is terminated. 
				*)
				If[!AssociationQ[remoteData],
					(* TODO: Failure[ ] *) 
					Throw[$Failed]
				];

				(*
				   Fake "TaskStarted" event
				*)
				If[firstTime
					,
				 	firstTime = False;
				 	Tasks`SetTaskEventValue[localDataID, "TaskType", "CloudTask"];
				 	Tasks`Package`handleEvent["CloudTaskStarted", localDataID, None, eventFuncs]
				];

				remoteCount = Lookup[remoteData, "Counter", $Failed];

				If[remoteCount == $Failed,
					(* TODO: Message *)
					Throw[$Failed]
				];

				(*
				    If remote data has changed, update localy and run event handlers
				*)
				If[localCount < remoteCount,

					(*
						Add special meta-data about the task to TaskEventData
					*)
					If[MemberQ[handlerKeys, "Task"],
						Tasks`SetTaskEventValue[localDataID, "Task", System`$CurrentTask]
					];

					If[MemberQ[handlerKeys, "TaskUUID"],
						Tasks`SetTaskEventValue[localDataID, "TaskUUID", taskID]
					];

					If[MemberQ[handlerKeys, "TaskType"],
						Tasks`SetTaskEventValue[localDataID, "TaskType", "CloudTask"]
					];

					If[MemberQ[handlerKeys, "EvaluationExpression"],
						Tasks`SetTaskEventValueHeld[localDataID, "EvaluationExpression", origExprHeld]
					];
					
					If[MemberQ[handlerKeys, "EvaluationResult"],
						tmp = Lookup[remoteData, "EvaluationResult", Missing["NotAvailable"]];
						Tasks`SetTaskEventValue[localDataID, "EvaluationResult", tmp]
					];

					activateEvents[localDataID, localData, remoteData, eventFuncs];
					localCount = remoteCount;
					localData = remoteData;
				];
				
				taskStatus = Lookup[remoteData, "TaskStatus", "Running"];

				If[taskStatus == "Finished",
					Tasks`Package`handleEvent["CloudTaskFinished", localDataID, None, eventFuncs];
					Tasks`Package`handleEvent["CloudTaskRemoved", localDataID, None, eventFuncs];
					deleteObject[obj];
					TaskRemove[$CurrentTask]
				]
			];
			If[res === $Failed,
				(* TODO: "task failed" event *)
				Tasks`Package`handleEvent["CloudTaskRemoved", localDataID, None, eventFuncs];
				deleteObject[obj];
				TaskRemove[$CurrentTask];
			]			
		]
	]
]


(************************************************************
 *	new CloudSubmit
 ************************************************************)

SetAttributes[doCloudDeploy, HoldFirst];

doCloudDeploy[ScheduledTask[expr_, sched_, taskopts:OptionsPattern[]], obj_, optsIn:OptionsPattern[]] := Block[
	{}
	,
	With[{opts = Sequence @@ Join[{optsIn}, {taskopts}]}
		,
		CloudDeploy[
			ScheduledTask[		
				CloudObject`Private`doEvaluation[expr, obj]
				,
				sched, opts]
		]
	]
]

doCloudDeploy[_ScheduledTask, ___] := ( (* Message[]; *) $Failed)

doCloudDeploy[expr:Except[_ScheduledTask], obj_, optsIn:OptionsPattern[]] := Block[
	{}
	,
	CloudDeploy[
		ScheduledTask[
			doEvaluation[expr, obj]
			,
			{Now}, optsIn]
	]	
]

doCloudDeploy[___] := $Failed


SetAttributes[CloudSubmitImpl, HoldFirst]

CloudSubmitImpl[expr_, objIn_, optsIn:OptionsPattern[]] := Module[
	{
		opts, hopts, hf, hk, handlerFunctions, handlerKeys, updateInterval,
		remoteData, mainobj,
		eventFuncs, spec,
		cloudBase, tmpObj,
		obj = objIn,
		counterStart = 0
	}
	,
	(*
		Check options
	*)
	opts = Join[Options[System`CloudSubmit], Flatten[{optsIn}]];
	hopts = CheckAndGetOptions[System`CloudSubmit, opts];
	If[!AssociationQ[hopts],
		Throw[$Failed]
	];

	cloudBase = Lookup[hopts, System`CloudBase, System`$CloudBase];

	(*
		Extract specified handler functions
	*)
	hf = Lookup[hopts, System`HandlerFunctions, Automatic];

	If[hf === Automatic,
		handlerFunctions = Association[],
		handlerFunctions = filterHandlerFunctions[System`CloudSubmit, hf, $CloudSubmitHandlerNames]
	];
	If[handlerFunctions === $Failed,
		Throw[$Failed]
	];

	(* here handlerFunctions is an association *)
	If[!AssociationQ[handlerFunctions], Throw[$Failed]];

	(*
		Extract specified handler keys
	*)
	hk = Lookup[hopts, System`HandlerFunctionsKeys, Automatic];
	handlerKeys = filterKeyNames[System`CloudSubmit, hk, $CloudSubmitKeyNames];
	If[handlerKeys === Automatic,
		handlerKeys = $DefaultCloudSubmitKeyNames
	];

	If[handlerKeys === $Failed, Throw[$Failed]];

	(*
	    Polling method, update interval
	    TODO: use Method option
	*)
	updateInterval = Lookup[hopts, "UpdateInterval", $Failed];
	If[updateInterval === $Failed,
		updateInterval = $DefaultUpdateInterval
	];

	(*
		Upload all the definitions and metadata
	*)
	remoteData = <|
		"Counter" -> counterStart,
		"TaskStatus" -> "Starting",
		"RemoteDataObject" -> obj
	|>;
	tmpObj = CloudPut[remoteData, obj];
	If[tmpObj === $Failed, Throw[$Failed]];

	(*
	    Run the cloud task
	*)
	mainobj = With[
		{obj = obj, taskOpts = Sequence @@ Join[FilterRules[opts, Options[ScheduledTask]], {AutoRemove -> True}]}
		,
		doCloudDeploy[expr, obj, taskOpts]
	];
 
	(*
	    Create local polling 
	*) 
 	eventFuncs = Tasks`Package`makeEventFunctions[$CloudTaskEventActions, handlerFunctions, handlerKeys];
	If[!AssociationQ[eventFuncs], Throw[$Failed]];
	
	With[{readHandler = makeReadHandler[obj, counterStart, handlerKeys, eventFuncs, Hold[expr], mainobj]}
		,
		spec = <|
			"Caller" -> System`CloudSubmit,
			"TaskEnvironment" -> "Cloud",
			"TaskType" -> "Cloud",
			"RealEvaluationExpression" -> Hold[readHandler[]], (* what to evaluate *)
			"EvaluationExpression" -> HoldForm[expr], (* what to display *)
			"Schedule" -> <|
				"RepeatInterval" -> updateInterval,
				"Repeated" -> True,
				"RepeatCount" -> Infinity
			|>,
			System`HandlerFunctions -> <||>, (* handlerFunctions *)
			System`HandlerFunctionsKeys -> handlerKeys,
			"UserData" -> <|
				"RemoteDataObject" -> obj,
				"RemoteObject" -> mainobj
			|>
		|>
	];
	Tasks`ScheduledToTaskObject[spec]
]


Unprotect[System`CloudSubmit];
Clear[System`CloudSubmit];
SetAttributes[System`CloudSubmit, {HoldFirst, ReadProtected}];


If[TrueQ[$VersionNumber >= 11.2],

	(*********************** New CloudSubmit **********************)


	Options[System`CloudSubmit] = {
		HandlerFunctions -> <||>,
		HandlerFunctionsKeys -> Automatic,
		Method -> Automatic,
		NotificationFunction -> Automatic,
		CloudBase :> $CloudBase
	};
	
	System`CloudSubmit[expr_, opts:OptionsPattern[]] := Block[
	   	{$CloudBase = handleCBase[OptionValue[CloudBase]], obj, res}
	   	,
	   	obj = System`CloudObject[];
	   	(
			res = Catch[CloudSubmitImpl[expr, obj, opts]];
			res /; res =!= $Failed
		) /; obj =!= $Failed
	];
	
	System`CloudSubmit[expr_, obj:_String|_CloudObject|_URL, opts:OptionsPattern[]] := Block[
		{$CloudBase = handleCBase[OptionValue[CloudBase]], res}
		,
		res = Catch[CloudSubmitImpl[expr, obj, opts]];
		res /; res =!= $Failed
	];
	
	e:System`CloudSubmit[_, opts:Except[OptionsPattern[]]] := Module[
		{res}
		,
		res = (
			Message[System`CloudSubmit::nonopt1, Last[{opts}], 1, HoldForm[e]];
			$Failed
		);
		res /; res =!= $Failed
	];
	
	System`CloudSubmit[___] := $Failed
	,


	(*********************** Old CloudSubmit, $VersionNumber < 11.2 **********************)

	Options[System`CloudSubmit] = {
	    NotificationFunction -> Automatic,
	    CloudBase :> $CloudBase
	};
	
	System`CloudSubmit[expr_, o:OptionsPattern[]] := Block[
	    {$CloudBase = handleCBase[OptionValue[CloudBase]]},
	    System`CloudSubmit[expr, CloudObject[], o]
	];
	
	System`CloudSubmit[expr_, obj:_String|_CloudObject|_URL, o:OptionsPattern[]] := Module[
	    {opts = Join[Flatten[{o}], Options[System`CloudSubmit]]},
	    Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
	        With[{taskOpts = Sequence @@ Join[FilterRules[opts, Options[ScheduledTask]], {AutoRemove -> True}]},
	            CloudDeploy[
	                ScheduledTask[expr, {Now}, taskOpts],
	                obj
	            ]
	        ]
	    ]
	]
]





Protect[System`CloudSubmit];

(************************************************************)

End[]

EndPackage[]
