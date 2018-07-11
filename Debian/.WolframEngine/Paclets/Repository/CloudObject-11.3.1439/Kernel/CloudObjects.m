(* ::Package:: *)

BeginPackage["CloudObject`"]

System`CloudObject;
System`IconRules;
System`MetaInformation;
System`AutoCopy;
System`$CloudObjectNameFormat;
System`CloudObjectNameFormat;

CloudDebug::usage = "CloudDebug[expr] evaluates expr with cloud debugging enabled."

$CloudObjectPacletInformation

Begin["`Private`"]

$PacletDirectory = DirectoryName[DirectoryName[$InputFileName]]
$PacletInfoFile = FileNameJoin[{$PacletDirectory, "PacletInfo.m"}]

LoadPacletInfo[] := 
	With[{
		data = 
			Block[{$Context = "PacletManager`"},
				Get[$PacletInfoFile]
			]
		},
		<| "Version" -> data[[2, 2]], "Location" -> $PacletDirectory |>
	]

$CloudObjectPacletInformation := $CloudObjectPacletInformation = LoadPacletInfo[]

userUUIDPrefix = "user-";

$CloudDebug = False;
$CloudDebugLevel = 2;

$CloudObjectsRoot = "/objects";
$CloudFilesRoot = "/files";
If[Not[StringQ[$CloudObjectNameFormat]], $CloudObjectNameFormat = "UserURLBase"];

Options[log] = {DebugLevel -> 1};
Attributes[log] = {HoldRest};
log[msg_String, Shortest[args___], OptionsPattern[]] :=
    If[$CloudDebug && $CloudDebugLevel >= OptionValue[DebugLevel],
        Print[ToString @ StringForm[msg, args]]
    ]

DebugLevel::usage = "DebugLevel is an option to log.";

SetAttributes[CloudDebug, HoldFirst];
Options[CloudDebug] = {DebugLevel -> 1};
CloudDebug[expr_, opts:OptionsPattern[]] := 
	Block[{$CloudDebug = True, $CloudDebugLevel = OptionValue[DebugLevel]},
	    expr
	]
	
extractURL[CloudObject[uri_String, ___]] := uri	
extractURL[uri_String] := uri

Options[parseURI] = {CloudObjectNameFormat -> Automatic};	

parseURI[uri_String, currentCloud_, currentUser_, rootDir_, currentDir_, objectsRoot_, filesRoot_, opts:OptionsPattern[]] :=
    Module[{protocol, host, port, pathname, baseobj, basepath, search, cloudprefix, request, nameFormat,
            cloud = currentCloud, user = currentUser, formatNameQ = False,
            uuid = None, path = None, ext = None, extraPath = {}
        },
        {protocol, host, port, pathname, search} = URLParse[uri, {"Scheme", "Domain", "Port", "Path", "Query"}];
        log["Parsed URL: `1`", {protocol, host, port, pathname, search}, DebugLevel -> 4];
        If[protocol === None,
           	formatNameQ = True;
           	baseobj = If[Length[pathname] >= 1 && First[pathname] === "", (* absolute path, such as CloudObject["/abspath"] *)
                          pathname = Rest[pathname];rootDir,
                          currentDir];
           	{protocol, host, port, basepath} = URLParse[extractURL[ReleaseHold[baseobj]], {"Scheme", "Domain", "Port", "Path"}];
           	pathname = Join[basepath, pathname]
        ];
        log["Parsing URI `1`", {protocol, host, port, pathname, search}, DebugLevel -> 4];
        nameFormat = OptionValue[CloudObjectNameFormat];
        Switch[protocol,
            "wolfram",
                If[host === None && Length[pathname] >= 1,
                   	{uuid, ext} = ParseUUID[First[pathname]];
                   	extraPath = Rest[pathname]
                ],
            "user",
                If[host === None && Length[pathname] >= 1,
                   	user = processCloudObjectNameFormat[First[pathname], nameFormat];
                  	path = Rest[pathname]
                ],
            "http" | "https" | None,     
            If[protocol =!= None && host =!= None,
               	cloud = URLBuild[{"Scheme" -> protocol, "Domain" -> host, "Port" -> port}];
               	If[pathname === {} || First[pathname] =!= "", PrependTo[pathname, ""]]
            ];
            log["Path `1` (object root: `2`, files root: `3`)", pathname, objectsRoot, filesRoot, DebugLevel -> 4];
            If[MatchQ[pathname, {___, objectsRoot | filesRoot, __}],
               	{cloudprefix, request, pathname} = Replace[pathname,
                    {Shortest[prefix___], type : objectsRoot | filesRoot, rest___} :> {{prefix}, type, {rest}}];
               	If[StringMatchQ[cloud, ___ ~~ "/"] && cloudprefix =!= {} && First[cloudprefix] === "",
                    cloud = StringDrop[cloud, -1]
               ];
               cloud = cloud <> StringJoin[Riffle[cloudprefix, "/"]];
               If[request === filesRoot,
                  	request = objectsRoot;
                  	AppendTo[search, "view" -> "file"]
               ];
               If[UUIDQ[First@pathname],
                   (* URI is of the form .../objects/<uuid>..." *)
                   log["UUID-based URI: `1`", pathname, DebugLevel -> 4];
                   {uuid, ext} = ParseUUID[First@pathname];
                   extraPath = Rest[pathname],
                   (* URI is of the form .../objects/<username>..." *)
                   log["Username-based URI `1`", pathname, DebugLevel -> 4];                   
                   user = First[pathname];
                   If[user === "~" || TrueQ[formatNameQ],
        				If[currentUser === None, CloudConnect[]];
        				user = processCloudObjectNameFormat[userFormat[nameFormat]];
        				user = If[user === None || user === $Failed || !StringQ[user], None, user];
                   ];
                   log["User: `1`", user, DebugLevel -> 3];
                   path = Rest[pathname];
                ]
            ]       
        ];
        log["Parsed URI: `1`", {cloud, uuid, user, path, ext, extraPath, search}, DebugLevel -> 4];
        {cloud, uuid, user, path, ext, extraPath, search}
    ]

parseURI[uri_String, base_String, opts:OptionsPattern[]] :=
    parseURI[StringTrim[uri], base, $CloudUserUUID,
                    Hold[$CloudRootDirectory],
                	Hold[$CloudDirectory],
                    StringSplit[$CloudObjectsRoot, "/", All][[2]],
                    StringSplit[$CloudFilesRoot, "/", All][[2]],
                    opts
                ]

parseURI[uri_String, opts:OptionsPattern[]] := parseURI[uri, $CloudBase, opts]

parseURI[CloudObject[uri_String], rest___] := parseURI[uri, rest]

userFormat[Automatic|"UUID"] := $CloudObjectNameFormat
userFormat[format_] := format
	
getUUID[CloudObject[uri_String, ___]] := Replace[
	parseURI[uri],
	{
		{_, uuid_String, __} :> uuid,
		_ :> $Failed
	}
]

getUUID[_] := $Failed

cloudObjectFromUUID[uuid_String] := cloudObjectFromUUID[$CloudBase, uuid]
cloudObjectFromUUID[cloud_String, uuid_String] := CloudObject[JoinURL[{cloud, $CloudObjectsRoot, uuid}]]

cloudObjectFromPathInfo[uuid_String] := cloudObjectFromUUID[$CloudBase, uuid]
cloudObjectFromPathInfo[cloud_String, allInfo_List, nameFormat_] := 
	With[{url = cloudURLFromPathInfo[cloud, allInfo, nameFormat]},
		If[url=!= $Failed, CloudObject[url], $Failed]
	]
cloudObjectFromPathInfo[__]:= $Failed	
	
cloudURLFromPathInfo[uuid_String] := cloudURLFromPathInfo[$CloudBase, uuid]
cloudURLFromPathInfo[cloud_String, allInfo_List, nameFormat_] := 
	Module[{info, owner, path, uuid, user},
		info = Lookup[allInfo, {"owner", "path", "uuid"}];
        If[MatchQ[info,{_List, Null|_String, _String}], {owner, path, uuid} = info, Return[$Failed]];
        If[path === Null, 
        	JoinURL[{cloud, $CloudObjectsRoot, uuid}]
        	,
        	If[StringMatchQ[path,$uuidPattern ~~ "/" ~~ __], 
        		JoinURL[{cloud, $CloudObjectsRoot, uuid}]
        		,
        		path = FileNameDrop[path, 1, OperatingSystem -> "Unix"];
        		user = Lookup[owner, Replace[nameFormat, {"UserURLBase" -> "userURLBase", "CloudUserID" -> "email", "CloudUserUUID" -> "uuid"}]];
        		If[StringQ[user],JoinURL[{cloud, $CloudObjectsRoot, user, path}], $Failed]
        	]
        ]
	]
cloudURLFromPathInfo[__]:= $Failed	

Unprotect[CloudObject]

$objectCreationOptions = {CloudObjectNameFormat -> Automatic, IconRules -> Automatic, MetaInformation -> {}, Permissions -> Automatic};
objectFunctionOptionsJoin[common_List, lis_List] := Sort[Join[common, lis]];

Options[CloudObject] = objectFunctionOptionsJoin[$objectCreationOptions, {AutoCopy -> False, SharingList -> {}, SourceLink -> Automatic}];

getCloudBase[base_String] :=
    Module[{scheme, domain, port, path, query, fragment},
        {scheme, domain, port, path, query, fragment} = URLParse[base, {"Scheme", "Domain", "Port", "PathString", "QueryString", "Fragment"}];
        If[And[
                StringQ[scheme],
                StringQ[domain],
                (*MatchQ[path, None|"/"|""],*)
                query === None,
                fragment === None
            ],
			(* memoize the result *)
           	getCloudBase[base] = 
            (* This will append a slash at the end. *)
            URLBuild[{
                "Scheme" -> scheme,
                "Domain" -> domain,
                "Port" -> port,
                "Path" -> path
            }],
        (* invalid cloud base *)
            Message[CloudObject::invbase, base];
            If[base === $CloudBase,
               	getCloudBase["https://www.wolframcloud.com/"],
               	getCloudBase[$CloudBase]
            ]
        ]
    ]

getCloudBase[Automatic|None|Null] = Automatic;

getCloudBase[base_] := 
	(Message[CloudObject::invbase, base]; getCloudBase[$CloudBase])

cloudBaseToDirectory[cbase_String] := Module[{scheme, path},
    {scheme, path} = URLParse[cbase, {"Scheme", "Path"}];
    If[scheme === None,
        CloudObject[cbase],
        If[path === {},
            CloudObject[CloudObject`JoinURL[cbase, $CloudObjectsRoot, "~"]],
            CloudObject[cbase]
        ]
    ]
]

processCloudObjectNameFormat[user_:"~", format_] := 
	With[{res = userSpecificationFromNameFormat[user, format]},
		If[res === None, 
			Message[CloudObjectNameFormat::una, format]; $UserURLBase,
			res
		]
	]
	
$CloudObjectNameFormat /: Set[HoldPattern[$CloudObjectNameFormat] , nameFormat_] /; ! TrueQ[$setFormat] := 
 Block[{$setFormat = True, format = nameFormat},
     setCloudObjectNameFormat[format, Set]
 ]
$CloudObjectNameFormat /: SetDelayed[HoldPattern[$CloudObjectNameFormat] ,nameFormat_] /; ! TrueQ[$setFormat] := 
 Block[{$setFormat = True, format = nameFormat},
     setCloudObjectNameFormat[format, SetDelayed]
 ]
setCloudObjectNameFormat[nameFormat:("UserURLBase" | "CloudUserID" | "CloudUserUUID" | "UUID"), set : Set | SetDelayed] := 
	If[set === Set, Identity, Function[Null]][$CloudObjectNameFormat = nameFormat]
setCloudObjectNameFormat[nameFormat_, set_] := (Message[$CloudObjectNameFormat::inv, nameFormat]; nameFormat)

userSpecificationFromNameFormat[user_, Automatic] := 
	userSpecificationFromNameFormat[user, Replace[$CloudObjectNameFormat, Automatic -> "UserURLBase"]]
userSpecificationFromNameFormat[user_, format:("UserURLBase" | "CloudUserID" | "CloudUserUUID")] := nameFormatOptionToValue[user, format]
userSpecificationFromNameFormat[user_, format_] := None

nameFormatOptionToValue[format_] := 
	Switch[format,
		"UserURLBase", 	(* silently fall back to $CloudUserUUID if $UserURLBase is not set *)
			If[StringQ[$UserURLBase], $UserURLBase, If[$CloudUserUUID === None, None, userUUIDPrefix <> $CloudUserUUID]],
		"CloudUserID", $CloudUserID,
		"CloudUserUUID",  
			If[$CloudUserUUID === None, None, userUUIDPrefix <> $CloudUserUUID],
		_, None 
	]

nameFormatOptionToValue["~", format_] := nameFormatOptionToValue[format]	

nameFormatOptionToValue[usr_, format_] :=
    Module[{json, importJSON, userInfo, res},
    	If[StringMatchQ[usr, userUUIDPrefix ~~ $uuidPattern] && format === "CloudUserUUID",
    		usr,
    		json = Replace[execute[$CloudBase, "GET", {"users"}, Parameters -> {"id" -> usr}], {
        	{_, bytes_List} :> FromCharacterCode[bytes], 
        	HTTPError[404, ___] :> (Message[CloudObject::invusr, usr]; $Failed),
        	other_ :> (checkError[other]; $Failed)}];
        	If[json === $Failed,
            	None,
            	importJSON = importFromJSON[json];
            	If[MatchQ[importJSON, {__List}],
            		userInfo = Flatten[importJSON];
            		res = Replace[ Lookup[userInfo,
            			Replace[format, {"UserURLBase" -> "userBaseUrl", "CloudUserID" -> "email", "CloudUserUUID" -> "uuid"}]],
            			Missing[x___] :> None]
            		,
            		None
            	];
            	If[StringQ[res] && format === "CloudUserUUID", userUUIDPrefix <> res, res]
        	]
    	]
    ]

optFinal[opts_List] := Sequence @@ FilterRules[opts, Except[CloudObjectNameFormat]]

$uuidPattern = 
	Repeated[HexadecimalCharacter, {8}] ~~ "-" ~~ 
	Repeated[HexadecimalCharacter, {4}] ~~ "-" ~~ 
	Repeated[HexadecimalCharacter, {4}] ~~ "-" ~~ 
	Repeated[HexadecimalCharacter, {4}] ~~ "-" ~~ 
	Repeated[HexadecimalCharacter, {12}];

uuidUriPattern = "http://" | "https://" ~~ __ ~~ $CloudObjectsRoot ~~ "/" ~~ $uuidPattern;
uuidUriQ[uri_String] := StringMatchQ[uri, uuidUriPattern]

uuidDirUriPattern = "http://" | "https://" ~~ __ ~~ $CloudObjectsRoot ~~ "/" ~~ $uuidPattern ~~ "/" ~~ __;		
uuidDirUriQ[uri_String] := StringMatchQ[uri, uuidDirUriPattern]	
		
fullNameUriPattern = "http://" | "https://" ~~ __ ~~ $CloudObjectsRoot ~~ "/" ~~ __ ~~ "/" ~~ __;
fullNameUriQ[uri_String] := StringMatchQ[uri, fullNameUriPattern]

fullNameUserUUIDUriPattern = "http://" | "https://" ~~ __ ~~ $CloudObjectsRoot ~~ "/" ~~ userUUIDPrefix ~~ $uuidPattern ~~ "/" ~~ __;
fullNameUserUUIDUriQ[uri_String] := StringMatchQ[uri, fullNameUserUUIDUriPattern]

cloudRootUriPattern =  "http://" | "https://" ~~ __ ~~ $CloudObjectsRoot ~~ "/" ~~ RegularExpression["[^/]+"];
cloudRootUriQ[uri_String] := StringMatchQ[uri, cloudRootUriPattern]

objectsBase[cloudbase_String] := 
	objectsBase[cloudbase] = JoinURL[cloudbase, $CloudObjectsRoot]

$processCloudObjectQ = True;
CloudObject[opts:OptionsPattern[]] := 
	Block[{$processCloudObjectQ = False, base = getCloudBase[$CloudBase], nameFormat},
		nameFormat = OptionValue[CloudObjectNameFormat];
		If[!MemberQ[{Automatic, "UUID"}, nameFormat], Message[CloudObjectNameFormat::una, nameFormat]];
		CloudObject[JoinURL[{base, $CloudObjectsRoot, System`CreateUUID[]}], optFinal[{opts}]]]
		
CloudObject[uri_String, opts:OptionsPattern[]]/;TrueQ[$processCloudObjectQ] := 
	Block[{$processCloudObjectQ = False},
		cloudObject[uri, opts]
	]	
	
CloudObject[URL[uri_String], opts:OptionsPattern[]] := CloudObject[uri,opts] 	

CloudObject[uri_, Automatic, opts:OptionsPattern[]] := CloudObject[uri, opts]

CloudObject[uri_, cbase_CloudObject, opts:OptionsPattern[]] :=
    Block[{$CloudDirectory = cbase}, CloudObject[uri, opts]]

CloudObject[uri_, cbase_String, opts:OptionsPattern[]] :=
    CloudObject[uri, cloudBaseToDirectory[cbase], opts]
    
CloudObject[uri_, URL[cbase_String], opts:OptionsPattern[]] :=
	   CloudObject[uri, cbase, opts]    

CloudObject[CloudObject[uri_, opts1:OptionsPattern[]], opts2:OptionsPattern[]] :=
    CloudObject[uri, opts1, opts2]   

CloudObject[args___] := (ArgumentCountQ[CloudObject, Length[DeleteCases[{args}, _Rule, Infinity]], 0, 2]; Null /; False)

Options[cloudObject] = Options[CloudObject];	

(* optimize the very common case of base/objects/{uuid} *)
cloudObject[uri_String, opts:OptionsPattern[]] /; uuidUriQ[uri] :=
    Module[{nameFormat, infoPath, newURI},
        nameFormat = OptionValue[CloudObjectNameFormat];
        If[MemberQ[{Automatic, "UUID"}, nameFormat],
            CloudObject[uri, optFinal[{opts}]]
            ,
            infoPath = Quiet[CloudObjectInformation[CloudObject[uri], "Path"]];
            If[MemberQ[{$Failed, None},infoPath],
            	Message[CloudObjectNameFormat::una, nameFormat]; 
                CloudObject[uri, optFinal[{opts}]]
                ,
                newURI = reconstructNamedURI[uri, nameFormat];
                CloudObject[newURI, optFinal[{opts}]]
            ]
        ]
    ]
	
cloudObject[uri_String, opts:OptionsPattern[]] /; uuidDirUriQ[uri] :=
    Module[{nameFormat, cloud, uuid, newURI},
        nameFormat = OptionValue[CloudObjectNameFormat];
        If[nameFormat === "UUID",
            {cloud, uuid} = getCloudAndUUID[uri];
            If[!(StringQ[cloud] && UUIDQ[uuid]),
            	$Failed,
            	newURI = JoinURL[{cloud, $CloudObjectsRoot, uuid}];
            	CloudObject[newURI, optFinal[{opts}]]
            ]
            ,
            If[nameFormat =!= Automatic, Message[CloudObjectNameFormat::una, nameFormat]];
            CloudObject[uri, optFinal[{opts}]]           
        ]
    ]

cloudObject[uri_String, opts:OptionsPattern[]] /; cloudRootUriQ[uri] :=
    Module[{nameFormat, cloudRoot, user, uriNew},
        nameFormat = OptionValue[CloudObjectNameFormat];
        If[nameFormat === Automatic && (!StringMatchQ[uri, ___ ~~ "/~"]),
        	CloudObject[uri, optFinal[{opts}]]
        	,
        	cloudRoot = StringTrim[uri, "/" ~~ RegularExpression["[^/]+"]];
        	user = processCloudObjectNameFormat[Last[URLParse[uri, "Path"]], Replace[nameFormat,"UUID" -> "UserURLBase"]];
        	If[user === None,
        		None
        		,
        		uriNew = JoinURL[{cloudRoot, user}];
        		If[nameFormat === "UUID",
        			CloudObject[reconstructUuidURI[uriNew], optFinal[{opts}]],
        			CloudObject[uriNew, optFinal[{opts}]]
        		]
        	]   	   	
		]
    ]    
    	

cloudObject[uri_String, opts:OptionsPattern[]] :=	
	Module[{nameFormat, newURI, cloud, uuid, user, path, ext, extraPath, search, res, finalURL},
		nameFormat = OptionValue[CloudObjectNameFormat];
		res = If[fullNameUriQ[uri]
			,
			Switch[nameFormat,
				Automatic, CloudObject[uri, optFinal[{opts}]],
				"UUID", 
				newURI = reconstructUuidURI[uri];
				If[MemberQ[{$Failed, None},newURI], 
					Message[CloudObject::cloudnf, uri]; uri,
					newURI
				],
				_,
				newURI = reconstructNamedURI[uri, nameFormat];
				If[newURI === $Failed, 
					Message[CloudObjectNameFormat::una, nameFormat];
					uri,
					newURI]				
			]
			,
			{cloud, uuid, user, path, ext, extraPath, search} = 
				parseURI[uri, CloudObjectNameFormat -> nameFormat];	
			If[uuid === None,
	            If[path === None,
	                (* don't check for shortened URLs unless the parse fails *)
	                If[URLUtilities`URLShortenedQ[uri],
	                    newURI = URLExpand[uri];
	                    log["New named URI: `1`", newURI, DebugLevel -> 4];
	                    newURI,
	                (* else *)
		                Message[CloudObject::invuri, uri];
		                $Failed
	                ],
	            (* else *)
	                If[user === None || user === "",
	                    Message[CloudObject::unauth, uri]; $Failed,
	                (* else *)
	                    newURI = StringJoin[
                            JoinURL[{cloud, $CloudObjectsRoot, user, path}],
	                        If[ext === None, "", "." <> ext],
                            JoinURLSearch[search]
	                    ];
	                    log["New named URI: `1`", newURI, DebugLevel -> 4];
	                    newURI
	                ]
	            ],
	        (* else: explicit UUID set *)
	            newURI = StringJoin[
                    JoinURL[{cloud, $CloudObjectsRoot, uuid, extraPath}],
	                If[ext === None, "", "." <> ext],
                    JoinURLSearch[search]
	            ];
	            log["New UUID-based URI: `1`", newURI, DebugLevel -> 4];
	            newURI
	        ]
		];
	        res = If[(res =!= $Failed) && (OptionValue[CloudObjectNameFormat] === "UUID"),
	        	finalURL = reconstructUuidURI[res];
	        	If[finalURL === $Failed, Message[CloudObjectNameFormat::una, "UUID"]; res,finalURL],
	        	res
	        ];
	        If[res === $Failed, $Failed, CloudObject[res, optFinal[{opts}]]]	        	
			
	]
	
cloudobject[___] := $Failed	

reconstructNamedURI[uri_String, nameFormat_String] :=
	Module[{cloud, uuid, json, allinfo, info},
		If[fullNameUserUUIDUriQ[uri] && nameFormat === "CloudUserUUID",
			uri
			,
			{cloud, uuid} = getCloudAndUUID[uri];
        	If[!(StringQ[cloud] && UUIDQ[uuid]), 
        		Message[CloudObject::cloudnf, CloudObject[uri]]; Return[$Failed]];
        		
        	json = Replace[execute[cloud, "GET", {"files", uuid, "info"}],
        		{
        			HTTPError[404, ___] :> (Message[CloudObject::cloudnf, CloudObject[uri]]; Return[$Failed]),
              		{_String, content_List} :> ($lastInfoJSON = FromCharacterCode[content]),
                	other_ :> (Message[CloudObject::srverr]; Return[$Failed])
            	}];
        	allinfo = importFromJSON[json];
        	If[ListQ[allinfo],
        		info = Replace[Lookup[allinfo, "files"], {x_List} :> x];       
        		cloudURLFromPathInfo[cloud, info, nameFormat],
        		Message[CloudObject::srverr];Return[$Failed]
        	]
		]	
	]	
		
reconstructUuidURI[uri_String] :=
	Module[{cloud, uuid},
		{cloud, uuid} = getCloudAndUUID[uri]; 
		If[!(StringQ[cloud] && UUIDQ[uuid]), Return[$Failed]];
		JoinURL[{cloud, $CloudObjectsRoot, uuid}]
	]
		
reconstructURI[uri_String, form_String]:=
	Replace[
    parseURI[uri],
    {
    (* parts: {cloud, uuid, user, path, ext, extraPath, search} *)

    (* anonymous cloud object *)
        {cloud_, uuid_String?UUIDQ, _, None, _, {}, _} :>
            MatchQ[execute[cloud, "GET", {"files", uuid, {"path"}}],
                {_String, _List}],
    (* named cloud object *)
        {cloud_, None, userid_, path_, _, _, _} :>
            MatchQ[execute[cloud, "GET", {"files"},
                Parameters -> {"path" -> userid <> "/" <>
                    FileNameJoin[path, OperatingSystem -> "Unix"]}],
                {_String, bytes_List /; bytes =!= {}}],
    (* named cloud object inside unnamed directory *)
        {cloud_, diruuid_, userid_, None, _, extraPath_List, _} :>
            MatchQ[execute[cloud, "GET", {"files"},
                Parameters -> {"path" -> diruuid<>"/"<>
                    FileNameJoin[extraPath, OperatingSystem -> "Unix"]}],
                {_String, bytes_List /; bytes =!= {}}]
    }
]		

(* Only use hyperlinks inside CloudObject in desktop Mathematica. Otherwise, a "This feature is not supported" dialog is shown. *)
If[$CloudEvaluation === True,
    (* In the Cloud, use RawBoxFormat to produce interactive output. *)
    BoxForm`MakeConditionalTextFormattingRule[CloudObject];
    Format[CloudObject[uri_String], StandardForm] :=
        CloudSystem`RawBoxFormat[Interpretation[CloudObject[Hyperlink[uri]], CloudObject[uri]]],
(* In desktop Mathematica, use MakeBoxes rather than Format *)
	BoxForm`MakeConditionalTextFormattingRule[CloudObject];
	CloudObject /: MakeBoxes[co: CloudObject[_String], fmt_] :=
        MakeCloudObjectBoxes[co, fmt];
	BoxForm`MakeConditionalTextFormattingRule[CloudObjectInformationData];
	CloudObjectInformationData /: MakeBoxes[data: CloudObjectInformationData[_Association], fmt_] :=
		MakeCloudObjectInformationDataBoxes[data, fmt];
]


SetAttributes[{MakeCloudObjectBoxes, MakeCloudObjectInformationDataBoxes}, HoldAllComplete]

MakeCloudObjectBoxes[CloudObject[uri_], fmt_] :=
	With[{boxes = MakeBoxes[Defer[CloudObject][Hyperlink[uri]], fmt]},
		InterpretationBox[boxes, CloudObject[uri], SelectWithContents -> True]]

MakeCloudObjectInformationDataBoxes[CloudObjectInformationData[data_Association], fmt_] :=
	Module[{name, type, normal},
		name = Replace[data[["Name"]], {s_String :> s, _ -> "--untitled--"}];
		type = Replace[data[["FileType"]],
			{File->"File information", Directory->"Directory information", _->"CloudObject information"}];
		normal = Normal[data]; (* needed for serializing -- see bug 263825 *)
		With[{
			boxes = ToBoxes[Panel[
				Column[{
					Row[{Style["CloudObject: ", FontColor -> Gray], name}],
					Item[
						OpenerView[{
							Style[type, Bold],
							BoxForm`Undeploy[Grid[List @@@ normal, Alignment -> {{Right, Left}}]]},
							False,
							Deployed -> True
						],
						Alignment -> Left
					]
					},
					Dividers -> {False,{False,True}},
					Spacings -> {Automatic,{Automatic,1.2}},
					FrameStyle -> LightGray,
					BaselinePosition -> {2,1}
				],
				BaselinePosition -> Baseline
			], fmt],
			normal = normal
			},
			InterpretationBox[boxes, CloudObjectInformationData[Association[normal]], SelectWithContents -> True]
		]
	]

End[]

EndPackage[]
