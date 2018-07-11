(* ::Package:: *)

BeginPackage["CloudObject`"]

Begin["`Private`"]

(* Options *)
Unprotect[CloudObject];

Off[Options::fnsym];

(* Cast to a throwaway head (optionsCloudObject) so I can set UpValues without danger of colliding
 * with CloudObject patterns. *)
CloudObject /: Verbatim[Options][obj_CloudObject, args___] :=
    Options[optionsCloudObject @@ obj, args];
    
optionsCloudObject /: Verbatim[Options][obj_optionsCloudObject] :=
    Options[obj, Join[Keys@Options[CloudObject], {"Active"}]];

optionsCloudObject /: Verbatim[Options][obj_optionsCloudObject, key_] :=
    Options[obj, {key}];

optionsCloudObject /: Verbatim[Options][obj_optionsCloudObject, keys_List] := Check[
    With[{val = lookupOption[#][CloudObject @@ obj]},
        Switch[val,
            Missing["KeyAbsent", #],
            Message[Options::optnf, #, CloudObject];
            Sequence @@ {},
            
            _,
            Rule[If[Head[#] === Rule, First[#], #], val]
        ]
    ]  & /@ keys,
    
    $Failed,
    {General::cloudnf, General::srverr, General::notperm, General::notauth, General::notparam, General::noicon}
]


CloudObject /: SetOptions[obj_CloudObject, o:OptionsPattern[]] :=
    SetOptions[optionsCloudObject @@ obj, Flatten[{o}]];
    
(* Use All here to conform to Desktop design and have SetOptions return
 * the complete, revised options list (slower). *)
$setOptionsReturnPolicy = "modified values only";

optionsCloudObject /: SetOptions[obj_optionsCloudObject, opts_List] := Module[
    {modified},
    modified = With[{val = setOption[#][CloudObject @@ obj]},
        Switch[val,
            Failure["nomethod", _],
            Message[SetOptions::optnf, First[#], CloudObject];
            Sequence @@ {},
            
            $Failed,
            Sequence @@ {},
            
            _,
            #
        ]
    ] & /@ opts;
    Which[$setOptionsReturnPolicy === All,
        Options[obj],
        
        True,
        modified
    ]
]

(*****************************************************************************)
(* MetaInformation *)

encodeMetaInformationValue[value_] := ToString[value, InputForm]
encodeMetaInformation[values:{(_String->_)...}] :=
    exportToJSON[values /. (key_ -> value_) :> (key -> encodeMetaInformationValue[value]), "Compact" -> True]
encodeMetaInformation[key_String -> value_] := encodeMetaInformation[{key -> value}]
encodeMetaInformation[other_] :=
    (Message[CloudObject::invmeta, other]; $Failed)

decodeMetaInformationValue[value_] := ToExpression[value]
decodeMetaInformation[values_] :=
    importFromJSON[values] /. (key_ -> value_) :> (key -> decodeMetaInformationValue[value])

deleteMetaInformation[obj_CloudObject, key_String] :=
    execute[obj, "DELETE", "files", {"properties", key}]

fetchAllRawMeta[obj_CloudObject] := Module[
    {content},
    content = responseToString @ execute[obj, "GET", "files", {"properties"}];
    If[content === $Failed, Return[$Failed]];
    If[content === "", Return[{}]];
    decodeMetaInformation[content]
]
 
setOption[MetaInformation -> key_String -> value_][obj_CloudObject] :=
    responseCheck[execute[obj, "PUT", "files", {"properties", key}, Body -> encodeMetaInformationValue[value]], SetOptions]

setOption[MetaInformation -> values_][obj_CloudObject] :=
    Module[{encoded, existing},
        encoded = encodeMetaInformation[values];
        If[encoded =!= $Failed,
            (* Delete existing (non-hidden) options first.*)
            existing = Lookup[Options[obj, MetaInformation], MetaInformation];
            If[!MatchQ[existing, Missing["KeyAbsent", _]],
                deleteMetaInformation[obj, #]& /@ Keys[existing];
            ];
            (* Post new options (only if non-empty, since server throws error otherwise). *)
            If[Length[values] > 0,
                responseCheck[execute[obj, "POST", "files", {"properties"}, Body -> encoded], SetOptions]
            ]
        ]
    ]

lookupOption[MetaInformation -> key_][obj_CloudObject] := Replace[
    execute[obj, "GET", "files", {"properties", key}],
    {   (* not using checkError for 404 because its behavior is different *)
        HTTPError[404, ___] :>
            If[FileExistsQ[obj],
                List[Rule[key, Missing["Undefined"]]],
                Message[CloudObject::cloudnf, obj];
                $Failed
            ],
        {type_String, contentBytes:{_Integer ...}} :>
        (* server is returning a JSON object for some reason *)
            decodeMetaInformation[responseToString[{type, contentBytes}]],
        {$Failed, $Failed} :> $Failed,
        other_ :> (checkError[other, CloudObject]; $Failed)
    }
]

lookupOption[MetaInformation][obj_CloudObject] := Module[
    {raw = fetchAllRawMeta[obj], med},
    If[raw === $Failed, Return[$Failed]];
    med = cookMeta[raw, Keys[raw]];
    FilterRules[med, {x_ /; !StringMatchQ[x, StartOfString ~~ "__" ~~ ___ ~~ EndOfString]}]
]

lookupOption[MetaInformation -> keys_List][obj_CloudObject] := Module[
    {raw = fetchAllRawMeta[obj]},
    If[raw === $Failed, Return[$Failed]];
    cookMeta[raw, keys]
] 

cookMeta[rawMeta:{___Rule}, keys_List] :=
    Thread[Rule[keys, Lookup[rawMeta, keys]]] /. Missing["KeyAbsent", _] :> Missing["Undefined"]

(*****************************************************************************)
(* Permissions *)

setOption[Permissions -> permissions_][obj_CloudObject] :=
    Module[{cloud, uuid, mimetype, accessJSON},
        If[invalidPermissionsGroups[permissions], Return[$Failed]];
        mimetype = CloudObjectInformation[obj, "MIMEType"];
        If[mimetype === $Failed, Return[$Failed]];
        accessJSON = toJSON@normalizePermissions[permissions, mimetype, SetOptions];
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        Replace[
            execute[
                cloud, "PUT", {"files", uuid, "permissions"},
                Body -> ToCharacterCode[accessJSON]
            ],
            {
                {_String, contentBytes:{_Integer ...}} :>
                    (Permissions -> permissions),
                other_ :> (checkError[other, CloudObject]; $Failed)
            }
        ]
    ]

lookupOption[Permissions][obj_CloudObject] := 
    Module[{cloud, uuid, permjson, serverPermissions},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        permjson = Replace[
            execute[cloud, "GET", {"files", uuid, "permissions"}],
            {
                {_String, contentBytes:{_Integer ...}} :>
                    FromCharacterCode[contentBytes],
                other_ :> (checkError[other, CloudObject]; Return[$Failed])
            }
        ];
        serverPermissions = importFromJSON[permjson];
        Map[convertFromServerPermissions, serverPermissions]
    ]

(*****************************************************************************)
(* IconRules *)

setOption[IconRules -> iconRules_][obj_CloudObject] :=
    IconRules -> SetCloudIcons[obj, iconRules, Asynchronous->True,
        "DeleteUnmentionedIcons" -> True, (* needed only with SetOptions *)
        "Content" :> CloudGet[obj], (* RuleDelayed b/c Content will only be used for Automatic icons *)
        "Deployment" :> (* RuleDelayed b/c Deployment will only be used for Automatic icons *)
            With[{info = CloudObjectInformation[obj]},
                With[{type = info[[1, "MIMEType"]],
                    permissions = Lookup[Options[obj, Permissions], Permissions]}, (* workaround for the fact that CloudObjectInformation doesn't return permissions in the right format right now *)
                    iconDeploymentType[type, permissions]
                ]
            ]
    ]

lookupOption[IconRules -> key_][obj_CloudObject] :=
    Module[{cloud, uuid, icon},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        icon = Replace[
            execute[cloud, "GET", {"files", uuid, "icon", key}],
            {   (* single out 404 because its error message is different *)
                HTTPError[404, ___] :> (
                    Message[CloudObject::noicon, key, obj];
                    Return[$Failed]
                ),
                {type_String, contentBytes:{_Integer ...}} :> (
                    $lastCloudObjectIconType = type;
                    constructIcon[contentBytes, type]
                ),
                other_ :> (
                    checkError[other, CloudObject];
                    Return[$Failed]
                )
            }
        ];
        If[Head[icon] === Image || Head[icon] === Graphics,
            List@Rule[key, icon],
            Message[CloudObject::noicon, key, obj];
            $Failed
        ]
    ]

constructIcon[contentBytes_List, type_] :=
    With[{contentString = FromCharacterCode[contentBytes]},
        tryImport[contentString, {type, "image/png", "image/jpg", "image/gif"}]
    ]

tryImport[content_, {}] := $Failed

tryImport[content_, {fmt_, rest___}] := Replace[
    Quiet[ImportString[content, fmt]],
    {
        result_Image :> result,
        result_Graphics :> result,
        _ :> tryImport[content, {rest}]
    }
]

lookupOption[IconRules][obj_CloudObject] :=
    Module[{cloud, uuid, icons = {"FileBrowser", "IOS", "Android", "WebPage"}, iconsList},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        iconsList = listIcons[cloud, uuid, CloudObject];
        If[ListQ[iconsList],
            lookupOption[IconRules -> Intersection[iconsList, icons]][obj],
            $Failed
        ]
    ]

lookupOption[IconRules -> keys_List][obj_CloudObject] :=
    Cases[
        Flatten @ Map[
            Quiet[lookupOption[IconRules -> #][obj], {CloudObject::noicon}] &,
            keys
        ],
        Verbatim[Rule][_, _Image]
    ];


(*****************************************************************************)
(* "Active" *)

lookupOption["Active"][obj_CloudObject] := 
    Module[{cloud, uuid, response},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        response = Replace[
            execute[cloud, "GET", {"files", uuid, "active"}],
            {
                {_String, contentBytes:{_Integer ...}} :> FromCharacterCode[contentBytes],
                other_ :> (checkError[other, CloudObject]; Return[$Failed])
            }
        ];
        StringMatchQ[StringTrim[response], "true" | "yes" | "t",
            IgnoreCase->True]
    ]

setOption["Active" -> activeQ_?BooleanQ][obj_CloudObject] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        Replace[
            execute[
                cloud, "PUT", {"files", uuid, "active"},
                Body -> activeQBody[activeQ]
            ],
            {
                {_String, contentBytes:{_Integer ...}} :> {"Active" -> activeQ},
                other_ :> (checkError[other, CloudObject]; $Failed)
            }
        ]
    ]

activeQBody[True] = ToCharacterCode["true"]
activeQBody[False] = ToCharacterCode["false"]

(*****************************************************************************)
(* SharingList *)

setOption[SharingList -> {}][obj_CloudObject] :=
    If[ deleteAllSharees[obj, SetOptions] === $Failed,
        $Failed,
        {SharingList -> {}}
    ]

setOption[SharingList -> users:{Alternatives[_String, _PermissionsGroup, _Rule]..}][obj_CloudObject] :=
    Module[{cloud, uuid, settingSharees},
      Catch[    
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        settingSharees = setSharees[cloud, uuid, Replace[users, Rule[x_, y_] :> x, {1}]];
        If[settingSharees === $Failed, 
        	$Failed,
        	{SharingList -> users}
        ]        
      ,
      $normalizeUserTag]          
    ]

lookupOption[SharingList][obj_CloudObject] := 
    Module[{cloud, uuid, values },
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!UUIDQ[uuid], Return[checkUUID[uuid, obj]]];
        values = getSharees[cloud, uuid, {"email", "displayName", "uuid"}];
        If[values === {}, 
            {},
            Map[If[Head[#[[1]]] =!= Missing, #[[1]], convertFromServerPermissionsGroup[#[[2]], #[[3]]]] &, values]]
        
    ]
    
(*****************************************************************************)
(* SourceLink *)

setOption[SourceLink -> value:(_CloudObject | None)][obj_CloudObject] := 
    SetOptions[obj, MetaInformation -> "__SourceLink"-> normalizeSourceLink[value]]
    
lookupOption[SourceLink][obj_CloudObject] := 
    Module[{meta, src},
    	meta = Options[obj, MetaInformation -> "__SourceLink"];
    	If[meta === $Failed,
    		$Failed,
    		src = Lookup[Lookup[meta, MetaInformation],"__SourceLink"];
    		If[StringQ[src], CloudObject[src], None]
    	]
    ]  
    
(*****************************************************************************)
(* AutoCopy *)

setOption[AutoCopy -> autocopyQ_?BooleanQ][obj_CloudObject] := 
    SetOptions[obj, MetaInformation -> "__AutoCopy" -> autocopyQ]
    
lookupOption[AutoCopy][obj_CloudObject] := 
    Module[{meta, autocopyQ},
    	meta = Options[obj, MetaInformation -> "__AutoCopy"];
    	If[meta === $Failed,
    		$Failed,
    		autocopyQ = Lookup[Lookup[meta, MetaInformation], "__AutoCopy"];
    		TrueQ[autocopyQ]
    	]
    ]      
        
(*****************************************************************************)
(* fallthroughs *)
lookupOption[key_][obj:CloudObject[args__, o:OptionsPattern[]]] := 
    Lookup[Join[Flatten[{o}], Options[CloudObject]], key]

setOption[key_][obj_CloudObject] := Failure["nomethod", <||>]

On[Options::fnsym];

Protect[CloudObject];

End[]

EndPackage[]
