(* ::Package:: *)

BeginPackage["CloudObject`"]

System`CloudObjectInformation;
System`CloudObjectInformationData;
System`CloudObjects;

CloudObject`CloudObjectUUIDForm;


Begin["`Private`"]

(* CloudObjects *)

Options[CloudObjects] = {"Directory" -> Automatic, "Type" -> All, CloudBase -> Automatic, CloudObjectNameFormat -> Automatic}

queryTypeValue["CloudEvaluation"] := expressionMimeType["CloudEvaluation"];
queryTypeValue["Expression"|Expression] := expressionMimeType[Expression];
queryTypeValue["Notebook"|Notebook] := expressionMimeType[Notebook];
queryTypeValue["ExternalBundle"|ExternalBundle] := expressionMimeType[ExternalBundle];
queryTypeValue["Directory"|Directory] := expressionMimeType[Directory];
queryTypeValue[type_String] := formatToMimeType[type];
queryTypeValue[symbol_Symbol] := expressionMimeType[symbol];
queryTypeValue[All] = All;
queryTypeValue[Verbatim[Alternatives][types___]] := queryTypeValue[{types}];
queryTypeValue[list_List] := If[MemberQ[list, All], All, StringRiffle[Map[queryTypeValue, list], ","]];
queryTypeValue[_] = $Failed;

iCloudObjects[cloud_String, path_, opts:OptionsPattern[CloudObjects]] :=
    Module[{query = {}, typevalue, type, nameFormat},
        typevalue = OptionValue[CloudObjects, {opts}, "Type"];
        type = queryTypeValue[typevalue];
        If[type === $Failed,
            Message[CloudObjects::invtype, typevalue];
            type = All;
        ];
        nameFormat = Replace[OptionValue[CloudObjectNameFormat], Automatic -> $CloudObjectNameFormat];
        If[path === "", nameFormat = "UUID"];
        If[type =!= All, AppendTo[query, "mimeType" -> type]];
        If[path =!= All, AppendTo[query, "path" -> path]];
        If[nameFormat =!= "UUID", AppendTo[query, "fields" -> "path,owner,uuid"]];
        Replace[responseToString[execute[cloud, "GET", {"files"}, Parameters -> query], CloudObjects],
        		result_String :> fileInfoToCloudObjects[cloud, result, nameFormat]
        	]
    ]
    
CloudObjects[All, opts:OptionsPattern[]] := 
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		iCloudObjects[$CloudBase, All, optsNoBase[{opts}]]]
		
CloudObjects[None, opts:OptionsPattern[]] := 
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		iCloudObjects[$CloudBase, "", optsNoBase[{opts}]]]
		
CloudObjects[obj_CloudObject, opts:OptionsPattern[]] :=
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
    	Module[{cloud, path, name},
        	{cloud, path} = getCloudAndPathList[obj];
        	name = StringJoin[Riffle[Join[path, {"*"}], "/"]];
        	iCloudObjects[cloud, name, optsNoBase[{opts}]]
    	]
	]

CloudObjects[Automatic, opts:OptionsPattern[]] := 
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]}, CloudObjects[CloudDirectory[], optsNoBase[{opts}]]]
	
CloudObjects[dir_String, opts:OptionsPattern[]] := 
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]}, CloudObjects[CloudObject[dir], optsNoBase[{opts}]]]
	
CloudObjects[URL[url_String], opts:OptionsPattern[]] := CloudObjects[url, opts]

(* If no directory is given as positional argument, take the option value (with default Automatic). *)
CloudObjects[opts:OptionsPattern[]] := CloudObjects[OptionValue["Directory"], opts]

(* Expand an Association out into a list of rules, which get treated as options. *)
CloudObjects[before___, assoc_Association, after___] := CloudObjects[before, Normal[assoc], after]

CloudObjects[dir_, type:(_String|_Symbol|_Alternatives), opts:OptionsPattern[]] := CloudObjects[dir, "Type" -> type, opts]

optsNoBase[opts_List] := Sequence @@ FilterRules[{opts}, Except[CloudBase]]	
	
(* List objects *)
CloudObjectsByType[contentType_String] :=
    Module[{response, uuids},
        response = responseToString @ execute[$CloudBase, "GET", {"files"},
            Parameters->{"mimeType" -> contentType}];
        If[!StringQ[response], Return[$Failed]];
        uuids = Map[FileNameTake[#, -1]&, StringSplit[response]];
        Map[cloudObjectFromUUID, uuids]
    ]
    
fileInfoToCloudObjects[cloud_, uuids_String, "UUID"] := 
	With[{uuidListing = Map[StringDrop[#, 7]&, StringSplit[uuids]]},
		If[ListQ[uuidListing] && Length[uuidListing] > 0,			
			Map[cloudObjectFromUUID[cloud,#]&, uuidListing],
			{}]
	] 
  
fileInfoToCloudObjects[cloud_, info_String, nameFormat_] :=
	With[{result = importFromJSON[info]},
		If[ListQ[result],
			Map[cloudObjectFromPathInfo[cloud, #, nameFormat]&, result],
			{}]
	]
	
        
(* CloudObjectInformation *)

CloudObjectInformation[obj_CloudObject] := cloudObjectInformation[obj]

CloudObjectInformation[obj_CloudObject, "UUID"] :=
    With[{result = Quiet[getCloudAndUUID[obj]]},
        If[MatchQ[result, {_String, _?CloudObject`UUIDQ}],
            Last[result],
        (* Else *)
            Message[CloudObjectInformation::cloudnf, obj];
            $Failed
        ]
    ]

CloudObjectInformation[obj_CloudObject, property_String] :=
    cloudObjectInformation[obj, CloudObjectInformation, "Elements" -> property]

CloudObjectInformation[obj_CloudObject, properties:{_String ..}] :=
    cloudObjectInformation[obj, CloudObjectInformation, "Elements" -> properties]

CloudObjectInformation[{}] := {}

CloudObjectInformation[objects:{_CloudObject ..}] := 
    cloudObjectInformation[objects, CloudObjectInformation]

CloudObjectInformation[objects:{_CloudObject ..}, property_String] := 
    cloudObjectInformation[objects, CloudObjectInformation, "Elements" -> property]

CloudObjectInformation[objects:{_CloudObject ..}, properties:{_String ..}] :=
    cloudObjectInformation[objects, CloudObjectInformation, "Elements" -> properties]

CloudObjectInformation[type_String, property_String] := 
    cloudObjectInformation[type, CloudObjectInformation, "Elements" -> property]

CloudObjectInformation[type_String, properties:{_String ..}] := 
    cloudObjectInformation[type, CloudObjectInformation, "Elements" -> properties]

Options[cloudObjectInformation] = {"Elements" -> Automatic}

cloudObjectInformation[obj_CloudObject, msghd_:CloudObjectInformation, opts:OptionsPattern[]] :=
    Module[{cloud, uuid, json, allinfo, files},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]), 
            Return[$Failed]
        ];

        json = Replace[
            execute[cloud, "GET", {"files", uuid, "info"}],
            {
                HTTPError[404, ___] :> (Message[msghd::cloudnf, obj]; Return[$Failed]),
                {_String, content_List} :>
                    ($lastInfoJSON = FromCharacterCode[content]),
                other_ :> (Message[msghd::srverr]; Return[$Failed])
            }
        ];

        allinfo = importFromJSON[json];
        If[!ListQ[allinfo],
            Message[msghd::srverr];
            Return[$Failed]
        ];

        files = Lookup[allinfo,
            If[KeyExistsQ[allinfo, "files"], "files", "directoryListing"]];
        If[files === {},
            Message[msghd::srverr]; (* internal error -- info about directories is broken *)
            Return[$Failed]
        ];
        
        objectInfo[First[files], "Elements" -> OptionValue["Elements"]]
    ]

cloudObjectInformation[objects:{_CloudObject ..}, msghd_:CloudObjectInformation, opts:OptionsPattern[]] :=
    Module[{args, bad, uuids, cloud, elements, fields, json, files},

        args = Map[Prepend[getCloudAndUUID[#], #]&, objects];

        (* test for objects that cannot be resolved *)
        bad = SelectFirst[args, ! MatchQ[#, {_, _String, _?CloudObject`UUIDQ}] &];

        If[Head[bad] =!= Missing,
            Message[msghd::cloudnf];
            Return[$Failed]
        ];
        
        uuids = Map[Last, args];
        cloud = args[[1, 2]];
        elements = OptionValue["Elements"];
        fields = resolveInfoFields[elements];
        
        json = Replace[
            execute[
                cloud, "GET", {"files"}, 
                Parameters -> {"fields" -> commaSeparated[fields], (* indicate v2 of the API, to return JSON *)
                "uuid" -> commaSeparated[uuids]}
            ],
            {
                HTTPError[404, ___] :> (
                    Message[msghd::cloudnf, objects]; 
                    Return[$Failed]),
                {_String, content_List} :>
                    ($lastInfoJSON = FromCharacterCode[content]),
                other_ :> ($lastInfoResult = other; Message[msghd::srverr]; 
                    Return[$Failed])
            }
        ];

        files = importFromJSON[json];
        If[!ListQ[files],
            Message[msghd::srverr];
            Return[$Failed]
        ];

        Map[objectInfo[#, "Elements" -> elements]&, files]
    ]

cloudObjectInformation[type_String, msghd_:CloudObjectInformation, opts:OptionsPattern[]] := 
    Module[{elements, fields, json, info},

        elements = OptionValue["Elements"];
        fields = resolveInfoFields[elements];
        
        json = Replace[
            execute[
                $CloudBase, "GET", {"files"}, 
                Parameters -> {"fields" -> commaSeparated[fields], (* indicate v2 of the API, to return JSON *)
                "mimeType" -> formatToMimeType[type]}
               ],
            {
                HTTPError[404, ___] :> (
                    Message[msghd::cloudnf, objects]; 
                    Return[$Failed]),
                {_String, content_List} :>
                    ($lastInfoJSON = FromCharacterCode[content]),
                other_ :> (
                    $lastInfoResult = other;
                    Message[msghd::srverr]; 
                    Return[$Failed])
            }
        ];

        info = importFromJSON[json];
        If[!ListQ[info],
            Message[msghd::srverr];
            Return[$Failed]
        ];

        Map[objectInfo[#, "Elements" -> elements]&, info]
    ]

commaSeparated[elts_List] := StringJoin[Riffle[elts, ","]]

resolveInfoFields[Automatic] := {"all"}

resolveInfoFields[field_String] := resolveInfoFields[{field}]

resolveInfoFields[fields_List] := 
    Map[Lookup[$jsonFields, #, handleUnknownProperty[#]]&, fields]

handleUnknownProperty[propery_String] := 
(
    Message[CloudObjectInformation::noprop, property];
    $Failed
)

Options[objectInfo] = {"Elements" -> Automatic}

objectInfo[info_List, opts:OptionsPattern[]] := objectInfo[Association[info], opts]

objectInfo[info_Association, OptionsPattern[]] := 
    Module[{elements = OptionValue["Elements"], mimetype = Lookup[info, "mimeType", None], 
        displayName, infoData = <||>},
        displayName = Lookup[info, "displayName", info["name"]];
        
        Do[
            infoData[elt] = 
            <|
                "UUID" -> info["uuid"],
                "Path" -> info["path"] /. {Null -> None},
                "Name" -> displayName,
                "DisplayName" -> displayName,
                "OwnerWolframUUID" -> info["ownerUUID"],
                "OwnerWolframID" :> Lookup[info["owner"], "email", Missing["Unavailable"]],
                "MIMEType" -> mimetype,
                "MimeType" -> mimetype,
                "FileType" ->
                    If[mimetype === "inode/directory" || bundleMimeTypeQ[mimetype],
                        Directory,
                        File
                    ],
                "FileByteCount" :> FromDigits[info["fileSize"]],
                "Created" :> DateObject[info["created"]],
                "LastAccessed" :> DateObject[info["lastAccessed"]],
                "LastModified" :> DateObject[info["lastModified"]],
                "FileHashMD5" :> 
                	With[{hash = info["fileHashMD5"]}, Replace[hash, {x_String :> FromDigits[x, 16], Null -> None}]],
                "Permissions" :> fromServerPermissions[info["filePermission"]],
                "Active" -> info["active"]
            |>[elt],
            {elt, Switch[elements,
                Automatic, Keys[$jsonFields],
                _String, {elements},
                _, elements]
            }
        ];
 
        Switch[elements,
            Automatic, System`CloudObjectInformationData[infoData],
            _String, First[Values[infoData]],
            _List, infoData
        ]
    ]

(* $jsonFields is used to assist in field selection.
 For each CloudObjectInformation property on the left-hand side, 
 it says what json field on the right-hand side we should ask from the
 server from which to derive that property. This allows us to efficiently
 request only the info fields from the server that are needed to return
 the properties requested in WL.
 *)
$jsonFields = <|
    "UUID" -> "uuid",
    "Path" -> "path",
    "Name" -> "displayName",
    "DisplayName" -> "displayName",
    "OwnerWolframUUID" -> "ownerUUID",
    "OwnerWolframID" -> "owner",
    "MIMEType" -> "mimeType",
    "MimeType" -> "mimeType",
    "FileType" -> "mimeType",
    "FileByteCount" -> "fileSize",
    "FileHashMD5" -> "fileHashMD5",
    "Created" -> "created",
    "LastAccessed" -> "lastAccessed",
    "LastModified" -> "lastModified",
    "Permissions" -> "filePermission",
    "Active" -> "active"
|>

(* TODO: This should probably be exposed through CloudObjectInformation, and maybe as a sub value of CloudObject.
 Maybe also as Normal[obj]. *)
CloudObjectUUIDForm[obj : CloudObject[url_, opts___]] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!StringQ[cloud], Return[$Failed]];
        CloudObject[URLBuild[{cloud, $cloudObjectRootPath, uuid}], opts]
    ]

(* work around URLBuild bug https://bugs.wolfram.com/show?number=340917, that adds / without checking for / in arguments *)
$cloudObjectRootPath = StringReplace[CloudObject`Private`$CloudObjectsRoot, StartOfString ~~ "/" -> ""]

CloudObjectUUIDForm[group_PermissionsGroup] := PermissionsGroup @@ CloudObjectUUIDForm[CloudObject @@ group]

End[]

EndPackage[]
