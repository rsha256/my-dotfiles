(* ::Package:: *)

BeginPackage["CloudObject`"]

System`Permissions;
System`SetPermissions;
System`ClearPermissions;
System`CloudPublish;
System`ApplicationIdentificationKey;
Hold[System`$Permissions];

Begin["`Private`"]

$Permissions = "Private";
$normalizeUserTag = "normalizeUserTag"

permissionSpecs = "Read" | "Write" | "Execute" | "Edit" | "Save" | "EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" | "IncrementalEvaluate" | "Interact";

normalizePermissionsSpec["r", type_, _] = {"Read"};
normalizePermissionsSpec["w", type_, _] = {"Write"};
normalizePermissionsSpec["x", type_, _] = {"Execute"};

normalizePermissionsSpec["Edit", type_, _] = {"CellEdit", "CellCreate", "CellDelete"};
normalizePermissionsSpec["Use", _, _] = {"Execute"};
normalizePermissionsSpec["Modify", _, _] = {"Write"};

normalizePermissionsSpec[list_List, type_, head_] :=
    Map[normalizePermissionsSpec[#, type, head]&, list]
normalizePermissionsSpec[spec_String?(StringMatchQ[#, Characters["rwx"]..]&), type_, head_] :=
    Map[normalizePermissionsSpec[#, type, head]&, Characters[spec]]

normalizePermissionsSpec[Automatic, "application/mathematica", _] := {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression", _] := {"Read"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook", _] := {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook.element", _] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, mime_String, _] /; StringMatchQ[mime, "application/vnd.wolfram.expression." ~~ __] := {"Execute"}
normalizePermissionsSpec[Automatic, _, _] := {"Read"};

normalizePermissionsSpec[All, type_, _] = Apply[List, permissionSpecs];
normalizePermissionsSpec[None, type_, _] = {};

normalizePermissionsSpec[spec:permissionSpecs, type_, _] = {spec};

normalizePermissionsSpec[spec_, type_, head_] := (Message[head::invperm, spec]; {})

normalizeUserSpec[user_String] :=
    If[StringFreeQ[user, ":"] && FreeQ[{"All", "Authenticated", "Owner"}, user],
        "user:" <> user,
        user
    ]

isOwner[user_] := user === $CloudUserID || StringQ[$CloudUserUUID] && (user === ("user-" <> $CloudUserUUID))

normalizePermissions["Public", type_, head_] :=
    {"All" -> Flatten[normalizePermissionsSpec[Automatic, type, head]], "Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions["Private", type_, head_] :=
    {"Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions[list_List, type_, head_] :=
    Join @@ Map[normalizePermissions[#, type, head]&, list]
normalizePermissions[user_String -> spec_, type_, head_] :=
    {normalizeUserSpec[user] -> Flatten[normalizePermissionsSpec[spec, type, head]]}
normalizePermissions[user_String -> _, _, head_] := (Message[head::selfperm, user]; {}) /; isOwner[user]
normalizePermissions[All -> spec_, type_, head_] := normalizePermissions["All" -> spec, type, head]
normalizePermissions[spec_String, type_, head_] := normalizePermissions[{"All" -> spec}, type, head]
normalizePermissions[Automatic, type_, head_] := normalizePermissions[$Permissions, type, head]
normalizePermissions[spec_, type_, head_] := (Message[head::invperm, spec]; {})
normalizePermissions[users_List -> spec_, type_, head_] := Join @@ Map[normalizePermissions[# -> spec, type, head]&, users]

groupIdentifier[group_PermissionsGroup] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[CloudObject @@ group];
        (* TODO: What should happen when the group is in a different cloud? *)
        If[uuid === None, Return[$Failed]];
        "wolfram:" <> uuid
    ]
    
keyIdentifier[PermissionsKey[key_]] :=
    If[validatePermissionsKey[key],
    	"key:" <> key,
    	Message[PermissionsKey::invkey, key]; $Failed
    ]

sakIdentifier[sak:Alternatives[_SecuredAuthenticationKey, _ApplicationIdentificationKey]]:=
	With[{key = consumerKey[sak]},
		If[StringQ[key],
			"sak:" <> key,
			Message[SecuredAuthenticationKey::invcsk, sak]; $Failed
		]
	]
	
consumerKey[(SecuredAuthenticationKey | ApplicationIdentificationKey)[properties_Association]] := 
	properties["ConsumerKey"]

normalizePermissions[group_PermissionsGroup -> spec_, type_, head_] :=
    Replace[
    	groupIdentifier[group], 
        {
    		$Failed :> (Message[head::invperm, group -> spec]; {}),
    		id_ :> normalizePermissions[id -> spec, type, head]
    	}
    ]
    
normalizePermissions[key_PermissionsKey, type_, head_] := normalizePermissions[key -> Automatic, type, head]   
normalizePermissions[key_PermissionsKey -> spec_, type_, head_] :=
    Replace[
    	keyIdentifier[key], 
        {
    		$Failed :> (Message[head::invperm, key -> spec];{}),
    		id_ :> normalizePermissions[id -> spec, type, head]
    	}
    ]

normalizePermissions[sak:Alternatives[_SecuredAuthenticationKey, _ApplicationIdentificationKey] -> spec_, type_, head_] :=
    Replace[
    	sakIdentifier[sak], 
        {
    		$Failed :> {},
    		id_ :> normalizePermissions[id -> spec, type, head]
    	}
    ]

escapeAndNormalizePermissions = Composition[toJSON, normalizePermissions]

fromServerPermissions["r"] := "Read"
fromServerPermissions["w"] := "Write"
fromServerPermissions["x"] := "Execute"
fromServerPermissions[p:("Read" | "Write" | "Execute" | "Edit" | "Save" |
    "EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" |
    "IncrementalEvaluate" | "Interact")] := p

fromServerPermissions[permjson_] := Replace[
    importFromJSON[permjson],
    {
        serverPermissions_List :>
            Map[convertFromServerPermissions, serverPermissions],
        other_ :> ($lastServerPermissionsJSON = permjson; $Failed)
    }
]

fromServerUserClass[class_] :=
    Which[StringMatchQ[class, "wolfram:" ~~ __],
        PermissionsGroup[class], (* TODO: denormalize to the group's name, take into account the cloud base *)
        StringMatchQ[class, "key:" ~~ __],
        PermissionsKey[StringDrop[class, 4]],
        StringMatchQ[class, "sak:" ~~ __],
        ApplicationIdentificationKey[<|"ConsumerKey" -> StringDrop[class, 4]|>],
        True,
        StringReplace[class, StartOfString ~~ "user:" -> ""]
    ]   

convertFromServerPermissions[class_ -> perms_String] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, Characters[perms]], _String]
convertFromServerPermissions[class_ -> perms_List] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, perms], _String]
    

normalizeUserSpecification[All, head_] := "All"
normalizeUserSpecification[usr_?(MemberQ[{"All", "Authenticated", "Owner"}, #] &), head_] := usr

normalizeUserSpecification[group_PermissionsGroup, head_] := 
    validateUserSpecification[group, head][[2]]
    
normalizeUserSpecification[key_PermissionsKey, head_] := key   
    
normalizeUserSpecification[usr_String, head_] :=
    validateUserSpecification[usr, head][[2]] 
        
normalizeUserSpecification[usr_, head_] :=    
    (Message[head::invusr, usr];Throw[$Failed, $normalizeUserTag])    
    
validateUserSpecification[All, head_] := {All, "All"}
validateUserSpecification[usr_?(MemberQ[{All, "All", "Authenticated", "Owner"}, #] &), head_] := {usr/. "All" -> All, usr}    
    
validateUserSpecification[group_PermissionsGroup, head_] :=
    Module[{cloud, uuid},    
        {cloud, uuid} = getCloudAndUUID[CloudObject @@ group];
        If[uuid === None, Message[head::invusr, group]; Throw[$Failed, $normalizeUserTag], {group, uuid}] 
    ]
    
validateUserSpecification[key_PermissionsKey, head_] :=
	 If[validatePermissionsKey[key], 
	 	{key, key},
	 	Message[head::invkey, key]; Throw[$Failed, $normalizeUserTag]
	 ]

validateUserSpecification[key:Alternatives[_SecuredAuthenticationKey, _ApplicationIdentificationKey], head_] :=
	 If[StringQ[consumerKey[key]], 
	 	{key, key},
	 	Message[head::invcsk, key]; Throw[$Failed, $normalizeUserTag]
	 ]
    
validateUserSpecification[usr_String, head_] :=
    Module[{json, data, userData},
        If[Not[TrueQ[authenticatedQ[]]],
           With[{res=CloudConnect[]}, 
               If[UnsameQ[res, $CloudUserID], Message[head::notauth]; Throw[$Failed, $normalizeUserTag]]
           ]
        ];
        json = Replace[
            execute[$CloudBase, "GET", {"users"}, Parameters -> {"id" ->  usr}],
            {
                {_, bytes_List} :> FromCharacterCode[bytes],
                HTTPError[403, ___] :> (Message[head::noaccess, usr];
                                        Throw[$Failed, $normalizeUserTag]),(*not allowed*)
                HTTPError[404, ___] :> (Message[head::invusr, usr];
                                        Throw[$Failed, $normalizeUserTag]),(*not found*)
                other_ :> (checkError[other, head];
                           Throw[$Failed, $normalizeUserTag])
             }
        ];
        data = importFromJSON[json];
        userData = validatedUserData[data];
        If[UUIDQ[userData],
        	{usr, userData},
        	(* Else *)
        	Message[head::srverr];
            Throw[$Failed, $normalizeUserTag]
        ]
    ]
    
validateUserSpecification[usr_, head_] :=
    (Message[head::invusr, usr];Throw[$Failed, $normalizeUserTag])
    
validatedUserData[data_List] :=
	With[{info = Quiet[Select[data, KeyExistsQ[#, "uuid"] &]]},
		If[MatchQ[data, {{__Rule}..}] && SameQ[Length[info], 1],
			Lookup[info[[1]], "uuid"],
			(* Else *)
			None
		]
	]    

(*****************************************************************************)
(* general function for modifying permissions *)
$userClasses = Alternatives[All, _String, _PermissionsGroup, _PermissionsKey, _SecuredAuthenticationKey, _ApplicationIdentificationKey];

modifyPermissions["Private", head_] :=
    modifyPermissions[$EvaluationCloudObject, "Private", head]


(*This is for special case SetPermissions["Private"].*)
modifyPermissions[obj_CloudObject, "Private", head_] :=
    Module[{opts, persInit, persNew},
        opts = Options[obj, Permissions];
        If[opts == {},
            {},
            persInit = Lookup[opts, Permissions];
            persNew = Select[persInit, #[[1]]==="Owner" &];
            Lookup[SetOptions[obj, Permissions->persNew], Permissions]
        ]
    ]
        
modifyPermissions[obj_CloudObject, "Public", head_] :=
    modifyPermissions[obj, {All-> Automatic}, head]

modifyPermissions[pers_Rule, head_] :=
    modifyPermissions[{pers}, head]

modifyPermissions[pers:{Rule[_, _] ..}, head_] :=
    modifyPermissions[$EvaluationCloudObject, pers, head]
    
modifyPermissions[obj_CloudObject, class_PermissionsKey, head_] :=   
	 modifyPermissions[obj, class -> Automatic, head]
    
modifyPermissions[obj_CloudObject, pers_Rule, head_] :=
    modifyPermissions[obj, {pers}, head]
    
modifyPermissions[obj_CloudObject, pers:{Rule[_, _] ..}, head_] :=
    Module[{persList, persExisting, persCombined, persNew, modifiedPers, notAllValid},
        Catch[
            persList = processPerms[pers];
            notAllValid = invalidPermissionsGroups[persList];
            If[notAllValid, Return[$Failed]];
            persExisting = Options[obj, Permissions];
            If[persExisting === $Failed, Return[$Failed]];
            (* Combine existing permissions with new ones.
                The new permissions come first so that they are prioritized by DeleteDuplicates. *)
            persCombined = Join[persList, Lookup[persExisting, Permissions, {}]];
			persNew = normalizePerms[persCombined, head];
            modifiedPers = SetOptions[obj, Permissions->persNew];
            If[modifiedPers === $Failed, Return[$Failed]];
            If[modifiedPers == {},
                {},
                Lookup[modifiedPers, Permissions]
            ]
        , $normalizeUserTag]
    ]  
    
modifyPermissions[obj_CloudObject, {}, head_] :=
	    Lookup[Options[obj, Permissions], Permissions]
	    
modifyPermissions[obj_CloudObject, pers_, head_] :=
	(Message[head::invperm, pers]; $Failed)
		    	
processPerms[pers:{Rule[_, _] ..}] :=
    Module[{mappedPerms, res},
    	mappedPerms = Flatten[Map[mapPermSettings, pers]];
        res = Normal[Merge[mappedPerms, Flatten]];
        Replace[res, Rule[class_, {per : Alternatives[All, Automatic]}] :> Rule[class, per], {1}]
    ]
        
mapPermSettings[Rule[users_List, per_]] := Table[user -> per, {user, users}]
mapPermSettings[perm_] := perm

normalizePerms[perms:{Rule[_, _] ..}, head_] :=
	Module[{normalizedPair},
		normalizedPair = Replace[perms, (usr_ -> per_) :> (validateUserSpecification[usr,head] -> per), {1}];
		If[Length[normalizedPair] > 1,
 				normalizedPair = DeleteDuplicates[normalizedPair, (#1[[1, 2]] === #2[[1, 2]]) &]];
		Replace[normalizedPair, ({usr_, uuid_} -> per_) :> (usr -> per), {1}]
	]

(*****************************************************************************)
(* SetPermissions *)    
SetPermissions[pers_] :=
    SetPermissions[$EvaluationCloudObject, pers] 
    
SetPermissions[uri_String, pers_] := SetPermissions[CloudObject[uri], pers]    
    
SetPermissions[obj_CloudObject, pers_] := 
    modifyPermissions[obj, pers, SetPermissions]
    
SetPermissions[obj_, pers_]:=
    (Message[SetPermissions::invcloudobj, obj]; Return[$Failed])    
    
SetPermissions[args___] :=
    (ArgumentCountQ[SetPermissions, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False)        

(*****************************************************************************)
(* ClearPermissions *) 

ClearPermissions[class_] := ClearPermissions[$EvaluationCloudObject, class]

ClearPermissions[uri_String, class_]:= ClearPermissions[CloudObject[uri], class]
    
ClearPermissions[obj_CloudObject, class:$userClasses] :=
    modifyPermissions[obj, class->{}, ClearPermissions]
        
ClearPermissions[obj_CloudObject, class:{$userClasses ..}] :=
    modifyPermissions[obj, Replace[class, x_ :> (x -> {}), {1}], ClearPermissions]  
    
ClearPermissions[obj_CloudObject, {}] :=
	modifyPermissions[obj, {}, ClearPermissions]
	          

ClearPermissions[obj_CloudObject, class_] := 
    (Message[ClearPermissions::invusr, class]; $Failed)
    
ClearPermissions[obj_, class_]:=
    (Message[ClearPermissions::invcloudobj, obj]; Return[$Failed])    
    
ClearPermissions[args___] :=
    (ArgumentCountQ[ClearPermissions, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False)                

End[]

EndPackage[]
