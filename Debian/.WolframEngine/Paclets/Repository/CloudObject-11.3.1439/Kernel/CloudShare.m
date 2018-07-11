(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["CloudObject`"]
(* Exported symbols added here with SymbolName::usage *)  

System`CloudShare;
System`SharingList;
System`CloudUnshare;

Begin["`Private`"] (* Begin Private Context *) 

Unprotect[System`CloudShare];

shareesSpecs = Alternatives[_String, _PermissionsGroup];
shareesSpecsForPerm = Alternatives[_String, _PermissionsGroup, _List];

CloudShare[user:shareesSpecs] := CloudShare[{user}];

CloudShare[users:{shareesSpecs..}] := cloudShare[users]
    
CloudShare[pers:Rule[shareesSpecsForPerm, _]] := CloudShare[{pers}]  

CloudShare[pers:{Rule[shareesSpecsForPerm, _]..}] := cloudShare[pers]
    
cloudShare[arg_List] :=
	 If[$CloudEvaluation,
        CloudShare[$EvaluationCloudObject, arg],
        CloudShare[EvaluationNotebook[], arg]
    ]   

CloudShare[arg_] := (Message[CloudShare::invusr, arg]; $Failed)

CloudShare[obj_CloudObject, user:shareesSpecs] := CloudShare[obj, {user}]
    
CloudShare[obj_CloudObject, users:{shareesSpecs..}] :=
    CloudShare[obj, Map[# -> Automatic &, users]]
    
CloudShare[obj_CloudObject, per:Rule[shareesSpecsForPerm, _]] := CloudShare[obj, {per}]    
    
CloudShare[obj_CloudObject, pers:{Rule[shareesSpecsForPerm, _]..}] :=
    Module[ {perms, cloud, uuid, sharees, addingSharees, persOld, persList, persNew, notAllValid},
     Catch[
     	perms = processPerms[pers];
        notAllValid = invalidPermissionsGroups[perms];
        If[notAllValid, Return[$Failed]];
        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        If[ !(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed]
        ];
        sharees = Keys[perms]; 
        addingSharees = addSharees[cloud, uuid, sharees];
        If[addingSharees===$Failed, Return[$Failed]];
        persOld = Options[obj, Permissions];
        If[ !MatchQ[persOld, {_Rule...}], Return[$Failed]];
        persList = Lookup[persOld, Permissions];
        persNew = addPermissions[persList, perms];
        SetPermissions[obj, persNew ];        
        obj
     , 
     $normalizeUserTag]    
    ] 
    
CloudShare[obj_CloudObject, arg_] :=  
	(Message[CloudShare::invusr, arg]; $Failed)     
    
CloudShare[nb : notebookExprPattern, users_] :=
    Module[{obj},
        (* Set IconRules->None until the bug is fixed where this closes the corresponding notebook. *)
        obj = CloudDeploy[nb, IconRules->None];
        If[obj === $Failed, Return[$Failed]];
        CloudShare[obj, users]
    ]
    
CloudShare[obj_, users_] :=
    (Message[CloudShare::invcloudobj, obj]; $Failed)        
    
CloudShare[args___] := 
    (ArgumentCountQ[CloudShare, Length[DeleteCases[{args}, _Rule, Infinity]], 1, 2]; Null /; False) 
    
CloudUnshare[] :=
	If[$CloudEvaluation,
        CloudUnshare[$EvaluationCloudObject]
        ,
        Message[CloudUnshare::cloudnf];
        $Failed
    ]
    
CloudUnshare[sharees:shareesSpecs] := CloudUnshare[{sharees}]

CloudUnshare[sharees:{shareesSpecs..}] := 
	If[$CloudEvaluation,
        CloudUnshare[$EvaluationCloudObject, sharees]
        ,
        Message[CloudUnshare::cloudnf];
        $Failed
    ]
    
CloudUnshare[obj_CloudObject] :=
    Module[ {cloud, uuid},
        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        If[ StringQ[cloud] && UUIDQ[uuid],
            deleteAllSharees[cloud, uuid, CloudUnshare];
            SetOptions[obj, Permissions -> "Private"];
            ,
            Message[CloudUnshare::cloudnf, obj];
            False
        ]
    ]

CloudUnshare[obj_CloudObject, sharees:shareesSpecs] := CloudUnshare[obj, {sharees}]

CloudUnshare[obj_CloudObject, sharees:{shareesSpecs..}] :=
    Module[ {cloud, uuid, persOld, persNew},
    	{cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        If[ StringQ[cloud] && UUIDQ[uuid],
            deleteSubsetSharees[cloud, uuid, sharees, CloudUnshare];
            persOld = Lookup[Options[obj, Permissions], Permissions];
            persNew = Select[persOld, !MemberQ[sharees, #[[1]]] &];
            SetOptions[obj, Permissions -> persNew];
            ,
            $Failed
        ]
    ]
	
CloudUnshare[obj_CloudObject, arg_] :=  
	(Message[CloudShare::invusr, arg]; $Failed)	

CloudUnshare[args___] := 
    (ArgumentCountQ[CloudUnshare, Length[DeleteCases[{args}, _Rule, Infinity]], 0, 2]; Null /; False)	        

getSharees[obj_CloudObject, key_String] := getSharees[obj, {key}]

getSharees[obj_CloudObject, keys_List] :=
     Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]),
            Message[CloudObject::cloudnf, obj];
            Return[$Failed];
        ];
        getSharees[cloud, uuid, keys]
    ]
    
getSharees[cloud_, uuid_, keys_] := Module[{json, data},
    json = Replace[
        execute[cloud, "GET", {"files", uuid, "sharees"}],
        {
            {_, bytes_List} :> FromCharacterCode[bytes],
            HTTPError[404, ___] :> (
                Message[CloudObject::cloudnf, obj]; 
                Return[$Failed]
            ),
            other_ :> (
                checkError[other, CloudShare]; 
                Return[$Failed]
            )
       }
    ];
    data = importFromJSON[json];
    If[data === {}, 
        {},
        Lookup[data, keys] ]
    
]

addSharees[obj_CloudObject, users:{Alternatives[_String, _PermissionsGroup]..}] :=
    Module[{cloud, uuid},
        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        addSharees[cloud, uuid, users]        
    ]

addSharees[cloud_, uuid_, users:{Alternatives[_String, _PermissionsGroup]..}] :=
	updateSharees[cloud, uuid, users, "POST"]
   
addSharees[___] := $Failed


(*This clears previousr sharees and replace it with new*) 
setSharees[obj_CloudObject, users:{Alternatives[_String, _PermissionsGroup]..}] :=
    Module[{cloud, uuid},
        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        setSharees[cloud, uuid, users]
    ]
       
setSharees[cloud_, uuid_, users:{Alternatives[_String, _PermissionsGroup]..}] :=
	updateSharees[cloud, uuid, users, "PUT"]
    
setSharees[___] := $Failed 

updateSharees[cloud_, uuid_, users:{Alternatives[_String, _PermissionsGroup]..}, method_] :=
    Module[{normalizeGroup, usersNew, body},
        normalizeGroup = Map[validateUserSpecification[#, CloudShare]&, Select[users, MatchQ[#, _PermissionsGroup]& ]];
        usersNew = Replace[users, Rule @@@ normalizeGroup, {1}];
        body = exportToJSON[usersNew]; 
        Replace[
            execute[cloud, method, {"files", uuid, "sharees"}, Body -> body],
            {
                {_, bytes_List} :> FromCharacterCode[bytes],            
                HTTPError[404, ___] :> (
                    Message[CloudObject::cloudnf, obj];
                    Return[$Failed]
                ),
                HTTPError[400, content_List,"application/json"] :> (
                    invalidShareeError[content, normalizeGroup];
                    Return[$Failed]
                ),
                other_ :> (
                    checkError[other, CloudShare];
                    Return[$Failed]
                )
            }
        ]
    ]

deleteAllSharees[obj_CloudObject, head_] :=
    Module[{cloud, uuid},
        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        deleteAllSharees[cloud, uuid, head]        
    ]       
    
deleteAllSharees[cloud_, uuid_, head_] := 
Replace[
    execute[cloud, "DELETE", {"files", uuid, "sharees"}],
    {
        {_String, {}} :> Null, (* success *)
        HTTPError[404, ___] :> (
            Message[head::cloudnf, obj];
            $Failed
        ),
        other_ :> (
            checkError[other, head]; $Failed
        )
    }
]
deleteAllSharees[___] := $Failed 

deleteSubsetSharees[obj_CloudObject, sharees_List, head_] :=
    Module[{cloud, uuid},
        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
        deleteSubsetSharees[cloud, uuid, sharees, head]        
    ]
    
deleteSubsetSharees[cloud_, uuid_, sharees_List, head_] := 
Replace[
    execute[cloud, "DELETE", {"files", uuid, "sharees"}, Body -> exportToJSON[sharees]],
    {
        {_String, {}} -> Null,
        HTTPError[400, {"extraData" -> data_, "errorCode" -> error_},___] :> (
            handleErrorDetails[error, importFromJSON[data], head]; $Failed),
        HTTPError[404, ___] :> (
            Message[head::cloudnf, obj]; $Failed),
        other_ :> (
            checkError[other, head]; $Failed
        )
    }
]
deleteSubsetSharees[___] := $Failed     

addPermissions[users:{Alternatives[_String, _PermissionsGroup, _Rule]...}, addPers_] :=
    Module[ {pers, persJoin, persGather},
        pers = Replace[users, x : Alternatives[_String, _PermissionsGroup] :> (x -> Automatic), {1}];
        persJoin = Join[pers, addPers];
        persGather = GatherBy[persJoin, normalizeUserSpecification[#[[1]], CloudShare] &];
        Map[#[[1, 1]] -> DeleteDuplicates[Flatten[#[[All, 2]]]] &, persGather]
    ]  
    
addPermissions[___] := $Failed      
    
invalidShareeError[data_, normalizedGroup_] :=
    Module[{users, failedUsers},
    	users = importFromJSON[Lookup[data, "extraData"]];
        failedUsers = Complement[users, normalizedGroup[[All,1]]];
        Message[CloudShare::invusr, Replace[failedUsers, {usr_String} :> usr]]
    ]
    
convertFromServerPermissionsGroup[displayName_, uuid_] := If[
    StringMatchQ[uuid, "user-" ~~ __],
    displayName,
    PermissionsGroup[displayName]
]

Protect[System`CloudShare];

End[] (* End Private Context *)

EndPackage[]
