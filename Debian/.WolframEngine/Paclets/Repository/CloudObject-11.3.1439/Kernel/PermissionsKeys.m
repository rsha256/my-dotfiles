(* ::Package:: *)

BeginPackage["CloudObject`"]

System`PermissionsKey;
System`DeletePermissionsKey;
System`PermissionsKeys;

Begin["`Private`"]

validatePermissionsKey[PermissionsKey[key_String]] := validatePermissionsKey[key]

validatePermissionsKey[key_String] := StringLength[key] > 0

validatePermissionsKey[PermissionsKey[]] := True
    
validatePermissionsKey[key___] := False

(* PermissionsKey *)

PermissionsKey[] := PermissionsKey[CreateUUID[]]

PermissionsKey[args__] := (ArgumentCountQ[PermissionsKey,Length[DeleteCases[{args},_Rule,Infinity]],0,1];Null/;False)
    
(* DeletePermissionsKey *)

Options[DeletePermissionsKey] = {CloudBase->Automatic}

DeletePermissionsKey[keys:{Alternatives[_PermissionsKey, _String] ..}, opts:OptionsPattern[]] := Map[DeletePermissionsKey[#, opts]&, keys]

DeletePermissionsKey[key_, opts:OptionsPattern[]] :=
	If[validatePermissionsKey[key]
			,
			Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]}, deletePermissionsKey[key]
			]
			,
			Message[DeletePermissionsKey::invkey, key]; 
			$Failed
		]
	
DeletePermissionsKey[args___] := (ArgumentCountQ[DeletePermissionsKey,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)

deletePermissionsKey[key_, msgHeader_: DeletePermissionsKey] :=
	Replace[
		execute[$CloudBase, "DELETE", {"permissionskeys", Replace[key, PermissionsKey[x_] :> x]}],
		{
			{_String, {}} :> Null,
			HTTPError[404, ___] :> (Message[msgHeader::keynf, key]; $Failed),
			other_ :> (checkError[other, msgHeader]; $Failed)
		}
	]
	
(* PermissionsKeys *)
Options[PermissionsKeys] = {CloudBase->Automatic}

PermissionsKeys[opts:OptionsPattern[]] :=
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		Replace[
			execute[$CloudBase, "GET", {"permissionskeys"}],
			{
				{_String, res:{_Integer ...}} :> constructPermsKeyList[res],
				other_ :> (checkError[other, PermissionsKeys]; $Failed)
			}
		]
]
	
PermissionsKeys[args___] := (ArgumentCountQ[PermissionsKeys,Length[DeleteCases[{args},_Rule,Infinity]],0,0];Null/;False)

constructPermsKeyList[bytes_] :=
	With[{json = importFromJSON[FromCharacterCode[bytes]]},
		Map[PermissionsKey, json]
	]
		
End[]

EndPackage[]
