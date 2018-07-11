BeginPackage["CloudObject`"]

Hold[System`$CloudRootDirectory];
System`CloudDirectory;
System`SetCloudDirectory;

Begin["`Private`"]

CloudDirectory[] := $CloudDirectory;

SetCloudDirectory[dir_] := ($CloudDirectory = CloudObject[dir]);

SetCloudDirectory[] := ($CloudDirectory = $CloudRootDirectory);

$CloudRootDirectory := Quiet[CloudObject[CloudObject`JoinURL[{$CloudBase, $CloudObjectsRoot, "~"}]]]

$CloudDirectory := $CloudRootDirectory;

End[]

EndPackage[]
