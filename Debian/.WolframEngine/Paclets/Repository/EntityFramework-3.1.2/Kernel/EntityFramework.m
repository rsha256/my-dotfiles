System`Private`NewContextPath[{"System`"}];

System`CanonicalName;
System`CommonName;
System`Dated;
System`Entity;
System`EntityClass;
System`EntityClassList;
System`EntityCopies;
System`EntityGroup;
System`EntityInstance;
System`EntityList;
System`EntityProperties;
System`EntityProperty;
System`EntityPropertyClass;
System`EntityStore;
System`EntityTypeName;
System`EntityValue;
System`RandomEntity;
System`$EntityStores;

EntityFramework`AddEntities;
EntityFramework`AddEntityClass;
EntityFramework`AddEntityPropertyClass;
EntityFramework`AverageEntity;
EntityFramework`BatchDownload;
EntityFramework`CommonNameCache;
EntityFramework`CommonNameCacheAdd;
EntityFramework`CustomEntityValue;
EntityFramework`DefaultEntityValue;
EntityFramework`EntityBulkFormatter;
EntityFramework`EntityPropertySequence;
EntityFramework`EntityStoreQ;
EntityFramework`EntityValueCache;
EntityFramework`EntityValueCacheAdd;
EntityFramework`GeneralEntityValue;
EntityFramework`GetLabels;
EntityFramework`InverseEntityProperty;
EntityFramework`MakeEntityFrameworkBoxes;
EntityFramework`ParameterizedEntity;
EntityFramework`PrefetchEntityValue;
EntityFramework`PrefetchPropertiesForEntity;
EntityFramework`ProcessAsynchronousResultsQueue;
EntityFramework`Qualified;
EntityFramework`$BatchEntityValue = True;
EntityFramework`$CacheUpdateCheck = True;
EntityFramework`$CommonNameBatchSize;
EntityFramework`$EntityListBatchSize = 10000;
EntityFramework`$EntityValueBatchSize = 500;
EntityFramework`$SendWAEvents = True;
EntityFramework`$ValidateTypes = True;

Experimental`FindEntities;

Internal`AddToEntityNameCache;
Internal`BulkFetchEntityNames;
Internal`CacheEntityNames;
Internal`ClearEntityValueCache;
Internal`DisableEntityFramework;
Internal`PossibleEntityListQ;
Internal`PossibleEntityPropertyListQ;
Internal`PossibleEntityPropertyQ;
Internal`PossibleEntityQ;
Internal`PreloadEntityNameCache;
Internal`$CacheEntityValue;
Internal`$DefaultEntityStores;

Begin["EntityFramework`Private`"];

$protected = Hold[
	CanonicalName,
	CommonName,
	Entity,
	EntityClass,
	EntityClassList,
	EntityCopies,
	EntityGroup,
	EntityInstance,
	EntityList,
	EntityProperties,
	EntityProperty,
	EntityPropertyClass,
	EntityStore,
	EntityTypeName,
	EntityValue,
	RandomEntity,

	Experimental`FindEntities,

	Internal`$DefaultEntityStores
];

Unprotect @@ $protected;

Internal`$CacheEntityValue := TrueQ[
And[
	Not[Developer`$ProtectedMode],
	Replace["FileBackedCaching",
		Replace["FileBackedCachingOptions", SystemOptions["FileBackedCachingOptions"]
		]
	]
]
]

Block[{Import},
Needs["WolframAlphaClient`"]
];(*initialize WAClient without bogus call to http://; see bug(291017) for more details...*)
Unprotect[Entity]; (* the previous call re-protects Entity *)

Options[EntityProperty] := {UnitSystem :> $UnitSystem};
Options[EntityPropertyClass] := {UnitSystem :> $UnitSystem};


Internal`AddToEntityNameCache[{type_, name_}, value_] := EntityFramework`CommonNameCacheAdd[Entity[type, name], value];
Internal`AddToEntityNameCache[l_List] := Internal`AddToEntityNameCache @@@ l;

Internal`PreloadEntityNameCache[expr_] := (
	CommonName[Cases[Hold[expr], _Entity, Infinity]];
);

(* used by Dataset component for faster formatting -- taliesinb *)
Internal`BulkFetchEntityNames[list_] := CommonName[list];

ConvertTemporaryMWASymbols[x_, newcontext_String:"Global`"] := Module[{names, new, warning},
	names = Names["Internal`MWASymbols`Temporary`*"];
	If[names==={}, Return[x]];
	new = StringReplace[names,__~~"`"->newcontext];
	warning = Quiet[Select[new, Names[#] =!= {} && Check[ToExpression[#, InputForm, OwnValues], {}] =!= {}&]];
	If[warning =!= {}, Message[EntityValue::val, ToExpression[#,InputForm,HoldForm]&/@warning]];
	x/.((ToExpression[#,InputForm,HoldPattern]:>With[{tmp=Symbol[StringReplace[#,__~~"`"->newcontext]]}, tmp/;True])&/@names)

]

(* possibly add security features here *)
ReleaseMWAComputeHold[args___] := ReleaseHold[
	ReplaceAll[args, 
		{Internal`MWASymbols`MWAHold[e_] :> With[{res=e}, res/;True], remote_Internal`MWASymbols`MWARemoteObject:>fetchRemoteObject[remote]}
		]
]
SetAttributes[Internal`MWASymbols`MWAData, HoldAll];(*see 268678*)
SetAttributes[Internal`MWASymbols`MWARemoteObject, HoldAll];

fetchRemoteObject[Internal`MWASymbols`MWARemoteObject[url_String,  "ImportAction" -> fun_]] := Module[{res},
	If[frontEndAvailableQ[], Monitor][
	res = Quiet[Check[fun[url], $Failed]], 
	Internal`LoadingPanel["Fetching remote content ...."]
	];
	If[res === $Failed, 
		Message[EntityValue::conopen, EntityValue]; Missing["RetrievalFailure"], 
		res
	]
]
fetchRemoteObject[other__] := Missing["RetrievalFailure"]


replaceWithDefault[expr_, rules_, default_, level_ : {0}] := Replace[expr, Flatten[{rules, _->default}], level];



$SpecialEVRanges = {
	{"RandomEntities", _} , {"Entities", _} , {"EntityCanonicalNames", _} , 
	{"EntityClasses", _} , {"RandomEntityClasses", _} , {"Properties", _}, 
	{"PropertyClasses", _}, {"EntityClassCanonicalNames", _}, 
	{"PropertyCanonicalNames", _}, {"PropertyClassCanonicalNames", _}, {"Properties", _}};


EntityValue[args___] := Block[{EntityFramework`$SendWAEvents = EntityFramework`$SendWAEvents},
		With[{res = EntityFramework`GeneralEntityValue[args]},
			res /; ! MatchQ[res, _EntityFramework`GeneralEntityValue]
		]
];


Experimental`FindEntities[s_String, filter_ : Automatic] := Module[
	{res, apires},
  apires=Internal`MWACompute["MWAFindEntities",{s,filter}];
  apires=ReleaseMWAComputeHold[apires];
  If[MatchQ[apires, {___Rule}], Message[Internal`FindEntities::TODO]; Return[$Failed]];
  res = Replace["Result", Append[apires, _ :> $Failed]];
  If[MatchQ["Messages"/.apires,{__}],Message[Internal`FindEntities::TODO]];   
  Internal`CacheEntityNames[apires];
  ConvertTemporaryMWASymbols[res]
]

(*** Wrappers for *Data functions ***)
$EVDataPacletHeads = Hold[System`AdministrativeDivisionData, System`AircraftData,
System`AirportData, 
System`BridgeData, System`BroadcastStationData, System`BuildingData, System`CometData,
System`CompanyData, System`ConstellationData, System`DamData, 
System`DeepSpaceProbeData, System`EarthImpactData, 
System`ExoplanetData, System`GalaxyData,
System`GeologicalPeriodData, System`HistoricalPeriodData, 
System`IslandData, System`LanguageData, System`LakeData, 
System`LaminaData, System`MannedSpaceMissionData, 
System`MedicalTestData, System`MeteorShowerData, 
System`MineralData, System`MinorPlanetData, System`MountainData, 
System`MovieData, System`NebulaData,
System`NeighborhoodData, System`NuclearExplosionData, 
System`NuclearReactorData, System`OceanData, System`ParkData, 
System`ParticleAcceleratorData, 
System`PersonData, System`PhysicalSystemData, System`PlaneCurveData, 
System`PlanetData, System`PlanetaryMoonData, System`PlantData, 
System`PulsarData, System`SatelliteData, 
System`SolarSystemFeatureData, System`SolidData, System`SpaceCurveData, 
System`SpeciesData, System`StarData, System`StarClusterData, System`SupernovaData, 
System`SurfaceData, System`TropicalStormData, 
System`TunnelData, System`UnderseaFeatureData, 
System`UniversityData, System`VolcanoData, System`ZIPCodeData];

$SpecialEVDataPacletHeads = Hold[
	System`AnatomyData,
	System`WolframLanguageData
];

$SpecialEVDataPacletCases = {
	System`AnatomyData -> "AnatomicalStructure",
	System`WolframLanguageData -> "WolframLanguageSymbol"
}

$entityStandardNamePattern[dp_] = _String;
$entityStandardNamePattern["Acronym" | "AdministrativeDivision" | "City" | "GivenName"] = {__String};
$entityStandardNamePattern["AlgebraicCode"] = {_String, {__Integer}};
$entityStandardNamePattern["AreaCode"] = _Integer;
$entityStandardNamePattern["CrystallographicSpaceGroup"] = {_String, _Integer};
$entityStandardNamePattern["Gene"] = {_String, {"Species" -> _String}};
$entityStandardNamePattern["WolframLanguageSymbol"] = Alternatives[_String, _Symbol];

$dataHeadToEntityType = Join[(# -> StringReplace[SymbolName[#], RegularExpression["Data$"] -> ""]) & /@ 
	List @@ EntityFramework`Private`$EVDataPacletHeads, $SpecialEVDataPacletCases]

(dataHeadToEntityTypeLookup[#[[1]]] = #[[2]]) & /@ $dataHeadToEntityType;

dataHeadToEntityTypeLookup[_] = None;

Unprotect@@$EVDataPacletHeads;
Unprotect@@$SpecialEVDataPacletHeads;

Clear@@$EVDataPacletHeads;
Clear@@$SpecialEVDataPacletHeads;

(#[args___] := With[{res=EVDataPacletDispatch[#, {args}]}, res/;res=!=$Failed]) & /@ (List@@$EVDataPacletHeads);
(#[args___] := With[{res=EVDataPacletDispatch[#, {args}]}, res/;res=!=$Failed]) & /@ (List@@$SpecialEVDataPacletHeads);

With[{heads=List@@$EVDataPacletHeads}, SetAttributes[heads,ReadProtected]];
With[{heads=List@@$SpecialEVDataPacletHeads}, SetAttributes[heads,ReadProtected]];

Protect@@$EVDataPacletHeads;
Protect@@$SpecialEVDataPacletHeads;

(* TODO: add flag to indicate entity or entity class when not specified *)
Clear[EVDataPacletDispatch];
EVDataPacletDispatch[head_, args_] := Module[{etype, res},
  etype = dataHeadToEntityTypeLookup[head];
  If[etype === None, Return[$Failed] (*shouldn't happen*)]; Block[{WolframAlphaClient`Private`$AlphaQueryMMode="paclet"},
  res = Switch[args,
   {} | {All | "Entities"}, 
       EntityValue[etype, "Entities"],
   {"Classes"|"EntityClasses"}, 
       EntityValue[etype, "EntityClasses"],
   {"Properties" | "PropertyCanonicalNames" | "SampleEntities" | "SampleEntityClasses"| 
   	"EntityCanonicalNames" | "EntityCount" | "EntityClassCount" | "EntityClassCanonicalNames"|
   	"RandomEntityClasses"|"PropertyClassCanonicalNames"|"PropertyClasses"|"RandomEntities"|
   	"RandomEntity" |"RandomEntityClass" | "PropertyCount" | "PropertyClassCount" | "LastUpdate"},
       EntityValue[etype, args[[1]]],
   {Alternatives@@$SpecialEVRanges},
   	   EntityValue[etype, args[[1]]],
   {_, _, _, __},(*too many args*)
   	   ArgumentCountQ[head,Length[args],0,3];$Failed,
   {All, __},
   		EntityValue[EntityClass[etype,All], Sequence@@Rest[args]],
   {$entityStandardNamePattern[etype], ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue[Entity[etype, args[[1]]], Sequence @@ Rest[args]],$Failed],
   {{($entityStandardNamePattern[etype] | _Entity) ..}, ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue[If[MatchQ[#, _Entity], #, Entity[etype, #]] & /@ args[[1]], Sequence @@ Rest[args]],$Failed],
   {Entity[etype,___], ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   {EntityClass[etype,___]},
   	   If[ValidArgsForEtypeQ[head, etype, args],EntityValue[First[args],"Entities"],$Failed],
   {EntityClass[etype,___],___},
   	   If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   {EntityPropertyClass[etype,___] ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   {Dated[__],___},
   	   If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   _,
       With[{arg=If[ListQ[args]&&Length[args]>0,First[args],Null]},(*safety valve in case we have bad arguments; shouldn't actually need this...*)
       	Message[head::notent,arg,head];$Failed]
   ]];
   If[MatchQ[res, $Failed|_EntityValue], res = $Failed];
   res
  ]
  
ValidArgsForEtypeQ[head_,etype_,args_List] := Switch[args,
	{},True,
	{_}, True,
	{_,_String,___},True,
	{_,(EntityProperty|EntityPropertyClass)[etype,__],___},True,
	{_,Dated[(_String|EntityProperty[etype,__]),_], ___}, True,
	{_,_List,___},True,(*TODO: fine-tune this; need to support things like {"RandomEntities",8} on top of _EntityProperty.. and _EntityPropertyClass..*)
	{_,_,___},With[{prop=Part[args,2]},Message[head::notprop,prop,head];False],
	_, False
]
  
(*** End code for wrappers ***)
$EVMMODES= {"utility", "paclet", "entity", "semantic"};


(*keep various flags & symbols from triggering Dynamic updating*)
Internal`SetValueNoTrack[{$EVQIDF,Internal`$QueryID,$EPLF, $annotation, $recursionFlag}, True];

Get[FileNameJoin[{DirectoryName[$InputFileName], #}]] & /@ {
	"Utilities.m", (* needs to be first *)

	"BatchApplied.m",
	"Caching.m",
	"CustomEntity.m",
	"DataUtilities.m",
	"DefaultEntity.m",
	"DefaultEntityTypes.m",
	"EntityFunctions.m",
	"EntityList.m",
	"EntityStore.m",
	"Formatting.m",
	"GeneralEntity.m",
	"OperatorForms.m",
	"Predicates.m",
	"Prefetch.m",
	"ToFromEntity.m"
};

Function[sym,
	SetAttributes[sym, {ReadProtected, Protected}],
	HoldAllComplete
] /@ $protected // ReleaseHold;

If[EntityFramework`GetCacheVersion[] =!= EntityFramework`$CacheVersion,
	Internal`ClearEntityValueCache[]
];

EntityFramework`LoadDefaultEntityTypes[];

End[];

System`Private`RestoreContextPath[];
