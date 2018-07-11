
Begin["EntityFramework`DefaultEntity`Private`"];


(* -------------------------------------------------- *)
(* call internal functions *)

EntityFramework`DefaultEntityTypeExistsQ["EntityType"] = True;
EntityFramework`DefaultEntityTypeExistsQ[type_String] := If[TrueQ[EntityFramework`$ValidateTypes],
	Replace[
		EntityList["EntityType"],
		{
			l_List :> MemberQ[l, Entity["EntityType", type]],
			_ :> $Failed
		}
	],
	True
];

EntityFramework`DefaultEntityValue[args___] := With[
	{res = Catch[iDefaultEntityValueUsingCache[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`GetLabels[args___] := With[
	{res = iGetLabels[args]},
	res /; res =!= $Failed
];

Unprotect[Internal`CacheEntityNames];
Internal`CacheEntityNames[res_] := cacheSideEffects[ReleaseHold[res] // Replace[{Except[_Rule], rules__Rule} :> {rules}]];

(* end call internal functions *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


(* -------------------------------------------------- *)
(* labels *)

Clear[iGetLabels];
iGetLabels[expr_] := EntityFramework`BatchDownload[
	Cases[
		Hold[expr] //. a_Association :> With[{tmp = Normal[a]}, tmp /; True],
		(Entity | EntityClass | EntityProperty | EntityPropertyClass)[_String, _] | EntityProperty[_, _, _],
		Infinity
	] // DeleteDuplicates,
	Function[batch,
		Replace[
			ReleaseHold[Internal`MWACompute[
				"MWANames",
				{batch}
			]] // EntityFramework`Private`ConvertTemporaryMWASymbols,
			Except[{Rule[_String, {Rule[_List, _] ...}] ...}] :> Missing["RetrievalFailure"]
		]
	],
	EntityFramework`$CommonNameBatchSize // Replace[
		Except[_Integer?Positive | Infinity] :> 2500
	],
	"Merge" -> Merge[Apply[Join]] /* Normal
];

(* end labels *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* caching *)

$specialProperties = "Source";
$notInCachePattern = Missing["NotInCache" | "OutdatedCache", ___];


Clear[iDefaultEntityValueUsingCache];

(* type list *)
iDefaultEntityValueUsingCache[] := EntityList[EntityClass["EntityType", {"Developmental" -> False, "PropertyCount" -> GreaterThan[1]}]] // Replace[{
	l_List :> CanonicalName[l],
	_ :> $Failed
}];

(* type properties *)
iDefaultEntityValueUsingCache[Entity[type_String], "BaseEntityType"] := Missing["NotAvailable"];
iDefaultEntityValueUsingCache[
	type_String | Entity[type_String],
	prop : Alternatives[
		"Label",
		"LabelPlural",
		"SampleEntities",
		"SampleEntityClasses"
	]
] := Replace[
	EntityValue[Entity["EntityType", type], EntityProperty["EntityType", prop]],
	Except[_List | _String] :> $Failed
];

iDefaultEntityValueUsingCache[_EntityProperty, "AggregationHandler"] := Missing["NotAvailable"];

(* list of entities - list of properties *)
iDefaultEntityValueUsingCache[
	ent : {Entity[type_String, _] ..},
	prop : {EntityProperty[type_String, Repeated[_, {1, 2}]] ..}
] := rectangularBatchUsingCache[ent, prop, getEntityValueMaxBatchSize];
iDefaultEntityValueUsingCache[
	prop : {EntityProperty[type_String, Repeated[_, {1, 2}]] ..},
	sub : {__String}
] := rectangularBatchUsingCache[prop, sub, getEntityValueMaxBatchSize[] &];

Clear[rectangularBatchUsingCache];
rectangularBatchUsingCache[ent_List, prop_List, batchSizeFunction_] := Module[
	{res = Transpose[EntityFramework`EntityValueCache[ent, prop]]},
	With[
		{pos = Position[res, _?(MemberQ[$notInCachePattern]), {1}, Heads -> False][[All, 1]]},
		If[pos =!= {},
			res[[pos]] = Transpose[EntityFramework`BatchDownload[
				ent,
				Thread[prop[[pos]] -> batchSizeFunction[prop[[pos]]]],
				iDefaultEntityValue
			]]
		]
	];
	Transpose[res]
];
rectangularBatchUsingCache[___] := fail[];

(* map over list of properties *)
iDefaultEntityValueUsingCache[
	ent : {Entity[type_String, _] ..},
	prop : {(EntityProperty[type_String, Repeated[_, {1, 2}]] | $specialProperties) ..}
] := Transpose[
	iDefaultEntityValueUsingCache[ent, #] & /@ prop
];

(* fall back to resolving Boolean properties on the client *)
iDefaultEntityValueUsingCache[ent : Entity[type_String, _] | {Entity[type_String, _] ..}, prop : EntityProperty[type_String, cond_?EntityFramework`ConditionQ]] := Replace[
	EntityFramework`EntityValueCache[ent, prop, "CallbackFunction" -> iDefaultEntityValueUsingBatching],
	{
		With[{m = Missing["QueryUnknownValue", ___] | Missing["QueryValueIncompatibleWithProperty", {___, _EntityFramework`EntityPropertySequence, ___}]},
			m | {m ..}
		] :> (EntityFramework`EvaluateCondition[EntityValue, ent, cond] // Replace[_EntityFramework`EvaluateCondition :> $Failed]),
		_EntityFramework`EntityValueCache :> $Failed
	}
];

iDefaultEntityValueUsingCache[ent : _Entity | {__Entity}, prop : EntityProperty[_, "SubLabel"]] := Condition[
	ConstantArray[Missing["UnknownProperty", List @@ prop], If[ListQ[ent], Length[ent], {}]],
	TrueQ[Internal`$CacheEntityValue] && ! EntityFramework`EntityPropertyExistsQ[prop]
];

iDefaultEntityValueUsingCache[ent_, prop : Repeated[_, {0, 1}]] := With[
	{cache = EntityFramework`EntityValueCache[
		ent,
		prop,
		"CallbackFunction" -> iDefaultEntityValueUsingBatching
	]},
	cache /; ! MatchQ[cache, _EntityFramework`EntityValueCache]
];

iDefaultEntityValueUsingCache[args___] := iDefaultEntityValueUsingBatching[args];

(* end caching *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* batching *)

Clear[getEntityValueMaxBatchSize];
getEntityValueMaxBatchSize[prop : _EntityProperty | {___EntityProperty}] := Replace[
	EntityValue[prop, "MaxBatchSize"],
	Except[_Integer?Positive | Infinity] :> getEntityValueMaxBatchSize[],
	{Boole[ListQ[prop]]}
];
getEntityValueMaxBatchSize[] := EntityFramework`$EntityValueBatchSize // Replace[
	Except[_Integer?Positive | Infinity] :> 500
];

Clear[iDefaultEntityValueUsingBatching];

iDefaultEntityValueUsingBatching[ent : (Entity | EntityClass | EntityProperty | EntityPropertyClass)[type_String, __], Repeated[_EntityProperty | _String, {0, 1}]] := Condition[
	Missing["UnknownType", type],
	TrueQ[Internal`$CacheEntityValue] && TrueQ[! EntityFramework`DefaultEntityTypeExistsQ[type]]
];
iDefaultEntityValueUsingBatching[
	ent : {(Entity | EntityProperty)[type_String, __] ..},
	Repeated[_EntityProperty | _String, {0, 1}]
] := Condition[
	ConstantArray[Missing["UnknownType", type], Length[ent]],
	TrueQ[Internal`$CacheEntityValue] && TrueQ[! EntityFramework`DefaultEntityTypeExistsQ[type]]
];

(* list of entities *)
iDefaultEntityValueUsingBatching[
	ent : {Entity[type_String, _] ..},
	prop : EntityProperty[type_String, Repeated[_, {1, 2}]] | $specialProperties
] /; EntityFramework`$BatchEntityValue := EntityFramework`BatchDownload[
	ent,
	Function[batch,
		iDefaultEntityValue[batch, prop]
	],
	If[Length[ent] === 1,
		1,
		Switch[prop,
			$specialProperties, getEntityValueMaxBatchSize[],
			_, getEntityValueMaxBatchSize[prop]
		]
	]
];

(* list of properties *)
iDefaultEntityValueUsingBatching[
	ent : Entity[type_String, _],
	prop : {(EntityProperty[type_String, Repeated[_, {1, 2}]] | $specialProperties) ..}
] /; EntityFramework`$BatchEntityValue := EntityFramework`BatchDownload[
	prop,
	Function[batch,
		iDefaultEntityValue[ent, batch]
	],
	If[Length[prop] === 1,
		1,
		getEntityValueMaxBatchSize[]
	]
];

iDefaultEntityValueUsingBatching[args___] := iDefaultEntityValue[args];

(* end batching *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* call server *)

Clear[evaluateMWAData];
evaluateMWAData[data_Internal`MWASymbols`MWAData] := Module[
	{res},
	res = Block[
		{WolframAlphaClient`Private`$AlphaQueryMMode = Replace[
			WolframAlphaClient`Private`$AlphaQueryMMode,
			Except[Alternatives @@ EntityFramework`Private`$EVMMODES] :> "entity"
		]},
		Internal`MWACompute[
			"MWACalculateData",
			{data, "Version" -> 0.2},
			"UnitSystem" -> OptionValue[EntityProperty, UnitSystem],
			"ContextPath" -> {"Internal`MWASymbols`", "System`", "EntityFramework`"}
		]
	];
	res = EntityFramework`Private`ReleaseMWAComputeHold[res];
	If[res === $Failed["ComputationTimeout"],
		Message[EntityValue::timeout, EntityValue]
	];
	If[! OptionQ[res],
		Return[$Failed]
	];
	issueMessages["Messages" /. res];
	res = Replace["Result", Append[Flatten[{res}], _ -> $Failed]];
	res = EntityFramework`Private`ConvertTemporaryMWASymbols[res];
	res
];

Clear[issueMessages];
issueMessages[msg_List] := Cases[
	msg,
	{{s_Symbol, tag_String}, args___} :> Message[MessageName[s, tag], args]
];

Clear[cacheSideEffects];
cacheSideEffects[rules : {__Rule}] := (
	Replace[rules, KeyValuePattern["LastUpdateRules" -> r : {__Rule}] :> EntityFramework`EntityValueCacheAdd[r[[All, 1]], "LastUpdate", r[[All, 2]]]];
	Replace[rules, KeyValuePattern["SubLabel" -> r : {__Rule}] :> KeyValueMap[cacheSubLabel, GroupBy[r, First /* First, MapAt[Apply[#[##2] &], {All, 1}]]]];
	Function[{ruleName, propName},
		Replace[rules, KeyValuePattern[ruleName -> r : {__Rule}] :> EntityFramework`EntityValueCacheAdd[EntityProperty @@@ r[[All, 1]], propName, r[[All, 2]]]]
	] @@@ {
		{"CachingRules", "Cacheable"},
		{"UnitSystemRules", "UnitSystemRules"},
		{"MaxBatchSizeRules", "MaxBatchSize"},
		{"DateQualifierRules", "DateQualifier"}
	};
	EntityFramework`CommonNameCacheAdd[rules]
);

Clear[cacheSubLabel];
cacheSubLabel[Entity, rules_] := GroupBy[rules, First /* EntityTypeName, EntityFramework`EntityValueCacheAdd[#[[All, 1]], EntityProperty[EntityTypeName[#[[1, 1]]], "SubLabel"], #[[All, 2]]] &];


Clear[iDefaultEntityValue];

(* last update *)
iDefaultEntityValue["LastUpdate"] /; Internal`$CacheEntityValue := Replace[
	Internal`MWACompute["MWACalculateData", {Internal`MWASymbols`MWAData["LastUpdate"]}],
	{
		HoldComplete[KeyValuePattern["Result" -> l : {__Rule}]] :> (
			EntityFramework`EntityValueCacheAdd[l[[All, 1]], "LastUpdate", l[[All, 2]]];
			l
		),
		_ :> Throw[$Failed, $tag]
	}
];

iDefaultEntityValue[
	ent : Entity[type_String, _] | {Entity[type_String, _] ..},
	prop : EntityProperty[type_String, __] | {EntityProperty[type_String, __] ..}
] := Module[
	{res = evaluateMWAData[Internal`MWASymbols`MWAData[ent, prop]]},
	If[res === $Failed,
		res = If[! ListQ[ent] && ! ListQ[prop],
			Missing["RetrievalFailure"],
			ConstantArray[
				Missing["RetrievalFailure"],
				{
					If[ListQ[ent], Length[ent], Nothing],
					If[ListQ[prop], Length[prop], Nothing]
				}
			]
		],
		EntityFramework`EntityValueCacheAdd[ent, prop, res]
	];
	res
];

iDefaultEntityValue[args___] := Module[
	{res},
	res = evaluateMWAData[Internal`MWASymbols`MWAData[args]];
	EntityFramework`EntityValueCacheAdd[args, res];
	res
];

(* end call server *)
(* -------------------------------------------------- *)


End[];
