Begin["EntityFramework`EntityList`Private`"];

(* -------------------------------------------------- *)
(* exported symbols *)

EntityClassList[args___] := With[
	{res = Catch[iEntityClassList[args], $tag]},
	res /; res =!= $Failed
];

EntityList[args___] := With[
	{res = Catch[iEntityList[args], $tag]},
	res /; res =!= $Failed
];

EntityProperties[args___] := With[
	{res = Catch[iEntityProperties[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`EntityPropertyClassList[args___] := With[
	{res = Catch[iEntityPropertyClassList[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`EvaluateCondition[args___] := With[
	{res = Catch[iEvaluateCondition[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`Filter[args___] := With[
	{res = Catch[iFilter[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`TakeOperatorQ[(TakeLargest | TakeSmallest)[_Integer?NonNegative]] := True;
EntityFramework`TakeOperatorQ[(TakeLargestBy | TakeSmallestBy)[_, _Integer?NonNegative]] := True;
EntityFramework`TakeOperatorQ[_] := False;

(* end exported symbols *)
(* -------------------------------------------------- *)

(* -------------------------------------------------- *)
(* utilities *)

fail[] := Throw[$Failed, $tag];

Clear[iMixedList];
iMixedList[{custom_, default_}, obj_] := Replace[
	EntityFramework`FindEntityStore[EntityTypeName[obj]],
	{
		store_EntityStore :> custom[{store, obj}],
		_ :> Replace[
			EntityFramework`DefaultEntityTypeExistsQ[EntityTypeName[obj]],
			{
				True :> default[obj],
				False :> Missing["UnknownType", EntityTypeName[obj]],
				_ :> fail[]
			}
		]
	}
];

Clear[iDefaultList];
iDefaultList[obj_, "Entities"] := iDefaultEntityList[obj];
iDefaultList[obj_, prop_String] := Replace[
	EntityFramework`EntityValueCache[obj, prop],
	Missing[mtype : "NotInCache" | "OutdatedCache", {___, cached_}] :> Replace[
		Internal`MWACompute["MWACalculateData", {Internal`MWASymbols`MWAData[obj, prop]}],
		{
			HoldComplete[KeyValuePattern["Result" -> l : _List | _Missing]] :> (
				EntityFramework`EntityValueCacheAdd[obj, prop, l];
				l
			),
			_ :> If[mtype === "OutdatedCache",
				Message[EntityValue::outdcache];
				cached,
				fail[]
			]
		}
	]
];

$entityStorePattern = _EntityStore | Function[EntityFramework`EntityStore`Private`iGetEntityStoreData[__]];
$queryFailure = Missing["QueryTooSlow" | "QueryUnknownValue", ___];

Clear[toHead];
toHead["Entities"] = Entity;
toHead["EntityClasses"] = EntityClass;
toHead["Properties"] = EntityProperty;
toHead["PropertyClasses"] = EntityPropertyClass;

Clear[getAll];
getAll[{store : $entityStorePattern, type_String | _[type_String, All, pspec : Repeated[All | Span[_Integer, _Integer], {0, 1}]]}, "Entities"] := With[
	{g = store[Entity[type], "EntityListFunction"]},
	If[MissingQ[g],
		store[Entity[type], "Entities"] // Replace[
			a_Association :> Thread[Entity[type, Cases[Keys[a], _String][[pspec]]]]
		],
		With[
			{list = g[]},
			If[ListQ[list],
				Thread[Entity[type, list[[pspec]]]],
				Message[EntityList::listres, HoldForm[g[]]];
				fail[]
			]
		]
	]
];
getAll[{store : $entityStorePattern, type_String | _[type_String, All, pspec : Repeated[All | Span[_Integer, _Integer], {0, 1}]]}, prop_String] := store[Entity[type], prop] // Replace[
	a_Association :> Thread[toHead[prop][type, Cases[Keys[a], _String][[pspec]]]]
];
getAll[{EntityValue, obj_}, prop_String] := iMixedList[{getAll[#, prop] &, iDefaultList[#, prop] &}, obj];
getAll[___] := fail[];

Clear[changeType];
changeType[_String, newType_] := newType;
changeType[head_[_String, args___], newType_] := head[newType, args];

Clear[getAllFromBase];
getAllFromBase[{store_, obj : type_String | _[type_String, __]}, prop_String] := With[
	{base = store[Entity[type], "BaseEntityType"]},
	If[StringQ[base],
		Replace[
			getAll[{store, changeType[obj, base]}, prop],
			EntityProperty[base, p__] :> EntityProperty[type, p],
			{1}
		],
		{}
	]
];
getAllFromBase[___] := fail[];

(* end utilities *)
(* -------------------------------------------------- *)

(* -------------------------------------------------- *)
(* entity class list *)

Clear[iEntityClassList];

iEntityClassList[{store_, type_String | Entity[type_String]}] := getAll[{store, type}, "EntityClasses"];
iEntityClassList[type_String | Entity[type_String]] := iEntityClassList[{EntityValue, type}];

iEntityClassList[{$entityStorePattern | EntityValue, arg_} | arg_] := (
	Message[EntityClassList::noent, arg];
	fail[]
);
iEntityClassList[args___] := (
	ArgumentCountQ[EntityClassList, Length[{args}], 1, 1];
	fail[]
);

(* end entity class list *)
(* -------------------------------------------------- *)

(* -------------------------------------------------- *)
(* entity list *)

Clear[iEntityList];

iEntityList[{store_, class : EntityClass[_String, All, Repeated[All | Span[_Integer, _Integer], {0, 1}]]}] := getAll[{store, class}, "Entities"];

(* custom *)
iEntityList[{store : $entityStorePattern, class : EntityClass[type_String, _String]}] := store[class, "Entities"] // Replace[{
	Except[{}, cond_?EntityFramework`ConditionQ] :> iEntityList[{store, EntityClass[type, cond]}],
	l_List :> Thread[Entity[type, l]],
	m_Missing :> m,
	_ :> fail[]
}];
iEntityList[{store : $entityStorePattern, class : EntityClass[_String, _?EntityFramework`ConditionQ]}] := expand[{store, class}];
iEntityList[{store : $entityStorePattern, EntityClass[type_String, name_, pspec : All | Span[_Integer, _Integer]]}] := iEntityList[{store, EntityClass[type, name]}][[pspec]];

(* mixed *)
iEntityList[{EntityValue, class : EntityClass[type_String, cond_?EntityFramework`ConditionQ, pspec : Repeated[All | Span[_Integer, _Integer], {0, 1}]]}] := If[EntityFramework`CustomEntityTypeExistsQ[type],
	expand[{
		EntityValue, (* use EntityValue instead of an entity store to support property sequence accross different entity stores and built-in types *)
		EntityClass[type, cond]
	}][[pspec]],
	Replace[
		EntityFramework`DefaultEntityTypeExistsQ[type],
		{
			True :> iDefaultEntityList[class],
			False :> Missing["UnknownType", type],
			_ :> fail[]
		}
	]
];
iEntityList[{EntityValue, class : EntityClass[type_String, _String | {(_String | _Integer | _List) ..}, Repeated[All | Span[_Integer, _Integer], {0, 1}]]}] :=
iMixedList[{iEntityList, iDefaultEntityList}, class];

iEntityList[{store : $entityStorePattern | EntityValue, type_String | Entity[type_String]}] := iEntityList[{store, EntityClass[type, All]}];
iEntityList[l_List | {$entityStorePattern | EntityValue, l_List} /; Internal`PossibleEntityListQ[l]] := l;
iEntityList[class_?Internal`PossibleEntityListQ] := iEntityList[{EntityValue, class}];
iEntityList[Entity[type_String]] := iEntityList[{EntityValue, type}];

(* wrappers *)
iEntityList[
	{store : $entityStorePattern | EntityValue, (w : Dated | HoldPattern[GeoVariant] | EntityFramework`Qualified)[ent_?Internal`PossibleEntityListQ, qual_]}
] := iEntityList[{store, ent}] // Replace[l_List :> Function[w[#, qual]] /@ l];

(* implicit entity fallback *)
iEntityList[Entity[type_String, cond_?EntityFramework`ConditionQ]] := iEntityList[EntityClass[type, cond]];

iEntityList[{$entityStorePattern | EntityValue, arg_} | arg_] := (
	Message[EntityList::noent, arg];
	fail[]
);
iEntityList[args___] := (
	ArgumentCountQ[EntityList, Length[{args}], 1, 1];
	fail[]
);


Clear[iDefaultEntityList];
iDefaultEntityList[class : EntityClass[type_String, cond_?EntityFramework`ConditionQ, rest___]] := With[
	{ncond = EntityFramework`NormalizeConditions[cond, type]},
	iDefaultEntityList[
		EntityClass[
			type,
			MapAt[
				Replace[EntityFramework`FlattenEntityProperty[EntityValue, #, type], _EntityFramework`FlattenEntityProperty :> fail[]] &,
				ncond,
				{All, 1}
			],
			rest
		]
	] /; KeyMemberQ[ncond, _Dated | _EntityFramework`Qualified]
];
iDefaultEntityList[class : EntityClass[_String, All | _String | {(_String | _Integer | _List) ..} | _?EntityFramework`ConditionQ]] := Replace[
	EntityFramework`EntityValueCache[class, "Entities"],
	Missing[mtype : "NotInCache" | "OutdatedCache", {___, cached_}] :> With[
		{bs = EntityFramework`$EntityListBatchSize},
		Replace[
			Internal`MWACompute["MWACalculateData", {Internal`MWASymbols`MWAData[class, "Entities"], "BatchSize" -> bs}],
			{
				HoldComplete[KeyValuePattern["Result" -> l : _List | _Missing]] :> (
					EntityFramework`EntityValueCacheAdd[class, "Entities", l];
					l
				),
				HoldComplete[KeyValuePattern["EntityCount" -> count_Integer]] :> With[{l = EntityFramework`BatchDownload[
					class,
					Function[batch,
						Replace[
							Internal`MWACompute["MWACalculateData", {Internal`MWASymbols`MWAData[batch, "Entities"]}],
							{
								HoldComplete[KeyValuePattern["Result" -> part_List]] :> part,
								_ :> Missing["RetrievalFailure"]
							}
						]
					],
					bs,
					count
				]},
					EntityFramework`EntityValueCacheAdd[class, "EntityCount", count];
					EntityFramework`EntityValueCacheAdd[class, "Entities", l];
					l
				],
				HoldComplete[$Failed["Disallowed symbols in arguments!"]] :> expand[{EntityValue, class}],
				_ :> If[mtype === "OutdatedCache",
					Message[EntityValue::outdcache];
					cached,
					fail[]
				]
			}
		]
	]
] // Replace[$queryFailure :> expand[{EntityValue, class}]];
iDefaultEntityList[class : EntityClass[_String, _, All | Span[_Integer, _Integer]]] := Replace[
	Internal`MWACompute["MWACalculateData", {Internal`MWASymbols`MWAData[class, "Entities"]}],
	{
		HoldComplete[KeyValuePattern["Result" -> l : _List | _Missing]] :> l,
		HoldComplete[$Failed["Disallowed symbols in arguments!"]] :> expand[{EntityValue, class}],
		_ :> fail[]
	}
] // Replace[$queryFailure :> expand[{EntityValue, class}]];
iDefaultEntityList[___] := fail[];

(* end entity list *)
(* -------------------------------------------------- *)

(* -------------------------------------------------- *)
(* entity property class list *)

Clear[iEntityPropertyClassList];

iEntityPropertyClassList[{store_, type_String | Entity[type_String]}] := getAll[{store, type}, "PropertyClasses"];
iEntityPropertyClassList[type_String | Entity[type_String]] := iEntityPropertyClassList[{EntityValue, type}];

iEntityPropertyClassList[args___] := (
	ArgumentCountQ[EntityFramework`EntityPropertyClassList, Length[{args}], 1, 1];
	fail[]
);

(* end entity property class list *)
(* -------------------------------------------------- *)

(* -------------------------------------------------- *)
(* entity properties *)

Clear[iEntityProperties];

iEntityProperties[{store_, class : EntityPropertyClass[_String, All, Repeated[All | Span[_Integer, _Integer], {0, 1}]]}] := Replace[
	getAll[{store, class}, "Properties"],
	l_List :> Union[l, getAllFromBase[{store, class}, "Properties"]]
];

(* custom *)
iEntityProperties[{store : $entityStorePattern, class : EntityPropertyClass[type_String, _String]}] := store[class, "Properties"] // Replace[{
	Except[{}, cond_?EntityFramework`ConditionQ] :> iEntityProperties[{store, EntityPropertyClass[type, cond]}],
	l_List :> Replace[l, {
		p_String :> EntityProperty[type, p],
		{p_String, q_} :> EntityProperty[type, p, EntityFramework`NormalizeQualifiers[q]]
	}, {1}],
	m_Missing :> m,
	_ :> fail[]
}];
iEntityProperties[{store : $entityStorePattern, class : EntityPropertyClass[_String, _?EntityFramework`ConditionQ]}] := expand[{store, class}];

(* mixed *)
iEntityProperties[{EntityValue, class : EntityPropertyClass[type_String, _String | _?EntityFramework`ConditionQ, Repeated[All | Span[_Integer, _Integer], {0, 1}]]}] :=
iMixedList[{iEntityProperties, iDefaultList[#, "Properties"] &}, class];

iEntityProperties[{store : $entityStorePattern | EntityValue, type_String | Entity[type_String]}] := iEntityProperties[{store, EntityPropertyClass[type, All]}];
iEntityProperties[l_List | {$entityStorePattern | EntityValue, l_List} /; Internal`PossibleEntityPropertyListQ[l]] := l;
iEntityProperties[class_?Internal`PossibleEntityPropertyListQ] := iEntityProperties[{EntityValue, class}];
iEntityProperties[type_String | (Entity | EntityClass)[type_String, ___]] := iEntityProperties[EntityPropertyClass[type, All]];

(* wrappers *)
iEntityProperties[
	{store : $entityStorePattern | EntityValue, (w : Dated | HoldPattern[GeoVariant] | EntityFramework`Qualified)[prop_?Internal`PossibleEntityPropertyListQ, qual_]}
] := iEntityProperties[{store, prop}] // Replace[l_List :> Function[w[#, qual]] /@ l];

iEntityProperties[args___] := (
	ArgumentCountQ[EntityProperties, Length[{args}], 1, 1];
	fail[]
);

(* end entity properties *)
(* -------------------------------------------------- *)

(* -------------------------------------------------- *)
(* filter utilities *)

Clear[iEvaluateCondition];
iEvaluateCondition[_, {}, _] := {};
iEvaluateCondition[store_, ent : Entity[type_String, _String] | {Entity[type_String, _String] ..}, EntityProperty[type_String, cond_?EntityFramework`ConditionQ] | cond_?EntityFramework`ConditionQ] := With[
	{ncond = EntityFramework`NormalizeConditions[cond, type]},
	Replace[
		EntityFramework`Filter[
			If[MemberQ[ncond, _[_, _?EntityFramework`TakeOperatorQ]],
				EntityClass[type, All],
				Flatten[{ent}]
			],
			ncond,
			store
		],
		If[ListQ[ent],
			{
				l_List :> Replace[ent, {Alternatives @@ l :> True, _ :> False}, {1}],
				m_Missing :> ConstantArray[m, Length[ent]],
				_ :> fail[]
			},
			{
				l_List :> MemberQ[l, ent],
				m_Missing :> m,
				_ :> fail[]
			}
		]
	]
];
iEvaluateCondition[___] := fail[];

Clear[iFilter];
iFilter[x : {} | _Missing, ___] := x;
iFilter[l : _List | (EntityClass | EntityPropertyClass)[_, All], cond_List, store_] := Module[
	{ent, condList, takeList},
	ent = l;
	{condList, takeList} = Lookup[
		GroupBy[
			cond,
			EntityFramework`TakeOperatorQ @* Last
		],
		{False, True},
		{}
	];
	Function[{prop, pred},
		If[ent === {},
			Throw[{}, $tag]
		];
		ent = filterCondition[
			ent,
			{
				prop,
				toPredicate[prop -> pred, store]
			},
			store,
			propertyValue[store, prop, "QueryHandler"]
		]
	] @@@ condList;
	If[takeList =!= {},
		ent = expand[{store, ent}];
		ent = Intersection @@ Function[{prop, take},
			AssociationThread[
				ent,
				checkResult[store[ent, prop]]
			] // take // Keys
		] @@@ takeList
	];
	ent = expand[{store, ent}];
	ent
];

Clear[propertyValue];
propertyValue[store_, prop_EntityProperty | EntityFramework`EntityPropertySequence[___, prop_EntityProperty], sub_] := store[prop, sub];
propertyValue[___] := Missing[];

Clear[expand];
expand[{_, l_List}] := l;
expand[{store_, class : EntityClass[_, All]}] := iEntityList[{store, class}];
expand[{store_, class : EntityPropertyClass[_, All]}] := iEntityProperties[{store, class}];
expand[{store_, (head : EntityClass | EntityPropertyClass)[type_String, cond_?EntityFramework`ConditionQ]}] := iFilter[
	head[type, All],
	EntityFramework`NormalizeConditions @@ {
		cond,
		If[head === EntityClass, type, Nothing]
	},
	store
];
expand[{store_, (head : EntityClass | EntityPropertyClass)[type_, name_, pspec : All | Span[_Integer, _Integer]]}] := expand[{store, head[type, name]}][[pspec]];

Clear[checkResult];
checkResult[{Missing[m : "UnknownProperty", args___] ..}] := Throw[Missing["Query" <> m, args], $tag];
checkResult[l_List] := l;
checkResult[_] := fail[]

Clear[filterCondition];
filterCondition[ent_, cond_, store_, _Missing] := filterCondition[ent, cond, store];
filterCondition[ent_, cond : {_, pred_}, store_, qh_] := Replace[
	qh[pred, ent],
	{
		Automatic | Default :> filterCondition[ent, cond, store],
		Except[{___Entity}] :> (
			Message[EntityList::entlistres, HoldForm[qh[pred, ent]]];
			fail[]
		)
	}
];
filterCondition[ent_, {prop_, pred_}, store_] := With[
	{list = expand[{store, ent}]},
	Pick[
		list,
		TrueQ @* pred /@ checkResult[store[list, prop]]
	]
];

Clear[toPredicate];
toPredicate[(Rule | RuleDelayed)[prop_, pred_], store_] := toPredicate[
	Replace[
		propertyValue[store, prop, "QueryPreprocessingFunction"],
		_Missing :> Identity
	][pred]
];
toPredicate[Interval[{min_, max_}]] := Between[{min, max}];
toPredicate[HoldPattern[Quantity][Interval[{min_, max_}], unit_]] := Between[{Quantity[min, unit], Quantity[max, unit]}];
toPredicate[value_Alternatives] := MatchQ[value];
toPredicate[value : HoldPattern[_Quantity] | _DateObject | _?NumericQ | True | False | _Entity | _EntityClass | _String | _Missing] := EqualTo[value];
toPredicate[m : MissingQ | _EqualTo | _MatchQ] := m;
toPredicate[value_] := And[! MissingQ[#], value[#]] &;

(* end filter utilities *)
(* -------------------------------------------------- *)

End[];
