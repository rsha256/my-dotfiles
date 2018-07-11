
Begin["EntityFramework`CustomEntity`Private`"];


(* -------------------------------------------------- *)
(* exported symbols *)

EntityFramework`AddEntities[args___] := With[
	{res = Catch[iAddEntities[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`AddEntityClass[args___] := With[
	{res = Catch[iAddEntityClass[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`AddEntityPropertyClass[args___] := With[
	{res = Catch[iAddEntityPropertyClass[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`CustomCommonName[args___] := With[
	{res = Catch[iCustomCommonName[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`CustomCommonName[l_List, opts : OptionsPattern[]] := EntityFramework`CustomCommonName[#, opts] & /@ l;

EntityFramework`CustomEntityTypeExistsQ[type_String] := typeQ[type];

EntityFramework`CustomEntityValue[args___] := With[
	{res = Catch[iCustomEntityValue[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`SetEntityValue[args___] := With[
	{res = Catch[iSetEntityValue[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`UnsetEntityValue[args___] := With[
	{res = Catch[iUnsetEntityValue[args], $tag]},
	res /; res =!= $Failed
];

$EntityStores = {};
Internal`$DefaultEntityStores = {};

(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


(* -------------------------------------------------- *)
(* utilities *)

transpose[<||>] := <||>;
transpose[assoc_?(AssociationQ[#] && MatchQ[Values[#], {__?AssociationQ}] &)] := assoc // Query[Transpose] // DeleteCases[#, Missing["KeyAbsent", _], {2}] &;

(* ent utilities *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* labels *)

Clear[iCustomCommonName];

Options[iCustomCommonName] = {
	Language :> CurrentValue[Language],
	"SubLabel" -> False
};

computeLabel[ent_Entity, language_] :=
iCustomEntityValue[ent, EntityProperty[EntityTypeName[ent], "Label", "Language" -> language]] //
Replace[_Missing :> iCustomEntityValue[ent, EntityProperty[EntityTypeName[ent], "Label"]]] //
Replace[Missing["UnknownProperty", _] :> Missing["NotAvailable"]];

Clear[fromLabelDisambiguationList];
fromLabelDisambiguationList[{m_Missing, _}] := m;
fromLabelDisambiguationList[{l_, _Missing}] := l;
fromLabelDisambiguationList[{l_, d_}] := EntityFramework`Qualified[l, "SubLabel" -> d];
fromLabelDisambiguationList[_] := fail[];

iCustomCommonName[ent : Entity[type_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]], OptionsPattern[]] := EntityFramework`FindEntityStore[type] // Replace[{
	store_EntityStore :> If[TrueQ[OptionValue["SubLabel"]],
		store[
			ent,
			{
				EntityFramework`DefaultKey[{"Label", "Language" -> OptionValue[Language]}, "Label"],
				EntityFramework`DefaultKey[{"SubLabel", "Language" -> OptionValue[Language]}, "SubLabel"]
			}
		] // Replace[{
			{_Missing, _Missing} :> ent[{EntityFramework`Qualified["Label", "Language" -> OptionValue[Language]], EntityFramework`Qualified["SubLabel", "Language" -> OptionValue[Language]]}],
			{_Missing, d_} :> {ent[EntityFramework`Qualified["Label", "Language" -> OptionValue[Language]]], d},
			{l_, _Missing} :> {l, ent[EntityFramework`Qualified["SubLabel", "Language" -> OptionValue[Language]]]}
		}] // Replace[{
			{_Missing, _Missing} :> ent[{"Label", "SubLabel"}],
			{_Missing, d_} :> {ent["Label"], d},
			{l_, _Missing} :> {l, ent["SubLabel"]}
		}] // fromLabelDisambiguationList,
		store[
			EntityFramework`DefaultValue[
				ent,
				computeLabel[ent, OptionValue[Language]]
			],
			EntityFramework`DefaultKey[
				{"Label", "Language" -> OptionValue[Language]},
				EntityFramework`DefaultValue[
					"Label",
					computeLabel[ent, OptionValue[Language]]
				]
			]
		]
	],
	_ :> Missing["UnknownType", EntityTypeName[ent]]
}];
iCustomCommonName[ent : (EntityClass | EntityProperty | EntityPropertyClass)[_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]], OptionsPattern[]] := EntityFramework`FindEntityStore[EntityTypeName[ent]] // Replace[{
	store_EntityStore :> If[TrueQ[OptionValue["SubLabel"]],
		store[
			ent,
			{
				EntityFramework`DefaultKey[{"Label", "Language" -> OptionValue[Language]}, "Label"],
				EntityFramework`DefaultKey[{"SubLabel", "Language" -> OptionValue[Language]}, "SubLabel"]
			}
		] // fromLabelDisambiguationList,
		store[
			ent,
			EntityFramework`DefaultKey[
				{"Label", "Language" -> OptionValue[Language]},
				"Label"
			]
		]
	],
	_ :> Missing["UnknownType", EntityTypeName[ent]]
}];
iCustomCommonName[_Entity | _EntityClass | _EntityProperty | _EntityPropertyClass, OptionsPattern[]] := Missing["NotSupported"];

iCustomCommonName[___] := fail[];

(* end labels *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* entity stores *)

Clear[updateEntityStore];
updateEntityStore[type_String, callback_] := With[
	{i = entityStorePosition[type, $EntityStores]},
	If[IntegerQ[i],
		$EntityStores[[i]] = callback[$EntityStores[[i]]],
		PrependTo[$EntityStores, callback[EntityStore[type]]]
	];
];
updateEntityStore[___] := fail[];

Clear[entityStorePosition];
entityStorePosition[type_String, stores_List] := FirstPosition[
	stores,
	_?(EntityFramework`EntityTypeExistsQ[#, type] &),
	Missing["NotFound"],
	{1},
	Heads -> False
] // Replace[{i_Integer} :> i];

Clear[getEntityStore];
getEntityStore[type_?StringQ] := EntityFramework`FindEntityStore[type] // Replace[
	Except[_EntityStore] :> With[{store = EntityStore[type]},
		PrependTo[$EntityStores, store];
		store
	]
];

Clear[setEntityStore];
setEntityStore[type_String, store_EntityStore] := With[
	{i = entityStorePosition[type, $EntityStores]},
	If[IntegerQ[i],
		$EntityStores[[i]] = store,
		PrependTo[$EntityStores, store]
	]
];

Clear[setTypeData];
setTypeData[type_String, {args___String}, newData_] := Module[
	{store = getEntityStore[type]},
	store[{"Types", type, args}] = newData;
	setEntityStore[type, store]
];

Clear[typeQ];
typeQ[type_String] := Or[
	EntityFramework`EntityTypeExistsQ[$EntityStores, type],
	AssociationQ[DataResource`$ResourceObjectEntityStores] && EntityFramework`EntityTypeExistsQ[Values[DataResource`$ResourceObjectEntityStores], type],
	EntityFramework`EntityTypeExistsQ[Internal`$DefaultEntityStores, type]
];
typeQ[_] := False;

(* end entity stores *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* mutation handler *)

EntityValue /: Set[EntityValue[args__], value_] := With[
	{res = Catch[iSetEntityValue[args, value], $tag]},
	value /; res =!= $Failed
];
EntityValue /: SetDelayed[EntityValue[args__], value_] := With[
	{res = Catch[iSetDelayedEntityValue[args, value], $tag]},
	Null /; res =!= $Failed
];
EntityValue /: Unset[EntityValue[args__]] := With[
	{res = Catch[iUnsetEntityValue[args], $tag]},
	Null /; res =!= $Failed
];


(* operator forms *)
(* see also OperatorForms.m *)

(* entity-like *)
Function[head,
	(* set *)
	head /: Set[ent_head[prop_], value_] := With[
		{res = Catch[iSetEntityValue[ent, prop, value], $tag]},
		value /; res =!= $Failed
	];
	head /: Set[ent_head[prop_String, qual_?EntityFramework`QualifierQ], value_] := With[
		{res = Catch[iSetEntityValue[ent, EntityFramework`Qualified[prop, qual], value], $tag]},
		value /; res =!= $Failed
	];
	(* set delayed *)
	head /: SetDelayed[ent_head[prop_], value_] := With[
		{res = Catch[iSetDelayedEntityValue[ent, prop, value], $tag]},
		Null /; res =!= $Failed
	];
	head /: SetDelayed[ent_head[prop_String, qual_?EntityFramework`QualifierQ], value_] := With[
		{res = Catch[iSetDelayedEntityValue[ent, EntityFramework`Qualified[prop, qual], value], $tag]},
		Null /; res =!= $Failed
	];
	(* unset *)
	head /: Unset[ent_head[prop_]] := With[
		{res = Catch[iUnsetEntityValue[ent, prop], $tag]},
		Null /; res =!= $Failed
	],
	HoldAllComplete
] /@ Hold[
	Entity,
	EntityClass
] // ReleaseHold;

(* property-like *)
Function[head,
	(* property[subproperty] *)
	head /: Set[prop_head[sub : _String | {__String}], value_] := With[
		{res = Catch[iSetEntityValue[prop, sub, value], $tag]},
		value /; res =!= $Failed
	];
	head /: SetDelayed[prop_head[sub : _String | {__String}], value_] := With[
		{res = Catch[iSetDelayedEntityValue[prop, sub, value], $tag]},
		Null /; res =!= $Failed
	];
	head /: Unset[prop_head[sub : _String | {__String}]] := With[
		{res = Catch[iUnsetEntityValue[prop, sub], $tag]},
		Null /; res =!= $Failed
	];

	(* property[entity] *)
	head /: Set[prop_head[ent_], value_] := With[
		{res = Catch[iSetEntityValue[ent, prop, value], $tag]},
		value /; res =!= $Failed
	];
	head /: SetDelayed[prop_head[ent_], value_] := With[
		{res = Catch[iSetDelayedEntityValue[ent, prop, value], $tag]},
		Null /; res =!= $Failed
	];
	head /: Unset[prop_head[ent_]] := With[
		{res = Catch[iUnsetEntityValue[ent, prop], $tag]},
		Null /; res =!= $Failed
	],
	HoldAllComplete
] /@ Hold[
	EntityProperty,
	EntityPropertyClass
] // ReleaseHold;


(* remove objects *)
Function[head,
	head /: Unset[obj_head] := With[
		{res = Catch[iUnsetEntityValue[obj], $tag]},
		Null /; res =!= $Failed
	],
	HoldAllComplete
] /@ Hold[
	Entity,
	EntityClass,
	EntityProperty,
	EntityPropertyClass
] // ReleaseHold;

(* end mutation handler *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* set entity value *)

Clear[iSetEntityValue];

(* fast path *)
iSetEntityValue[ent : Entity[type_String, _String], prop : EntityProperty[type_String, pname_String], value_] := Module[
	{store = getEntityStore[type]},
	(
		store[ent, pname] = value;
		setEntityStore[type, store];
		value
	) /; MissingQ[store[prop, "InsertionFunction"]]
];

(* single - single *)
iSetEntityValue[ent_Entity, prop_EntityProperty, value_] := (iSetEntityValue[{ent}, {prop}, {{value}}]; value);
iSetEntityValue[prop_EntityProperty, sub_String, value_] := (iSetEntityValue[{prop}, {sub}, {{value}}]; value);

(* list - single *)
iSetEntityValue[ent : {__Entity}, prop_EntityProperty, value_List] := (iSetEntityValue[ent, {prop}, List /@ value]; value);
iSetEntityValue[prop : {__EntityProperty}, sub_String, value_List] := (iSetEntityValue[prop, {sub}, List /@ value]; value);

(* single - list *)
iSetEntityValue[ent_Entity, prop : {__EntityProperty}, value_List] := (iSetEntityValue[{ent}, prop, {value}]; value);
iSetEntityValue[prop_EntityProperty, sub : {__String}, value_List] := (iSetEntityValue[{prop}, sub, {value}]; value);

(* list - list *)
iSetEntityValue[ent : {Entity[type_String, _String] ..}, prop : {EntityProperty[type_String, _String, Repeated[_, {0, 1}]] ..}, value_List] /; {Length[ent], Length[prop]} === Dimensions[value, 2] := Module[
	{store = getEntityStore[type]},
	With[
		{insf = store[#, "InsertionFunction"] & /@ prop},
		With[
			{ins = PositionIndex[Not @* MissingQ /@ insf]},
			If[KeyExistsQ[ins, False],
				With[
					{
						nprop = prop[[ins[False]]],
						nvalue = value[[All, ins[False]]]
					},
					Do[
						With[
							{
								ee = ent[[e]],
								pp = nprop[[p]]
							},
							store[ee, pp] = nvalue[[e, p]]
						],
						{e, Length[ent]},
						{p, Length[nprop]}
					]
				]
			];
			If[KeyExistsQ[ins, True],
				Function[i,
					insf[[i]][AssociationThread[
						ent,
						value[[All, i]]
					]]
				] /@ ins[True]
			]
		]
	];
	setEntityStore[type, store];
	value
];
(* property normalization *)
iSetEntityValue[
	ent : Entity[type_String, _String] | {Entity[type_String, _String] ..},
	prop : _String | EntityFramework`Qualified[_String, _?EntityFramework`QualifierQ] | _List?(MemberQ[_String | EntityFramework`Qualified[_String, _?EntityFramework`QualifierQ]]),
	value_
] := iSetEntityValue[
	ent,
	Replace[
		prop,
		s_String | EntityFramework`Qualified[s_String, q_?EntityFramework`QualifierQ] :> EntityProperty[type, s, q],
		{Boole[ListQ[prop]]}
	],
	value
];
iSetEntityValue[prop : {EntityProperty[type_String, _String, Repeated[_, {0, 1}]] ..}, sub : {__String}, value_List] /; {Length[prop], Length[sub]} === Dimensions[value, 2] := Module[
	{store = getEntityStore[type]},
	store[{"Types", type, "Properties"}] = Merge[
		{
			store[type, "Properties"],
			AssociationThread[
				Replace[prop, EntityProperty[_, name_, qual_ : {}] :> EntityFramework`NormalizeQualifiers[{name, qual}], {1}],
				AssociationThread[sub, #] & /@ value
			]
		},
		Apply[Join]
	];
	setEntityStore[type, store];
	value
];

(* entity classes *)
iSetEntityValue[EntityClass[type_String, name_String], "Entities", value_] := (iAddEntityClass[type, {name, value}]; value)
iSetEntityValue[EntityClass[type_String, All], "Entities", value : {(_String | Entity[type_String, _String]) ...}] := With[
	{names = Replace[value, Entity[_, name_] :> name, {1}]},
	If[typeQ[type],
		Complement[EntityList[type], Thread[Entity[type, names]]] // Replace[
			l_List :> iUnsetEntityValue[l]
		]
	];
	iAddEntities[type, names];
	value
];

(* property classes *)
iSetEntityValue[EntityPropertyClass[type_String, name_String], "Properties", value_] := (iAddEntityPropertyClass[type, {name, value}]; value);
iSetEntityValue[EntityPropertyClass[type_String, All], "Properties", value : {(_String | EntityProperty[type_String, _String]) ...}] := With[
	{names = Replace[value, EntityProperty[_, name_] :> name, {1}]},
	If[typeQ[type],
		Complement[EntityProperties[type], Thread[EntityProperty[type, names]]] // Replace[
			l_List :> iUnsetEntityValue[l]
		]
	];
	iAddEntities[type, {}, names];
	value
];

(* type properties *)
iSetEntityValue[Entity[type_String], prop_String, value_] := (
	setTypeData[type, {prop}, value];
	value
);

iSetEntityValue[___] := fail[];

(* end set entity value *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* set delayed entity value *)

Clear[iSetDelayedEntityValue];
SetAttributes[iSetDelayedEntityValue, HoldAll];

iSetDelayedEntityValue[ent : Entity[type_String, ename_String], prop : EntityProperty[type_String, pname_String, qual : Repeated[_?EntityFramework`QualifierQ, {0, 1}]], value_] := updateEntityStore[type, Module[
	{
		store = #,
		p = EntityFramework`NormalizeQualifiers[{pname, qual}],
		data
	},
	data = store[type, "Properties"];
	If[! KeyExistsQ[data, p],
		AssociateTo[data, p -> <||>];
		store[type, "Properties"] = data
	];
	data = store[type, "Entities"];
	If[! KeyExistsQ[data, ename],
		AssociateTo[data, ename -> <||>];
	];
	data[[ename, Key[p]]] := value;
	store[type, "Entities"] = data;
	store
] &];
iSetDelayedEntityValue[ent : Entity[type_String, _String], prop_String | EntityFramework`Qualified[prop_String, qual_?EntityFramework`QualifierQ], value_] := iSetDelayedEntityValue[ent, EntityProperty[type, prop, qual], value];

iSetDelayedEntityValue[___] := fail[];

(* end set delayed entity value *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* unset entity value *)

Clear[iUnsetEntityValue];

(* unset values - properties *)
iUnsetEntityValue[ent : Entity[type_String, _String] | {Entity[type_String, _String] ..}, prop : EntityProperty[type_String, _String]] := iUnsetEntityValue[ent, {prop}];
iUnsetEntityValue[Entity[type_String, ename_String], prop : {EntityProperty[type_String, _String] ..}] := (
	setTypeData[
		type,
		{"Entities", ename},
		Delete[
			Lookup[getEntityStore[type][type, "Entities"], ename, <||>],
			List /@ prop[[All, 2]]
		]
	];
);
iUnsetEntityValue[ent : {Entity[type_String, _String] ..}, prop : {EntityProperty[type_String, _String] ..}] := (
	setTypeData[
		type,
		{"Entities"},
		Delete[
			getEntityStore[type][type, "Entities"],
			Tuples[{ent[[All, 2]], prop[[All, 2]]}]
		]
	];
);
iUnsetEntityValue[ent : Entity[type_String, _] | {Entity[type_String, _] ..}, prop_String] := iUnsetEntityValue[ent, EntityProperty[type, prop]];

(* unset values - subproperties *)
iUnsetEntityValue[prop : EntityProperty[type_String, _String] | {EntityProperty[type_String, _String] ..}, sub_String] := iUnsetEntityValue[prop, {sub}];
iUnsetEntityValue[EntityProperty[type_String, pname_String], sub : {__String}] := (
	setTypeData[
		type,
		{"Properties", pname},
		Delete[
			Lookup[getEntityStore[type][type, "Properties"], pname, <||>],
			List /@ sub
		]
	];
);
iUnsetEntityValue[prop : {EntityProperty[type_String, _String] ..}, sub : {__String}] := (
	setTypeData[
		type,
		{"Properties"},
		Delete[
			getEntityStore[type][type, "Properties"],
			Tuples[{prop[[All, 2]], sub}]
		]
	];
);

(* remove objects *)
iUnsetEntityValue[ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass] := iUnsetEntityValue[{ent}];
iUnsetEntityValue[ent : {Entity[type_String, _String] ..}] := (
	setTypeData[
		type,
		{"EntityClasses"},
		If[KeyMemberQ[#, "Entities"],
			ReplacePart[#, "Entities" -> DeleteCases[#Entities, Alternatives @@ ent[[All, 2]]]],
			#
		] & /@ getEntityStore[type][type, "EntityClasses"]
	];
	setTypeData[
		type,
		{"Entities"},
		Delete[
			getEntityStore[type][type, "Entities"],
			List /@ ent[[All, 2]]
		]
	];
);
iUnsetEntityValue[prop : {EntityProperty[type_String, _String] ..}] := (
	setTypeData[
		type,
		{"PropertyClasses"},
		If[KeyMemberQ[#, "Properties"],
			ReplacePart[#, "Properties" -> DeleteCases[#Properties, Alternatives @@ prop[[All, 2]]]],
			#
		] & /@ getEntityStore[type][type, "PropertyClasses"]
	];
	setTypeData[
		type,
		{"Entities"},
		Delete[List /@ prop[[All, 2]]] /@ getEntityStore[type][type, "Entities"]
	];
	setTypeData[
		type,
		{"Properties"},
		Delete[
			getEntityStore[type][type, "Properties"],
			List /@ prop[[All, 2]]
		]
	];
);
iUnsetEntityValue[ent : {(head : EntityClass | EntityPropertyClass)[type_String, _String] ..}] := With[
	{p = Switch[head, EntityClass, "EntityClasses", EntityPropertyClass, "PropertyClasses"]},
	setTypeData[
		type,
		{p},
		Delete[
			getEntityStore[type][type, p],
			List /@ ent[[All, 2]]
		]
	];
];
iUnsetEntityValue[l : {_[type_String, __] ..}] := (
	iUnsetEntityValue /@ GatherBy[l, Head];
);

(* remove type *)
iUnsetEntityValue[type_String] := With[
	{i = entityStorePosition[type, $EntityStores]},
	If[IntegerQ[i],
		$EntityStores = Delete[
			$EntityStores,
			i
		];
		,
		fail[]
	]
];

iUnsetEntityValue[___] := fail[];

(* end unset entity value *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* data access *)

Clear[insertRawData];
insertRawData[_, <||>] := Null;
insertRawData[type_, values_] := Module[
	{entities = getEntityStore[type][type, "Entities"]},

	(* add entities *)
	entities = Join[
		AssociationMap[First[#] -> <||> &, values],
		entities
	];

	(* insert data of properties with insertion function *)
	values // transpose // KeyValueMap[Function[{p, e},
		(* don't support "InsertionFunction" with property qualifiers for now *)
		With[{ins = Replace[p, {s_String :> EntityValue[EntityProperty[type, s], "InsertionFunction"], _ :> Missing[]}]},
			If[! MissingQ[ins],
				ins[
					KeyMap[Entity[type, #] &, e]
				]
			]
		]
	]];

	(* insert data of properties without insertion function *)
	entities = Merge[
		{
			entities,
			(* don't support "InsertionFunction" with property qualifiers for now *)
			values
				// transpose
				// KeySelect[Function[p, MissingQ[Replace[p, {s_String :> EntityValue[EntityProperty[type, s], "InsertionFunction"], _ :> Missing[]}]]]]
				// KeyMap[EntityFramework`NormalizeQualifiers]
				// transpose
		},
		Apply[Join]
	] // KeySort;

	setTypeData[type, {"Entities"}, entities];
];

(* end data access *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* get type data *)

Clear[getTypeData];

getTypeData[Entity[type_String], prop_String] := getEntityStore[type][type, EntityFramework`DefaultValue[prop, Missing["NotAvailable"]]];
getTypeData[type_String, prop_String] := getEntityStore[type][type, EntityFramework`DefaultValue[prop, getEntityClassData[EntityClass[type, All], prop]]];
getTypeData[type_String, prop_] := EntityValue[EntityClass[type, All], prop];

getTypeData[___] := fail[];

(* end get type data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* get entity data *)

Clear[getEntityData];

(* entity - property *)
getEntityData[
	ent : _Entity | {___Entity},
	prop : EntityProperty[_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]]
] := EntityFramework`EvaluateEntityEntityProperty[EntityValue, ent, prop] // Replace[_EntityFramework`EvaluateEntityEntityProperty :> fail[]];

(* Boolean property *)
getEntityData[
	ent : Entity[type_String, _String] | {Entity[type_String, _String] ..},
	EntityProperty[type_String, cond_?EntityFramework`ConditionQ]
] := EntityFramework`EvaluateCondition[EntityValue, ent, cond] // Replace[_EntityFramework`EvaluateCondition :> fail[]];

(* parameterized entity *)
getEntityData[
	ent : _EntityFramework`ParameterizedEntity | _List?(MemberQ[_EntityFramework`ParameterizedEntity]),
	prop : EntityProperty[_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]]
] := EntityFramework`EvaluateParameterizedEntity[EntityValue, ent, prop] // Replace[_EntityFramework`EvaluateParameterizedEntity :> fail[]];

getEntityData[ent_Entity, prop_] := First[getEntityData[{ent}, prop]];

getEntityData[ent_, prop_EntityPropertyClass] := getEntityData[ent, EntityProperties[prop]];

getEntityData[ent_, "CanonicalName"] := Replace[
	getEntityData[ent, "Entity"],
	e_Entity :> CanonicalName[e],
	{1}
];
getEntityData[ent_, "Entity"] := validate[ent];

getEntityData[ent_, m_Missing] := Replace[
	validate[ent],
	Except[Missing["UnknownEntity", _]] :> m,
	{1}
];

getEntityData[ent_, {}] := ConstantArray[{}, If[ListQ[ent], Length[ent], {}]];
getEntityData[ent_, prop_List] := If[ListQ[ent], Transpose, Identity][getEntityData[ent, #] & /@ prop];

getEntityData[ent : {Entity[type_, _] ..}, prop_String] := getEntityData[ent, EntityProperty[type, prop]];

getEntityData[___] := fail[];

(* end get entity data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* get entity class data *)

Clear[getEntityClassData];

(* entity count *)
getEntityClassData[class : EntityClass[type_String, _String], "EntityCount"] := getEntityStore[type][class, "Entities"] // Replace[{
	Except[{}, rules_?EntityFramework`ConditionQ] :> getEntityClassData[EntityClass[type, rules], "EntityCount"],
	l_List :> Length[l]
}];
getEntityClassData[class_EntityClass, "EntityCount"] := EntityList[class] // Replace[{
	l_List :> Length[l],
	_EntityList :> fail[]
}];

(* random entities *)
getEntityClassData[class_EntityClass, {"RandomEntities", n_Integer?NonNegative}] := EntityList[class] // Replace[{
	l_List :> RandomSample[l, Min[n, Length[l]]],
	m_Missing :> m,
	_ :> fail[]
}];
getEntityClassData[class_EntityClass, "RandomEntities"] := getEntityClassData[class, {"RandomEntities", 10}];
getEntityClassData[class_EntityClass, "RandomEntity"] := getEntityClassData[class, {"RandomEntities", 1}] // Replace[{
	{x_} :> x,
	m_Missing :> m,
	_ :> fail[]
}];

getEntityClassData[class_EntityClass, prop_] := EntityList[class] // Replace[{
	l_List :> getEntityData[l, prop],
	_EntityList :> fail[]
}];

getEntityClassData[___] := fail[];

(* end get entity class data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* get property data *)

Clear[getPropertyData];

getPropertyData[prop : EntityProperty[type_String, __], sub_String] := getEntityStore[type][prop, sub];

getPropertyData[prop : {__EntityProperty}, sub : _String | {___String}] := getPropertyData[#, sub] & /@ prop;
getPropertyData[prop_EntityProperty, sub : {___String}] := getPropertyData[prop, #] & /@ sub;

getPropertyData[___] := fail[];

(* end get property data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* get property class data *)

Clear[getPropertyClassData];

getPropertyClassData[class_EntityPropertyClass, prop_] := EntityProperties[class] // Replace[{
	l_List :> getPropertyData[l, prop],
	_EntityProperties :> fail[]
}];

getPropertyClassData[___] := fail[];

(* end get property class data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* validation *)

Clear[validate];
validate[ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass] := First[validate[{ent}]];
validate[ent : {(Entity | EntityClass | EntityProperty | EntityPropertyClass)[_String, __] ..}] := With[
	{pi = PositionIndex[Head /@ ent]},
	ReplacePart[
		ent,
		Join[
			(* entities *)
			# -> Missing["UnknownEntity", List @@ ent[[#, 1 ;; 2]]] & /@ With[
				{i = Lookup[pi, Entity, {}]},
				Pick[
					i,
					Not @* TrueQ /@ entityQ[ent[[i]]]
				]
			],

			(* entity classes *)
			# -> Missing["UnknownEntityClass", List @@ ent[[#, 1 ;; 2]]] & /@ Select[
				Lookup[pi, EntityClass, {}],
				! EntityFramework`EntityClassExistsQ[ent[[#]]] &
			],

			(* properties *)
			# -> Missing["UnknownProperty", ent[[#]] // Replace[EntityProperty[t_, n_, q_ : {}, ___] :> {t, n, If[q === {}, Nothing, EntityFramework`NormalizeQualifiers[q]]}]] & /@ Select[
				Lookup[pi, EntityProperty, {}],
				! EntityFramework`EntityPropertyExistsQ[ent[[#]]] &
			],

			(* property classes *)
			# -> Missing["UnknownPropertyClass", List @@ ent[[#, 1 ;; 2]]] & /@ Select[
				Lookup[pi, EntityPropertyClass, {}],
				! EntityFramework`EntityPropertyClassExistsQ[ent[[#]]] &
			]
		]
	]
];
validate[___] := fail[];


(* entity *)
Clear[entityQ];
entityQ[ent : Entity[type_String, _String] | {Entity[type_String, _String] ..}] := EntityFramework`EntityExistsQ[getEntityStore[type], ent];
entityQ[ent_Entity] := First[entityQ[{ent}]];
entityQ[{_}] := {False};
entityQ[l_List] := entityQ /@ l;

(* end validation *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* add data *)

Clear[ruleList];
ruleList[key_, value_] := Alternatives[
	{(Rule | RuleDelayed)[key, value] ...},
	_?(AssociationQ[#] && KeyFreeQ[#, Except[key]] && MatchQ[Values[#], {value ...}] &)
];

Clear[iAddEntities];

iAddEntities[type_, x___, values_List?(MemberQ[_String]), y___] := iAddEntities[type, x, Replace[values, name_String :> name -> {}, {1}], y];
iAddEntities[type_, x___, HoldPattern[values_Dataset], y___] := iAddEntities[type, x, Normal[values], y];
iAddEntities[
	type_String | {type_String, opts : (Rule | RuleDelayed)[_String, _] ...},
	ent : ruleList[_String, ruleList[_String | {_String, _?EntityFramework`QualifierQ}, _]] : {},
	prop : ruleList[_String | {_String, _?EntityFramework`QualifierQ}, ruleList[_String, _]] : {}
] := Module[
	{data = getEntityStore[type][type]},
	data = Join[
		data,
		<|
			opts,
			"Properties" -> (Merge[
				{
					# -> {} & /@ DeleteDuplicates[Flatten[Keys[Values[ent]], 1]],
					Lookup[data, "Properties", <||>],
					Association /@ Association[prop]
				},
				Merge[Last]
			] // KeyMap[EntityFramework`NormalizeQualifiers] // KeySort)
		|>
	];
	setTypeData[type, {}, data];
	insertRawData[
		type,
		Association /@ Association[ent] // Map[KeyMap[Replace[{p_String, {}} :> p]]]
	];
];

iAddEntities[___] := fail[];


Clear[iAddEntityClass];
iAddEntityClass[
	type_String,
	classes : {{_String, Optional[_List | _?EntityFramework`ConditionQ | Automatic, Automatic], Optional[ruleList[_, _], {}]} ...}
] := (
	iAddEntityClass[type, #] & /@ classes;
);
iAddEntityClass[
	type_String,
	{
		name_String,
		members : (_List | _?EntityFramework`ConditionQ | Automatic) : Automatic,
		props : ruleList[_, _] : {}
	}
] := Module[
	{store = getEntityStore[type]},
	Switch[members,
		_?EntityFramework`ConditionQ,
		store[EntityClass[type, name], "Entities"] = EntityFramework`NormalizeConditions[members],

		_List,
		store[EntityClass[type, name], "Entities"] = members
			// Cases[_String | Entity[type, __]]
			// Replace[#, Entity[type, s_String] :> s, {1}] &
	];
	KeySelect[<|props|>, StringQ] // KeyValueMap[Function[{p, v},
		store[EntityClass[type, name], p] = v
	]];
	Module[
		{c = store[type, "EntityClasses"]},
		If[! KeyExistsQ[c, name],
			AssociateTo[c, name -> <||>];
			store[{"Types", type, "EntityClasses"}] = c
		]
	];
	setEntityStore[type, store];
];
iAddEntityClass[___] := fail[];


Clear[iAddEntityPropertyClass];
iAddEntityPropertyClass[
	type_String,
	classes : {{_String, Optional[_List | _?EntityFramework`ConditionQ | Automatic, Automatic], Optional[ruleList[_, _], {}]} ...}
] := (
	iAddEntityPropertyClass[type, #] & /@ classes;
);
iAddEntityPropertyClass[
	type_String,
	{
		name_String,
		members : (_List | _?EntityFramework`ConditionQ | Automatic) : Automatic,
		props : ruleList[_, _] : {}
	}
] := Module[
	{store = getEntityStore[type]},
	Switch[members,
		_?EntityFramework`ConditionQ,
		store[EntityPropertyClass[type, name], "Properties"] = EntityFramework`NormalizeConditions[members],

		_List,
		store[EntityPropertyClass[type, name], "Properties"] = members
			// Cases[_String | EntityProperty[type, __]]
			// Replace[#, EntityProperty[type, s_String] :> s, {1}] &
	];
	KeySelect[<|props|>, StringQ] // KeyValueMap[Function[{p, v},
		store[EntityPropertyClass[type, name], p] = v
	]];
	Module[
		{c = store[type, "PropertyClasses"]},
		If[! KeyExistsQ[c, name],
			AssociateTo[c, name -> <||>];
			store[{"Types", type, "PropertyClasses"}] = c
		]
	];
	setEntityStore[type, store];
];
iAddEntityPropertyClass[___] := fail[];


(* end add data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* custom entity value *)

Clear[iCustomEntityValue];


(* type list *)
iCustomEntityValue[] := Flatten[
	Function[store,
		If[EntityFramework`EntityStoreQ[store],
			store[{"Types"}] // Keys,
			{}
		]
	] /@ Join[
		$EntityStores,
		If[AssociationQ[DataResource`$ResourceObjectEntityStores], Values[DataResource`$ResourceObjectEntityStores], {}],
		Internal`$DefaultEntityStores
	],
	1
] // DeleteDuplicates;


iCustomEntityValue[type : Entity[_String] | _String, arg_] := getTypeData[type, arg];
iCustomEntityValue[ent : _Entity | _EntityFramework`ParameterizedEntity | {(_Entity | _EntityFramework`ParameterizedEntity) ..}, arg_] := getEntityData[ent, arg];
iCustomEntityValue[class_EntityClass, arg_] := getEntityClassData[class, arg];
iCustomEntityValue[prop : _EntityProperty | {__EntityProperty}, arg_] := getPropertyData[prop, arg];
iCustomEntityValue[class_EntityPropertyClass, arg_] := getPropertyClassData[class, arg];
iCustomEntityValue[arg_] := validate[arg];


iCustomEntityValue[args__] /; ! FreeQ[{args}, (Entity | EntityClass | EntityProperty | EntityPropertyClass)[_String, ___]] := Missing["NotSupported"];

iCustomEntityValue[___] := fail[];

(* end custom entity value *)
(* -------------------------------------------------- *)


End[];
