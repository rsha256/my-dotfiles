Begin["EntityFramework`EntityStore`Private`"];


(* -------------------------------------------------- *)
(* exported symbols *)

SetAttributes[EntityFramework`DefaultValue, HoldRest];

Options[EntityStore] = {
	Initialization :> None
};
store_EntityStore[args___] := With[
	{res = Catch[iGetEntityStoreData[store, args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`EntityStoreQ[args___] := With[
	{res = iEntityStoreQ[args]},
	res /; res =!= $Failed
];
EntityFramework`SetEntityStoreData[args___] := With[
	{res = iSetEntityStoreData[args]},
	res /; res =!= $Failed
];

(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


(* -------------------------------------------------- *)
(* formatting *)

Clear[short];
short[x_] := short[x, {3, 3}];
short[l_List, {left_Integer, right_Integer}] /; Length[l] > left + right := Join[
	Take[l, left],
	{"\[Ellipsis]"},
	Take[l, -right]
];
short[x_, ___] := Short[x];

Clear[makeSummaryItem];
makeSummaryItem[lable_String, {}] := BoxForm`SummaryItem[{
	lable <> ": ",
	"none"
}];
makeSummaryItem[lable_String, list_List] := BoxForm`SummaryItem[{
	lable <> ": ",
	short[list],
	Row[{" (", Length[list], ")"}]
}];

EntityStore /: MakeBoxes[
	store : _EntityStore?EntityFramework`EntityStoreQ,
	fmt_
] /; BoxForm`UseIcons := Module[
	{always, sometimes},
	Replace[
		store[],
		{
			{type_String} :> (
				always = {
					BoxForm`SummaryItem[{"Type: ", type}],
					makeSummaryItem["Entities", Keys[store[type, "Entities"]]],
					makeSummaryItem["Properties", Keys[store[type, "Properties"]]]
				};
				sometimes = {
					makeSummaryItem["Entity classes", Keys[store[type, "EntityClasses"]]],
					makeSummaryItem["Property classes", Keys[store[type, "PropertyClasses"]]]
				}
			),
			types_List :> (
				always = {
					Join[{BoxForm`SummaryItem[{"Types: ", ""}]}, types],
					Join[{BoxForm`SummaryItem[{"Entity count: ", ""}]}, Length[store[#, "Entities"]] & /@ types],
					Join[{BoxForm`SummaryItem[{"Property count: ", ""}]}, Length[store[#, "Properties"]] & /@ types]
				};
				sometimes = {
					Join[{BoxForm`SummaryItem[{"Entity class count: ", ""}]}, Length[store[#, "EntityClasses"]] & /@ types],
					Join[{BoxForm`SummaryItem[{"Property class count: ", ""}]}, Length[store[#, "PropertyClasses"]] & /@ types]
				}
			)
		}
	];
	BoxForm`ArrangeSummaryBox[
		EntityStore,
		"EntityStore[<>]",
		None,
		always,
		sometimes,
		fmt
	]
];

(* end formatting *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* normalization *)

EntityStore[
	types : Alternatives[
		_String | (_String -> _?AssociationQ),
		{(_String | (_String -> _?AssociationQ)) ..}
	],
	opts : OptionsPattern[]
] := With[
	{store = EntityStore[
		<|
			"Types" -> <|Replace[
				Flatten[{types}],
				type_String :> type -> <||>,
				{1}
			]|>
		|> // normalizeEntityStoreData,
		opts
	]},
	store /; If[iEntityStoreQ[store],
		True,
		Message[EntityStore::invent]
	]
];

Clear[normalizeName];
normalizeName[(Entity | EntityClass | EntityProperty | EntityPropertyClass)[_, name_String]] := name;
normalizeName[x_] := EntityFramework`NormalizeQualifiers[x];

Clear[normalizeTypeData];
normalizeTypeData[data : KeyValuePattern[entry : "Entities" | "EntityClasses" | "Properties" | "PropertyClasses" -> ent : {___String}]] := ReplacePart[
	data,
	entry -> AssociationThread[ent, ConstantArray[<||>, Length[ent]]]
];
normalizeTypeData[data : KeyValuePattern[entry : "Properties" -> prop : {(_String | {_String, _?EntityFramework`QualifierQ}) ..}]] := ReplacePart[
	data,
	entry -> AssociationThread[EntityFramework`NormalizeQualifiers /@ prop, ConstantArray[<||>, Length[prop]]]
];
normalizeTypeData[data : KeyValuePattern[entry : "Entities" | "EntityClasses" | "Properties" | "PropertyClasses" -> HoldPattern[ent_Dataset]]] := ReplacePart[
	data,
	entry -> Normal[ent]
];
normalizeTypeData[data : KeyValuePattern[entry : "Entities" | "EntityClasses" | "Properties" | "PropertyClasses" -> ent : <|(_ -> _?AssociationQ) ..|>]] := With[
	{
		newEnt = ent
			// KeyMap[normalizeName]
			// Map[KeyMap[normalizeName]]
	},
	ReplacePart[
		data,
		entry -> newEnt
	] /; newEnt =!= ent
];
normalizeTypeData[data : KeyValuePattern["Entities" -> ent_?(validEntryQ["Entities", #] &)]] := Module[
	{d = data},
	DeleteDuplicates[Flatten[Keys[Values[ent]], 1]] // Replace[
		Except[{}, l_] :> (d["Properties"] = Merge[
			{
				Lookup[data, "Properties", <||>],
				Function[# -> <||>] /@ l
			},
			First
		])
	];
	d /; d =!= data
];
normalizeTypeData[data : KeyValuePattern["EntityClasses" -> _?(validEntryQ["EntityClasses", #] &)]] := Module[
	{d = data},
	DeleteDuplicates[Flatten[Cases[
		d["EntityClasses"],
		KeyValuePattern["Entities" -> e : {(_String | _Entity) ..}] :> Cases[e, name_String | Entity[_, name_String] :> name]
	], 1]] // Replace[
		Except[{}, l_] :> (d["Entities"] = Merge[
			{
				Lookup[d, "Entities", <||>],
				Function[# -> <||>] /@ l
			},
			First
		])
	];
	d /; d =!= data
];
normalizeTypeData[data : KeyValuePattern["PropertyClasses" -> _?(validEntryQ["PropertyClasses", #] &)]] := Module[
	{d = data},
	DeleteDuplicates[Flatten[Cases[
		d["PropertyClasses"],
		KeyValuePattern["Properties" -> p : {(_String | _EntityProperty) ..}] :> Cases[p, name_String | EntityProperty[_, name_String] :> name]
	], 1]] // Replace[
		Except[{}, l_] :> (d["Properties"] = Merge[
			{
				Lookup[d, "Properties", <||>],
				Function[# -> <||>] /@ l
			},
			First
		])
	];
	d /; d =!= data
];
normalizeTypeData[x_] := x;

Clear[normalizeEntityStoreData];
normalizeEntityStoreData[data : KeyValuePattern["Types" -> types_?AssociationQ]] := Module[
	{d = data},
	d["Types"] = FixedPoint[normalizeTypeData, #] & /@ types;
	d
];
normalizeEntityStoreData[x_] := x;

(* end normalization *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* validation *)

Clear[validEntryQ];
validEntryQ[_, <||>] := True;
validEntryQ["Entities", entry_] := MatchQ[entry, <|(_String -> _?AssociationQ) ..|>];
validEntryQ["EntityClasses", entry_] := MatchQ[entry, <|(_String -> _?AssociationQ) ..|>];
validEntryQ["Properties", entry_] := MatchQ[entry, <|(_String | {_String, _?EntityFramework`QualifierQ} -> _?AssociationQ) ..|>];
validEntryQ["PropertyClasses", entry_] := MatchQ[entry, <|(_String -> _?AssociationQ) ..|>];
validEntryQ[_, _] := False;

Clear[validTypesQ];
validTypesQ[assoc : <|(_String -> _?AssociationQ) ..|>] := AllTrue[
	assoc,
	Function[typeData,
		AllTrue[
			{"Entities", "EntityClasses", "Properties", "PropertyClasses"},
			Function[entry,
				Or[
					KeyFreeQ[typeData, entry],
					validEntryQ[entry, typeData[entry]]
				]
			]
		]
	]
];
validTypesQ[_] := False;

Clear[iEntityStoreQ];
iEntityStoreQ[EntityStore[assoc : <|(_String -> _) ...|>, OptionsPattern[]]] := Or[
	KeyFreeQ[assoc, "Types"],
	validTypesQ[assoc["Types"]]
];
iEntityStoreQ[_] := False;
iEntityStoreQ[___] := $Failed;

(* end validation *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* get data *)

$namePattern = _String | (List | EntityFramework`Qualified)[_String, _?EntityFramework`QualifierQ];

Clear[makeMissing];
makeMissing[type_String] := Missing["UnknownType", type];
makeMissing[head_[type_, name_String]] := Missing[toMissingName[head], {type, name}];
makeMissing[head_[type_, EntityFramework`Qualified[name_String, qual_]]] := Missing[toMissingName[head], {type, name, If[Length[qual] === 0, Nothing, EntityFramework`NormalizeQualifiers[qual]]}];
makeMissing[head_[type_, name_, qual_]] := makeMissing[head[type, EntityFramework`Qualified[name, qual]]];

Clear[toMissingName];
toMissingName[Entity] := "UnknownEntity";
toMissingName[EntityClass] := "UnknownEntityClass";
toMissingName[EntityProperty] := "UnknownProperty";
toMissingName[EntityPropertyClass] := "UnknownPropertyClass";

Clear[toEntry];
toEntry[Entity] := "Entities";
toEntry[EntityClass] := "EntityClasses";
toEntry[EntityProperty] := "Properties";
toEntry[EntityPropertyClass] := "PropertyClasses";


Clear[iGetEntityStoreData];

(* initialization *)
iGetEntityStoreData[EntityStore[data_, opts : OptionsPattern[] /; {opts} =!= {}], args___] := (
	OptionValue[EntityStore, {opts}, Initialization, Once];
	iGetEntityStoreData[EntityStore[data], args]
);

(* semantic *)

(* type list *)
iGetEntityStoreData[EntityStore[data_?AssociationQ]] := Keys[data[["Types"]]];

(* type properties *)
iGetEntityStoreData[EntityStore[data_?AssociationQ], Entity[type_String], prop_] := With[
	{res = data[["Types", type]]},
	If[AssociationQ[res],
		lookup[res, prop, If[MatchQ[prop, "Entities" | "EntityClasses" | "Properties" | "PropertyClasses"], <||>, Missing["NotAvailable"]]],
		makeMissing[type]
	]
];
iGetEntityStoreData[store_EntityStore, Entity[type_String]] := iGetEntityStoreData[store, {"Types", EntityFramework`DefaultValue[type, makeMissing[type]]}];
iGetEntityStoreData[store_, type_String, rest___] := iGetEntityStoreData[store, Entity[type], rest];

(* entity - property *)
iGetEntityStoreData[
	store_,
	ent : _Entity | {___Entity},
	prop : EntityProperty[_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]]
] := EntityFramework`EvaluateEntityEntityProperty[store, ent, prop] // Replace[_EntityFramework`EvaluateEntityEntityProperty :> fail[]];

(* features *)
Function[{patt, ev, args},
	If[MemberQ[args, 1],
		iGetEntityStoreData[
			store_,
			ent : patt | _List?(MemberQ[patt]),
			prop_
		] := ev[iGetEntityStoreData[store, ##] &, ent, prop] // Replace[Blank[ev] :> fail[]]
	];
	If[MemberQ[args, 2],
		iGetEntityStoreData[
			store_,
			ent_,
			prop : patt | _List?(MemberQ[patt])
		] := ev[iGetEntityStoreData[store, ##] &, ent, prop] // Replace[Blank[ev] :> fail[]]
	]
] @@@ {
	{_EntityFramework`EntityPropertySequence, EntityFramework`EvaluateEntityPropertySequence, {2}},
	{EntityProperty[_String, _?EntityFramework`ConditionQ], EntityFramework`EvaluateCondition, {2}},
	{_Missing, EntityFramework`EvaluateMissing, {1, 2}}
};


(* special behavior *)
(* fall back to type-level properties *)
iGetEntityStoreData[
	store_EntityStore,
	prop : EntityProperty[type_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]],
	sub : "AggregationHandler" | "DefaultFunction" | "QueryHandler"
] := iGetEntityStoreData[
	store,
	prop,
	EntityFramework`DefaultValue[
		sub,
		Replace[
			iGetEntityStoreData[store, type, sub],
			Except[_Missing, f_] :> f[prop]
		]
	]
];
(* default values for "Entities" and "Properties" *)
iGetEntityStoreData[store_EntityStore, class_EntityClass, prop : "Entities"] := iGetEntityStoreData[store, class, EntityFramework`DefaultValue[prop, {}]];
iGetEntityStoreData[store_EntityStore, class_EntityPropertyClass, prop : "Properties"] := iGetEntityStoreData[store, class, EntityFramework`DefaultValue[prop, {}]];

(* fast path *)
iGetEntityStoreData[_, _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass, {}] := {};
iGetEntityStoreData[
	store : EntityStore[data_],
	ent : (head : Entity | EntityClass | EntityProperty | EntityPropertyClass)[
		type_String,
		name_String
	],
	prop : _String | {__String}
] := With[
	{res = data[["Types", type, toEntry[head], name]]},
	If[ListQ[prop],
		Replace[
			Values[res[[prop]]],
			Missing["KeyAbsent", _] :> Missing["NotAvailable"],
			{1}
		],
		If[KeyExistsQ[res, prop],
			res[[prop]],
			Missing["NotAvailable"]
		]
	] /; AssociationQ[res]
];

(* list of objects *)
iGetEntityStoreData[_, {}, _String] := {};
iGetEntityStoreData[
	store_EntityStore,
	ent : {(head : Entity | EntityClass | EntityProperty | EntityPropertyClass)[type_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]] ..},
	prop_String
] := iGetEntityStoreData[store, #, prop] & /@ ent;

(* list of properties *)
iGetEntityStoreData[
	store_EntityStore,
	ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass,
	Except[{_String, _?EntityFramework`QualifierQ}, prop_List]
] := iGetEntityStoreData[store, ent, #] & /@ prop;

iGetEntityStoreData[
	store_EntityStore,
	EntityProperty[type_String, name_String, qual_?EntityFramework`QualifierQ],
	prop : Repeated[_, {0, 1}]
] := iGetEntityStoreData[
	store,
	EntityProperty[type, EntityFramework`Qualified[name, qual]],
	prop
];
iGetEntityStoreData[
	store_EntityStore,
	ent : (head : Entity | EntityClass | EntityProperty | EntityPropertyClass)[
		type_String,
		name : $namePattern
	],
	prop : Repeated[_, {0, 1}]
] := iGetEntityStoreData[
	store,
	EntityFramework`DefaultValue[ent, makeMissing[ent]],
	prop
];
iGetEntityStoreData[
	store_EntityStore,
	EntityFramework`DefaultValue[
		(head : Entity | EntityClass | EntityProperty | EntityPropertyClass)[
			type_String,
			name : $namePattern
		],
		default_
	],
	prop : Repeated[$namePattern | EntityFramework`DefaultValue[$namePattern, _] | EntityFramework`DefaultKey[$namePattern, _], {0, 1}]
] := iGetEntityStoreData[
	store,
	{
		"Types",
		EntityFramework`DefaultValue[type, makeMissing[type]],
		EntityFramework`DefaultValue[toEntry[head], default],
		EntityFramework`DefaultValue[name, default],
		prop
	}
];

(* end semantic *)


(* structural *)

iGetEntityStoreData[
	EntityStore[data_?AssociationQ],
	prop : {($namePattern | EntityFramework`DefaultValue[$namePattern, _] | EntityFramework`DefaultKey[$namePattern, _]) ...}
] := Module[{tag}, Catch[Fold[
	Function[{d, p},
		If[AssociationQ[d],
			If[MatchQ[p, _EntityFramework`DefaultValue],
				lookup[d, p[[1]], Throw[p[[2]], tag]],
				lookup[d, p]
			],
			Throw[Missing["NotAvailable"], tag]
		]
	],
	data,
	prop
], tag]];

Clear[lookup];
SetAttributes[lookup, HoldAllComplete];

lookup[assoc_, prop_String, default_ : Missing["NotAvailable"]] := Lookup[assoc, prop, default];
lookup[assoc_, EntityFramework`Qualified[prop_, {} | <||>], rest___] := lookup[assoc, prop, rest];
lookup[assoc_, EntityFramework`DefaultKey[prop_, key_], rest___] := lookup[assoc, prop, lookup[assoc, key, rest]];
lookup[assoc_, EntityFramework`DefaultValue[prop_, default_], ___] := lookup[assoc, prop, default];
lookup[assoc_, prop_List, default_ : Missing["NotAvailable"]] := Lookup[assoc, Key[EntityFramework`NormalizeQualifiers[prop]], default];
lookup[assoc_, prop_EntityFramework`Qualified, default_ : Missing["NotAvailable"]] := Replace[
	EntityFramework`NormalizeQualifiers[List @@ prop],
	Normal[Append[assoc, _ :> default]]
];
lookup[assoc_, prop_, rest___] := With[
	{p = prop},
	lookup[assoc, p, rest] /; p =!= Unevaluated[prop]
];
lookup[___] := fail[];

(* end structural *)

iGetEntityStoreData[___] := fail[];

(* end get data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* set data *)

Language`SetMutationHandler[EntityStore, EntityStoreHandler];
SetAttributes[EntityStoreHandler, HoldAllComplete];

Clear[EntityStoreHandler];
EntityStoreHandler[Set[s_Symbol[args___], value_]] := With[
	{res = iSetEntityStoreData[s, args, value]},
	(
		s = res;
		value
	) /; res =!= $Failed
];

Clear[iSetEntityStoreData];


(* semantic *)

(* fast path *)
iSetEntityStoreData[
	store : EntityStore[data_, opts : OptionsPattern[]],
	Entity[type_String, name_String],
	prop_String,
	value_
] := With[
	{t = data[["Types", type]]},
	Module[
		{d = data},
		d[["Types", type, "Entities", name, prop]] = value;
		EntityStore[d, opts]
	] /; AssociationQ[t] && AssociationQ[t[["Properties", prop]]] && AssociationQ[t[["Entities", name]]]
];

(* introduce new members from classes *)
iSetEntityStoreData[
	store_EntityStore,
	(head : EntityClass | EntityPropertyClass)[type_String, name_String],
	prop : "Entities" | "Properties",
	value_List?(MemberQ[_String])
] := Module[
	{s = store},
	s = iSetEntityStoreData[
		s,
		{"Types", type, prop},
		Module[
			{e = store[EntityFramework`DefaultValue[#, <||>] & /@ {"Types", type, prop}]},
			If[! KeyExistsQ[e, #],
				AssociateTo[e, # -> <||>]
			] & /@ Cases[value, _String];
			e
		]
	];
	s = iSetEntityStoreData[
		s,
		{"Types", type, toEntry[head], name, prop},
		value
	];
	s
];
(* introduce new properties *)
iSetEntityStoreData[
	store_EntityStore,
	(head : Entity)[type_String, name_String],
	prop : _String | {_String, _?EntityFramework`QualifierQ},
	value_
] := Module[
	{s = store},
	Module[
		{
			p = s[EntityFramework`DefaultValue[#, <||>] & /@ {"Types", type, "Properties"}],
			nprop = EntityFramework`NormalizeQualifiers[prop]
		},
		If[! KeyExistsQ[p, nprop],
			AssociateTo[p, nprop -> <||>];
			s = iSetEntityStoreData[s, {"Types", type, "Properties"}, p]
		];
	];
	s = iSetEntityStoreData[s, {"Types", type, toEntry[head], name, prop}, value];
	s
];
iSetEntityStoreData[
	store_EntityStore,
	ent_Entity,
	EntityProperty[_, prop_String, Optional[qual_?EntityFramework`QualifierQ, {}]],
	value_
] := iSetEntityStoreData[store, ent, {prop, qual}, value];
iSetEntityStoreData[
	store_EntityStore,
	(head : Entity | EntityClass | EntityProperty | EntityPropertyClass)[type_String, name : _String | {_String, _?EntityFramework`QualifierQ}],
	prop : _String | {_String, _?EntityFramework`QualifierQ},
	value_
] := iSetEntityStoreData[
	store,
	{"Types", type, toEntry[head], name, prop},
	value
];
iSetEntityStoreData[store_EntityStore, type_String, prop : _String | {_String, _?EntityFramework`QualifierQ}, value_] := iSetEntityStoreData[store, {"Types", type, prop}, value];

(* end semantic *)


(* structural *)

iSetEntityStoreData[EntityStore[_Association, opts : OptionsPattern[]], {}, newData_?AssociationQ] := EntityStore[newData, opts];
iSetEntityStoreData[EntityStore[data_?AssociationQ, opts : OptionsPattern[]], prop : {(_String | {_String, _?EntityFramework`QualifierQ}) ..}, newData_] := Module[
	{
		d = ConstantArray[<||>, Length[prop]],
		nprop = EntityFramework`NormalizeQualifiers /@ prop,
		tag
	},
	d[[1]] = data;
	Catch[
		Do[
			d[[i + 1]] = Lookup[d[[i]], Key[nprop[[i]]], Throw[Null, tag]],
			{i, Length[prop] - 1}
		],
		tag
	];
	d[[Length[prop], Key[Last[nprop]]]] = newData;
	Do[
		d[[i, Key[nprop[[i]]]]] = d[[i + 1]],
		{i, Length[prop] - 1, 1, -1}
	];
	EntityStore[d[[1]], opts]
];

(* end structural *)

iSetEntityStoreData[___] := $Failed;

(* end set data *)
(* -------------------------------------------------- *)


End[];
