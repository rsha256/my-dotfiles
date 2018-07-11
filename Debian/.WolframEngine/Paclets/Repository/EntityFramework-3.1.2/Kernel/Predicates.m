Begin["EntityFramework`Predicates`Private`"]

(* -------------------------------------------------- *)
(* exported symbols *)

EntityFramework`EntityClassExistsQ[args___] := With[
	{res = Catch[iEntityClassExistsQ[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EntityExistsQ[args___] := With[
	{res = Catch[iEntityExistsQ[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EntityPropertyClassExistsQ[args___] := With[
	{res = Catch[iEntityPropertyClassExistsQ[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EntityPropertyExistsQ[args___] := With[
	{res = Catch[iEntityPropertyExistsQ[args], $tag]},
	res /; res =!= $Failed
];

(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


(* -------------------------------------------------- *)
(* structural *)

(* possible entity *)
Internal`PossibleEntityQ[Entity[_?StringQ, _]] := True;

Internal`PossibleEntityQ[EntityFramework`AverageEntity[_?Internal`PossibleEntityListQ]] := True;
Internal`PossibleEntityQ[Dated[_?Internal`PossibleEntityQ, _]] := True;
Internal`PossibleEntityQ[EntityCopies[_?Internal`PossibleEntityQ, _]] := True;
Internal`PossibleEntityQ[EntityGroup[_?Internal`PossibleEntityListQ]] := True;
Internal`PossibleEntityQ[EntityInstance[_?Internal`PossibleEntityQ, _]] := True;
Internal`PossibleEntityQ[EntityFramework`ParameterizedEntity[_?StringQ, _?EntityFramework`ConditionQ]] := True;
Internal`PossibleEntityQ[EntityFramework`Qualified[_?Internal`PossibleEntityQ, _]] := True;
Internal`PossibleEntityQ[HoldPattern[GeoVariant][Entity[_?StringQ, _], _]] := True;

Internal`PossibleEntityQ[_] := False;

(* possible entity list *)
Internal`PossibleEntityListQ[_?StringQ] := True;
Internal`PossibleEntityListQ[{}] := True;
Internal`PossibleEntityListQ[{Entity[_?StringQ, _] ..}] := True;
Internal`PossibleEntityListQ[{__?Internal`PossibleEntityQ}] := True;
Internal`PossibleEntityListQ[EntityClass[_?StringQ, _, Repeated[All | Span[_Integer, _Integer], {0, 1}]]] := True;
Internal`PossibleEntityListQ[(Dated | HoldPattern[GeoVariant] | EntityFramework`Qualified)[_?Internal`PossibleEntityListQ, _]] := True;

Internal`PossibleEntityListQ[_] := False;

(* possible property *)
Internal`PossibleEntityPropertyQ[_?StringQ] := True;
Internal`PossibleEntityPropertyQ[EntityProperty[_?StringQ, _?StringQ, Repeated[_?EntityFramework`QualifierQ, {0, 1}]]] := True;
Internal`PossibleEntityPropertyQ[EntityProperty[_?StringQ, _?EntityFramework`ConditionQ]] := True;

Internal`PossibleEntityPropertyQ[Dated[_?Internal`PossibleEntityPropertyQ, _]] := True;
Internal`PossibleEntityPropertyQ[EntityFramework`Qualified[_?Internal`PossibleEntityPropertyQ, _]] := True;

Internal`PossibleEntityPropertyQ[EntityFramework`EntityPropertySequence[___?Internal`PossibleEntityPropertyQ]] := True;
Internal`PossibleEntityPropertyQ[EntityFramework`InverseEntityProperty[_?StringQ, _?StringQ, Repeated[_?EntityFramework`QualifierQ, {0, 1}]]] := True;

Internal`PossibleEntityPropertyQ[_] := False;

(* possible property list *)
Internal`PossibleEntityPropertyListQ[{}] := True;
Internal`PossibleEntityPropertyListQ[{EntityProperty[_?StringQ, _?StringQ] ..}] := True;
Internal`PossibleEntityPropertyListQ[{__?Internal`PossibleEntityPropertyQ}] := True;
Internal`PossibleEntityPropertyListQ[EntityPropertyClass[_?StringQ, All | _?StringQ | _?EntityFramework`ConditionQ, Repeated[All | Span[_Integer, _Integer], {0, 1}]]] := True;
Internal`PossibleEntityPropertyListQ[(Dated | HoldPattern[GeoVariant] | EntityFramework`Qualified)[_?Internal`PossibleEntityPropertyListQ, _]] := True;

Internal`PossibleEntityPropertyListQ[_] := False

(* end structural *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* exists *)

Clear[conditionExistsQ];
conditionExistsQ[store_, cond_, type_] := AllTrue[
	EntityFramework`NormalizeConditions[cond, type],
	iEntityPropertyExistsQ[store, First[#]] &
];


Clear[existsInBaseQ];
(* single *)
existsInBaseQ[store : _EntityStore | EntityValue, Except[List, head_][type_, name__], pred_] := With[
	{base = store[Entity[type], "BaseEntityType"]},
	And[
		StringQ[base],
		pred[store, head[base, name]]
	]
];
(* list *)
existsInBaseQ[_, {}, _] := {};
existsInBaseQ[store : _EntityStore | EntityValue, obj : {head_[type_, __] ..}, pred_] := With[
	{base = store[Entity[type], "BaseEntityType"]},
	If[StringQ[base],
		pred[store, ReplacePart[obj, {_, 1} -> base]],
		ConstantArray[False, Length[obj]]
	]
];


Clear[existsLiterallyQ];
existsLiterallyQ[EntityStore[data_, OptionsPattern[]], Except[List, head_][type_, name_]] := AssociationQ[data["Types", type, toEntry[head], name]];
existsLiterallyQ[store : EntityStore[data_, OptionsPattern[]], obj : {head_[type_, _] ..}] := (
	If[! EntityFramework`EntityTypeExistsQ[store, type],
		Return[ConstantArray[False, Length[obj]]]
	];
	With[
		{e = data["Types", type, toEntry[head]]},
		If[AssociationQ[e],
			KeyExistsQ[e, #] & /@ obj[[All, 2]],
			ConstantArray[False, Length[obj]]
		]
	]
);


Clear[validatesQ];
validatesQ[EntityStore[data_, OptionsPattern[]], Entity[type_, name_]] := With[
	{f = data["Types", type, "EntityValidationFunction"]},
	If[MissingQ[f],
		With[
			{g = data["Types", type, "EntityListFunction"]},
			If[MissingQ[g],
				False,
				With[
					{list = g[]},
					If[ListQ[list],
						MemberQ[list, name],
						Message[EntityList::listres, HoldForm[g[]]];
						False
					]
				]
			]
		],
		TrueQ[f[name]]
	]
];
validatesQ[EntityStore[_, OptionsPattern[]], {}] := {};
validatesQ[EntityStore[data_, OptionsPattern[]], ent : {Entity[type_, _] ..}] := With[
	{f = data["Types", type, "EntityValidationFunction"]},
	If[MissingQ[f],
		With[
			{g = data["Types", type, "EntityListFunction"]},
			If[MissingQ[g],
				ConstantArray[False, Length[ent]],
				With[
					{list = g[]},
					If[ListQ[list],
						MemberQ[list, #] & /@ ent[[All, 2]],
						Message[EntityList::listres, HoldForm[g[]]];
						ConstantArray[False, Length[ent]]
					]
				]
			]
		],
		If[MatchQ[f, _EntityFramework`BatchApplied],
			TrueQ /@ f[EntityFramework`BatchList[ent[[All, 2]]]],
			TrueQ @* f /@ ent[[All, 2]]
		]
	]
];


Clear[existsQ];

(* single *)
existsQ[store : EntityStore[data_?AssociationQ, OptionsPattern[]], obj : Entity[_?StringQ, _?StringQ]] := Or[
	existsLiterallyQ[store, obj],
	validatesQ[store, obj]
];
existsQ[store : EntityStore[data_?AssociationQ, OptionsPattern[]], obj : Except[List, head_][_?StringQ, _?StringQ]] := existsLiterallyQ[store, obj];
existsQ[store : EntityStore[data_?AssociationQ, OptionsPattern[]], Except[List, head_][type_?StringQ, name_?StringQ, qual_?EntityFramework`QualifierQ]] := With[
	{n = EntityFramework`NormalizeQualifiers[{name, qual}]},
	If[StringQ[n],
		existsQ[store, head[type, name]],
		With[
			{p = data["Types", type, toEntry[head]]},
			And[
				AssociationQ[p],
				KeyMemberQ[p, _?(MatchQ[n, #] &)]
			]
		]
	]
];
existsQ[ev : EntityValue, ent : Except[List, head_][type_?StringQ, __]] := EntityFramework`FindEntityStore[type] // Replace[{
	s_EntityStore :> existsQ[s, ent],
	_ :> Replace[
		ev[ent],
		{
			Except[_ev, x_] :> MatchQ[x, _head],
			_ :> fail[]
		}
	]
}];

(* list *)
existsQ[_, {}] := {};
existsQ[store : EntityStore[data_?AssociationQ, OptionsPattern[]], ent : {Entity[type_, _] ..}] := Module[
	{res},
	res = existsLiterallyQ[store, ent];
	With[
		{pos = Position[res, False, {1}, Heads -> False][[All, 1]]},
		res[[pos]] = validatesQ[store, ent[[pos]]]
	];
	res
];
existsQ[store : EntityStore[data_?AssociationQ, OptionsPattern[]], obj : {head_[type_, _] ..}] := existsLiterallyQ[store, obj];
existsQ[ev : EntityValue, ent : {head_[type_, _] ..}] := EntityFramework`FindEntityStore[type] // Replace[{
	s_EntityStore :> existsQ[s, ent],
	_ :> Replace[
		ev[ent],
		{
			l_List :> MatchQ[_head] /@ l,
			_ :> fail[]
		}
	]
}];

existsQ[___] := fail[];


Clear[toEntry];
toEntry[Entity] := "Entities";
toEntry[EntityClass] := "EntityClasses";
toEntry[EntityProperty] := "Properties";
toEntry[EntityPropertyClass] := "PropertyClasses";


(* type *)
EntityFramework`EntityTypeExistsQ[EntityStore[data_?AssociationQ, OptionsPattern[]], Entity[type_?StringQ] | type_?StringQ] := AssociationQ[data["Types", type]];
EntityFramework`EntityTypeExistsQ[{}, Entity[_?StringQ] | _?StringQ] := False;
EntityFramework`EntityTypeExistsQ[stores_List, Entity[type_?StringQ] | type_?StringQ] := AnyTrue[stores, EntityFramework`EntityTypeExistsQ[#, type] &];
EntityFramework`EntityTypeExistsQ[_, Entity[_?StringQ] | _?StringQ] := False; (* potentially add message about invalid store *)

EntityFramework`EntityTypeExistsQ[Entity[type_?StringQ] | type_?StringQ] := With[
	{res = Or[
		EntityFramework`CustomEntityTypeExistsQ[type],
		EntityFramework`DefaultEntityTypeExistsQ[type]
	]},
	res /; MatchQ[res, False | True]
];


(* entity *)
Clear[iEntityExistsQ];

iEntityExistsQ[store : _EntityStore | EntityValue, ent_Entity] := Or[
	existsQ[store, ent],
	existsInBaseQ[store, ent, iEntityExistsQ]
];
iEntityExistsQ[_, {}] := {};
iEntityExistsQ[store : _EntityStore | EntityValue, ent : {__Entity}] := Module[
	{res},
	res = existsQ[store, ent];
	With[
		{pos = Position[res, False, {1}, Heads -> False][[All, 1]]},
		If[pos =!= {},
			res[[pos]] = existsInBaseQ[store, ent[[pos]], iEntityExistsQ]
		]
	];
	res
];

(* in class *)
iEntityExistsQ[{store_EntityStore, class : EntityClass[type_String, _String]}, ent : Entity[type_String, name_String]] := store[class, "Entities"] // Replace[{
	Except[{}, cond_?EntityFramework`ConditionQ] :> iEntityExistsQ[{store, EntityClass[type, cond]}, ent],
	l_List :> MemberQ[l, name]
}];
iEntityExistsQ[{store_EntityStore, class : EntityClass[type_String, cond_?EntityFramework`ConditionQ]}, ent : Entity[type_String, _String]] := store[ent, EntityProperty[type, cond]];
iEntityExistsQ[{ev : EntityValue, class : EntityClass[type_String, _String]}, ent : Entity[type_String, name_]] := EntityFramework`FindEntityStore[type] // Replace[{
	store_EntityStore :> Replace[
		store[class, "Entities"],
		{
			Except[{}, cond_?EntityFramework`ConditionQ] :> iEntityExistsQ[{ev, EntityClass[type, cond]}, ent],
			l_List :> MemberQ[l, name]
		}
	],
	_ :> ev[ent, EntityProperty[type, "EntityClasses" -> ContainsAny[{class}]]]
}];
iEntityExistsQ[{ev : EntityValue, EntityClass[type_String, cond_?EntityFramework`ConditionQ]}, ent : Entity[type_String, _]] := ev[ent, EntityProperty[type, cond]];
iEntityExistsQ[{store : _EntityStore | EntityValue, EntityClass[type_String, All]}, ent : Entity[type_String, _String]] := iEntityExistsQ[store, ent];
iEntityExistsQ[{_, EntityClass[ctype_String, _]}, Entity[etype_String, _]] /; ctype =!= etype := False;

iEntityExistsQ[ent_Entity] := iEntityExistsQ[EntityValue, ent];
iEntityExistsQ[class_EntityClass, entity_Entity] := iEntityExistsQ[{EntityValue, class}, entity];

iEntityExistsQ[___] := fail[];


(* entity class *)
Clear[iEntityClassExistsQ];

iEntityClassExistsQ[_, EntityClass[_?StringQ, All]] := True;
iEntityClassExistsQ[store : _EntityStore | EntityValue, EntityClass[type_?StringQ, cond_?EntityFramework`ConditionQ]] := conditionExistsQ[store, cond, type];
iEntityClassExistsQ[store : _EntityStore | EntityValue, class_EntityClass] := Or[
	existsQ[store, class],
	existsInBaseQ[store, class, iEntityClassExistsQ]
];

iEntityClassExistsQ[class_EntityClass] := iEntityClassExistsQ[EntityValue, class];

iEntityClassExistsQ[___] := fail[];


(* property *)
Clear[iEntityPropertyExistsQ];

iEntityPropertyExistsQ[store : _EntityStore | EntityValue, EntityProperty[type_?StringQ, cond_?EntityFramework`ConditionQ]] := conditionExistsQ[store, cond, type];
iEntityPropertyExistsQ[store : _EntityStore | EntityValue, prop_EntityProperty] := Or[
	existsQ[store, prop],
	existsInBaseQ[store, prop, iEntityPropertyExistsQ]
];

(* in class *)
iEntityPropertyExistsQ[{store : _EntityStore | EntityValue, class_EntityPropertyClass}, prop_EntityProperty] := EntityProperties[{store, class}] // Replace[{
	l_List :> MemberQ[EntityFramework`NormalizeQualifiers /@ l, EntityFramework`NormalizeQualifiers[prop]],
	_ :> fail[]
}];

iEntityPropertyExistsQ[prop_EntityProperty] := iEntityPropertyExistsQ[EntityValue, prop];
iEntityPropertyExistsQ[class_EntityPropertyClass, prop_] := iEntityPropertyExistsQ[{EntityValue, class}, prop];

iEntityPropertyExistsQ[___] := fail[];


(* property class *)
Clear[iEntityPropertyClassExistsQ];

iEntityPropertyClassExistsQ[_, EntityPropertyClass[_?StringQ, All | _?EntityFramework`ConditionQ]] := True;
iEntityPropertyClassExistsQ[store : _EntityStore | EntityValue, class_EntityPropertyClass] := Or[
	existsQ[store, class],
	existsInBaseQ[store, class, iEntityPropertyClassExistsQ]
];

iEntityPropertyClassExistsQ[class : _EntityPropertyClass] := iEntityPropertyClassExistsQ[EntityValue, class];

iEntityPropertyClassExistsQ[___] := fail[];

(* end exists *)
(* -------------------------------------------------- *)

End[];
