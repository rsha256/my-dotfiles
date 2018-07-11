(* for entity[property] = value assignments see "mutation handler" in CustomEntity.m *)

(* entity-like *)
Function[head,
	ent_head[prop_String, qual_?EntityFramework`QualifierQ] := With[
		{res = EntityValue[ent, EntityFramework`Qualified[prop, qual]]},
		res /; ! MatchQ[res, _EntityValue]
	];
	ent_head[args__] := With[
		{res = EntityValue[ent, args]},
		res /; ! MatchQ[res, _EntityValue]
	],
	HoldAllComplete
] /@ Hold[
	EntityFramework`AverageEntity,
	Entity,
	EntityClass,
	EntityCopies,
	EntityGroup,
	EntityInstance,
	EntityFramework`ParameterizedEntity
] // ReleaseHold;


(* property-like *)
Function[head,
	prop_head[sub : _String | {__String}, assoc : Repeated[_String, {0, 1}]] := With[
		{res = EntityValue[prop, sub, assoc]},
		res /; ! MatchQ[res, _EntityValue]
	];
	prop_head[ent_, assoc : Repeated[_String, {0, 1}]] := With[
		{res = EntityValue[ent, prop, assoc]},
		res /; ! MatchQ[res, _EntityValue]
	],
	HoldAllComplete
] /@ Hold[
	EntityProperty,
	EntityPropertyClass,
	EntityFramework`EntityPropertySequence,
	EntityFramework`InverseEntityProperty
] // ReleaseHold;


(* wrappers *)
Function[wrapper,
	(ent : wrapper[_?Internal`PossibleEntityQ | _?Internal`PossibleEntityListQ, _])[args__] := With[
		{res = EntityValue[ent, args]},
		res /; ! MatchQ[res, _EntityValue]
	];
	(prop : wrapper[_?Internal`PossibleEntityPropertyQ | _?Internal`PossibleEntityPropertyListQ, _])[sub : _String | {__String}] := With[
		{res = EntityValue[prop, sub]},
		res /; ! MatchQ[res, _EntityValue]
	];
	(prop : wrapper[_?Internal`PossibleEntityPropertyQ | _?Internal`PossibleEntityPropertyListQ, _])[ent_] := With[
		{res = EntityValue[ent, prop]},
		res /; ! MatchQ[res, _EntityValue]
	]
] /@ {
	Dated,
	EntityFramework`Qualified
};
