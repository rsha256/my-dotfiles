Begin["EntityFramework`General`Private`"];

(* -------------------------------------------------- *)
(* exported symbols *)

EntityFramework`GeneralEntityValue[args___] := With[
	{res = Catch[iGeneralEntityValue[propagateMissing[args]], $tag]},
	res /; res =!= $Failed
];

(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


$specialProperties = "Source";

Clear[propagateMissing];
propagateMissing[x___, m : Missing[Except["Propagated"], ___], y___] := propagateMissing[x, Missing["Propagated", m], y];
propagateMissing[x___, l_List?(MemberQ[Missing[Except["Propagated"], ___]]), y___] := propagateMissing[x, Replace[l, m : Missing[Except["Propagated"], ___] :> Missing["Propagated", m], {1}], y];
propagateMissing[args___] := Sequence[args];

Clear[replaceValid];
replaceValid[value_, rule_RuleDelayed] := Replace[
	value,
	{
		rule,
		If[MatchQ[rule, _[_List, _]],
			{___, m_Missing, ___} :> m,
			m_Missing :> m
		],
		_ :> fail[]
	}
];


Clear[iGeneralEntityValue];


(* type list *)
iGeneralEntityValue[] := With[{
	custom = EntityFramework`CustomEntityValue[],
	default = EntityFramework`DefaultEntityValue[]
},
	If[! AllTrue[{custom, default}, ListQ],
		fail[]
	];
	Union[custom, default]
];


(* optimization *)
iGeneralEntityValue[EntityClass[type_, {}, crest___], rest___] := iGeneralEntityValue[EntityClass[type, All, crest], rest];


(* lists *)
iGeneralEntityValue[class_, "Entities"] := EntityList[class] // Replace[_EntityList :> fail[]];
iGeneralEntityValue[class_, "EntityCanonicalNames"] := iGeneralEntityValue[class, "Entities"] // Replace[l_List :> l[[All, 2]]];

iGeneralEntityValue[type_, "EntityClasses"] := EntityClassList[type] // Replace[_EntityClassList :> fail[]];
iGeneralEntityValue[type_, "EntityClassCanonicalNames"] := iGeneralEntityValue[type, "EntityClasses"] // Replace[l_List :> l[[All, 2]]];
iGeneralEntityValue[type_, "EntityClassCount"] := iGeneralEntityValue[type, "EntityClasses"] // Replace[l_List :> Length[l]];

iGeneralEntityValue[class_, "Properties"] := EntityProperties[class] // Replace[_EntityProperties :> fail[]];
iGeneralEntityValue[class_, "PropertyCanonicalNames"] := iGeneralEntityValue[class, "Properties"] // Replace[l_List :> l[[All, 2]]];
iGeneralEntityValue[class_, "PropertyCount"] := iGeneralEntityValue[class, "Properties"] // Replace[l_List :> Length[l]];

iGeneralEntityValue[type_, "PropertyClasses"] := EntityFramework`EntityPropertyClassList[type] // Replace[_EntityFramework`EntityPropertyClassList :> fail[]];
iGeneralEntityValue[type_, "PropertyClassCanonicalNames"] := iGeneralEntityValue[type, "PropertyClasses"] // Replace[l_List :> l[[All, 2]]];
iGeneralEntityValue[type_, "PropertyClassCount"] := iGeneralEntityValue[type, "PropertyClasses"] // Replace[l_List :> Length[l]];


(* type properties *)
iGeneralEntityValue[Entity[type_?StringQ], "EntityStore"] := EntityFramework`FindEntityStore[type] // Replace[_EntityFramework`FindEntityStore :> fail[]];


(* fast path *)
iGeneralEntityValue[
	ent : Entity[type_String, _String] | {Entity[type_String, _String] ..},
	prop : EntityProperty[type_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]] | $specialProperties | {(EntityProperty[type_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]] | $specialProperties) ..}
] := iMixedEntityValue[ent, prop];
iGeneralEntityValue[
	prop : EntityProperty[type_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]] | {EntityProperty[type_String, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]] ..},
	sub : _String | {__String}
] := iMixedEntityValue[prop, sub];
iGeneralEntityValue[type : Entity[_String], prop_String] := iMixedEntityValue[type, prop];


(* special cases *)
iGeneralEntityValue[{}] := {};
iGeneralEntityValue[{}, _] := {};
iGeneralEntityValue[Except[_String, _?Internal`PossibleEntityQ | _?Internal`PossibleEntityPropertyQ], {}] := {};
iGeneralEntityValue[ent_List, {}] := ConstantArray[{}, Length[ent]];
iGeneralEntityValue[ent_List, "EntityCount"] := Length[ent];


(* type properties *)
iGeneralEntityValue[
	type_String,
	prop : Alternatives[
		"EntityStore",
		"EntityListFunction",
		"EntityValidationFunction",
		"Label",
		"LabelPlural",
		"LastUpdate",
		{"Properties" | "PropertyCanonicalNames", None | HoldPattern[_QuantityVariable] | {{_String, _Integer | _Rational} ...} | _String | _StringExpression | _RegularExpression},
		"RandomEntityClass",
		"RandomEntityClasses",
		{"RandomEntityClasses", _Integer?NonNegative},
		"SampleEntities",
		"SampleEntityClasses"
	]
] := iGeneralEntityValue[Entity[type], prop];

(* entity properties *)
iGeneralEntityValue[type_String, prop__] := iGeneralEntityValue[EntityClass[type, All], prop];


(* associations *)
iGeneralEntityValue[
	ent_,
	prop : Repeated[_, {0, 1}],
	assoc : Alternatives[
		"Association",
		"Dataset",
		"EntityAssociation",
		"EntityPropertyAssociation",
		"NonMissingEntityAssociation",
		"NonMissingPropertyAssociation",
		"PropertyAssociation",
		"PropertyEntityAssociation"
	]
] := EntityFramework`EvaluateAssociation[iGeneralEntityValue, ent, prop, assoc] // Replace[_EntityFramework`EvaluateAssociation :> fail[]];


(* non-missing properties *)
iGeneralEntityValue[
	ent_,
	prop : Repeated[_, {0, 1}],
	"NonMissingProperties"
] := EntityFramework`EvaluateNonMissingProperties[iGeneralEntityValue, ent, prop] // Replace[_EntityFramework`EvaluateNonMissingProperties :> fail[]];


(* features *)
Function[{patt, ev, args},
	If[MemberQ[args, 1],
		iGeneralEntityValue[
			ent : patt | _List?(MemberQ[patt]),
			prop_
		] := ev[iGeneralEntityValue, ent, prop] // Replace[Blank[ev] :> fail[]]
	];
	If[MemberQ[args, 2],
		iGeneralEntityValue[
			ent_,
			prop : patt | _List?(MemberQ[patt])
		] := ev[iGeneralEntityValue, ent, prop] // Replace[Blank[ev] :> fail[]]
	]
] @@@ {
	{_EntityFramework`EntityPropertySequence, EntityFramework`EvaluateEntityPropertySequence, {2}},
	{_EntityFramework`InverseEntityProperty, EntityFramework`EvaluateInverseEntityProperty, {2}},
	{_EntityFramework`Qualified, EntityFramework`EvaluateQualified, {1, 2}},
	{_Dated, EntityFramework`EvaluateDated, {1, 2}},
	{_EntityInstance, EntityFramework`EvaluateEntityInstance, {1}},
	{_Missing, EntityFramework`EvaluateMissing, {1, 2}},
	{"CanonicalName" | "Entity", EntityFramework`EvaluateSpecialProperty, {2}}
};

iGeneralEntityValue[
	ent : _Missing | _List?(MemberQ[_Missing])
] := EntityFramework`EvaluateMissing[iGeneralEntityValue, ent] // Replace[_EntityFramework`EvaluateMissing :> fail[]];


(* normalize arguments *)
iGeneralEntityValue[args__] := With[
	{n = EntityFramework`NormalizeEntityValueArguments[{args}]},
	iGeneralEntityValue @@ n /; n =!= {args}
];


(* aggregation *)

(* move aggregate functions into the property *)
iGeneralEntityValue[
	ent_?Internal`PossibleEntityListQ,
	prop : _EntityProperty | _String | {(_EntityProperty | _String) ..},
	Except[Automatic | _String | _Missing | _List, agg_]
] := With[
	{type = EntityFramework`UniqueEntityTypeName[ent]},
	iGeneralEntityValue[
		ent,
		Replace[
			prop,
			{
				EntityProperty[t_, n_, q_ : {}] :> EntityProperty[t, n, q, agg],
				n_String :> EntityProperty[type, n, {}, agg]
			},
			{If[ListQ[prop], 1, 0]}
		]
	] /; StringQ[type]
];

(* apply default aggregate function Identity *)
iGeneralEntityValue[ent_, prop : {___EntityProperty, EntityProperty[_, _, _, Except[_String, _]], ___EntityProperty}?(MemberQ[_?(Length[#] < 4 &)])] := iGeneralEntityValue[
	ent,
	Replace[
		prop,
		EntityProperty[type_, name_, qualifiers_ : {}] :> EntityProperty[type, name, qualifiers, Identity],
		{1}
	]
];

iGeneralEntityValue[ent_?Internal`PossibleEntityListQ, prop : EntityProperty[_, _, qual_, Except[_String, agg_]]] := With[
	{handler = iGeneralEntityValue[prop[[1 ;; 3]], "AggregationHandler"]},
	If[MissingQ[handler] || FailureQ[handler],
		replaceValid[
			iGeneralEntityValue[ent, prop[[1 ;; 3]]],
			l_List :> agg[l]
		],
		Replace[
			handler[agg, ent, <|qual|>],
			Automatic | Default :> replaceValid[
				iGeneralEntityValue[ent, prop[[1 ;; 3]]],
				l_List :> agg[l]
			]
		]
	]
];
iGeneralEntityValue[ent_?Internal`PossibleEntityListQ, prop : {EntityProperty[_, _, _, Except[_String, _]] ..}] := iGeneralEntityValue[ent, #] & /@ prop;

(* end aggregation *)


(* expand entity class *)
iGeneralEntityValue[
	ent : EntityClass[type_String, _],
	prop : (EntityProperty[type_String, Repeated[_, {1, 2}]] | $specialProperties) | {(EntityProperty[type_String, Repeated[_, {1, 2}]] | $specialProperties) ...}
] := replaceValid[
	EntityList[ent],
	l_List :> iGeneralEntityValue[l, prop]
];

(* expand property class *)
iGeneralEntityValue[
	ent : Entity[type_String, _] | {Entity[type_String, _] ...} | EntityClass[type_String, __],
	prop : EntityPropertyClass[type_String, _]
] := With[
	{p = EntityProperties[prop]},
	replaceValid[
		p,
		l_List :> iGeneralEntityValue[ent, l]
	] /; ! MissingQ[p]
];
iGeneralEntityValue[
	prop : EntityPropertyClass[type_String, _],
	sub : _String | {___String}
] := replaceValid[
	EntityProperties[prop],
	l_List :> iGeneralEntityValue[l, sub]
];


iGeneralEntityValue[args___] := iMixedEntityValue[args];


(* custom and default entities *)
Clear[iMixedEntityValue];

iMixedEntityValue[arg : _?StringQ | _[_?StringQ, ___] | {_[_?StringQ, ___] ..}, rest___] := With[
	{type = EntityFramework`UniqueEntityTypeName[arg]},
	If[EntityFramework`CustomEntityTypeExistsQ[type],
		EntityFramework`CustomEntityValue[arg, rest] // Replace[_EntityFramework`CustomEntityValue :> fail[]],
		EntityFramework`DefaultEntityValue[arg, rest] // Replace[_EntityFramework`DefaultEntityValue :> fail[]]
	] /; StringQ[type]
];
(* list of objects of different type *)
iMixedEntityValue[ent : {_[_?StringQ, ___] ..}, rest : Repeated[_, {0, 1}]] := Fold[
	Function[{e, p},
		ReplacePart[e, Thread[p -> iGeneralEntityValue[ent[[p]], rest]]]
	],
	ent,
	Values[PositionIndex[First /@ ent]]
];
With[{obj = Except[
	_String | _List,
	_?Internal`PossibleEntityQ | _?Internal`PossibleEntityListQ | _?Internal`PossibleEntityPropertyQ | _?Internal`PossibleEntityPropertyListQ
]},
	iMixedEntityValue[ent : obj | {obj ..}] := EntityFramework`DefaultEntityValue[ent] // Replace[_EntityFramework`DefaultEntityValue :> fail[]]
];
iMixedEntityValue[
	ent : _?Internal`PossibleEntityQ | _?Internal`PossibleEntityListQ,
	prop : _?Internal`PossibleEntityPropertyQ | _?Internal`PossibleEntityPropertyListQ,
	agg : Repeated[_, {0, 1}]
] := EntityFramework`DefaultEntityValue[ent, prop, agg] // Replace[_EntityFramework`DefaultEntityValue :> fail[]];
iMixedEntityValue[
	prop : _?Internal`PossibleEntityPropertyQ | _?Internal`PossibleEntityPropertyListQ,
	sub : _?StringQ | {__?StringQ}
] := EntityFramework`DefaultEntityValue[prop, sub] // Replace[_EntityFramework`DefaultEntityValue :> fail[]];

(* fallback: list of lists or classes *)
iMixedEntityValue[l_List, prop_] := iGeneralEntityValue[#, prop] & /@ l;

iMixedEntityValue[___] := fail[];


End[];
