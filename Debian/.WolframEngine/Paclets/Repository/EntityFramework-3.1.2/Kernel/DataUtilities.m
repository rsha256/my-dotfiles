Begin["EntityFramework`DataUtilities`Private`"];

(* -------------------------------------------------- *)
(* exported symbols *)

EntityFramework`EvaluateAssociation[args___] := With[
	{res = Catch[iEvaluateAssociation[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateDated[args___] := With[
	{res = Catch[iEvaluateDated[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateEntityEntityProperty[args___] := With[
	{res = Catch[iEvaluateEntityEntityProperty[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateEntityInstance[args___] := With[
	{res = Catch[iEvaluateEntityInstance[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateEntityPropertySequence[args___] := With[
	{res = Catch[iEvaluateEntityPropertySequence[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateInverseEntityProperty[args___] := With[
	{res = Catch[iEvaluateInverseEntityProperty[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateMissing[args___] := With[
	{res = Catch[iEvaluateMissing[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateNonMissingProperties[args___] := With[
	{res = Catch[iEvaluateNonMissingProperties[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateParameterizedEntity[args___] := With[
	{res = Catch[iEvaluateParameterizedEntity[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateQualified[args___] := With[
	{res = Catch[iEvaluateQualified[args], $tag]},
	res /; res =!= $Failed
];
EntityFramework`EvaluateSpecialProperty[args___] := With[
	{res = Catch[iEvaluateSpecialProperty[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`FlattenEntityProperty[args___] := With[
	{res = Catch[iFlattenEntityProperty[args], $tag]},
	res /; res =!= $Failed
];

(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


(* -------------------------------------------------- *)
(* general *)

Clear[iMapCases];
iMapCases[r_Rule, expr_] := iMapCases[{r}, expr];
iMapCases[{}, expr_] := expr;
iMapCases[rules : {Rule[_, _] ..}, expr_] := Module[
	{res = expr, oldpos = {}},
	Function[{patt, f},
		With[
			(* avoid processing the same position more than once *)
			{pos = Complement[Position[expr, patt, {1}, Heads -> False][[All, 1]], oldpos]},
			If[pos =!= {},
				oldpos = Join[oldpos, pos];
				res[[pos]] = If[MatchQ[f, _EntityFramework`BatchApplied],
					f[EntityFramework`BatchList[expr[[pos]]]],
					f /@ expr[[pos]]
				]
			]
		]
	] @@@ rules;
	res
];
iMapCases[___] := $Failed;

(* end general *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* entity - property *)

Clear[makeMissing];
makeMissing[type_String | Entity[type_String]] := Missing["UnknownType", type];
makeMissing[EntityInstance[_, qual_]] := Missing["UnknownEntityInstanceQualifier", qual];
makeMissing[head_[type_, name_String]] := Missing[toMissingName[head], {type, name}];
makeMissing[head_[type_, EntityFramework`Qualified[name_String, qual_]]] := Missing[toMissingName[head], {type, name, If[Length[qual] === 0, Nothing, EntityFramework`NormalizeQualifiers[qual]]}];
makeMissing[head_[type_, name_, qual_]] := makeMissing[head[type, EntityFramework`Qualified[name, qual]]];
makeMissing[___] := fail[];

Clear[toMissingName];
toMissingName[Entity] := "UnknownEntity";
toMissingName[EntityClass] := "UnknownEntityClass";
toMissingName[EntityProperty] := "UnknownProperty";
toMissingName[EntityPropertyClass] := "UnknownPropertyClass";
toMissingName[___] := fail[];

Clear[computeValue];
computeValue[_, {}, __] := {};
computeValue[store_, ent_, prop : EntityProperty[_, _, qual : Repeated[_, {0, 1}]], {post_, baseData_ : Function[Missing["NotAvailable"]]}] := store[prop, "DefaultFunction"] // Replace[{
	_Missing :> baseData[ent, prop],
	f_ :> post[If[ListQ[ent],
		Replace[
			f,
			{
				b_EntityFramework`BatchApplied :> b @@ {
					EntityFramework`BatchList[ent],
					Replace[<|qual|>, <||> :> Nothing]
				},
				_ :> If[{qual} === {},
					f /@ ent,
					Function[f[#, <|qual|>]] /@ ent
				]
			}
		],
		If[{qual} === {},
			f[ent],
			f @@ {
				ent,
				Replace[<|qual|>, <||> :> Nothing]
			}
		]
	]]
}];

Clear[formatValue];
formatValue[_, m_Missing, _] := m;
formatValue[_, {}, _, True] := {};
formatValue[store_, value_, prop_, listQ : False | True : False] := store[prop, "FormattingFunction"] // Replace[{
	_Missing :> value,
	f_ :> If[listQ,
		Replace[
			value,
			Except[_Missing, v_] :> f[v],
			{1}
		],
		f[value]
	]
}];

validatesQ[args___] := EntityFramework`Predicates`Private`validatesQ[args];
existsQ[args___] := EntityFramework`Predicates`Private`existsQ[args];

Clear[evaluateStoreEP];
(* single entity *)
evaluateStoreEP[
	store : EntityStore[data_?AssociationQ, OptionsPattern[]],
	ent : Entity[type_String, ename_String],
	prop : EntityProperty[type_String, pname_String, qual : Repeated[_?EntityFramework`QualifierQ, {0, 1}]],
	{baseEntityQ_, basePropertyQ_, baseData_}
] := (
	If[{qual} === {},
		Replace[
			data[["Types", type, "Entities", ename, pname]],
			Except[Missing["KeyAbsent", _], value_] :> Return[formatValue[store, value, prop]]
		],
		Module[{tag}, Replace[
			store[EntityFramework`DefaultValue[#, tag] & /@ {"Types", type, "Entities", ename, EntityFramework`Qualified[pname, qual]}],
			Except[tag, value_] :> Return[formatValue[store, value, prop]]
		]]
	];
	Module[
		{tag, compute, validateCompute},
		compute := If[existsQ[store, prop],
			computeValue[store, ent, prop, {formatValue[store, #, prop] &, baseData}],
			If[basePropertyQ[prop],
				baseData[ent, prop],
				makeMissing[prop]
			]
		];
		validateCompute := If[validatesQ[store, ent],
			compute,
			If[baseEntityQ[ent],
				If[basePropertyQ[prop],
					baseData[ent, prop],
					makeMissing[prop]
				],
				makeMissing[ent]
			]
		];
		Catch[
			store[{
				"Types",
				EntityFramework`DefaultValue[type, Throw[makeMissing[type], tag]],
				EntityFramework`DefaultValue["Entities", Throw[validateCompute, tag]],
				EntityFramework`DefaultValue[ename, Throw[validateCompute, tag]],
				EntityFramework`DefaultValue[If[{qual} === {}, pname, EntityFramework`Qualified[pname, qual]], Throw[compute, tag]]
			}] // formatValue[store, #, prop] &,
			tag
		]
	]
);
(* list of entities *)
evaluateStoreEP[_, {}, _EntityProperty, _] := {};
evaluateStoreEP[
	store : EntityStore[data_?AssociationQ, OptionsPattern[]],
	ent : {Entity[type_String, _String] ..},
	prop : EntityProperty[type_String, pname_String, qual : Repeated[_?EntityFramework`QualifierQ, {0, 1}]],
	{baseEntityQ_, basePropertyQ_, baseData_}
] := (
	If[! EntityFramework`EntityTypeExistsQ[store, type],
		Return[ConstantArray[makeMissing[type], Length[ent]]]
	];
	If[! existsQ[store, prop],
		If[basePropertyQ[prop],
			Module[
				{res = ConstantArray[Null, Length[ent]]},
				With[
					{baseValidPos = Pick[Range[Length[ent]], baseEntityQ[ent]]},
					res[[baseValidPos]] = baseData[ent[[baseValidPos]], prop];
					With[
						{invPos = Complement[Range[Length[ent]], baseValidPos]},
						res[[invPos]] = makeMissing /@ ent[[invPos]]
					]
				];
				Return[res]
			]
		];
		Module[
			{res = ConstantArray[makeMissing[prop], Length[ent]]},
			With[
				{pos = Pick[
					Range[Length[ent]],
					And @@@ Transpose[{Not /@ existsQ[store, ent], Not /@ baseEntityQ[ent]}]
				]},
				res[[pos]] = makeMissing /@ ent[[pos]]
			];
			Return[res]
		]
	];
	Module[
		{res, validNoDataPos, invPos},
		res = data[["Types", type, "Entities"]];
		If[AssociationQ[res],
			Module[
				{listedEntPos = All, dataPos = All},
				res = Lookup[res, ent[[All, 2]], listedEntPos = Null; Missing["KeyAbsent", Null]];
				If[listedEntPos === Null,
					listedEntPos = Position[res, Except[Missing["KeyAbsent", _]], {1}, Heads -> False][[All, 1]]
				];
				With[
					{p = EntityFramework`NormalizeQualifiers[{pname, qual}]},
					res[[listedEntPos]] = If[
						Or[
							StringQ[p],
							And[
								(* the property exist verbatim *)
								AssociationQ[data[["Types", type, "Properties", Key[p]]]],
								(* no other property matches this property *)
								KeyFreeQ[
									data[["Types", type, "Properties"]],
									Except[Verbatim[p], other_ /; MatchQ[p, other]]
								]
							]
						],
						Lookup[res[[listedEntPos]], Key[p], dataPos = Null; Missing["KeyAbsent", Null]],
						Replace[
							p,
							Normal[res[[listedEntPos]]] // Map[Append[_ :> (dataPos = Null; Missing["KeyAbsent", Null])]]
						]
					]
				];
				If[listedEntPos === All,
					If[dataPos === All,
						Return[formatValue[store, res, prop, True]],
						dataPos = Position[res, Except[Missing["KeyAbsent", _]], {1}, Heads -> False][[All, 1]];
						validNoDataPos = Complement[Range[Length[ent]], dataPos];
						invPos = {}
					],
					With[
						{unlistedEntPos = Complement[Range[Length[ent]], listedEntPos]},
						validNoDataPos = Pick[
							unlistedEntPos,
							validatesQ[store, ent[[unlistedEntPos]]]
						];
						invPos = Complement[unlistedEntPos, validNoDataPos]
					];
					If[dataPos =!= All,
						dataPos = Pick[listedEntPos, MatchQ[Except[Missing["KeyAbsent", _]]] /@ res[[listedEntPos]]];
						validNoDataPos = Join[validNoDataPos, Complement[listedEntPos, dataPos]]
					]
				];
				res[[dataPos]] = formatValue[store, res[[dataPos]], prop, True]
			],
			res = ConstantArray[Null, Length[ent]];
			validNoDataPos = Pick[
				Range[Length[ent]],
				validatesQ[store, ent]
			];
			invPos = Complement[Range[Length[ent]], validNoDataPos]
		];
		res[[validNoDataPos]] = computeValue[store, ent[[validNoDataPos]], prop, {formatValue[store, #, prop, True] &, baseData}];
		With[
			{baseValidPos = Pick[invPos, baseEntityQ[ent[[invPos]]]]},
			res[[baseValidPos]] = baseData[ent[[baseValidPos]], prop];
			invPos = Complement[invPos, baseValidPos]
		];
		res[[invPos]] = makeMissing /@ ent[[invPos]];
		res
	]
);
evaluateStoreEP[___] := fail[];

Clear[changeType];
changeType[Except[List, head_][_, args__], newType_] := head[newType, args];
changeType[l_List, newType_] := Module[
	{res = l},
	res[[All, 1]] = newType;
	res
];

Clear[getBaseData];
getBaseData[_, {}, _] := {};
getBaseData[store_, ent : Entity[type_, _] | {Entity[type_, _] ..}, prop_] := With[
	{base = store[Entity[type], "BaseEntityType"]},
	If[StringQ[base],
		Replace[
			store[changeType[ent, base], changeType[prop, base]],
			Missing["UnknownEntity" | "UnknownProperty", ___] :> Missing["NotAvailable"],
			{Boole[ListQ[ent]]}
		],
		ConstantArray[Missing["NotAvailable"], If[ListQ[ent], Length[ent], {}]]
	]
];

Clear[baseFunctions];
baseFunctions[store_] := {
	EntityFramework`EntityExistsQ[store, #] &,
	EntityFramework`EntityPropertyExistsQ[store, #] &,
	getBaseData[store, ##] &
};

Clear[extendsQ];
extendsQ[store_, type_String, possibleBase_String] := With[
	{base = store[Entity[type], "BaseEntityType"]},
	StringQ[base] && (base === possibleBase || extendsQ[store, base, possibleBase])
];

Clear[iEvaluateEntityEntityProperty];
iEvaluateEntityEntityProperty[_, {}, _EntityProperty] := {};
iEvaluateEntityEntityProperty[store_EntityStore, ent : Entity[type_String, _] | {Entity[type_String, _] ..}, prop : EntityProperty[type_String, __]] := evaluateStoreEP[store, ent, prop, baseFunctions[store]];
iEvaluateEntityEntityProperty[ev : EntityValue, ent : Entity[type_String, _] | {Entity[type_String, _] ..}, prop : EntityProperty[type_String, __]] := EntityFramework`FindEntityStore[type] // Replace[{
	store_EntityStore :> evaluateStoreEP[store, ent, prop, baseFunctions[ev]],
	_ :> Replace[EntityFramework`DefaultEntityValue[ent, prop], _EntityFramework`DefaultEntityValue :> fail[]]
}];
iEvaluateEntityEntityProperty[store_, ent : Entity[etype_String, _] | {Entity[etype_String, _] ..}, prop : EntityProperty[ptype_String, __]] /; etype =!= ptype := If[
	extendsQ[store, ptype, etype],
	Replace[
		iEvaluateEntityEntityProperty[store, changeType[ent, ptype], prop],
		Missing["UnknownEntity", {_, e_}] :> Missing["UnknownEntity", {etype, e}],
		{Boole[ListQ[ent]]}
	],
	ConstantArray[Missing["IncompatibleTypes", {etype, ptype}], If[ListQ[ent], Length[ent], {}]]
];
iEvaluateEntityEntityProperty[___] := fail[];

(* end entity - property *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* parameterized entity *)

Clear[iEvaluateParameterizedEntity];

iEvaluateParameterizedEntity[store_, ent : EntityFramework`ParameterizedEntity[type_String, cond_?EntityFramework`ConditionQ], prop_EntityProperty] := Lookup[
	EntityFramework`NormalizeConditions[cond, type],
	prop,
	If[existsQ[store, prop],
		computeValue[store, ent, prop, {formatValue[store, #, prop] &}],
		makeMissing[prop]
	]
];
iEvaluateParameterizedEntity[store_, ent_List?(MemberQ[_EntityFramework`ParameterizedEntity]), prop_EntityProperty] := Module[
	{res, pos},
	res = ConstantArray[Null, Length[ent]];
	pos = Position[ent, _EntityFramework`ParameterizedEntity, {1}, Heads -> False][[All, 1]];
	res[[pos]] = iEvaluateParameterizedEntity[store, #, prop] & /@ ent[[pos]];
	pos = Complement[Range[Length[ent]], pos];
	res[[pos]] = store[ent[[pos]], prop];
	res
];

iEvaluateParameterizedEntity[___] := fail[];

(* end parameterized entity *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* property sequence *)

Clear[iEvaluateEntityPropertySequence];

(* single property sequence *)
(* empty *)
iEvaluateEntityPropertySequence[store_, ent_, EntityFramework`EntityPropertySequence[]] := If[Internal`PossibleEntityListQ[ent],
	EntityList[ent] // Replace[_EntityList :> fail[]],
	ent
];
(* trivial *)
iEvaluateEntityPropertySequence[store_, ent_, EntityFramework`EntityPropertySequence[prop_]] := store[ent, prop];
(* long *)
iEvaluateEntityPropertySequence[store_, ent_, prop : EntityFramework`EntityPropertySequence[_, __]] := store[
	Replace[
		store[ent, First[prop]],
		Except[_Missing | _?Internal`PossibleEntityQ, inv_] :> Missing["InvalidForeignKey", inv],
		{Boole[Internal`PossibleEntityListQ[ent]]}
	],
	Rest[prop]
];

(* list containing property sequence *)
(* empty *)
iEvaluateEntityPropertySequence[store_, ent : _?Internal`PossibleEntityQ | _?Internal`PossibleEntityListQ, prop_List?(MemberQ[EntityFramework`EntityPropertySequence[]])] := Module[
	{res, listQ},
	listQ = ! Internal`PossibleEntityQ[ent];
	res = ConstantArray[
		If[listQ,
			EntityList[ent] // Replace[{
				m_Missing :> Return[m, Module],
				_EntityList :> fail[]
			}],
			ent
		],
		Length[prop]
	];
	With[
		{pos = Position[prop, Except[EntityFramework`EntityPropertySequence[]], {1}, Heads -> False][[All, 1]]},
		If[pos =!= {},
			res[[pos]] = If[listQ, Transpose, # &][store[ent, prop[[pos]]]]
		]
	];
	If[listQ,
		res = Transpose[res]
	];
	res
];
(* trivial *)
iEvaluateEntityPropertySequence[store_, ent_, prop_List?(MemberQ[EntityFramework`EntityPropertySequence[_]])] := store[ent, Replace[prop, EntityFramework`EntityPropertySequence[p_] :> p, {1}]];
(* long *)
iEvaluateEntityPropertySequence[store_, ent_?Internal`PossibleEntityQ, prop_List?(MemberQ[EntityFramework`EntityPropertySequence[_, __]])] := Module[
	{res},
	res = store[ent, Replace[prop, EntityFramework`EntityPropertySequence[p_, __] :> p, {1}]];
	With[
		{pos = Position[prop, EntityFramework`EntityPropertySequence[_, __], {1}, Heads -> False][[All, 1]]},
		res[[pos]] = Replace[
			Transpose[{res[[pos]], prop[[pos]]}],
			{
				{e_?Internal`PossibleEntityQ, ps_} :> store[e, Rest[ps]],
				{m_Missing, _} :> m,
				{inv_, _} :> Missing["InvalidForeignKey", inv]
			},
			{1}
		]
	];
	res
];
iEvaluateEntityPropertySequence[store_, ent_?Internal`PossibleEntityListQ, prop_List?(MemberQ[EntityFramework`EntityPropertySequence[_, __]])] := Transpose[store[ent, #] & /@ prop];

iEvaluateEntityPropertySequence[___] := fail[];

(* end property sequence *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* inverse property *)

Clear[iEvaluateInverseEntityProperty];

iEvaluateInverseEntityProperty[store_, ent_?Internal`PossibleEntityQ, prop : EntityFramework`InverseEntityProperty[type_?StringQ, _?StringQ, Repeated[_?EntityFramework`QualifierQ, {0, 1}]]] := If[
	EntityFramework`EntityExistsQ[ent],
	If[
		EntityFramework`EntityPropertyExistsQ[EntityProperty @@ prop],
		Keys[Select[
			store[EntityClass[type, All], EntityProperty @@ prop, "EntityAssociation"],
			MatchQ[ent | _List?(MemberQ[ent])]
		]],
		makeMissing[EntityProperty @@ prop]
	],
	makeMissing[ent]
];
iEvaluateInverseEntityProperty[store_, ent_?Internal`PossibleEntityListQ, prop_EntityFramework`InverseEntityProperty] := Replace[
	expand[ent],
	l_List :> Function[iEvaluateInverseEntityProperty[store, #, prop]] /@ l
];
iEvaluateInverseEntityProperty[store_, ent_, prop_List?(MemberQ[_EntityFramework`InverseEntityProperty])] := If[
	Internal`PossibleEntityListQ[ent],
	Replace[{
		{(m_?classMissingQ) ..} :> m,
		l_List :> Transpose[l]
	}],
	Identity
][store[ent, #] & /@ prop];

iEvaluateInverseEntityProperty[___] := fail[];

(* end inverse property *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* flatten property *)

Clear[iFlattenEntityProperty];

iFlattenEntityProperty[store_, prop_, type_?StringQ] := FixedPoint[
	Replace[{
		Dated[p_, d_] :> dateQualify[store, p, d],
		q_EntityFramework`Qualified :> flattenProperty[q]
	}],
	typeProperty[prop, type]
];

iFlattenEntityProperty[___] := fail[];

(* end flatten property *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* qualified *)

Clear[flattenProperty];
flattenProperty[prop_EntityProperty] := prop;
flattenProperty[EntityFramework`Qualified[EntityProperty[type_String, name_String, Optional[qual1_?EntityFramework`QualifierQ, {}], rest___], qual2_]] := EntityProperty[type, name, mergeQualifiers[qual1, qual2], rest];
flattenProperty[wrapper_[prop_, rest___]] := wrapper[flattenProperty[prop], rest];
flattenProperty[___] := fail[];

Clear[mergeQualifiers];
mergeQualifiers[qual1_, qual2_] := Merge[{qual1, qual2}, First] // Normal;

Clear[optionallyQualify];
optionallyQualify[store_, prop_, qual_] := MapThread[
	Function[{p, validQuals},
		If[MatchQ[validQuals, {___String}],
			EntityFramework`Qualified[p, KeyTake[qual, validQuals]],
			Switch[EntityFramework`EntityPropertyExistsQ[flattenProperty[EntityFramework`Qualified[p, qual]]],
				True, EntityFramework`Qualified[p, qual],
				False, p,
				_, fail[]
			]
		]
	],
	{
		prop,
		store[prop, "Qualifiers"]
	},
	Boole[ListQ[prop]]
];

Clear[typeProperty];
typeProperty[pname_String, type_String] := EntityProperty[type, pname];
typeProperty[prop_EntityProperty, _] := prop;
typeProperty[wrapper_[prop_, rest___], type_] := wrapper[typeProperty[prop, type], rest];
typeProperty[___] := fail[];

Clear[iEvaluateQualified];

(* qualified property - second argument *)
iEvaluateQualified[
	store_,
	ent_,
	prop : EntityFramework`Qualified[_, _?EntityFramework`QualifierQ] | _List?(MemberQ[EntityFramework`Qualified[_, _?EntityFramework`QualifierQ]])
] := EntityFramework`UniqueEntityTypeName[ent] // Replace[{
	type_String :> Which[
		Internal`PossibleEntityPropertyListQ[prop],
		store[ent, Replace[EntityProperties[prop], p : EntityFramework`Qualified[_, _?EntityFramework`QualifierQ] :> flattenProperty[typeProperty[p, type]], {1}]],
		Internal`PossibleEntityPropertyQ[prop],
		store[ent, flattenProperty[typeProperty[prop, type]]],
		True,
		fail[]
	],
	_ :> If[Internal`PossibleEntityListQ[ent],
		EntityList[ent] // Replace[{
			l_List :> Function[store[#, prop]] /@ l,
			_EntityList :> fail[]
		}],
		fail[]
	]
}];

(* qualified property - first argument *)
iEvaluateQualified[
	store_,
	prop : EntityFramework`Qualified[_, _?EntityFramework`QualifierQ] | _List?(MemberQ[EntityFramework`Qualified[_, _?EntityFramework`QualifierQ]]) /; Internal`PossibleEntityPropertyQ[prop] || Internal`PossibleEntityPropertyListQ[prop],
	sub : _String | {__String}
] := If[Internal`PossibleEntityPropertyQ[prop],
	store[flattenProperty[prop], sub],
	store[Replace[EntityProperties[prop], p : EntityFramework`Qualified[_, _?EntityFramework`QualifierQ] :> flattenProperty[p], {1}], sub]
];

(* qualified entity *)
iEvaluateQualified[
	store_,
	EntityFramework`Qualified[ent_, qual_?EntityFramework`QualifierQ],
	prop_
] := EntityFramework`UniqueEntityTypeName[ent] // Replace[{
	type_String :> Which[
		Internal`PossibleEntityPropertyListQ[prop],
		store[ent, Replace[EntityProperties[prop], l_List :> optionallyQualify[store, typeProperty[#, type] & /@ l, qual]]],
		Internal`PossibleEntityPropertyQ[prop],
		store[ent, optionallyQualify[store, typeProperty[prop, type], qual]],
		True,
		fail[]
	],
	_ :> If[Internal`PossibleEntityListQ[ent],
		EntityList[EntityFramework`Qualified[ent, qual]] // Replace[{
			l_List :> Function[store[#, prop]] /@ l,
			_EntityList :> fail[]
		}],
		fail[]
	]
}];
iEvaluateQualified[
	store_,
	ent_List?(MemberQ[EntityFramework`Qualified[_, _?EntityFramework`QualifierQ]]),
	prop_
] := store[#, prop] & /@ ent;

iEvaluateQualified[___] := fail[];

(* end qualified *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* dated *)

Clear[dateQ];
dateQ[All] := True;
dateQ[date_] := IntegerQ[date] || DateObjectQ[date];

Clear[dateQualify];
dateQualify[store_, prop_, date_] := EntityFramework`Qualified[
	prop,
	Replace[
		store[prop, "DateQualifier"],
		Except[_String] :> "Date"
	] -> normalizeDate[date]
];

Clear[normalizeDate];
normalizeDate[year_Integer] := DateObject[{year}];
normalizeDate[date_] := date;

Clear[optionallyDate];
optionallyDate[store_, prop_, date_] := store[prop, "DateQualifier"] // Replace[{
	dq_String :> EntityFramework`Qualified[prop, dq -> normalizeDate[date]],
	None :> prop,
	_ :> optionallyQualify[store, prop, "Date" -> normalizeDate[date]]
}];

Clear[iEvaluateDated];

(* dated property - second argument *)
iEvaluateDated[
	store_,
	ent_,
	prop : Dated[_, _?dateQ] | _List?(MemberQ[Dated[_, _?dateQ]])
] := EntityFramework`UniqueEntityTypeName[ent] // Replace[{
	type_String :> Which[
		Internal`PossibleEntityPropertyListQ[prop],
		store[ent, Replace[EntityProperties[prop], Dated[p_, d_?dateQ] :> dateQualify[store, typeProperty[p, type], d], {1}]],
		Internal`PossibleEntityPropertyQ[prop],
		store[ent, dateQualify[store, typeProperty[First[prop], type], prop[[2]]]],
		True,
		fail[]
	],
	_ :> If[Internal`PossibleEntityListQ[ent],
		EntityList[ent] // Replace[{
			l_List :> Function[store[#, prop]] /@ l,
			_EntityList :> fail[]
		}],
		fail[]
	]
}];

(* dated property - first argument *)
iEvaluateDated[
	store_,
	prop : Dated[Except[_String], _?dateQ] | _List?(MemberQ[Dated[Except[_String], _?dateQ]]) /; Internal`PossibleEntityPropertyQ[prop] || Internal`PossibleEntityPropertyListQ[prop],
	sub : _String | {__String}
] := If[Internal`PossibleEntityPropertyQ[prop],
	store[dateQualify[store, First[prop], prop[[2]]], sub],
	store[Replace[EntityProperties[prop], Dated[Except[_String, p_], d_?dateQ] :> dateQualify[store, p, d], {1}], sub]
];

(* dated entity *)
iEvaluateDated[
	store_,
	Dated[ent_, date_?dateQ],
	prop_
] := EntityFramework`UniqueEntityTypeName[ent] // Replace[{
	type_String :> Which[
		Internal`PossibleEntityPropertyListQ[prop],
		store[ent, Replace[EntityProperties[prop], l_List :> Function[optionallyDate[store, typeProperty[#, type], date]] /@ l]],
		Internal`PossibleEntityPropertyQ[prop],
		store[ent, optionallyDate[store, typeProperty[prop, type], date]],
		True,
		fail[]
	],
	_ :> If[Internal`PossibleEntityListQ[ent],
		EntityList[Dated[ent, date]] // Replace[{
			l_List :> Function[store[#, prop]] /@ l,
			_EntityList :> fail[]
		}],
		fail[]
	]
}];
iEvaluateDated[
	store_,
	ent_List?(MemberQ[Dated[_, _?dateQ]]),
	prop_
] := store[#, prop] & /@ ent;

iEvaluateDated[___] := fail[];

(* end dated *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* entity instance *)

Clear[rulePattern];
rulePattern[lhs_] := With[
	{rule = (Rule | RuleDelayed)[lhs, _]},
	rule | {rule ...} | <|rule ...|>
];

Clear[toRuleList];
toRuleList[a_?AssociationQ] := Normal[a];
toRuleList[r : _Rule | _RuleDelayed] := {r};
toRuleList[x_] := x;

Clear[iEvaluateEntityInstance];

(* qualifiers, date *)
iEvaluateEntityInstance[store_, EntityInstance[ent_, qual_?EntityFramework`QualifierQ], prop_] := store[EntityFramework`Qualified[ent, qual], prop];
iEvaluateEntityInstance[store_, EntityInstance[ent_, date_?DateObjectQ], prop_] := store[Dated[ent, date], prop];
iEvaluateEntityInstance[store_, ent_List?(MemberQ[EntityInstance[_, _?EntityFramework`QualifierQ | _?DateObjectQ]]), prop_] := store[
	Replace[
		ent,
		{
			EntityInstance[e_, qual_?EntityFramework`QualifierQ] :> EntityFramework`Qualified[e, qual],
			EntityInstance[e_, date_?DateObjectQ] :> Dated[e, date]
		},
		{1}
	],
	prop
];

(* function parameters *)
Clear[applyFunctionParameter];
applyFunctionParameter[x_, None] := x;
applyFunctionParameter[l_List, param_] := applyFunctionParameter[#, param] & /@ l;
applyFunctionParameter[HoldPattern[Function][x_Symbol, body_], param_] := applyFunctionParameter[Function[{x}, body], param];
applyFunctionParameter[HoldPattern[Function][l_List, body_], param_] := With[
	{rules = FilterRules[param, l]},
	With[
		{rest = DeleteCases[l, Alternatives @@ rules[[All, 1]]]},
		If[rest === {},
			body /. rules,
			Function @@ (Hold[rest, body] /. rules)
		]
	]
];
applyFunctionParameter[x_, _] := x;

iEvaluateEntityInstance[store_, EntityInstance[ent_, param : rulePattern[_Symbol]], prop_] := applyFunctionParameter[store[ent, prop], param];
iEvaluateEntityInstance[store_, ent_List?(MemberQ[EntityInstance[_, rulePattern[_Symbol]]]), prop_] := MapThread[
	applyFunctionParameter,
	{
		store[
			Replace[ent, EntityInstance[e_, rulePattern[_Symbol]] :> e, {1}],
			prop
		],
		Replace[ent, {EntityInstance[_, p : rulePattern[_Symbol]] :> p, _ :> None}, {1}]
	}
];

(* quantity variables *)
iEvaluateEntityInstance[store_, EntityInstance[ent_, qvar : rulePattern[HoldPattern[QuantityVariable][_, _String]]], prop_] := ReplaceAll[store[ent, prop], qvar];
iEvaluateEntityInstance[store_, ent_List?(MemberQ[EntityInstance[ent_, qvar : rulePattern[HoldPattern[QuantityVariable][_, _String]]]]), prop_] := MapThread[
	Compose,
	{
		Replace[ent, {EntityInstance[_, q : rulePattern[HoldPattern[QuantityVariable][_, _String]]] :> ReplaceAll[q], _ :> (# &)}, {1}],
		store[
			Replace[ent, EntityInstance[e_, rulePattern[HoldPattern[QuantityVariable][_, _String]]] :> e, {1}],
			prop
		]
	}
];

(* quantity *)
iEvaluateEntityInstance[
	store_,
	ent : EntityInstance[_, HoldPattern[_Quantity]] | _List?(MemberQ[EntityInstance[_, HoldPattern[_Quantity]]]),
	prop_
] := EntityFramework`DefaultEntityValue[ent, prop] // Replace[_EntityFramework`DefaultEntityValue :> fail[]];

(* list of mixed rules *)
iEvaluateEntityInstance[
	store_,
	EntityInstance[ent_, rules : rulePattern[_]],
	prop_
] := With[
	{rulegroups = GatherBy[toRuleList[rules], First /* Head]},
	store[
		Fold[EntityInstance, ent, rulegroups],
		prop
	] /; Length[rulegroups] > 1
];

(* validation *)
iEvaluateEntityInstance[store_, ent : EntityInstance[_, _] | _List?(MemberQ[EntityInstance[_, _]]), prop_] := store[
	Replace[
		ent,
		ei : EntityInstance[_, _] :> makeMissing[ei],
		{Boole[ListQ[ent]]}
	],
	prop
];

iEvaluateEntityInstance[___] := fail[];

(* end entity instance *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* associations *)

expand[l_List] := l;
expand[class : _EntityClass | _String] := EntityList[class] // Replace[_EntityList :> fail[]];
expand[class_EntityPropertyClass] := EntityProperties[class] // Replace[_EntityProperties :> fail[]];
expand[_] := fail[];

Clear[possibleListQ];
possibleListQ[x_] := Internal`PossibleEntityListQ[x] || Internal`PossibleEntityPropertyListQ[x];

Clear[iEvaluateAssociation];

(* association *)
iEvaluateAssociation[store_, ent_, prop : EntityProperty[_, _, _, Except[_String, _]] | _List?(MemberQ[EntityProperty[_, _, _, Except[_String, _]]]), "Association"] := If[ListQ[prop],
	AssociationThread[prop, store[ent, prop]],
	store[ent, prop]
];
iEvaluateAssociation[store_, ent_?possibleListQ, prop_, "Association"] := Which[
	Internal`PossibleEntityPropertyListQ[prop],
	Replace[
		expand[ent],
		e_List :> AssociationThread[e, iEvaluateAssociation[store, ent, prop, "PropertyAssociation"]]
	],
	Internal`PossibleEntityPropertyQ[prop],
	iEvaluateAssociation[store, ent, prop, "EntityAssociation"],
	True,
	fail[]
];
iEvaluateAssociation[store_, ent : _?Internal`PossibleEntityQ | _?Internal`PossibleEntityPropertyQ, prop_, "Association"] := Which[
	Internal`PossibleEntityPropertyListQ[prop],
	iEvaluateAssociation[store, ent, prop, "PropertyAssociation"],
	Internal`PossibleEntityPropertyQ[prop],
	store[ent, prop],
	True,
	fail[]
];

(* entity association *)
iEvaluateAssociation[store_, ent_?possibleListQ, prop_, "EntityAssociation"] := Replace[
	expand[ent],
	l_List :> AssociationThread[l, store[ent, prop]]
];

(* property association *)
iEvaluateAssociation[store_, ent_, prop_?Internal`PossibleEntityPropertyListQ, "PropertyAssociation"] := Replace[
	store[ent, prop],
	l_List :> With[
		{p = expand[prop]},
		Map[
			If[ListQ[p],
				AssociationThread[p, #] &,
				p &
			],
			l,
			{Boole[possibleListQ[ent]]}
		]
	]
];

(* entity-property association *)
iEvaluateAssociation[store_, ent_?possibleListQ, prop_?possibleListQ, "EntityPropertyAssociation"] := iEvaluateAssociation[store, ent, prop, "Association"];

(* property-entity association *)
iEvaluateAssociation[store_, ent_?possibleListQ, prop_?possibleListQ, "PropertyEntityAssociation"] := Replace[
	expand[prop],
	p_List :> AssociationThread[
		p,
		Replace[
			expand[ent],
			{
				e_List :> Function[v,
					AssociationThread[e, v]
				] /@ Transpose[store[ent, prop]],
				e_ :> ConstantArray[e, Length[p]]
			}
		]
	]
];

(* dataset *)
iEvaluateAssociation[store_, ent_, prop_, "Dataset"] := Dataset[store[ent, prop, "Association"]];

(* non-missing entity association *)
iEvaluateAssociation[store_, ent_?possibleListQ, prop_?Internal`PossibleEntityPropertyQ, "NonMissingEntityAssociation"] := Replace[
	store[ent, prop, "EntityAssociation"],
	a_Association :> DeleteMissing[a]
];
iEvaluateAssociation[store_, ent_?possibleListQ, prop_?Internal`PossibleEntityPropertyListQ, "NonMissingEntityAssociation"] := Replace[
	store[ent, prop, "PropertyEntityAssociation"],
	a_Association :> Replace[
		Values[a],
		b_Association :> DeleteMissing[b],
		{1}
	]
];

(* non-missing property association *)
iEvaluateAssociation[store_, ent_, prop_?Internal`PossibleEntityPropertyListQ, "NonMissingPropertyAssociation"] := Replace[
	store[ent, prop, "PropertyAssociation"],
	l : _List | _Association :> DeleteMissing[l, 1 + Boole[possibleListQ[ent]]]
];

iEvaluateAssociation[store_, ent_, assoc_String] := EntityFramework`UniqueEntityTypeName[ent] // Replace[{
	type_String :> store[ent, EntityPropertyClass[type, All], assoc],
	_ :> If[ListQ[ent],
		Switch[assoc,
			"PropertyEntityAssociation",
			fail[],
			_,
			With[
				{pi = Values[PositionIndex[EntityFramework`UniqueEntityTypeName /@ ent]]},
				Part[Join @@ Function[store[ent[[#]], assoc]] /@ pi, InversePermutation[Join @@ pi]]
			]
		],
		fail[]
	]
}];

iEvaluateAssociation[___] := fail[];

(* end associations *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* non-missing properties *)

Clear[iEvaluateNonMissingProperties];

iEvaluateNonMissingProperties[store_, ent_?Internal`PossibleEntityQ, prop_?Internal`PossibleEntityPropertyListQ] := Pick[expand[prop], Not @* MissingQ /@ store[ent, prop]];
iEvaluateNonMissingProperties[store_, ent_?Internal`PossibleEntityListQ, prop_?Internal`PossibleEntityPropertyListQ] := store[ent, prop] // Replace[
	_List :> With[
		{p = expand[prop]},
		Function[data,
			Pick[p, Not @* MissingQ /@ data]
		] /@ store[ent, prop]
	]
];
iEvaluateNonMissingProperties[store_, ent_] := store[ent, EntityPropertyClass[EntityFramework`UniqueEntityTypeName[ent], All], "NonMissingProperties"];

iEvaluateNonMissingProperties[___] := fail[];

(* end non-missing properties *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* missing *)

Clear[classMissingQ];
classMissingQ[Missing["UnknownEntityClass" | "UnknownPropertyClass", ___]] := True;
classMissingQ[_] := False;

Clear[iEvaluateMissing];

(* one argument *)
iEvaluateMissing[_, m_Missing] := m;
iEvaluateMissing[store_, l_List?(MemberQ[_Missing])] := iMapCases[
	{
		_Missing -> Function[#],
		_ -> EntityFramework`BatchApplied[store[#] &]
	},
	l
];

(* two arguments *)
(* missing in the first argument *)
iEvaluateMissing[store_, m_Missing, prop_] := If[classMissingQ[m],
	m,
	Which[
		ListQ[prop],
		ConstantArray[m, Length[prop]],
		Internal`PossibleEntityPropertyListQ[prop],
		store[prop, "PropertyCount"] // Replace[{
			i_Integer :> ConstantArray[m, i],
			_ :> m
		}],
		True,
		m
	]
];
iEvaluateMissing[store_, l_List?(MemberQ[_Missing]), prop_] := iMapCases[
	{
		_Missing -> Function[m, iEvaluateMissing[store, m, prop]],
		_ -> EntityFramework`BatchApplied[store[#, prop] &]
	},
	l
];

(* missing in the second argument *)
iEvaluateMissing[store_, ent_, m_Missing] := Which[
	ListQ[ent],
	Replace[
		store[ent],
		Except[_Missing] :> m,
		{1}
	],
	Internal`PossibleEntityListQ[ent],
	store[ent, "EntityCount"] // Replace[
		i_Integer :> ConstantArray[m, i]
	],
	Internal`PossibleEntityPropertyListQ[ent],
	store[ent, "PropertyCount"] // Replace[
		i_Integer :> ConstantArray[m, i]
	],
	True,
	m
];
iEvaluateMissing[store_, ent_, l_List?(MemberQ[_Missing])] := If[possibleListQ[ent],
	expand[ent] // Replace[
		el_List :> Transpose[iMapCases[
			{
				_Missing -> Function[m, iEvaluateMissing[store, ent, m]],
				_ -> EntityFramework`BatchApplied[Transpose[store[ent, #]] &]
			},
			l
		]]
	],
	iMapCases[
		{
			_Missing -> Function[m, iEvaluateMissing[store, ent, m]],
			_ -> EntityFramework`BatchApplied[store[ent, #] &]
		},
		l
	]
];

iEvaluateMissing[___] := fail[];

(* end missing *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* special property *)

Clear[iEvaluateSpecialProperty];

(* canonical name *)
iEvaluateSpecialProperty[store_, ent_, "CanonicalName"] := iEvaluateSpecialProperty[store, ent, "Entity"] // Replace[
	Except[_Missing, x_] :> CanonicalName[x]
];

(* entity *)
iEvaluateSpecialProperty[_, ent : _Entity | _EntityProperty | {(_Entity | _EntityProperty) ...}, "Entity"] := ent;
iEvaluateSpecialProperty[store_, ent : _EntityClass | EntityPropertyClass, "Entity"] := store[expand[ent], "Entity"];

iEvaluateSpecialProperty[store_, ent_, prop_List?(MemberQ["CanonicalName" | "Entity"])] := If[possibleListQ[ent],
	expand[ent] // Replace[
		l_List :> Transpose[store[l, #] & /@ prop]
	],
	store[ent, #] & /@ prop
];

iEvaluateSpecialProperty[___] := fail[];

(* end special property *)
(* -------------------------------------------------- *)


End[];
