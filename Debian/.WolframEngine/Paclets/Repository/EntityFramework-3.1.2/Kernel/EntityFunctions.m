Begin["EntityFramework`EntityFunctions`Private`"];


(* -------------------------------------------------- *)
(* exported symbols *)

CanonicalName[(Entity | EntityClass | EntityProperty | EntityPropertyClass)[_, name_, ___]] := name;
CanonicalName[l_List] := CanonicalName /@ l;
CanonicalName[args___] := (ArgumentCountQ[CanonicalName, Length[{args}], 1, 1]; Null /; False);
CanonicalName[arg : Except[_Entity | _EntityClass | _EntityProperty | _EntityPropertyClass | _List]] := (Message[CanonicalName::noent, arg]; Null /; False);


Options[CommonName] = {
	Language :> CurrentValue[Language]
};

CommonName[args___] := Block[{EntityFramework`$SendWAEvents = False},
		With[{res = Catch[iMixedCommonName[args], $tag]},
			res /; res =!= $Failed
		]
];


EntityTypeName[type_String] := type;
EntityTypeName[(Entity | EntityClass | EntityProperty | EntityPropertyClass)[type_String, ___]] := type;
EntityTypeName[EntityInstance[ent_Entity, ___]] := EntityTypeName[ent];
EntityTypeName[l_List] := EntityTypeName /@ l;
EntityTypeName[args___] := (ArgumentCountQ[EntityTypeName, Length[{args}], 1, 1]; Null /; False);
EntityTypeName[arg : Except[_Entity | _EntityClass | _EntityProperty | _EntityPropertyClass | _List]] := (Message[EntityTypeName::noent, arg]; Null /; False);


RandomEntity[args___] := With[
	{res = Catch[iRandomEntity[args], $tag]},
	res /; res =!= $Failed
];


EntityFramework`$CommonNameBatchSize = 2500;


(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


(* -------------------------------------------------- *)
(* common name *)

Clear[iMixedCommonName];

iMixedCommonName[{}, ___] := {};
iMixedCommonName[ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass, opts : OptionsPattern[]] := Which[
	EntityFramework`CustomEntityTypeExistsQ[EntityTypeName[ent]],
	EntityFramework`CustomCommonName[ent, opts] // Replace[_EntityFramework`CustomCommonName :> fail[]],

	EntityFramework`DefaultEntityTypeExistsQ[EntityTypeName[ent]] // Replace[Except[True | False] :> fail[]],
	iCommonNameUsingCache[ent, Sequence @@ FilterRules[{opts}, "SubLabel"]],

	True,
	Missing["UnknownType", EntityTypeName[ent]]
];
iMixedCommonName[ent : {(_Entity | _EntityClass | _EntityProperty | _EntityPropertyClass) ..}, opts : OptionsPattern[]] := Module[
	{
		res = ent,
		pi = PositionIndex[ent]
	},
	With[
		{custom = PositionIndex[EntityFramework`CustomEntityTypeExistsQ /@ EntityTypeName[Keys[pi]]]},
		If[KeyExistsQ[custom, True],
			With[
				{c = pi[[custom[True]]]},
				res[[Join @@ Values[c]]] = Join @@ MapThread[
					ConstantArray,
					{
						EntityFramework`CustomCommonName[Keys[c], opts] // Replace[_EntityFramework`CustomCommonName :> fail[]],
						Length /@ Values[c]
					}
				];
				pi = KeyComplement[{pi, c}]
			]
		]
	];
	With[
		{default = PositionIndex[EntityFramework`DefaultEntityTypeExistsQ /@ EntityTypeName[Keys[pi]]]},
		If[KeyMemberQ[default, Except[False | True]],
			fail[]
		];
		If[KeyExistsQ[default, True],
			With[
				{c = pi[[default[True]]]},
				res[[Join @@ Values[c]]] = Join @@ MapThread[
					ConstantArray,
					{
						iCommonNameUsingCache[Keys[c], Sequence @@ FilterRules[{opts}, "SubLabel"]],
						Length /@ Values[c]
					}
				];
				pi = KeyComplement[{pi, c}]
			]
		]
	];
	res = ReplacePart[
		res,
		KeyValueMap[Function[{e, p},
			List /@ p -> Missing["UnknownType", EntityTypeName[e]]
		], pi]
	];
	res
];

iMixedCommonName[args___, OptionsPattern[]] := (
	ArgumentCountQ[CommonName, Length[{args}], 1, 1];
	fail[]
);
iMixedCommonName[arg_, OptionsPattern[]] := (
	Message[CommonName::noent, arg];
	fail[]
);


Clear[iCommonName];

iCommonName[ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass | {(_Entity | _EntityClass | _EntityProperty | _EntityPropertyClass) ...}] := Module[
	{res},
	res = EntityFramework`GetLabels[ent];
	If[! MatchQ[res, {___Rule}],
		Return[If[ListQ[ent],
			ConstantArray[Missing["RetrievalFailure"], Length[ent]],
			Missing["RetrievalFailure"]
		]]
	];
	res = Join @@ Replace[
		res,
		{
			("Label" -> list_List) :> Function[{l, r}, First[l] @@ Rest[l] -> r] @@@ list,
			("EntityNameRules" -> list_List) :> Function[{l, r}, Entity @@ l -> r] @@@ list,
			("EntityClassNameRules" -> list_List) :> Function[{l, r}, EntityClass @@ l -> r] @@@ list,
			("PropertyClassNameRules" -> list_List) :> Function[{l, r}, EntityPropertyClass @@ l -> r] @@@ list,
			_ -> {}
		},
		{1}
	];
	res = Replace[
		ent,
		Append[res, _ :> Missing["RetrievalFailure"]],
		{Boole[ListQ[ent]]}
	];
	res
];

iCommonName[___] := fail[];


Clear[iCommonNameUsingCache];

iCommonNameUsingCache[ent_, opts : OptionsPattern[{"SubLabel" -> False}]] := Condition[
	If[TrueQ[OptionValue["SubLabel"]],
		If[ListQ[ent],
			Module[
				{res = ConstantArray[Null, Length[ent]]},
				With[
					{pos = PositionIndex[MatchQ[_Entity] /@ ent]},
					If[KeyExistsQ[pos, True],
						res[[pos[True]]] = MapThread[
							Function[{label, sub},
								If[MissingQ[sub],
									label,
									EntityFramework`Qualified[label, "SubLabel" -> sub]
								]
							],
							{
								iCommonNameUsingCache[ent[[pos[True]]]],
								EntityValue[ent[[pos[True]]], "SubLabel"]
							}
						];
					];
					If[KeyExistsQ[pos, False],
						res[[pos[False]]] = iCommonNameUsingCache[ent[[pos[False]]]];
					]
				];
				res
			],
			With[
				{label = iCommonNameUsingCache[ent]},
				If[MatchQ[ent, _Entity],
					With[
						{sub = EntityValue[ent, "SubLabel"]},
						If[MissingQ[sub],
							label,
							EntityFramework`Qualified[label, "SubLabel" -> sub]
						]
					],
					label
				]
			]
		],
		iCommonNameUsingCache[ent]
	],
	{opts} =!= {}
];

iCommonNameUsingCache[ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass | _List] := With[
	{cache = EntityFramework`CommonNameCache[
		ent,
		"CallbackFunction" -> iCommonName
	]},
	cache /; ! MatchQ[cache, _EntityFramework`CommonNameCache]
];

iCommonNameUsingCache[args___] := iCommonName[args];

(* end common name *)
(* -------------------------------------------------- *)


Clear[iRandomEntity];

iRandomEntity[class_] := Replace[
	iRandomEntity[class, 1],
	{
		{e_} :> e,
		_ :> fail[]
	}
];

iRandomEntity[class_, n_Integer?NonNegative] := Module[
	{res},
	Replace[
		EntityValue[validateClass[class], "EntityCount"],
		{
			count_Integer /; count < n :> (
				Message[RandomEntity::insfct, n, count];
				fail[]
			),
			count_ /; ! IntegerQ[count] :> (
				validateResult[count, class];
				fail[]
			)
		}
	];
	res = validateResult[EntityValue[validateClass[class], {"RandomEntities", n}], class];
	If[Length[res] =!= n,
		fail[]
	];
	res
];
iRandomEntity[class_, UpTo[n_Integer?NonNegative]] := validateResult[EntityValue[validateClass[class], {"RandomEntities", n}], class];

Clear[validateClass];
validateClass[class : _EntityClass | _String | _Entity] := class;
validateClass[other_] := (
	Message[RandomEntity::ntype, other];
	fail[]
);

Clear[validateResult];
validateResult[res : _Integer | _List, _] := res;
validateResult[Missing["UnknownType" | "UnknownEntityClass" | "UnknownEntity", __], input_] := (
	Message[RandomEntity::ntype, input];
	fail[]
);
validateResult[___] := fail[];

iRandomEntity[class_, other_] := (
	Message[RandomEntity::intnm, HoldForm[RandomEntity[class, other]], 2];
	fail[]
);

iRandomEntity[args___] := (
	System`Private`Arguments[RandomEntity[args], {1, 2}];
	fail[]
);


End[];
