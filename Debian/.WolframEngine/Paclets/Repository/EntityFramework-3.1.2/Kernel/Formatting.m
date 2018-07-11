
Begin["EntityFramework`Formatting`Private`"];


Function[head,
	head /: MakeBoxes[x_head, fmt_] := (
		Entity;
		With[
			{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]},
			boxes /; boxes =!= $Failed
		]
	),
	HoldAllComplete
] /@ Hold[
	Entity,
	EntityClass,
	EntityProperty,
	EntityPropertyClass
] // ReleaseHold;


$formatEntity = True;

getetypeLabel[type_String] := Block[{EntityFramework`$SendWAEvents = False}, EntityValue[Entity[type], "Label"] // Replace[
	Except[_String] :> type
]];
geteclasstypeLabel[type_String] := Block[{EntityFramework`$SendWAEvents = False}, EntityValue[Entity[type], "LabelPlural"] // Replace[
	Except[_String] :> type
]];

makeTooltip[e_] := With[
	{str = ToString[e, InputForm]},
	MakeBoxes[str, StandardForm]
];

MakeTypesetBoxes = Function[Null, Block[{BoxForm`UseTextFormattingQ = False}, MakeBoxes[##]], HoldAllComplete];

EntityFramework`Private`GetEntityName[e_, timeout_] := CommonName[e] /. _Missing :> $Failed

Clear[EntityFramework`MakeEntityFrameworkBoxes];
SetAttributes[EntityFramework`MakeEntityFrameworkBoxes, HoldAllComplete];

(* standard form *)
EntityFramework`MakeEntityFrameworkBoxes[
	ent : Alternatives[
		Entity[_String, Except[_?EntityFramework`ConditionQ, _String | _List | _Integer]],
		EntityClass[_String, Except[_?EntityFramework`ConditionQ, _String | All | _List]],
		EntityProperty[_String, _String],
		EntityPropertyClass[_String, _String]
	],
	StandardForm,
	l_ : Automatic
] /; $formatEntity := Block[{EntityFramework`$CacheUpdateCheck = False, EntityFramework`$ValidateTypes = False, $UnitSystem = "Metric"},
	With[
		{label = Replace[l, Automatic :> getLabel[ent]]},
		TemplateBox[
			{
				MakeTypesetBoxes[label, StandardForm],
				Block[{$formatEntity = False}, MakeBoxes[ent, StandardForm]],
				makeTooltip[ent],
				Switch[Head[ent],
					Entity, ToBoxes[getetypeLabel[EntityTypeName[ent]], StandardForm],
					EntityClass, ToBoxes[geteclasstypeLabel[EntityTypeName[ent]], StandardForm],
					_, Nothing
				]
			},
			ToString[Head[ent]]
		] /; ! MatchQ[label, _Missing | _CommonName]
	]
];

(* traditional form *)
EntityFramework`MakeEntityFrameworkBoxes[
	ent : Alternatives[
		Entity[_String, Except[_?EntityFramework`ConditionQ, _String | _List | _Integer]],
		EntityClass[_String, Except[_?EntityFramework`ConditionQ, _String | All | _List]],
		EntityProperty[_String, _String],
		EntityPropertyClass[_String, _String]
	],
	TraditionalForm,
	l_ : Automatic
] /; $formatEntity := With[
	{label = Replace[l, Automatic :> getLabel[ent]]},
	With[{
		boxes = MakeTypesetBoxes[label, TraditionalForm],
		style = ToString[Head[ent]]
	},
		InterpretationBox[boxes, ent, BaseStyle -> style]
	] /; ! MatchQ[label, _Missing | _CommonName]
];

(* qualified property *)
EntityFramework`MakeEntityFrameworkBoxes[EntityProperty[type_String, name_String, qual_ /; Length[Unevaluated[qual]] === 0], rest__] := EntityFramework`MakeEntityFrameworkBoxes[EntityProperty[type, name], rest];

EntityFramework`MakeEntityFrameworkBoxes[
	prop : EntityProperty[_String, _String, _?EntityFramework`QualifierQ],
	fmt_
] /; $formatEntity && BoxForm`sufficientVersionQ[11.2] := With[{res = Catch[With[{
	base = makeQualifiedPropertyBaseBoxes[prop, fmt],
	rest = makeEntityPropertyInfoBoxes[prop, fmt],
	tooltip = makeTooltip[prop],
	qvallist = makeQualifierValueList[prop, fmt]
},
	InterpretationBox[
		DynamicModuleBox[
			{open = False},
			TemplateBox[{base, rest, Dynamic[open], tooltip, qvallist}, "ImplicitEntityPropertyToggle"]
		],
		prop,
		SelectWithContents -> True
	]
], $tag]}, res /; res =!= $unevaluatedTag]

(* implicitly defined *)
EntityFramework`MakeEntityFrameworkBoxes[
	ent : (Entity | EntityClass)[_String, _?EntityFramework`ConditionQ],
	fmt_
] /; $formatEntity && BoxForm`sufficientVersionQ[11.2] := With[{
	base = makeGeneralizedEntityBaseBoxes[ent, fmt],
	rest = makeGeneralizedEntityInfoBoxes[ent, fmt],
	tooltip = makeTooltip[ent]
},
	InterpretationBox[
		DynamicModuleBox[
			{open = False},
			TemplateBox[{base, rest, Dynamic[open], tooltip}, "ImplicitEntityToggle"]
		],
		ent,
		SelectWithContents -> True
	]
];

Clear[getLabel];
getLabel[ent_] := Quiet[
	CommonName[ent, "SubLabel" -> True],
	{EntityValue::conopen, EntityValue::nodat, EntityValue::outdcache}
] // Replace[{
	_CommonName :> ConstantArray[
		Missing["RetrievalFailure"],
		If[ListQ[ent], Length[ent], {}]
	],
	c_ :> MapThread[
		Function[{l, e},
			Replace[
				l,
				Missing["NotAvailable"] | Missing["UnknownProperty", {_, "Label"}] :> CanonicalName[e]
			]
		],
		{
			Map[
				formatDisambiguation,
				c,
				{Boole[ListQ[ent]]}
			],
			ent
		},
		Boole[ListQ[ent]]
	]
}];

Clear[formatDisambiguation];
formatDisambiguation[EntityFramework`Qualified[value_, (Rule | RuleDelayed)["SubLabel", d_] | KeyValuePattern[(Rule | RuleDelayed)["SubLabel", d_]]]] := Row[{
	value,
	Style[
		Row[{"(", d, ")"}],
		Gray,
		FontWeight -> "Plain"
	]
}, " "];
formatDisambiguation[x_] := x;

makeQualifiedPropertyBaseBoxes[prop_, fmt_] := With[{label = getLabel[prop]},
	If[MatchQ[label, _Missing | _CommonName], 
		Throw[$unevaluatedTag, $tag],
		TemplateBox[{MakeBoxes[label, fmt]}, "ImplicitEntityPropertyBase"]
	]
]

SetAttributes[{
	makeEntityPropertyInfoBoxes,
	makeQualifierValueList,

	makeGeneralizedEntityBaseBoxes,
	makeGeneralizedEntityInfoBoxes,
	makeGeneralizedEntityInfoRow,
	makeGeneralizedEntityInfoRowLHSBoxes,
	makeGeneralizedEntityInfoRowRHSBoxes,
	makeContainsSequenceBoxes,
	makeQualifiedPropertyBaseBoxes
}, HoldAllComplete];


makeEntityPropertyInfoBoxes[prop : _[_, _, qual_], fmt_] := Module[
	{qualifiers, qualifierValues},
	{qualifiers, qualifierValues} = Transpose[List @@@ EntityFramework`NormalizeQualifiers[qual]];
	With[
		{l = prop["QualifierValueLabels"]},
		If[MatchQ[l, KeyValuePattern[{}]],
			qualifierValues = Replace[
				Thread[{qualifiers, qualifierValues}],
				Append[Replace[l, a_Association :> Normal[a]], {_, v_} :> v],
				{1}
			]
		]
	];
	With[
		{l = prop["QualifierLabels"]},
		If[MatchQ[l, KeyValuePattern[{}]],
			qualifiers = Replace[qualifiers, l, {1}]
		]
	];
	Block[
		{BoxForm`UseTextFormattingQ = False},
		ToBoxes[
			Column[
				RawBoxes /@ MapThread[
					makeEntityPropertyInfoRow[Rule[##], fmt] &,
					{qualifiers, qualifierValues}
				],
				DefaultBaseStyle -> "GeneralizedEntityInfoGrid",
				Spacings -> Automatic
			],
			fmt
		]
	]
];

makeQualifierValueList[prop : _[_, _, qual_], fmt_] := Module[
	{qualifiers, qualifierValues},
	{qualifiers, qualifierValues} = Transpose[List @@@ EntityFramework`NormalizeQualifiers[qual]];
	With[
		{l = prop["QualifierValueLabels"]},
		If[MatchQ[l, KeyValuePattern[{}]],
			qualifierValues = Replace[
				Thread[{qualifiers, qualifierValues}],
				Append[Replace[l, a_Association :> Normal[a]], {_, v_} :> v],
				{1}
			]
		]
	];
	TemplateBox[
		Map[MakeBoxes[#, fmt]&, qualifierValues],
		"ImplicitEntityPropertySequence"
	]
];

makeEntityPropertyInfoRow[(Rule | RuleDelayed)[label_, value_], fmt_] := TemplateBox[
	{
		RowBox[{" ",MakeBoxes[label, fmt]}],
		"\":\"",
		MakeBoxes[value, fmt]
	},
	"ImplicitEntityInfoRow"
];


makeGeneralizedEntityBaseBoxes[head_[type_, ___], fmt_] := TemplateBox[
	{With[{label = geteclasstypeLabel[type]}, MakeBoxes[label, fmt]]},
	Switch[head,
		Entity, "ImplicitEntityBase",
		EntityClass, "ImplicitEntityClassBase"
	]
];

makeGeneralizedEntityInfoBoxes[_[etype_, cond_List], fmt_] := Block[
	{BoxForm`UseTextFormattingQ = False},
	Module[{boxes},
		boxes = Map[
			Function[x, makeGeneralizedEntityInfoRow[x, fmt], HoldAllComplete],
			Unevaluated[cond]
		];
		ToBoxes[Column[RawBoxes /@ boxes, DefaultBaseStyle -> "GeneralizedEntityInfoGrid"], fmt]
	]
];
makeGeneralizedEntityInfoBoxes[head_[type_, cond_], fmt_] := With[
	{c = EntityFramework`NormalizeConditions[cond]},
	makeGeneralizedEntityInfoBoxes[head[type, c], fmt]
];

makeGeneralizedEntityInfoRowLHSBoxes[e_EntityProperty, fmt_] := With[{boxes = MakeBoxes[e, fmt]}, 
	If[MatchQ[boxes, TemplateBox[_, "EntityProperty"]],
		ReplacePart[boxes, -1 -> "GrayEntityFrame"],
		boxes
	]
]
makeGeneralizedEntityInfoRowLHSBoxes[label_, fmt_] := RowBox[{" ", MakeBoxes[label, fmt]}]

makeGeneralizedEntityInfoRowRHSBoxes[expr:(head_[values_List]), fmt_] := Switch[head,
	ContainsAll, makeContainsSequenceBoxes["\"all of\"", values, fmt],
	ContainsAny, makeContainsSequenceBoxes["\"any of\"", values, fmt],
	ContainsNone, makeContainsSequenceBoxes["\"none of\"", values, fmt],
	ContainsExactly, makeContainsSequenceBoxes["\"exactly\"", values, fmt],
	ContainsOnly, makeContainsSequenceBoxes["\"only\"", values, fmt],
	_, MakeBoxes[expr, fmt]
]
makeGeneralizedEntityInfoRowRHSBoxes[value_, fmt_] := MakeBoxes[value, fmt]

makeContainsSequenceBoxes[text_, values_, fmt_] := RowBox[{
	StyleBox[text, "ImplicitEntityContains"], 
	TemplateBox[Map[MakeBoxes[#, fmt]&, values], "EntityContainsSequence"]
}]

makeGeneralizedEntityInfoRow[(Rule | RuleDelayed)[label_, value_], fmt_] := TemplateBox[
	{
		makeGeneralizedEntityInfoRowLHSBoxes[label, fmt],
		"\":\"",
		makeGeneralizedEntityInfoRowRHSBoxes[value, fmt]
	},
	"ImplicitEntityInfoRow"
];

EntityFramework`MakeEntityFrameworkBoxes[___] := $Failed;


Unprotect[EntityFramework`EntityBulkFormatter]; (* references in typesetfuns.mc *)
EntityFramework`EntityBulkFormatter[args___] := Block[{EntityFramework`$SendWAEvents = False}, With[
	{res = iEntityBulkFormatter[args]},
	res /; res =!= $Failed
]];

Clear[iEntityBulkFormatter];
iEntityBulkFormatter[ent : {Entity[_, _] ..}, fmt : StandardForm | TraditionalForm] := Module[
	{res = getLabel[ent]},
	If[! ListQ[res],
		Return[$Failed]
	];
	With[
		{pos = Position[res, Missing["NotAvailable"] | Missing["UnknownProperty", {_, "Label"}], {1}, Heads -> False]},
		If[pos =!= {},
			res = ReplacePart[
				res,
				Thread[pos -> CanonicalName[Extract[ent, pos]]]
			]
		]
	];
	res = Block[{getetypeLabel},
		getetypeLabel[type_] := getetypeLabel[type] = EntityValue[Entity[type], "Label"] // Replace[
			Except[_String] :> type
		];
		MapThread[
			Function[{e, l},
				Replace[
					EntityFramework`MakeEntityFrameworkBoxes[e, fmt, l],
					$Failed :> Block[{$formatEntity = False},
						MakeBoxes[e, fmt]
					]
				]
			],
			{ent, res}
		]
	];
	res
];
iEntityBulkFormatter[___] := $Failed;


Internal`SetValueNoTrack[{$formatEntity}, True];


End[];
