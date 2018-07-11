
Begin["EntityFramework`Utilities`Private`"];


(* -------------------------------------------------- *)
(* exported symbols *)

EntityFramework`BatchDownload[args___] := With[
	{res = Catch[iBatchDownload[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`FindEntityStore[args___] := With[
	{res = iFindEntityStore[args]},
	res /; res =!= $Failed
];


With[
	{rule = (Rule | RuleDelayed)[_?Internal`PossibleEntityPropertyQ, _]},
	EntityFramework`ConditionQ[rule | {rule ...} | <|rule ...|>] := True
];
EntityFramework`ConditionQ[_] := False;

Clear[EntityFramework`NormalizeConditions];
EntityFramework`NormalizeConditions[{}, ___] := {};
EntityFramework`NormalizeConditions[<||>, ___] := {};
EntityFramework`NormalizeConditions[assoc_?AssociationQ, rest___] := EntityFramework`NormalizeConditions[Normal[assoc], rest];
EntityFramework`NormalizeConditions[cond_?EntityFramework`ConditionQ] := Flatten[{cond}];
EntityFramework`NormalizeConditions[cond_?EntityFramework`ConditionQ, type_String] := Function[{p, v}, Rule @@ {
	p // Replace[s_String :> EntityProperty[type, s]],
	v
} // FixedPoint[Replace[{
	(Rule | RuleDelayed)[p1_, (Rule | RuleDelayed)[p2_, value_]] :> EntityFramework`EntityPropertySequence[p1, p2] -> value,
	(EntityFramework`EntityPropertySequence[x___, EntityFramework`EntityPropertySequence[y__], z___] -> value_) :> EntityFramework`EntityPropertySequence[x, y, z] -> value
}], #] &] @@@ Flatten[{cond}];
EntityFramework`NormalizeConditions[x_, ___] := x;


With[
	{rule = (Rule | RuleDelayed)[_String | HoldPattern[UnitSystem], _]},
	EntityFramework`QualifierQ[rule | {rule ...} | <|rule ...|>] := True
];
EntityFramework`QualifierQ[_] := False;

Clear[EntityFramework`NormalizeQualifiers];
EntityFramework`NormalizeQualifiers[{}] := {};
EntityFramework`NormalizeQualifiers[<||>] := {};
EntityFramework`NormalizeQualifiers[prop : EntityProperty[_, _]] := prop;
EntityFramework`NormalizeQualifiers[EntityProperty[type_String, name_String, qual_?EntityFramework`QualifierQ]] := Replace[
	EntityFramework`NormalizeQualifiers[{name, qual}],
	{
		p_String :> EntityProperty[type, p],
		{p__} :> EntityProperty[type, p]
	}
];
EntityFramework`NormalizeQualifiers[{name_String}] := name;
EntityFramework`NormalizeQualifiers[(List | EntityFramework`Qualified)[name_String, {} | <||>]] := name;
EntityFramework`NormalizeQualifiers[assoc_Association] := EntityFramework`NormalizeQualifiers[Normal[assoc]];
EntityFramework`NormalizeQualifiers[rules_?EntityFramework`QualifierQ] := Flatten[{rules}] // SortBy[First];
EntityFramework`NormalizeQualifiers[(head : List | EntityFramework`Qualified)[name_String, qual_]] := head[name, EntityFramework`NormalizeQualifiers[qual]] // Replace[_[n_, {}] :> n];
EntityFramework`NormalizeQualifiers[x_] := x;


$entityClassProperties = Alternatives[
	"Entities",
	"EntityCount",
	"RandomEntity", "RandomEntities", {"RandomEntities", _Integer?NonNegative}
];

$specialProperties = "CanonicalName" | "Entity" | "Source";

Clear[EntityFramework`NormalizeEntityValueArguments];
EntityFramework`NormalizeEntityValueArguments[args_List?(MemberQ[(EntityClass | EntityProperty)[_String, _?EntityFramework`ConditionQ]])] := With[
	{n = Replace[
		args,
		(head : EntityClass | EntityProperty)[type_String, cond_?EntityFramework`ConditionQ] :> With[
			{n = EntityFramework`NormalizeConditions[cond, type]},
			head[type, n] /; n =!= cond
		],
		{1}
	]},
	EntityFramework`NormalizeEntityValueArguments[n] /; n =!= args
];
EntityFramework`NormalizeEntityValueArguments[{type_String, prop : $entityClassProperties}] := {EntityClass[type, All], prop};
EntityFramework`NormalizeEntityValueArguments[{
	Except[_String, ent : _?Internal`PossibleEntityQ | _?Internal`PossibleEntityListQ],
	prop : Except[
		$entityClassProperties | $specialProperties | "NonMissingProperties",
		_String | {___, Except[$specialProperties, _String], ___}
	]
}] := With[
	{type = EntityFramework`UniqueEntityTypeName[ent]},
	{
		ent,
		Replace[
			prop,
			Except[$specialProperties, name_String] :> EntityProperty[type, name],
			{Boole[ListQ[prop]]}
		]
	} /; StringQ[type]
];
EntityFramework`NormalizeEntityValueArguments[args_List] := args;

EntityFramework`UniqueEntityTypeName[arg_] := Catch[iUniqueEntityTypeName[arg], $tag];

EntityFramework`$RetrievalFailurePattern = Missing["RetrievalFailure" | "EntityListDefaultInvalidList", ___];

(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


frontEndAvailableQ[] := Head[$FrontEnd] === FrontEndObject;


$lowBatchTiming = 2; (* seconds *)

Clear[makeBatch];
makeBatch[ent_List, span_Span] := ent[[span]];
makeBatch[EntityClass[type_, name_], span_Span] := EntityClass[type, name, span];


Clear[iBatchDownload];

Options[iBatchDownload] = {
	"Merge" -> Apply[Join],
	"ShowStatus" :> frontEndAvailableQ[],
	"StatusText" -> Function[Row[{"Downloading ", #, " of ", #2 ," values...."}]]
};

iBatchDownload[{}, __] := {};
iBatchDownload[list_List, batchHandler_, maxBatchSize : _Integer | Infinity, opts : OptionsPattern[]] := iBatchDownload[list, batchHandler, maxBatchSize, Length[list], opts];
iBatchDownload[list_, batchHandler_, maxBatchSize : _Integer | Infinity, listLength_, OptionsPattern[]] := Module[
	{
		res = {},
		batchSize = Min[maxBatchSize, listLength],
		retrieved = 0,
		next,
		batchRes,
		batchTiming,
		issueMessage = True
	},
	next = batchSize;
	If[listLength > 1 && TrueQ[OptionValue["ShowStatus"]] && frontEndAvailableQ[], Monitor][
		While[retrieved < listLength,
			next = Min[
				listLength,
				retrieved + batchSize
			];
			{batchTiming, batchRes} = AbsoluteTiming[Quiet[
				batchHandler[makeBatch[list, retrieved + 1 ;; next]],
				{EntityValue::ctimeout, EntityValue::dbexcp, EntityValue::timeout}
			]];
			If[FreeQ[batchRes, Missing["RetrievalFailure"], {0, 1}],
				(* success *)
				res = OptionValue["Merge"][{res, batchRes}];
				retrieved = next;
				If[batchSize < maxBatchSize && batchTiming < $lowBatchTiming,
					batchSize = Min[
						2 * batchSize,
						maxBatchSize
					]
				],
				(* failure *)
				If[batchSize > 1,
					(* retry with smaller batch *)
					batchSize = Max[1, Ceiling[batchSize / 4]],
					(* give up on this one-element batch *)
					AppendTo[res, Missing["RetrievalFailure"]];
					If[issueMessage,
						Message[EntityValue::nodat];
						issueMessage = False
					];
					retrieved = next
				]
			]
		],
		If[TrueQ[OptionValue["ShowStatus"]] && frontEndAvailableQ[],
			Internal`LoadingPanel[OptionValue["StatusText"][next, listLength]]
		]
	];
	res
];
iBatchDownload[ent_List, prop : {__Rule}, f_, OptionsPattern[]] := Module[{
	propRetrieved = 0,
	batchFactor = 1,
	currentPropBatch = <||>,
	nextProp,
	batchRes,
	batchTiming,
	res = {}
},
	nextProp[] := prop[[propRetrieved + 1 + Length[currentPropBatch]]];
	If[Length[ent] * Length[prop] > 1 && TrueQ[OptionValue["ShowStatus"]] && frontEndAvailableQ[], Monitor][
		While[propRetrieved < Length[prop],
			While[
				Or[
					Length[currentPropBatch] === 0,
					And[
						propRetrieved + Length[currentPropBatch] < Length[prop],
						Total[Length[ent] / Append[currentPropBatch, nextProp[]]] <= batchFactor
					]
				],
				AppendTo[currentPropBatch, nextProp[]]
			];
			{batchTiming, batchRes} = AbsoluteTiming[If[Length[currentPropBatch] > 1,
				Quiet[
					Check[f[ent, Keys[currentPropBatch]], $Failed],
					{EntityValue::ctimeout, EntityValue::dbexcp, EntityValue::timeout}
				],
				List /@ iBatchDownload[
					ent,
					f[#, First[Keys[currentPropBatch]]] &,
					Max[1, Round[batchFactor * First[Values[currentPropBatch]]]],
					"ShowStatus" -> False
				]
			]];
			If[ListQ[batchRes],
				res = If[res === {},
					batchRes,
					Transpose[Join[Transpose[res], Transpose[batchRes]]]
				];
				propRetrieved += Length[currentPropBatch];
				If[batchFactor < 1 && batchTiming < $lowBatchTiming,
					batchFactor = Min[2 * batchFactor, 1]
				],
				batchFactor /= 4
			];
			currentPropBatch = <||>
		],
		If[TrueQ[OptionValue["ShowStatus"]] && frontEndAvailableQ[],
			Internal`LoadingPanel[OptionValue["StatusText"][Length[ent] * (propRetrieved + Length[currentPropBatch]), Length[ent] * Length[prop]]]
		]
	];
	res
];

iBatchDownload[___] := fail[];


Clear[iFindEntityStore];
iFindEntityStore[type_?StringQ] := SelectFirst[
	$EntityStores,
	EntityFramework`EntityTypeExistsQ[#, type] &,
	SelectFirst[
		If[AssociationQ[DataResource`$ResourceObjectEntityStores],
			DataResource`$ResourceObjectEntityStores,
			{}
		],
		EntityFramework`EntityTypeExistsQ[#, type] &,
		SelectFirst[
			Internal`$DefaultEntityStores,
			EntityFramework`EntityTypeExistsQ[#, type] &
		]
	]
];
iFindEntityStore[___] := $Failed;


Clear[iUniqueEntityTypeName];
iUniqueEntityTypeName[type_String] := type;
iUniqueEntityTypeName[_[type_String, __]] := type;
iUniqueEntityTypeName[{_[type_String, __] ..}] := type;
iUniqueEntityTypeName[Except[List][obj_, ___]] := iUniqueEntityTypeName[obj];
iUniqueEntityTypeName[l : {__}] := With[
	{type = iUniqueEntityTypeName[First[l]]},
	If[AllTrue[Rest[l], iUniqueEntityTypeName[#] === type &],
		type,
		Throw[$Failed, $tag]
	]
];
iUniqueEntityTypeName[_] := Throw[$Failed, $tag];


End[];
