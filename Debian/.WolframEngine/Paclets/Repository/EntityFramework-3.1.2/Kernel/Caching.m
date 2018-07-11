
Begin["EntityFramework`Caching`Private`"];


(* -------------------------------------------------- *)
(* exported symbols *)

Options[EntityFramework`CachedEntityFunction] = {
	"FailurePattern" -> Missing["RetrievalFailure"],
	"LastUpdate" -> Missing["NotAvailable"],
	UpdateInterval -> Infinity,
	"UseFileCache" :> EntityFramework`$UseFileCache,
	"UseMemoryCache" :> EntityFramework`$UseMemoryCache
};

cf_EntityFramework`CachedEntityFunction[args___] := With[
	{res = Catch[iCachedEntityFunction[cf, args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`CommonNameCache[args___] := With[
	{res = Catch[iCommonNameCache[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`CommonNameCacheAdd[args___] := With[
	{res = Catch[iCommonNameCacheAdd[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`EntityValueCache[args___] := With[
	{res = Catch[iEntityValueCache[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`EntityValueCacheAdd[args___] := With[
	{res = Catch[iEntityValueCacheAdd[args], $tag]},
	res /; res =!= $Failed
];

EntityFramework`EntityValueReadOnlyCache;

EntityFramework`GetCacheVersion[] := Quiet[
	Check[
		Get[FileNameJoin[{$CacheRepositoryRootDirectory, "cacheversion"}]] // Replace[
			Except[_Integer?(GreaterThan[1])] :> 1
		],
		1,
		{Get::noopen}
	],
	{Get::noopen}
];

EntityFramework`$CacheVersion = 3;
EntityFramework`$MaxMemoryCacheSize = 1/2 * 2^30;
EntityFramework`$UseFileCache := Not[Developer`$ProtectedMode];
EntityFramework`$UseMemoryCache = True;
EntityFramework`$UseReadOnlyCache = False;

Internal`ClearEntityValueCache[type_String] := (
	$memoryCache = KeySelect[
		$memoryCache,
		Not@*MatchQ[{type | _[type, ___], _}]
	];
	deleteDir[FileNameJoin[{$CacheRepositoryRootDirectory, makeTypeKey[type]}]];
);
Internal`ClearEntityValueCache[] := (
	$memoryCache = <||>;
	If[EntityFramework`$UseFileCache,
		deleteDir /@ FileNames[All, $CacheRepositoryRootDirectory];
		If[! DirectoryQ[$CacheRepositoryRootDirectory],
			CreateDirectory[$CacheRepositoryRootDirectory]
		];
		Put[
			EntityFramework`$CacheVersion,
			FileNameJoin[{$CacheRepositoryRootDirectory, "cacheversion"}]
		];
		With[
			{file = FileNameJoin[{$CacheRepositoryRootDirectory, "readme.txt"}]},
			WriteString[file, $readmeText];
			Close[file]
		];
	]
);
Internal`ClearEntityValueCache[All] := Internal`ClearEntityValueCache[];

(* end exported symbols *)
(* -------------------------------------------------- *)


fail[] := Throw[$Failed, $tag];


(* -------------------------------------------------- *)
(* cached entity function *)

Clear[iCachedEntityFunction];
iCachedEntityFunction[_, {}, ___] := {};
iCachedEntityFunction[
	EntityFramework`CachedEntityFunction[f_, opts : OptionsPattern[]],
	ent : _Entity | {__Entity},
	qual : Repeated[_?EntityFramework`QualifierQ, {0, 1}]
] := Module[
	{failurePattern, callback},
	failurePattern = OptionValue[EntityFramework`CachedEntityFunction, "FailurePattern"];
	callback[e_, p_] := With[
		{r = If[MatchQ[f, _EntityFramework`BatchApplied],
			f[EntityFramework`BatchList[e], qual] // Replace[
				Except[_List?(Length[#] === Length[e] &)] :> fail[]
			],
			f[#, qual] & /@ e
		]},
		With[
			{pos = Position[r, _?(Not @* MatchQ[failurePattern]), {1}, Heads -> False][[All, 1]]},
			If[Length[pos] > 0,
				addToCache[e[[pos]], p, r[[pos]], opts]
			]
		];
		r
	];
	applyCallback[
		getFromCache[ent, makeFunctionKey[f, qual], opts],
		callback,
		{"NotInCache", "OutdatedCache", failurePattern, EntityFramework`CachedEntityFunction}
	]
];
iCachedEntityFunction[___] := fail[];

Clear[makeFunctionKey];
makeFunctionKey[f_] := ToString[Hash[f]];
makeFunctionKey[f_, {} | <||>] := makeFunctionKey[f];
makeFunctionKey[f_, qual_] := makeFunctionKey[{f, EntityFramework`NormalizeQualifiers[qual]}];

(* end cached entity function *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* utilities *)

(* patterns *)

Clear[propertyPattern];
propertyPattern[type_ : _String] := Alternatives[
	EntityProperty[type, _String, Repeated[_?EntityFramework`QualifierQ, {0, 1}]],
	EntityProperty[type, _?EntityFramework`ConditionQ]
];

Clear[normalize];
normalize[(head : EntityClass | EntityProperty | EntityPropertyClass)[type_String, cond_?EntityFramework`ConditionQ]] := head[type, EntityFramework`NormalizeConditions[cond, type] // Sort];
normalize[EntityProperty[type_String, name_String, qual_ : {}]] := EntityProperty[type, name, EntityFramework`NormalizeQualifiers[qual]];
normalize[x_] := x;

(* end patterns *)


(* keys *)

Clear[makeTypeKey];
makeTypeKey[type_String] := StringJoin[IntegerString[Hash[type], 32]];

Clear[keyFunction];
keyFunction[prop_] := Composition[ToString, Hash];

Clear[makeKey];
makeKey[ent_, prop_ : None] := keyFunction[prop][normalize[ent]];

Clear[makeCacheFileName];
makeCacheFileName[type_String, prop_] := With[
	{typeKey = makeTypeKey[type]},
	FileNameJoin[{
		$CacheRepositoryRootDirectory,
		typeKey,
		StringJoin[typeKey, "-", makeKey[prop], ".dat"]
	}]
];
makeCacheFileName[ent_, prop_] := makeCacheFileName[getType[ent], prop];
makeCacheFileName[ent_, prop_, _] := makeCacheFileName[ent, prop];

(* end keys *)


Clear[getType];
getType[type_String] := type;
getType[Except[List][type_String, __]] := type;
getType[{_[type_String, __] ..}] := type;
getType[___] := fail[];

Clear[deleteDir];
deleteDir[dir_String] := If[DirectoryQ[dir],
	DeleteDirectory[dir, DeleteContents -> True]
];

Clear[streamEvaluate];
streamEvaluate[file_String, f_, mode : OpenAppend | OpenRead | OpenWrite : OpenRead] := AbortProtect[Module[
	{
		stream = mode[file],
		value
	},
	If[stream === $Failed, Throw[$Failed, "Find"]];
	value = f[stream];
	Close[stream];
	value
]];

Clear[clearCorruptCache];
clearCorruptCache[file_String] := Function[f,
	If[FileExistsQ[f],
		Quiet[Close[f], {General::openx}];
		Quiet[DeleteFile[f], {DeleteFile::fdnfnd}]
	]
] @* Function[FileNameJoin[{
	DirectoryName[file],
	FileBaseName[FileBaseName[file]] <> #
}]] /@ {".dat", ".1.index", ".2.index"};

(* end utilities *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* add data *)

$labeledObject = (Entity | EntityClass | EntityProperty | EntityPropertyClass)[_, _] | EntityProperty[_, _, _];

Clear[iCommonNameCacheAdd];

iCommonNameCacheAdd[ent : $labeledObject, value_] := iCommonNameCacheAdd[{ent}, {value}];
iCommonNameCacheAdd[ent : {$labeledObject ..}, value_List] /; Length[ent] === Length[value] := (
	Function[i,
		addToCache[ent[[i]], "Label", value[[i]]]
	] /@ Values[PositionIndex[EntityTypeName /@ ent]];
);

iCommonNameCacheAdd[rules : {Rule[_String, _] ..}] := With[
	{r = Cases[rules, HoldPattern[Rule][_, {__Rule}]]},
	Function[label,
		label // Replace[r] // Replace[
			l : {__Rule} :> GroupBy[
				l,
				First /* First -> Apply[{First[#] @@ Rest[#], #2} &],
				Transpose /* Apply[iEntityValueCacheAdd[#1, label, #2] &]
			]
		]
	] /@ {"Label", "QualifierLabels", "QualifierValueLabels"};
	Replace[
		Join[
			"EntityNameRules" // Replace[r] // Replace[{
				l_List :> Function[{e, v}, {Entity @@ e, v}] @@@ l,
				_ :> {}
			}],
			"EntityClassNameRules" // Replace[r] // Replace[{
				l_List :> Function[{e, v}, {EntityClass @@ e, v}] @@@ l,
				_ :> {}
			}],
			"PropertyClassNameRules" // Replace[r] // Replace[{
				l_List :> Function[{e, v}, {EntityPropertyClass @@ e, v}] @@@ l,
				_ :> {}
			}]
		],
		{
			l : {__} :> iCommonNameCacheAdd @@ Transpose[l],
			_ :> fail[]
		}
	]
];

iCommonNameCacheAdd[___] := fail[];


Clear[iEntityValueCacheAdd];

iEntityValueCacheAdd[ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass | {(_Entity | _EntityClass | _EntityProperty | _EntityPropertyClass) ..}, value_] /; MissingQ[value] || Head[ent] === Head[value] := addToCache[ent, "Normalization", value];

iEntityValueCacheAdd[ent : Entity[type_String, _], prop : propertyPattern[type_String], value_] := If[cacheableQ[prop],
	addToCache[ent, prop, value]
];
iEntityValueCacheAdd[ent : {Entity[type_String, _] ..}, prop : propertyPattern[type_String], value_List] /; Length[ent] === Length[value] := If[cacheableQ[prop],
	addToCache[ent, prop, value]
];
iEntityValueCacheAdd[ent : Entity[type_String, _], prop : {propertyPattern[type_String] ..}, value_List] /; Length[prop] === Length[value] := arrayAdd[ent, prop, value];
iEntityValueCacheAdd[ent : {Entity[type_String, _] ..}, prop : {propertyPattern[type_String] ..}, value_List] /; Dimensions[value, 2] === {Length[ent], Length[prop]} := arrayAdd[ent, prop, value];

iEntityValueCacheAdd[prop : propertyPattern[], sub_String, value_] := addToCache[prop, sub, value];
iEntityValueCacheAdd[prop : {propertyPattern[type_String] ..}, sub_String, value_List] /; Length[prop] === Length[value] := addToCache[prop, sub, value];
iEntityValueCacheAdd[prop : propertyPattern[], sub : {__String}, value_List] /; Length[sub] === Length[value] := arrayAdd[prop, sub, value];
iEntityValueCacheAdd[prop : {propertyPattern[type_String] ..}, sub : {__String}, value_List] /; Dimensions[value, 2] == {Length[prop], Length[sub]} := arrayAdd[prop, sub, value];

iEntityValueCacheAdd[class : EntityClass[_String, _], prop : "Entities", value : _List | Missing["QueryUnknownValue", ___]] := If[cacheableQ[class],
	addToCache[class, prop, value]
];
iEntityValueCacheAdd[class : EntityClass[_String, _], prop : "EntityCount", value : _Integer?NonNegative | Missing["QueryUnknownValue", ___]] := If[cacheableQ[class],
	addToCache[class, prop, value]
];
iEntityValueCacheAdd[class : EntityPropertyClass[_String, _], prop : "Properties", value_List] := addToCache[class, prop, value];

iEntityValueCacheAdd[type_String | Entity[type_String], prop : "EntityClasses", value : {___EntityClass}] := addToCache[type, prop, value];
iEntityValueCacheAdd[type_String | Entity[type_String], prop : "PropertyClasses", value : {___EntityPropertyClass}] := addToCache[type, prop, value];
iEntityValueCacheAdd[type_String | Entity[type_String], prop : "LastUpdate", value : _?Internal`PossibleDateQ | Missing["NotAvailable"]] := addToCache[type, prop, value];
iEntityValueCacheAdd[type_List, prop : "LastUpdate", value_List] /; Length[type] === Length[value] := MapThread[iEntityValueCacheAdd[#1, prop, #2] &, {type, value}];

iEntityValueCacheAdd[___] := fail[];


Clear[arrayAdd];
arrayAdd[ent_, prop_List, value_List] := (
	MapThread[
		Function[{p, v},
			iEntityValueCacheAdd[ent, p, v]
		],
		{
			prop,
			If[ListQ[ent], Transpose, Identity][value]
		}
	];
);


Clear[cacheableQ];
cacheableQ[(EntityClass | EntityProperty)[type_String, cond_?EntityFramework`ConditionQ]] := AllTrue[
	EntityFramework`NormalizeConditions[cond, type][[All, 1]],
	cacheableQ
];
cacheableQ[prop_EntityProperty] := TrueQ[EntityValue[prop, "Cacheable"]];
cacheableQ[_EntityClass] := True;
cacheableQ[_] := False;

Clear[addToCache];
addToCache[_, _, EntityFramework`$RetrievalFailurePattern, ___] := Null;
addToCache[ent_List, prop_, value_List?(MemberQ[EntityFramework`$RetrievalFailurePattern]), rest___] /; Length[ent] === Length[value] := With[
	{pos = Position[value, Except[EntityFramework`$RetrievalFailurePattern], {1}, Heads -> False][[All, 1]]},
	If[Length[pos] > 0,
		addToCache[ent[[pos]], prop, value[[pos]], rest]
	];
];
addToCache[ent_, prop_, value_, OptionsPattern[EntityFramework`CachedEntityFunction]] := (
	If[! TrueQ[Internal`$CacheEntityValue],
		Return[Null]
	];
	If[OptionValue["UseMemoryCache"],
		addToMemoryCache[ent, prop, value]
	];
	If[OptionValue["UseFileCache"],
		addToFileCache[ent, prop, value]
	];
);
addToCache[__] := Null;

(* end add data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* get data *)

Clear[iCommonNameCache];

iCommonNameCache[{}] := {};

iCommonNameCache[ent : $labeledObject | {$labeledObject ..}] := Replace[
	If[ListQ[ent],
		With[
			{pi = PositionIndex[EntityTypeName[ent]]},
			Permute[
				Join @@ Function[
					getFromCache[ent[[#]], "Label", "LastUpdate" :> 1]
				] /@ pi,
				Join @@ pi
			]
		],
		getFromCache[ent, "Label", "LastUpdate" :> 1]
	],
	{
		Missing["NotInCache", {e_, "Label"}] :> Missing["NotInCommonNameCache", {e}],
		Missing["OutdatedCache", {e_, "Label", value_}] :> Missing["OutdatedCommonNameCache", {e, value}]
	},
	{Boole[ListQ[ent]]}
];
iCommonNameCache[ent_, "CallbackFunction" -> callback_] := applyCallback[
	iCommonNameCache[ent],
	callback,
	{"NotInCommonNameCache", "OutdatedCommonNameCache", EntityFramework`$RetrievalFailurePattern, EntityValue}
];

iCommonNameCache[___] := fail[];


Clear[iEntityValueCache];

iEntityValueCache[ent : _Entity | _EntityClass | _EntityProperty | _EntityPropertyClass | {(_Entity | _EntityClass | _EntityProperty | _EntityPropertyClass) ..}] := getFromCache[ent, "Normalization"];

iEntityValueCache[ent : Entity[type_String, _] | {Entity[type_String, _] ..}, prop : propertyPattern[type_String]] := Module[
	{res, usys, usysrules, target},
	res = getFromCache[ent, prop, "LastUpdate" :> getLastUpdate[type]];
	If[
		And[
			Internal`PossibleQuantityQ[res], (* prevent unnecessary loading of the quantity framework *)
			! MatchQ[prop, EntityProperty[_, _, (Rule | RuleDelayed)[UnitSystem | "UnitSystem", _] | {___, (Rule | RuleDelayed)[UnitSystem | "UnitSystem", _], ___}]]
		],
		usys = OptionValue[EntityProperty, UnitSystem];
		If[MatchQ[usys, "Metric" | "Imperial"],
			usysrules = EntityValue[prop, "UnitSystemRules"];
			If[MatchQ[usysrules, {__Rule}],
				target = usys /. usysrules;
				If[! MatchQ[target, "Metric" | "Imperial"],
					res = UnitConvert[res, target]
				]
			]
		]
	];
	res
];
iEntityValueCache[ent : Entity[type_String, _] | {Entity[type_String, _] ..}, prop : {propertyPattern[type_String] ..}] := If[ListQ[ent], Transpose, Identity][
	iEntityValueCache[ent, #] & /@ prop
];
iEntityValueCache[prop : propertyPattern[type_String] | {propertyPattern[type_String] ..}, sub_String] := getFromCache[prop, sub, "LastUpdate" :> getLastUpdate[type]];
iEntityValueCache[prop : propertyPattern[] | {propertyPattern[type_String] ..}, sub : {__String}] := If[ListQ[prop], Transpose, Identity][
	iEntityValueCache[prop, #] & /@ sub
];
iEntityValueCache[class : EntityClass[type_String, _], prop : "Entities" | "EntityCount"] := getFromCache[class, prop, "LastUpdate" :> getLastUpdate[type]];
iEntityValueCache[class : EntityPropertyClass[type_String, _], prop : "Properties"] := getFromCache[class, prop, "LastUpdate" :> getLastUpdate[type]];

iEntityValueCache[type_String | Entity[type_String], prop : "EntityClasses" | "PropertyClasses"] := getFromCache[type, prop, "LastUpdate" :> getLastUpdate[type]];
iEntityValueCache[type_String | Entity[type_String], prop : "LastUpdate"] := getFromCache[type, prop, "LastUpdate" :> AbsoluteTime[] - 86400];
iEntityValueCache[type_String | Entity[type_String], prop : "LastUpdate", "CallbackFunction" -> f_] := iEntityValueCache[Entity[type], prop] // Replace[{
	Missing["NotInCache", ___] :> f[Entity[type], prop],
	Missing["OutdatedCache", {___, oldValue_}] :> Replace[
		f[Entity[type], prop],
		EntityFramework`$RetrievalFailurePattern :> oldValue
	]
}];

iEntityValueCache[args__, "CallbackFunction" -> callback_] := applyCallback[
	Replace[
		iEntityValueCache[args],
		Missing["NotInCache", {ent_, "Normalization"}] :> Missing["NotInCache", {ent}],
		{Boole[ListQ[First[{args}]]]}
	],
	callback,
	{"NotInCache", "OutdatedCache", EntityFramework`$RetrievalFailurePattern, EntityValue}
];

iEntityValueCache[___] := fail[];


Clear[applyCallback];
applyCallback[value_, None, _] := value;
applyCallback[value_, _, {notInCache_, outdated_, _, _}] /; FreeQ[value, Missing[notInCache | outdated, _]] := value;
applyCallback[value_, callback_, {notInCache_, outdated_, failurePattern_, messageHead_}] := Module[
	{res, container, issueMessage = True},
	res = value;
	res = res /. {
		Missing[notInCache, {x___}] :> Inactive[callback][x],
		Missing[outdated, {x___, cached_}] :> container[Inactive[callback][x], cached]
	};
	With[
		{pos = Position[{res}, Inactive[callback][___]]},
		If[Length[pos] === 0,
			Return[value]
		];
		{res} = {res} // ReplacePart[
			Thread[pos -> Activate[makeBatches[Extract[{res}, pos]], callback]]
		]
	];
	res = res /. {
		container[v_?(FreeQ[#, failurePattern, {0, 1}] &), _] :> v,
		container[_, cached_] :> (
			If[issueMessage,
				Message[messageHead::outdcache];
				issueMessage = False
			];
			cached
		)
	};
	res
];


Clear[makeBatches];
makeBatches[{}] := {};
makeBatches[l : {(f : Inactive[_])[Except[_List | _String | _EntityClass | _EntityPropertyClass], Except[_List, prop_]] ..}] := f[l[[All, 1]], prop] /; True; (* prefer collecting first argument *)
makeBatches[l : {(f : Inactive[_])[Except[_List, ent_], Except[_List | _String | _EntityPropertyClass]] ..}] := f[ent, l[[All, 2]]];
makeBatches[l : {(f : Inactive[_])[Except[_List]] ..}] := f[l[[All, 1]]];
makeBatches[expr_] := expr;


Clear[getFromCache];
getFromCache[
	ent_,
	prop : propertyPattern[] | _String,
	opts : OptionsPattern[EntityFramework`CachedEntityFunction]
] := Module[
	{res},
	res = makeMissing[ent, prop];
	If[! TrueQ[Internal`$CacheEntityValue],
		Return[res]
	];
	If[OptionValue["UseMemoryCache"],
		res = retrieveRemaining[res, getFromMemoryCache[##, opts] &, If[ListQ[ent], 1, 0]]
	];
	If[OptionValue["UseFileCache"],
		res = retrieveRemaining[res, getFromFileCache[##, opts] &, If[ListQ[ent], 1, 0]]
	];
	If[EntityFramework`$UseReadOnlyCache,
		res = retrieveRemaining[
			res,
			Function[{e, p},
				With[
					{data = EntityFramework`EntityValueReadOnlyCache[e, p]},
					If[
						Or[
							(* check whether the read only cache failed *)
							MatchQ[data, _EntityFramework`EntityValueReadOnlyCache | $Failed],

							(* in the list case check whether the result is a list of the right length *)
							ListQ[e] && (! ListQ[data] || Length[data] =!= Length[e])
						],
						makeMissing[e, p],
						data
					]
				]
			],
			If[ListQ[ent], 1, 0]
		]
	];
	If[
		And[
			TrueQ[EntityFramework`$SendWAEvents],
			(* check whether any data was taken from the cache *)
			! MatchQ[res, If[ListQ[ent], {Missing["NotInCache", _] ...}, Missing["NotInCache", _]]],

			(*EntityFramework`$SendWAEvents is blocked in DefaultEntityValue; setting the value to False to avoid duplicate billing*)
			EntityFramework`$SendWAEvents = False;
			(* check for sufficient credits *)
			CloudSystem`Private`SendWAEvent["entity"] === False
		],
		Message[CloudSystem`Cloud::creditlimit];
		res = $Failed
	];
	res
];
getFromCache[___] := fail[];

Clear[retrieveRemaining];
retrieveRemaining[expr_, ret_, 1] := With[
	{pos = First /@ Position[expr, Missing["NotInCache" | "OutdatedCache", _], {1}, Heads -> False]},
	If[pos === {},
		Return[expr]
	];
	ReplacePart[
		expr,
		Thread[pos -> ret[
			expr[[pos, 2, 1]],
			expr[[First[pos], 2, 2]]
		]] // DeleteCases[_ -> Missing["NotInCache", _]] (* to avoid replacing Missing["OutdatedCache", ...] with Missing["NotInCache", ...] *)
	]
];
retrieveRemaining[expr_, ret_, level_Integer] := Replace[
	expr,
	{
		Missing["NotInCache", {args___}] :> ret[args],
		Missing["OutdatedCache", {args___, _}] :> With[
			{value = ret[args]},
			value /; ! MatchQ[value, Missing["NotInCache", _]]
		]
	},
	{level}
];

(* end get data *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* caching utilities *)

Clear[getLastUpdate];
getLastUpdate[type_String] := If[TrueQ[EntityFramework`$CacheUpdateCheck],
	AbsoluteTime[
		iEntityValueCache[type, "LastUpdate", "CallbackFunction" -> EntityValue] // Replace[
			Except[_?Internal`PossibleDateQ] :> AbsoluteTime[] - 86400
		]
	],
	0
]

Clear[possibleLastUpdate];
possibleLastUpdate[lu_?Internal`PossibleDateQ, _] := AbsoluteTime[lu];
possibleLastUpdate[_, Infinity] := 0;
possibleLastUpdate[_, ui_Integer] := AbsoluteTime[] - ui;
possibleLastUpdate[_, HoldPattern[ui_Quantity]] := possibleLastUpdate[Null, QuantityMagnitude[ui, "Seconds"]];
possibleLastUpdate[_, _] := Infinity;

Clear[makeMissing];
makeMissing[ent_, prop_] := Map[
	Missing["NotInCache", {#, prop}] &,
	ent,
	{If[ListQ[ent], 1, 0]}
];

(* end caching utilities *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* memory cache *)

$memoryCache = <||>;

Clear[addToMemoryCache];
addToMemoryCache[ent_, prop_, value_] := If[ByteCount[$memoryCache] < EntityFramework`$MaxMemoryCacheSize,
	AssociateTo[
		$memoryCache,
		If[ListQ[ent],
			Thread[Thread[{normalize /@ ent, normalize[prop]}] -> Thread[{value, AbsoluteTime[]}]],
			normalize /@ {ent, prop} -> {value, AbsoluteTime[]}
		]
	]
];

Clear[addToMemoryCacheRaw];
addToMemoryCacheRaw[rules : <|__|>] := If[ByteCount[$memoryCache] + ByteCount[rules] < EntityFramework`$MaxMemoryCacheSize,
	$memoryCache = Join[$memoryCache, KeyMap[Map[normalize], rules]];
];

Clear[getFromMemoryCache];
getFromMemoryCache[ent : _[type_String, __] | type_String, prop_, OptionsPattern[EntityFramework`CachedEntityFunction]] := Lookup[
	$memoryCache,
	Key[normalize /@ {ent, prop}]
] // Replace[{
	Missing["KeyAbsent", _] :> Missing["NotInCache", {ent, prop}],
	{value_, _?(LessThan[possibleLastUpdate @@ OptionValue[{"LastUpdate", UpdateInterval}]])} :> Missing["OutdatedCache", {ent, prop, value}],
	{value_, _} :> value
}];
getFromMemoryCache[
	ent_,
	prop : propertyPattern[] | _String,
	OptionsPattern[EntityFramework`CachedEntityFunction]
] := Module[
	{lastUpdate},
	lastUpdate := lastUpdate = possibleLastUpdate @@ OptionValue[{"LastUpdate", UpdateInterval}];
	MapThread[
		Function[{e, v},
			v // Replace[{
				{value_, _?(LessThan[lastUpdate])} :> Missing["OutdatedCache", {e, prop, value}],
				{value_, _} :> value
			}]
		],
		{
			ent,
			Lookup[
				$memoryCache,
				If[ListQ[ent],
					Key /@ Thread[{normalize /@ ent, normalize[prop]}],
					Key[normalize /@ {ent, prop}]
				]
			] // If[ListQ[ent],
				ReplacePart[#, With[
					{pos = First /@ Position[#, Missing["KeyAbsent", _], {1}, Heads -> False]},
					Thread[pos -> Function[Missing["NotInCache", {#, prop}]] /@ ent[[pos]]]
				]] &,
				Replace[Missing["KeyAbsent", _] :> Missing["NotInCache", {ent, prop}]]
			]
		},
		If[ListQ[ent], 1, 0]
	]
];

(* end memory cache *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* file cache lock *)

Clear[fileLockedQ];
fileLockedQ[file_String] := FileExistsQ[file <> ".lock"];

Clear[lockFile];
lockFile[file_String] := CreateFile[file <> ".lock"];

Clear[unlockFile];
unlockFile[file_String] := With[
	{lock = file <> ".lock"},
	If[FileExistsQ[lock],
		DeleteFile[lock]
	]
];

Clear[withLockedFileCache];
SetAttributes[withLockedFileCache, HoldRest];
withLockedFileCache[f_, args_List, alreadyLockedResult_ : $Failed] := With[
	{file = makeCacheFileName @@ args},
	If[fileLockedQ[file],
		Return[alreadyLockedResult]
	];
	CheckAbort[
		Module[
			{value},
			lockFile[file];
			value = f @@ args;
			unlockFile[file];
			value
		],
		unlockFile[file];
		Abort[]
	]
];

(* end file cache lock *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* file cache *)

(* add *)
Clear[addToFileCache];
addToFileCache[args__] := withLockedFileCache[
	Function[Catch[
		iAddToFileCache[##],
		$corruptCache,
		Function[index, clearCorruptCache[index]; $Failed]
	]],
	{args}
];

Clear[iAddToFileCache];
iAddToFileCache[
	ent : (Entity | EntityClass | EntityProperty | EntityPropertyClass)[_String, __] | {(Entity | EntityClass | EntityProperty | EntityPropertyClass)[_String, __] ..} | _String,
	prop : propertyPattern[] | _String,
	value_
] := AbortProtect[Catch[Module[
	{dataFile, pos},

	dataFile = makeCacheFileName[ent, prop];

	If[FileExistsQ[dataFile],
		checkFileSize[dataFile],
		createDirectoryIfNeeded[dataFile]
	];

	streamEvaluate[dataFile, Function[stream,
		If[ListQ[ent],
			Block[{time = AbsoluteTime[]},
				With[{strings = makeDataExpToWrite /@ value},
					pos = Accumulate[Prepend[
						StringLength[Most[strings]],
						StreamPosition[stream]
					]];
					WriteString[stream, StringJoin[strings]]
				]
			],
			pos = StreamPosition[stream];
			WriteString[stream, makeDataExpToWrite[value]]
		]
	], OpenAppend];

	addToIndex[dataFile, ent, pos, prop]
], "Find"]];


(* get *)
Clear[getFromFileCache];
getFromFileCache[ent_, prop_, opts : OptionsPattern[EntityFramework`CachedEntityFunction]] := withLockedFileCache[
	Function[Catch[
		iGetFromFileCache[##, opts],
		$corruptCache,
		Function[index, clearCorruptCache[index]; makeMissing[ent, prop]]
	]],
	{ent, prop},
	makeMissing[ent, prop]
];

Clear[iGetFromFileCache];
iGetFromFileCache[
	ent_,
	prop : propertyPattern[] | _String,
	OptionsPattern[EntityFramework`CachedEntityFunction]
] := Catch[Module[
	{res, dataFile, index, lastUpdate, forMemoryCache = <||>},

	dataFile = makeCacheFileName[ent, prop];

	If[! FileExistsQ[dataFile],
		Return[makeMissing[ent, prop]]
	];

	index = getIndex[dataFile, ent, prop];
	lastUpdate := lastUpdate = possibleLastUpdate @@ OptionValue[{"LastUpdate", UpdateInterval}];

	res = streamEvaluate[dataFile, Function[stream,
		Map[
			Function[e,
				If[KeyExistsQ[index, e],

					Quiet[Check[
						SetStreamPosition[stream, index[e]],
						Throw[First[stream], $corruptCache],
						{SetStreamPosition::stmrng}
					], {SetStreamPosition::stmrng}];
					decodeData[
						Read[stream, Record, RecordSeparators -> $recordDelimiter],
						dataFile
					] // Replace[{
						{value_, _?(LessThan[lastUpdate])} :> Missing["OutdatedCache", {e, prop, value}],
						expr : {value_, _} :> (
							If[OptionValue["UseMemoryCache"],
								AssociateTo[forMemoryCache, {e, prop} -> expr]
							];
							value
						)
					}],

					makeMissing[e, prop]
				]
			],
			ent,
			{If[ListQ[ent], 1, 0]}
		]
	]];
	If[OptionValue["UseMemoryCache"],
		addToMemoryCacheRaw[forMemoryCache]
	];
	res
], "Find"];

(* end file cache *)
(* -------------------------------------------------- *)


(* -------------------------------------------------- *)
(* index *)

(* index file contents: *)
(* key1-pos1|key2-pos2|... *)

Clear[toIndexFile];
toIndexFile[dataFile_String?(StringEndsQ[".dat"]), level_Integer] := StringJoin[
	StringDrop[dataFile, -3],
	ToString[level],
	".index"
];

Clear[indexLength];
indexLength[1] = 10000;

Clear[readIndex];
readIndex[indexFile_String?FileExistsQ] := With[
	{i = StringSplit[ReadList[indexFile, Record, RecordSeparators -> "|"], "-"]},
	If[MatchQ[i, {{_String, _String} ...}],
		<|Rule @@@ i|>,
		Throw[indexFile, $corruptCache]
	]
];
readIndex[_] := <||>;

Clear[addToIndex];
addToIndex[dataFile_String, Except[_List, ent_], pos_Integer, prop_] := addToIndex[dataFile, {ent}, {pos}, prop];
addToIndex[dataFile_String, ent_List, pos : {__Integer}, prop_] /; Length[ent] === Length[pos] := Module[
	{index, indexFile},
	indexFile = toIndexFile[dataFile, 1];
	index = readIndex[indexFile];
	AssociateTo[
		index,
		Thread[Rule[makeKey[#, prop] & /@ ent, ToString /@ pos]]
	];
	If[Length[index] > indexLength[1],
		If[FileExistsQ[indexFile],
			DeleteFile[indexFile]
		];
		indexFile = toIndexFile[dataFile, 2];
		index = Join[
			readIndex[indexFile],
			index
		]
	];
	If[Length[index] === 0,
		Return[]
	];
	WriteString[indexFile, StringRiffle[List @@@ Normal[index], "|", "-"]];
	Close[indexFile]; (* bug 315404 *)
];

$readFullIndexThreshold = 1000;

Clear[getIndex];
getIndex[_, {}, _] := <||>;
getIndex[dataFile_String, Except[_List, ent_], prop_] := getIndex[dataFile, {ent}, prop];
getIndex[dataFile_String, ent_List /; Length[ent] > $readFullIndexThreshold, prop_] := Module[
	{
		index = Join @@ Table[
			readIndex[toIndexFile[dataFile, level]],
			{level, 2}
		],
		res = <||>
	},
	Function[e,
		index[makeKey[e, prop]] // Replace[
			s_String :> Quiet[Check[
				ToExpression[s],
				Throw[dataFile, $corruptCache]
			]]
		] // Replace[
			i_Integer :> AssociateTo[res, e -> i]
		]
	] /@ ent;
	res
];
getIndex[dataFile_String, ent_List, prop_] := Module[
	{index, indexFile, stream},
	index = <||>;
	Do[
		indexFile = toIndexFile[dataFile, level];
		If[! FileExistsQ[indexFile],
			Continue[]
		];
		stream = OpenRead[indexFile];
		Function[e,
			If[! KeyExistsQ[index, e],
				getKeyFromStream[stream, makeKey[e, prop]] // Replace[
					i_Integer :> (
						AssociateTo[
							index,
							e -> i
						];
						If[Length[index] === Length[ent],
							Close[stream];
							Break[];
						]
					)
				]
			]
		] /@ ent;
		Close[stream],
		{level, 2}
	];
	index
];

getKeyFromStream[stream_, key_] := Module[
	{res},
	SetStreamPosition[stream, 0];
	res = Find[stream, key, RecordSeparators -> "|"];
	If[res === EndOfFile,
		Return[None]
	];
	res = StringSplit[res, "-", 2];
	If[Length[res] =!= 2,
		Throw[First[stream], $corruptCache]
	];
	res = Quiet[Check[
		ToExpression[res[[2]]],
		Throw[First[stream], $corruptCache]
	]];
	res
];

(* end index *)
(* -------------------------------------------------- *)


$CacheRepositoryRootDirectory := FileNameJoin[{$UserBaseDirectory, "Knowledgebase"}]

createDirectoryIfNeeded[file_String] := With[
	{dir = DirectoryName[file]},
	If[! DirectoryQ[dir],
		CreateDirectory[dir]
	]
];

(* legal disclaimer about not modifying files we've set up here ....*)
$readmeText = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                                                                   \
  !!!
!!!                     !!! END USER WARNING !!!                      \
  !!!
!!!                                                                   \
  !!!
!!!                  !!! DO NOT MODIFY SYSTEM FILES !!!               \
  !!!
!!!                                                                   \
  !!!
!!!    MODIFICATION OF THE SYSTEM FILES IN THIS DIRECTORY MAY CAUSE:  \
  !!!
!!!                                                                   \
  !!!
!!!        - SYSTEM INSTABILITY;                                      \
  !!!
!!!        - DATA LOSS;                                               \
  !!!
!!!        - PROGRAM CRASHING; AND/OR                                 \
  !!!
!!!        - ADDITIONAL NEGATIVE AND UNDEFINED BEHAVIOR.              \
  !!!
!!!                                                                   \
  !!!
!!!                                                                   \
  !!!
!!!    THE SYSTEM FILES IN THIS DIRECTORY ARE PROVIDED \"AS IS\",     \
    !!!
!!!    WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING    \
  !!!
!!!    BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY OR \
FITNESS  !!!
!!!    FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL THE AUTHORS OR     \
  !!!
!!!    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER    \
  !!!
!!!    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR \
OTHERWISE,  !!!
!!!    ARISING FROM, OUT OF OR IN CONNECTION WITH ANY MODIFICATION OF \
  !!!
!!!    THE FILES FOR ANY PURPOSE.                                     \
  !!!
!!!                                                                   \
  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\
";

$recordDelimiter = "//[y~t]/w?v=dQw4w9WgXcQ";

Clear[decodeData];
decodeData[data_, file_] := Quiet[Check[
	Uncompress[data],
	Throw[file, $corruptCache]
]]

time := AbsoluteTime[]

encodeData[value_] := Compress[{value,time}]

makeDataExpToWrite[value_] := StringJoin[encodeData[value], $recordDelimiter]

$MaxFileSize := Replace["MaxByteCount", Replace["FileBackedCachingOptions", SystemOptions["FileBackedCachingOptions"]]]
checkFileSize[dataFile_String] := If[
	And[FileExistsQ[dataFile], FileByteCount[dataFile]>$MaxFileSize],
	Throw[$Failed, "Find"];
]

End[];
