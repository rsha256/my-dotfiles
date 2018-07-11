Begin["EntityFramework`Prefetch`Private`"];

EntityFramework`PrefetchEntityValue[pairs:{{_,_}..}] := With[
	{bytype = GatherBy[pairs, typeGatherFunction]}, 
	Scan[checkPropertiesAndPrefetch, bytype]
]

EntityFramework`PrefetchPropertiesForEntity[e_Entity] := With[
	{props = getSessionProperties[EntityTypeName[e]]},
	EntityFramework`PrefetchEntityValue[Map[{e, #}&, props]]
]

$asynchResultsQueue = {};


checkPropertiesAndPrefetch[pairs:{{_[type_, _], _} ..}] := With[
	{groups = GatherBy[pairs, propGatherFunction]},
	Scan[prefetchIfNotCached[getEnts[#], getProp[#]]&, groups]
]

typeGatherFunction[{e_, _}] := EntityTypeName[e]
propGatherFunction[{_, prop_}] := prop

getEnts[list:{{_, _}..}] := list[[All, 1]]
getProp[{{_, prop_}..}] := prop

(*getSessionProperties are cachable properties which have been accessed during the current kernel session*)
(*Toni:  do you think there is a better way to do this?*)
getSessionProperties[type_String] := DeleteDuplicates[
	Cases[
		Keys[EntityFramework`Caching`Private`$memoryCache],
		{Entity[type, _], p_EntityProperty} :> p
	]
]

inAsynchQueue[e_, p_] := MemberQ[$asynchResultsQueue, {{e, p} -> _}]
inAsynchQueue[___] := False

(*create an asynchronous query if the value isn't in the cache already*)
prefetchIfNotCached[ents_List, p_] := With[
	{tofetch = valuesNotInCache[ents, p]},
	If[tofetch =!= {},
		makeQueries[tofetch]
		(*else do nothing*)
	]
]

(*get a list of entity-property pairs which are not in the cache or asynch results queue*)
valuesNotInCache[ents_List, p_] := With[
	{missing = Cases[EntityFramework`EntityValueCache[ents, p], Missing["NotInCache" | "OutdatedCache", _]]},
	DeleteCases[extractQueries[missing], q:{_, _} /; inAsynchQueue[q]]
]

extractQueries[l_List] := Map[extractQuery, l]

extractQuery[Missing["NotInCache", q_]] := q
extractQuery[Missing["OutdatedCache", {args___, _}]] := {args}
extractQuery[___] := Nothing

$prefetchPartitionThreshold = 20; (*number of things that can be in a single query; low to limit load on Alpha*)
$prefetchAsynchLimit = 15; (*number of possible concurrent Asynch calls; too many and things start blowing up...*)

partitionEntities[list_List, n_]:=Block[
	{steps = Range[1, Length[list], n], pairs},
	pairs = Map[{#, #+n-1}&, steps];
	pairs = ReplacePart[pairs, -1 -> {pairs[[-1, 1]], Length[list]}];(*last pair should end with total length*)
	Map[Take[list, #]&, pairs]
]

makeQueries[pairs_List] := If[
	Length[pairs] < $prefetchPartitionThreshold, 
	makeAsynchQuery[pairs], 
	Map[
		makeAsynchQuery,
		partitionEntities[pairs, $prefetchPartitionThreshold]
	]
]

(*if more than 15 asynch tasks,do nothing*)
asynchAvailableQ[] := TrueQ[Length[AsynchronousTasks[]] < $prefetchAsynchLimit]


makeAsynchQuery[query_List] /; asynchAvailableQ[] := Block[
	{WolframAlphaClient`Private`$AlphaQueryMMode = "prefetch"}, 
	prefetchQuery[query]
]



prefetchQuery[queries : {__}] := With[{
	usys = OptionValue[EntityProperty, UnitSystem], 
    prop = getProp[queries], 
    ents = getEnts[queries]
    },
    Internal`MWACompute[
    	"MWACalculateData", 
    	{
    		Internal`MWASymbols`MWAData[ents, prop], 
    		"Version" -> 0.2
    	}, 
    	"UnitSystem" -> usys, 
    	"ContextPath" -> {"Internal`MWASymbols`", "System`", "EntityFramework`"},
    	"Asynchronous" -> True,
    	"AsynchronousFunction" -> Function[{async, type, data}, 
		If[type === "data", addToAsynchResultsQueue[queries, data]]
	]
    ]
]


addToAsynchResultsQueue[queries_List, data_] := With[{res = toResultForm[data]},
	If[SameQ[Length[res], Length[queries]],
		$asynchResultsQueue = Join[$asynchResultsQueue, Thread[queries->res]]
	]
]

toResultForm[{data:{__Integer}}] :=  Module[{res = FromCharacterCode[data, "UTF-8"]},
	res = Block[
		{$ContextPath = {"Internal`MWASymbols`", "System`", "EntityFramework`"}, $Context = "Internal`MWASymbols`Temporary`"},
		res = Quiet[Check[Uncompress[res, HoldComplete], $Failed]] 
	] /. WolframAlphaClient`Private`$FromMWARules;
	If[MatchQ[res, HoldComplete[{{__Rule}, ___}]],
		"Result" /. First[ReleaseHold[res]],
		{}
	]
]
toResultForm[___] := {}

EntityFramework`ProcessAsynchronousResultsQueue[] := Module[{res = $asynchResultsQueue}, 
	(*reset queue first for any new results that migth come back during scan*)
	$asynchResultsQueue = {};
	addToCacheFromQueue[res]
]

addToCacheFromQueue[res_List] := Scan[
	EntityFramework`EntityValueCacheAdd[fromCacheQueueForm[#]]&,
	res
]

fromCacheQueueForm[{ent_, prop_} -> value_] := Sequence[ent, prop, value]
  
End[];

