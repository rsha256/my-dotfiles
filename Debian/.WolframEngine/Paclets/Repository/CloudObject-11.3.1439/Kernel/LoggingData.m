(* ::Package:: *)

BeginPackage["CloudObject`"]

System`CloudLoggingData;

Begin["`Private`"]

(* CloudLoggingData *)

Options[CloudLoggingData] = {"TimeSeriesBinSize" -> Automatic, MaxItems -> Automatic, CloudBase -> Automatic}

CloudLoggingData["Elements"] := 
    {"TotalCalls", "AverageCallRate","CallRateTimeSeries","TotalCredits","AverageCreditsPerCall","CalledObjectsCount","CallDetails"}
CloudLoggingData["Categories"] := {"API", "Form", "Computation", "Task", "CDF", All}
CloudLoggingData["Periods"] := 
    {"LastMinute", "LastHour", "LastDay", "LastWeek", "LastMonth", "LastYear", "MonthToDate", "QuarterToDate", "YearToDate", All}
    
defaultElements = {"TotalCalls", "TotalCredits", "CallRateTimeSeries"}
defaultPeriods = {"LastDay", "LastWeek", "LastMonth", All}

CloudLoggingData[opts : OptionsPattern[]] := 
    Module[{results},
        results=Catch[Map[Check[#->CloudLoggingData[All, #,defaultElements, opts],
            Throw[$Failed, unavailable], CloudLoggingData::unavailable]&, defaultPeriods], unavailable];
        If[ListQ[results],
            Association@@results,
            results
        ]
    ]

CloudLoggingData[objs_, opts : OptionsPattern[]] := 
    Module[{objects = getObjectID[objs], results},
        If[objects==={} || Head[objects]===Missing,
            $Failed,
            results=Catch[Map[Check[#->CloudLoggingData[objects, #,defaultElements, opts],
                Throw[$Failed, unavailable], CloudLoggingData::unavailable]&, defaultPeriods], unavailable];
            If[ListQ[results],
                Association@@results,
                results
            ]
        ]
    ]

CloudLoggingData[objs_, period_, opts : OptionsPattern[]] := 
    With[{objects = getObjectID[objs]},
        If[objects==={} || Head[objects]===Missing,
            $Failed,
            CloudLoggingData[objects, period, defaultElements, opts]
        ]
    ]
            
CloudLoggingData[objs_, HoldPattern[{start_DateObject, end_DateObject}], elems_, opts:OptionsPattern[]] :=
    Module[{temporalRange = formatTimeRange[{start, end}, OptionValue["TimeSeriesBinSize"]],
        rawData, objects},
        objects = getObjectID[If[ListQ[objs], Take[objs, Replace[OptionValue[MaxItems], {x_Integer :> x, _ -> All}]], objs]];
        If[objects==={} || Head[objects]===Missing, Return[$Failed]];
        Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
            rawData = getLoggingData[objects, elems, temporalRange];
            If[rawData === $Failed, Return[$Failed]];
            FormatRawData[objects, elems, rawData]
        ]
    ]
            
CloudLoggingData[objs_, period_List, elems_, opts:OptionsPattern[]] := 
    Module[{objects = getObjectID[objs], results},
        If[objects==={} || Head[objects]===Missing,
            $Failed,
            results=Catch[Map[Check[#->CloudLoggingData[objects, #, elems, opts],
                Throw[$Failed, unavailable], CloudLoggingData::unavailable]&, period], unavailable];
            If[ListQ[results],
                Association@@results,
                results
            ]
        ]
    ]


CloudLoggingData[objs_, period_, elems_, opts:OptionsPattern[]] :=
    Module[{temporalRange = formatTimeRange[period, OptionValue["TimeSeriesBinSize"]],
        rawData, objects},
        objects = getObjectID[If[ListQ[objs], Take[objs, Replace[OptionValue[MaxItems], {x_Integer :> x, _ -> All}]], objs]];
        If[objects==={} || Head[objects]===Missing, Return[$Failed]];
        Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
            rawData = getLoggingData[objects, elems, temporalRange];
            If[rawData === $Failed, Return[$Failed]];
            FormatRawData[objects, elems, rawData]
        ]
    ]

getObjectID[objs_List] := DeleteMissing[getObjectID/@objs]
getObjectID[obj_String] := If[CloudObject`UUIDQ[obj] || MemberQ[CloudLoggingData["Categories"], obj], obj, Message[System`CloudLoggingData::invobj, obj]; Missing["Invalid"]]
getObjectID[obj_CloudObject] := CloudObjectInformation[obj, "UUID"]
getObjectID[head_Symbol] := 
    Switch[head,
        APIFunction, "API",
        FormFunction, "Form",
        Delayed, "Computation",
        ScheduledTask, "Task",
        Notebook, "CDF",
        All, "All",
        _, Message[System`CloudLoggingData::invobj, head];Missing["Invalid"]
    ]
getObjectID[obj_] := Block[{},Message[System`CloudLoggingData::invobj, obj];Missing["Invalid"]]

formatTimeRange[time_, Automatic] := formatTimeRange[time]
formatTimeRange[time_, stepSize_] := 
    With[{step = toStandardStepDurationString[stepSize]},
        If[Head[step]===Missing,
            formatTimeRange[time],
            ReplacePart[formatTimeRange[time], -1 -> ("stepDuration" -> toStandardStepDurationString[stepSize])]
        ]
    ]
formatTimeRange["LastMinute"] := 
    {"start" -> toStandardTimeString[Now - Quantity[1, "Minutes"]], "end" -> toStandardTimeString[Now], "stepDuration" -> "1m"}
formatTimeRange["LastHour"] := 
    {"start" -> toStandardTimeString[Now - Quantity[1, "Hours"]], "end" -> toStandardTimeString[Now], "stepDuration" -> "1m"}
formatTimeRange["LastDay"] := 
    {"start" -> toStandardTimeString[Now - Quantity[24, "Hours"]], "end" -> toStandardTimeString[Now], "stepDuration" -> "1h"}
formatTimeRange["LastWeek"] := 
    {"start" -> toStandardTimeString[Now - Quantity[7, "Days"]], "end" -> toStandardTimeString[Now], "stepDuration" -> "1h"}
formatTimeRange["LastMonth"] := 
    {"start" -> toStandardTimeString[Now - Quantity[30, "Days"]], "end" -> toStandardTimeString[Now], "stepDuration" -> "1h"}
formatTimeRange["LastYear"] := 
    {"start" -> toStandardTimeString[Now - Quantity[1, "Years"]], "end" -> toStandardTimeString[Now], "stepDuration" -> "1d"}
formatTimeRange["MonthToDate"] := 
    With[{start = {DateValue["Year"], DateValue["Month"], 1, 0, 0, 0}, end = Now},
        {"start" -> toStandardTimeString[start], 
            "end" -> toStandardTimeString[end], 
            "stepDuration" -> optimalBinSize[start, end]}
    ]
formatTimeRange["QuarterToDate"] := 
    With[{start = {DateValue["Year"], 3*(DateValue[Now, "Quarter"]) - 2, 1, 0, 0, 0}, end = Now},
        {"start" -> toStandardTimeString[start], 
            "end" -> toStandardTimeString[end], 
            "stepDuration" -> optimalBinSize[start, end]}
    ]
formatTimeRange["YearToDate"] := 
    With[{start = {DateValue["Year"], 1, 1, 0, 0, 0}, end = Now},
        {"start" -> toStandardTimeString[start], 
            "end" -> toStandardTimeString[end], 
            "stepDuration" -> optimalBinSize[start, end]}
    ]
formatTimeRange[All] := 
    With[{start = {2014,3,22, 0, 0, 0}, end = Now},
        {"start" -> toStandardTimeString[start], 
            "end" -> toStandardTimeString[end],
            "stepDuration" -> optimalBinSize[start, end]}
    ]
formatTimeRange[HoldPattern[date_DateObject]] := 
    {"start" -> toStandardTimeString[date], "end" -> toStandardTimeString[Now], "stepDuration" -> optimalBinSize[date, Now]}
formatTimeRange[HoldPattern[{start_DateObject, end_DateObject}]] := 
    {"start" -> toStandardTimeString[start], "end" -> toStandardTimeString[end], "stepDuration" -> optimalBinSize[start, end]}
    
formatTimeRange[period_] :=
    (Message[System`CloudLoggingData::invper, period]; formatTimeRange[All])

toStandardTimeString[date_] := 
    DateString[TimeZoneConvert[DateObject[DateList[date],TimeZone -> $TimeZone], 0], 
        {"Year", "-", "Month", "-", "Day", " ", "Hour24", ":", "Minute", ":", "Second"}]

fromStandardTimeString[date_] := TimeZoneConvert[DateObject[DateList[date], TimeZone -> 0]]

optimalBinSize[start_, end_] := 
    Module[{units = {{1, "Hour"}, {3, "Hour"}, {1, "Day"}, {1, "Week"}, {1, "Month"}, {1, "Quarter"}, {1, "Year"}}, 
        steps, positions},
        steps = (N@Round[QuantityMagnitude@DateDifference[start, end, #[[2]]]]/#[[1]]) & /@ units;
        positions = Map[First, First[Position[steps, #]] & /@ Select[steps, (# >= 100 && # <= 1000) &]];
        Which[positions =!= {}, toStandardStepDurationString[First[units[[positions]]]],
            steps[[1]] < 100, "1h",
            True, "1y"
        ]
    ]

toStandardStepDurationString[{n_, ("Hour" | "Hours")}] := ToString[n] <> "h"
toStandardStepDurationString[{n_, ("Day" | "Days")}] := ToString[n]<>"d"
toStandardStepDurationString[{n_, ("Week" | "Weeks")}] := ToString[n]<>"w"
toStandardStepDurationString[{n_, ("Month" | "Months")}] := ToString[n]<>"mo"
toStandardStepDurationString[{n_, ("Quarter" | "Quarters")}] := ToString[n]<>"q"
toStandardStepDurationString[{n_, ("Year" | "Years")}] := ToString[n]<>"y"
toStandardStepDurationString[HoldPattern[quantity_Quantity]] :=
    Module[{units = {"Hour", "Day", "Week", "Month", "Quarter", "Year"}},
        Do[With[{step = UnitConvert[quantity, unit]}, 
            If[QuantityMagnitude[step] >= 1, 
                Return[toStandardStepDurationString[{Round[QuantityMagnitude[step]], unit}]]]],
        {unit, units}]
    ]
toStandardStepDurationString[n_] := (Message[System`CloudLoggingData::invsize, n];Missing["Invalid"])

fromStandardStepDurationString[stepDuration_String] := 
    Which[
        StringMatchQ[stepDuration, __ ~~ RegularExpression["[dhqwy]"]], 
            Quantity[#[[1]], #[[2]]]&@{ToExpression[StringDrop[stepDuration, -1]], 
                Replace[StringTake[stepDuration, -1], {"h" -> "Hour", "d" -> "Day", "w" -> "Week", "q" -> "Quarter", "y" -> "Year"}]},
        StringMatchQ[stepDuration, __ ~~ "mo"], Quantity[#[[1]], #[[2]]]&@{ToExpression[StringDrop[stepDuration, -2]], "Month"},
        True, Missing["Unknown"]
    ]

getLoggingData[object_String, more___] := getLoggingData[{object}, more]

getLoggingData[objects_List, elems_, timeRange_] :=
    Module[{deployments = SelectDeployments[objects], objs, query, results, summaries},
        objs = Complement[objects, deployments];
        query = timeRange;
        If[deployments =!= {}, AppendTo[query, "deploymentTypes" -> toStandardListString[deployments]]];
        If[objs =!= {}, AppendTo[query, "objects" -> toStandardListString[objs]]];
        summaries = summariesNeeded[elems];
        If[summaries === {}, Return[$Failed]];
        AppendTo[query, "summaryTypes" -> toStandardListString[summaries]];
        results = Quiet[execute[$CloudBase, "GET", {"logs", "summary"}, Parameters -> query]];
        results = Replace[
            results,
            {
                err_HTTPError :> (
                    checkError[$lastLoggingDataError = err, CloudLoggingData];
                    $Failed
                ),
                {_, bytes_List} :> importFromJSON[FromCharacterCode[bytes]]
            }
        ];
        If[results === $Failed || Head[results] === HTTPError, 
            $Failed, 
            Association[results]
        ]
    ]
    
FormatRawData[objects_List, elems_, rawData_] := AssociationMap[FormatRawData[#, elems, rawData]&, objects]

FormatRawData["All", elems_, rawData_] := FormatRawData[Most[$deploymentTypes], elems, rawData]

FormatRawData[object_, elems_List, rawData_] := 
    Module[{category = If[MemberQ[$deploymentTypes, object], "deploymentTypes", "objects"], data, times},
        data = Lookup[rawData[category], object];
        times = FormatTimeData[rawData["times"]];
        AssociationMap[FormatDataSummary[Lookup[data, Lookup[$summaryMapping, #]], times, #]&, elems]
    ]
    
FormatRawData[object_, elem_, rawData_] :=
    Module[{category = If[MemberQ[$deploymentTypes, object], "deploymentTypes", "objects"], data, times},
        data = Lookup[rawData[category], object];
        times = FormatTimeData[rawData["times"]];
        FormatDataSummary[Lookup[data, Lookup[$summaryMapping, elem]], times, elem]
    ]

SelectDeployments[objs_] := Select[Flatten[{objs}], MemberQ[$deploymentTypes, #] &]

$deploymentTypes = {"API", "Form", "Computation", "Task", "CDF", "All"};

summariesNeeded[elems_] := DeleteMissing[DeleteDuplicates[Flatten[{Lookup[$summaryMapping, elems]}]]] (*Need to keep the brackets to prevent failures*)

$summaryMapping = {"TotalCalls" -> "CallTotal", 
   "AverageCallRate" -> "CallTotal", 
   "CallRateTimeSeries" -> "CallCount", 
   "TotalCredits" -> "CreditUsageTotal", 
   "CreditTally" -> "CreditTally",
   "AverageCreditUsageRate" -> "CreditUsageTotal", 
   "AverageCreditsPerCall" -> {"CallTotal", "CreditUsageTotal"}, 
   "CreditUsageTimeSeries" -> "CreditCount", 
   "ObjectsUsed" -> "ObjectsUsed", 
   "CallDetails" -> {"ObjectsUsed", "CallCount"},
   "CalledObjectsCount" -> {"ObjectsUsed", "CallCount"},
   elem_String :> (Message[System`CloudLoggingData::inelem, elem];Missing["NotAvailable"])};

toStandardListString[list_List] := StringJoin@@Riffle[list, ","]
toStandardListString[{}] := ""
toStandardListString[{elem_}] := elem
toStandardListString[elem_String] := elem

FormatTimeData[times_List] := Map[fromStandardTimeString, times]

FormatDataSummary[data_, _, "TotalCalls"] := data
FormatDataSummary[data_, times_, "AverageCallRate"] := 
    N[((data/Length[times])/DateDifference[times[[1]], times[[2]]])]
FormatDataSummary[data_, times_, "CallRateTimeSeries"] := 
    TimeSeries[data, {times}]
FormatDataSummary[data_, _, "TotalCredits"] := Abs[data]
FormatDataSummary[data_, times_, "AverageCreditUsageRate"] := 
    N[Abs[((data/Length[times])/DateDifference[times[[1]], times[[2]]])]]
FormatDataSummary[{calls_, credits_}, _, "AverageCreditsPerCall"] := 
    If[calls === 0, 0, Abs[N[credits/calls]]]
FormatDataSummary[data_, times_, "CreditUsageTimeSeries"] := 
    TimeSeries[Abs/@data, {times}]
FormatDataSummary[data_, _, "ObjectsUsed"] := data
FormatDataSummary[{objects_, _}, times_, "CallDetails"] := createCallDetails[objects, times]
FormatDataSummary[{}, _, "CallDetails"] := Missing["NotAvailable"]
FormatDataSummary[{objects_, _}, times_, "CalledObjectsCount"] := createCallCount[objects, times]
FormatDataSummary[{}, _, "CalledObjectsCount"] := Missing["NotAvailable"]
FormatDataSummary[data_, _, "CreditTally"] := Association[Map[Apply[Rule, #] &, data]]

FormatDataSummary[_, _, _String] := Missing["NotAvailable"]

createCallDetails[{}, times_] := {}

createCallDetails[objects_, times_] :=
    Module[{start = times[[1]], end, stepDuration, rawData, detailTimes, detailObjectsData},
        stepDuration = DateDifference[start, times[[2]]];
        end = times[[-1]] + stepDuration;
        rawData = Quiet[Check[getLoggingData[objects, {"CallRateTimeSeries", "CreditUsageTimeSeries"},
         {"start" -> toStandardTimeString[start], 
             "end" -> toStandardTimeString[end], 
             "stepDuration" -> toStandardStepDurationString[stepDuration]}], $Failed]];
        If[rawData === $Failed, Return[$Failed]];
        detailTimes = FormatTimeData[rawData["times"]];
        detailObjectsData = rawData["objects"];
        Select[SortBy[Join @@ Map[createDetails[detailTimes, #[[2]], #[[1]], {"CallCount", "CreditCount"}] &, 
            detailObjectsData // Normal], #["Time"] &], #["Calls"]=!=0&]
    ]

createDetails[times_, data_, uuid_, elems_List] :=
    Map[Association["Time" -> First[#], 
        Sequence[MapThread[Rule, {detailsDataTitles[elems], Abs/@Rest[#]}]], 
        "UUID" -> uuid] &, 
        Transpose[Prepend[Lookup[data, Flatten[{elems}]], times]]]
  
detailsDataTitles[elems_] := Map[StringReplace[#, "Count"->"s"]&, elems]  

createCallCount[objects_, times_] :=
    Module[{start = times[[1]], end, stepDuration, data},
        stepDuration = DateDifference[start, times[[2]]];
        end = times[[-1]] + stepDuration;
        data = Select[CloudLoggingData[objects, {start, end}, "TotalCalls"], #>0&]
    ]
      
End[]

EndPackage[]
