(* :Title: Benchmark *)

(* :Authors: Schoeller Porter *)

(* :Context: Benchmarking` *)

(* :Package Version: 8.0 *)

(* :Copyright: Copyright 1990-2016, Wolfram Research, Inc.  *)

(* :History:
	Version 5.1 by Schoeller Porter, September 2004.
	Version 5.1.1 by Schoeller Porter, February 2005.
	Version 5.2 by Schoeller Porter, June 2005.
	Version 6.0 by Schoeller Porter, Feb 2006.
	Version 8.0 by Joshua Martell, Apr 2010.
  Version 9.0+ by Arnoud Buzing
*)

(* :Keywords: *)

(* :Source: *)

(* :Warning: *)

(* :Wolfram Language Version: 10.3 *)

(* :Limitation: *)


BeginPackage["Benchmarking`"];

(* === Usage & Options =================================================== *)

$Benchmarks = {"WolframMark"};
$Benchmarks::usage = "$Benchmarks is list of benchmarks that can be run by \
this package.";

If[!ValueQ[Benchmark::usage], Benchmark::usage = "\!\(\*RowBox[{\"Benchmark\", \"[\", \"]\"}]\) runs the \!\(\*StyleBox[\"WolframMark\", FontSlant -> \"Italic\"]\) benchmark."];
If[!ValueQ[BenchmarkReport::usage], BenchmarkReport::usage = "\!\(\*RowBox[{\"BenchmarkReport\", \"[\", \"]\"}]\) runs the \!\(\*StyleBox[\"WolframMark\", FontSlant -> \"Italic\"]\) benchmark and produces a report in a separate notebook comparing this system to a selection of reference systems. \n\!\(\*RowBox[{\"BenchmarkReport\", \"[\", RowBox[{SubscriptBox[StyleBox[\"system\", \"TI\"], StyleBox[\"1\", \"TR\"]], \",\", SubscriptBox[StyleBox[\"system\", \"TI\"], StyleBox[\"2\", \"TR\"]], \",\", StyleBox[\"\[Ellipsis]\", \"TR\"], \",\", SubscriptBox[StyleBox[\"data\", \"TI\"], StyleBox[\"1\", \"TR\"]], \",\", SubscriptBox[StyleBox[\"data\", \"TI\"], StyleBox[\"2\", \"TR\"]], \",\", StyleBox[\"\[Ellipsis]\", \"TR\"]}], \"]\"}]\) produces a custom report comparing the specified systems from $BenchmarkSystems and the specified data returned from Benchmark."];
If[!ValueQ[$BenchmarkSystems::usage], $BenchmarkSystems::usage = "$BenchmarkSystems gives the names of systems for which the \!\(\*StyleBox[\"WolframMark\", FontSlant -> \"Italic\"]\) benchmark data is known."];


BenchmarkReport::unknown = "None of the requested systems could be found \
in $BenchmarkSystems: `1`.";
BenchmarkReport::oneknown = "Only one of the requested systems was found \
in $BenchmarkSystems: `1`";
BenchmarkReport::onedata = "Only one benchmark result was supplied. Two or \
more sets of results are needed to make a comparison report.";

(* === Function Definitions ============================================== *)

Begin["`Private`"]; (* Benchmarking`Private` *)

Quiet[Needs["BarCharts`"]];
$CheckpointFile = "MBench-" <> $MachineName <> ".m";

$ReportFont = "Times New Roman";
$GraphicsFont = "Times New Roman";
$MachineNameGraphicsFontSize = 12;
$SystemGraphicsFontSize = 10;
$FixedReportFont = "Courier";
$FixedFontSize = 10;
$TitleFontSize = 18;

$HighlightColor = RGBColor[0.180392, 0.54902, 0.729412];
$GrayTextColor = GrayLevel[0.6];
$ColorGradient = {
	{0.882353, 0.882353, 0.882353}, {0.878431, 0.878431, 0.878431},
	{0.87451, 0.87451, 0.87451}, {0.870588, 0.862745, 0.862745},
	{0.862745, 0.858824, 0.854902}, {0.854902, 0.847059, 0.839216},
	{0.847059, 0.835294, 0.831373}, {0.843137, 0.831373, 0.815686},
	{0.835294, 0.815686, 0.807843}, {0.827451, 0.807843, 0.792157},
	{0.827451, 0.807843, 0.792157}, {0.815686, 0.792157, 0.780392},
	{0.803922, 0.784314, 0.768627}, {0.784314, 0.764706, 0.752941},
	{0.784314, 0.764706, 0.752941}, {0.772549, 0.756863, 0.741176},
	{0.760784, 0.741176, 0.733333}, {0.741176, 0.733333, 0.72549},
	{0.721569, 0.72549, 0.717647}, {0.709804, 0.713725, 0.717647},
	{0.690196, 0.709804, 0.717647}, {0.666667, 0.705882, 0.717647},
	{0.666667, 0.705882, 0.717647}, {0.635294, 0.701961, 0.717647},
	{0.603922, 0.698039, 0.717647}, {0.580392, 0.694118, 0.721569},
	{0.54902, 0.694118, 0.72549}, {0.54902, 0.694118, 0.72549},
	{0.501961, 0.690196, 0.729412}, {0.478431, 0.686275, 0.733333},
	{0.45098, 0.682353, 0.741176}, {0.45098, 0.682353, 0.741176},
	{0.423529, 0.67451, 0.745098}, {0.396078, 0.666667, 0.752941},
	{0.376471, 0.658824, 0.756863}, {0.360784, 0.647059, 0.760784},
	{0.34902, 0.635294, 0.764706}, {0.337255, 0.619608, 0.768627},
	{0.333333, 0.607843, 0.768627}, {0.329412, 0.584314, 0.772549},
	{0.329412, 0.584314, 0.772549}, {0.329412, 0.560784, 0.776471},
	{0.333333, 0.529412, 0.776471}, {0.341176, 0.498039, 0.780392},
	{0.341176, 0.498039, 0.780392}, {0.352941, 0.466667, 0.784314},
	{0.368627, 0.427451, 0.784314}, {0.384314, 0.388235, 0.788235},
	{0.403922, 0.345098, 0.792157}, {0.403922, 0.345098, 0.792157},
	{0.423529, 0.298039, 0.796078}, {0.447059, 0.254902, 0.8},
	{0.447059, 0.254902, 0.8}, {0.466667, 0.211765, 0.803922},
	{0.490196, 0.172549, 0.807843}, {0.490196, 0.172549, 0.807843},
	{0.509804, 0.133333, 0.807843}, {0.533333, 0.0901961, 0.811765},
	{0.545098, 0.0470588, 0.811765}, {0.568627, 0., 0.815686},
	{0.568627, 0., 0.815686}, {0.580392, 0., 0.815686},
	{0.6, 0., 0.819608}, {0.6, 0., 0.819608}
	};

$CharacterLookupTable = FromCharacterCode /@
	Join[Table[i, {i, 48, 57}], Table[i, {i, 65, 90}]];

(*
unsortedunion[]:
  Union[] that does not sort. Taken from the Help Browser.
*)
unsortedunion[x_] := Module[{f}, f[y_] := (f[y] = Sequence[]; y); f /@ x]

(*
mungeinput[]:
  mungeinput[] munges the given input into the proper input format for
  gettests[]. The definitions below are pretty self-explanitory.
*)
mungeinput[filter_] := Module[{mungedinput},
	mungedinput = Which[
		Depth[filter] == 1, {{{filter}}},
		Depth[filter] == 2, Table[{{filter[[i]]}},
			{i, Length[filter]}],
		Depth[filter] == 3, {filter},
		Depth[filter] == 4, filter
        	]; (* Which *)
	If[MemberQ[Map[StringQ, Flatten[mungedinput]], False],
		Return[{}],
		Return[mungedinput]
		] (* If *)
] (* mungeinput *)


(*
formatresults[]:
  formatresults[] munges the numeric input so that it always
  returns a string with the specified number of decimal places.
*)
formatresults[a_Integer, b_] := formatresults[N[a], b]

formatresults[a_Real, b_] := ToString[Quiet@NumberForm[a, {Infinity, b}, NumberPadding -> {"","0"}]]


(*printTemporaryOrPrint*)
printTemporaryOrPrint := If[$Notebooks,
	PrintTemporary,
	Print]


(*
getbenchmarks[]:
  getbenchmarks[] gets a sublist from the given list of benchmark tests that
  match the given keyword in the Keywords or TestName tags.
*)
getbenchmarks[benchmarklist_List, keyword_?StringQ] :=
Module[{testidlist, keywordlist},
	testidlist = Cases[benchmarklist,
		{___, "TestName" -> keyword, ___}];
	keywordlist = Cases[benchmarklist,
		{___, "Keywords" -> {___, keyword, ___}, ___}];
	Return[Union[testidlist, keywordlist]]
] (* getbenchmarks *)


(*
gettests[]:
  gettests[] selects a subset of the entire list of available benchmark
  tests (stored in the variable benchmarktests). Its takes a structed list
  as input.

  This input list structure defines how to combine the keywords to select
  a particular subset of tests. The general structure is:

    {{{"a", "b", ...}, {"c", "d", ...}}, ...}

  So, this will find all tests with both keywords "a" and "b" but
  not both keywords "c" and "d" and so on.
*)
gettests[filter_] :=
Module[{testsgotten, sortedtestids, sortedtests},
	testsgotten = Union @@ Map[Complement @@ Map[Intersection @@
		Map[getbenchmarks[benchmarktests, #]&, #]&, #]&, filter];
	sortedtestids = Sort[Map[("TestName" /. #)&, testsgotten]];
	sortedtests = Flatten[Map[getbenchmarks[benchmarktests, #]&,
		sortedtestids], 1];
	Return[sortedtests]
] (* gettests *)


(*
getlibraryfromsystem[]
*)
getlibraryfromsystem[systems__String] :=
Module[{benchmarksystems, truefalselist, pos},
	benchmarksystems = (("MachineName" /. #) <> " (" <> \
		("System" /. #) <> ")")& /@ $BenchmarkLibrary;
	truefalselist = Table[
		StringMatchQ[{systems}[[i]], #]& /@ benchmarksystems,
		{i, Length[{systems}]}];
	pos = Union[Flatten[Position[#, True] & /@ truefalselist]];
	Return[$BenchmarkLibrary[[pos]]]
];


(*
makescore[]:
  makescore[] calculates the overall score of the benchmark as the
  geometric mean of the run times of the individual tests.
*)
makescore[data_List, benchmark_String] :=
Module[{overallscore, reference},
	reference = "TotalTime" /. First[Cases[Cases[$BenchmarkLibrary,
		{___, "BenchmarkName" -> benchmark, ___}],
		{___, "BenchmarkReference" -> True, ___}]];
	overallscore = ToExpression[formatresults[
		(reference / (Plus @@ #))&[data[[All,2]]], 3]];
	Return[overallscore]
] (* makescore *)


(*
makegridboxtable[]:
*)
makegridboxtable[datatable_List] := Map[Which[
		NumericQ[#], "\<\"" <> formatresults[#, 2] <> "\"\>",
		StringQ[#], "\<\"" <> StringReplace[#, "\n" -> "\\n"] <> "\>\"",
		True, #
	]&, datatable, {2}]


(*
makecomparisontable[]:
  makecomparison[] generates the comparison table displayed in the
  report notebook generated by BenchmarkReport[].
*)
makecomparisontable[ndata_, hdata_, testids_, benchmarkname_String] :=
Module[
	{results, nresults, hresults, gridboxdata, tabledata, orderlist,
		coloring, title, caption},
	nresults = With[{mydata = #}, Flatten[{("BenchmarkResult"/.#),
		GridBox[{
			{StyleBox["\<\"" <> ("MachineName" /. #) <> "\>\"",
				FontFamily -> $GraphicsFont,
				FontWeight -> "Bold",
				FontSize -> $MachineNameGraphicsFontSize]},
			{StyleBox["\<\"" <> ("System" /. #) <> "\>\"",
				FontFamily -> $GraphicsFont,
				FontColor -> $GrayTextColor,
				FontSize -> $SystemGraphicsFontSize]}
		}, ColumnAlignments -> Right, RowSpacings -> 0.2]& [mydata],
		("TotalTime" /. mydata),
		("Results" /. mydata)[[All, 2]]
		}]] & /@ ndata;
	hresults = With[{mydata = #}, Flatten[{("BenchmarkResult"/.#),
		GridBox[{
			{StyleBox["\<\">> " <> ("MachineName" /. #)
				<> " <<\"\>",
				FontFamily -> $GraphicsFont,
				FontColor -> $HighlightColor,
				FontSize -> $MachineNameGraphicsFontSize,
				FontWeight -> "Bold"]},
			{StyleBox["\<\"" <> ("System" /. #) <> "\>\"",
				FontFamily -> $GraphicsFont,
				FontColor -> GrayLevel[0.6],
				FontSize -> $SystemGraphicsFontSize]}
		}, ColumnAlignments -> Right, RowSpacings -> 0.2]& [mydata],
		("TotalTime" /. mydata),
		("Results" /. mydata)[[All, 2]]
		}]] & /@ hdata;
	results = Drop[#,1]& /@ (With[{sortdata = #},
			sortdata[[Ordering[(#[[1]]) & /@ sortdata,
				All, Greater]]]]&[Join[nresults,hresults]]);
	orderlist = Ordering /@ Drop[Transpose[results], 1];
	coloring = Part[Transpose[Sort[Transpose[
		{#, Table[x, {x, 1.0, 0.01, -0.99/(Length[#] - 1)}]}
		]]], 2] & /@ orderlist;
	gridboxdata = Transpose[With[
		{transdata = Transpose[makegridboxtable[results]]},
		Join[{transdata[[1]]},
      		Map[StyleBox[GridBox[{{#[[1]]}}, ColumnWidths -> 4.5,
      			RowMinHeight -> 3.5,
			RowAlignments -> Center],
			FontColor -> If[#[[2]] > 0.5, GrayLevel[0.9],
				GrayLevel[0]],
			FontFamily -> $GraphicsFont,
			FontSize -> $SystemGraphicsFontSize,
   			Background -> RGBColor @@ ($ColorGradient[[
   					Ceiling[Length[$ColorGradient]
					* #[[2]]]]])]&,
		MapThread[{#1, #2}&, {Drop[transdata,1], coloring}, 2], {2}]
		]]];

	tabledata = {GridBox[Join[{StyleBox[#,FontFamily -> $GraphicsFont,
            FontSize -> $MachineNameGraphicsFontSize,
            FontWeight -> "Bold"]&/@
       	 	Join[{"\<\" \"\>","\<\"Total\"\>"},#&/@(("\<\"Test "
			<> ToString[#] <> "\"\>")& /@
			(Range[Length[testids]]))]}, gridboxdata],
            ColumnAlignments -> {"Right", "Center"}]};
    title = {GridBox[{{
		GridBox[{{" "}},
			ColumnWidths -> $MachineNameGraphicsFontSize * 1.2],
		GridBox[{{
			StyleBox[benchmarkname,
				FontFamily -> $GraphicsFont,
				FontSize -> $TitleFontSize,
				FontColor -> $HighlightColor,
				FontSlant -> "Italic"],
			StyleBox["Detailed Timings",
				FontFamily -> $GraphicsFont,
				FontSize -> $TitleFontSize,
				FontColor -> $GrayTextColor]
			}}, ColumnAlignments -> {"Right","Left"},
				ColumnWidths -> $MachineNameGraphicsFontSize
					* 2.03]
		}}, RowMinHeight -> 6]};
    caption = {GridBox[{{
		GridBox[{{" "}},
			ColumnWidths -> $MachineNameGraphicsFontSize * 1.2],
    	GridBox[{{
    		StyleBox["\<\"Timings are CPU time in seconds\"\>",
  			FontFamily -> $GraphicsFont,
			FontSize -> $SystemGraphicsFontSize,
			FontColor -> $GrayTextColor]
    		}}, ColumnWidths -> $MachineNameGraphicsFontSize * 4.15]
		}}]};
	Return[{title, tabledata, caption}]
] (* makecomparisontable *)


(*
makereportplot[]:
  makereportplot[] generates the comparison plot displayed in the
  report notebook generated by BenchmarkReport[].
*)
makereportplot[data_List, label_String] :=
Module[{flipdata, mydata, rest, restplotdata, myplotdata, plotdata,
		highlightpos, chart, maxnum},
	flipdata = Transpose[{Reverse[#1], #2, #3, #4} & @@ Transpose[data]];
	highlightpos = Flatten[Position[flipdata[[All,4]],True]];

	mydata = flipdata[[highlightpos]];
	If[Length[mydata] == 0,
		plotdata = {#[[1]], #[[3]], 0.5} & /@ flipdata,
		myplotdata = {#[[1]], #[[3]], 0.5} & /@ mydata;
		rest = flipdata[[Complement[Range[Length[flipdata]],
			highlightpos]]
		]; (* If *)
	restplotdata = If[Length[rest] === 0,
		{{1, 0, 0}}, {#[[1]], #[[3]], 0.5} & /@ rest];
	plotdata = {myplotdata, restplotdata}];

	maxnum = Max[data[[All,3]]];

	chart = GeneralizedBarChart[plotdata,
		Axes -> False,
		BarOrientation -> Horizontal,
		BarStyle -> If[Length[highlightpos] == 0, GrayLevel[0.6],
			{$HighlightColor, GrayLevel[0.6]}],
		DisplayFunction -> Identity,
        Frame -> True,
        BarEdges -> False,
        PlotRange -> {{-0.02, maxnum + .2}, {0.3, Length[data] + 0.7}},
        FrameLabel -> {Style["Faster systems give larger numbers",
		FontFamily -> $ReportFont, FontColor -> GrayLevel[.6]],
		None,
		Style[label <> " \!\(\* StyleBox[\"System Comparison\",\nFontSlant->\"Plain\",\nFontColor-> GrayLevel[.6]]\)",
			FontColor -> $HighlightColor, FontSlant -> "Italic",
			FontFamily -> $ReportFont, FontSize -> 18.], None},
		Epilog -> {Text[
		Style[formatresults[#[[3]], 2], FontSize -> 11,
			FontFamily -> $ReportFont], {#[[3]] + .01, #[[1]]},
				{-1, 0}] & /@ flipdata},
		FrameTicks -> {None, {#[[1]], Grid[
			With[{compname = #[[1]], OSname = If[Length[#] > 1,
				#[[2]], ""]},
			{If[#[[3]],
			{Style[">> " <> compname <> " <<",
				FontFamily -> $ReportFont,
				FontWeight -> "Bold",
				FontSize -> 12.,
				FontColor -> $HighlightColor]},
			{Style[compname, FontFamily -> $ReportFont,
				FontWeight -> "Bold", FontSize -> 12.]}],
			{Style[OSname, FontFamily -> $ReportFont,
				FontColor -> GrayLevel[.5], FontSize -> 11.]}
			}]&[Join[StringSplit[#[[2]],"\n"],{#[[4]]}]],
				ColumnAlignments -> Right], {0, 0},
				{Opacity[0],AbsoluteThickness[0]}}&
				/@ flipdata,
			None, None},
		GridLines -> {{#, {GrayLevel[.6], AbsoluteDashing[{3, 6}]}}&
			/@ Range[.25, maxnum + .1, .25], None},
		AspectRatio -> Length[data]/15,
		ImageSize -> 900];
	Return[chart]
] (* makereportplot *)


(*
sortresult[]:
*)
sortresult[listtosort_List, order_:Greater] := listtosort[[#]] & /@
	Ordering[listtosort[[All, 2]], All, order]

(*
mungecode[]:
*)
mungecode[testid_String] := First[ToBoxes[First[ToExpression[
	StringReplace[StringReplace[ToString[InputForm[
		HoldForm["Code"] /. gettests[mungeinput[testid]]
		]], "Benchmarking`Private`" -> ""],
		"benchmark" -> "Benchmarking`Private`benchmark"]
	]], StandardForm]] (* mungecode *)


(*
benchmarkcompare[]
  benchmarkcompare[] generates a comparison report of the given benchmark
  output, either in text or notebook form.
*)
Options[benchmarkcompare] = {"Report" -> "Notebook"};
benchmarkcompare[data__List, opts___Rule] := Module[
	{reportnb, benchmark, fullcomplist, textreport, nbreportopt,
		summarycomplist, highlightopt, highlightlist,
		showdetailedtime},
	nbreportopt = "Report" /. {opts} /. Options[benchmarkcompare];
	highlightopt = "Highlight" /. {opts} /. {"Highlight" -> {{""}}};

	showdetailedtime = If[MemberQ["Results" /. {data}, "Results"],
		False, True];

	fullcomplist = With[{sortdata = #},
		sortdata[[Ordering[("BenchmarkResult" /. #) & /@ sortdata,
			All, Greater]]]]&[List[data]];
	benchmark = Union["BenchmarkName" /. $BenchmarkLibrary];
	If[Length[benchmark] != 1, Return[]];
	benchmark = First[benchmark];

	testidlist = Sort["TestName" /.Cases[benchmarktests, {___,
		"Keywords" -> {___, benchmark, ___}, ___}]];

	summarycomplist = {"TestName" -> "Summary", "ComparisonTable" ->
 		Transpose[Join[{Range[Length[fullcomplist]]},
			Transpose[sortresult[{("MachineName" /. #) <> "\n"
			<> ("System" /. #), "BenchmarkResult" /. #,
			With[{benchmarkresult=#}, Or @@ (If[StringMatchQ[
				("MachineName" /. benchmarkresult), #[[1]]]
				&& StringMatchQ[
				("System"/.benchmarkresult),#[[2]]],
			True,False]& /@ highlightopt)]}& /@ fullcomplist]]
		]]};

	highlightlist = fullcomplist[[Flatten[Position[
		("ComparisonTable" /. summarycomplist)[[All,4]], True]]]];

	textreport = StringJoin[
		"=== " <> benchmark <> " System Comparison ===\n\n",
		StringReplace[ToString[TableForm[Join[
			{("MachineName" /. #) <> "\n" <> ("System" /. #),
			ToString[Quiet@NumberForm[("BenchmarkResult" /. #), {4,2},
			NumberPadding -> {"", "0"}]] <> "\n"}&
		/@ fullcomplist], TableSpacing -> {2,3}]], "\n\n" -> "\n"],
		"\n\n(Faster systems give larger numbers)\n"];
	If[showdetailedtime,
		textreport = StringJoin[textreport,
		"\n\n",
		"=== " <> benchmark <> " Detailed Timings ===\n\n",
		StringReplace[ToString[TableForm[Join[
			{Join[{" ", "Total"}, Table["Test " <> ToString[i],
				{i, Length[testidlist]}]]},
 			Join[{("MachineName" /. #) <> "\n"
				<> ("System" /. #),
				Quiet@NumberForm[("TotalTime" /. #), {4,1},
 					NumberPadding -> {"", "0"}]},
 				Quiet@NumberForm[#, {4,2},
 					NumberPadding -> {"", "0"}]&
					/@ (("Results" /. #)[[All,2]])
 				]& /@ fullcomplist
			], TableSpacing -> {2,2}]], "\n\n" -> "\n"],
		"\n\n(Timings are CPU time in seconds)\n"
		]];
	If[highlightlist != {},
		textreport = StringJoin[
			"=== System Information ===\n\n",
			StringJoin @@ (StringJoin[#, "\n\n"] & /@ (
			StringReplace[ToString[TableForm[#]],
				"\n\n" -> "\n"]& /@ ({
				{"Machine Name:", "MachineName" /. #},
				{"System:", "System" /. #},
				{"Date:", "Date" /. #},
				{"Wolfram Language Version:",
					"FullVersionNumber" /. #},
				{"Benchmark Result:", ToString[
           				Quiet@NumberForm[("BenchmarkResult" /. #),
					{4, 2}, NumberPadding -> {"", "0"}]]}
			} & /@ highlightlist)
			)), "\n", textreport], textreport
		]; (* If *)

	If[nbreportopt === "Notebook",
	reportnb = NotebookPut[Notebook[{},
		WindowTitle -> benchmark <> " Report",
		WindowSize -> {1200, 850}]];

	NotebookWrite[reportnb, {
	Cell[TextData[{StyleBox[benchmark, FontSlant -> "Italic"],
		" Benchmark Report"}], "Title",
		FontFamily -> $ReportFont]}];
	If[highlightlist != {},
		NotebookWrite[reportnb, {
		Cell[CellGroupData[
			Cell[BoxData[GridBox[makegridboxtable[{
				{"Machine Name:", "MachineName" /. #},
         			{"System:", "System" /. #},
				{"Date:", "Date" /. #},
				{"Wolfram Language Version:",
					"FullVersionNumber" /. #},
				{"Benchmark Result:", ToString[NumberForm[
					("BenchmarkResult" /. #), {4,2},
					NumberPadding -> {"","0"}]]}
         	}], ColumnAlignments -> Left, ColumnSpacings -> 2]],
			"Text", ShowStringCharacters -> False,
			CellMargins -> {{Inherited,Inherited},
			{Inherited, 30}}, FontFamily -> $ReportFont]& /@
			highlightlist, Open],"Output",FontFamily -> $ReportFont]}]
		]; (* If *)
	NotebookWrite[reportnb, {
	Cell[TextData[{StyleBox[benchmark, FontSlant -> "Italic"],
		" Results"}], "Section",
		FontFamily -> $ReportFont, CellFrame -> False],
	Cell[BoxData[ToBoxes[makereportplot["ComparisonTable" /. summarycomplist,
			benchmark]]], "Graphics", FontFamily -> $ReportFont,
			CellHorizontalScrolling -> True,
			CellMargins -> {{64, Inherited},{Inherited,Inherited}}]
	}];
	If[showdetailedtime, NotebookWrite[reportnb, {
	Cell[BoxData[GridBox[
		makecomparisontable[Complement[fullcomplist, highlightlist],
			highlightlist, testidlist, benchmark],
		ColumnAlignments -> "Left", RowMinHeight -> 2]],
		"Text", CellHorizontalScrolling -> True, ShowStringCharacters -> False]
	}]];
	NotebookWrite[reportnb, {
	CellGroupData[Join[
 		{Cell[TextData[{StyleBox[benchmark, FontSlant -> "Italic"],
			" Sources"}], "Section", FontFamily -> $ReportFont,
			CellFrame -> False]},
 		CellGroupData[{
		Cell["Test " <> ToString[Position[testidlist, #][[1,1]]] <>
			": " <> #, "Subsection", FontFamily -> $ReportFont,
			CellDingbat -> None],
		Cell[BoxData[First[ToBoxes[First[ToExpression[
			StringReplace[ToString[InputForm[
				HoldForm["Code"] /. gettests[mungeinput[#]]
			]], {"Benchmarking`Private`" -> "",
			"BenchmarkTiming" -> "AbsoluteTiming"}]
			]], StandardForm]]], "Input"]
		}, Closed]& /@ testidlist
		], Open],
	Cell["", "Section", CellFrame -> False, ShowCellBracket -> False],
	CellGroupData[{
		Cell["[Plain Text Version]", "Subsubsection",
			ShowGroupOpenCloseIcon->True,
			FontFamily -> $FixedReportFont, FontSize -> 14,
			CellDingbat -> None, ShowCellBracket -> False],
		Cell[textreport, "Text", FontFamily -> $FixedReportFont,
			FontSize -> $FixedFontSize, PageWidth -> Infinity,
			CellHorizontalScrolling -> True]
	}, Closed],
	Cell["", "Section", CellFrame -> False, ShowCellBracket -> False,
		FontSize -> 8, CellMargins -> {{0,0},{0,0}}],
	Cell[TextData[{
 		"This ", StyleBox[benchmark,
			FontSlant -> "Italic"],
		" report was created with Wolfram Language ",
		First[StringSplit["ProductVersion" /. $ProductInformation]],
		". ", StyleBox["WolframMark", FontSlant -> "Italic"],
		" is a trademark of Wolfram Research, Inc."
		}], "SmallText", ShowCellBracket -> False]
	}];
	SelectionMove[reportnb, All, Notebook];
	SelectionMove[reportnb, Before, Cell];
	Return[reportnb],
	Return[textreport]
	] (* If nbreportopt *)
] (* benchmarkcompare *)


(*
BenchmarkTiming[]:
*)
SetAttributes[BenchmarkTiming, HoldAll];
BenchmarkTiming[func_] :=
Module[{runtime},
	ClearSystemCache[];
	runtime = AbsoluteTiming[func][[1]] /. {Second -> 1};
	SeedRandom[];
	Return[{ToExpression[formatresults[runtime, 3]]}]
] (* BenchmarkTiming *)


(*
BenchmarkReport[]:
  Benchmark[] is a wrapper for the Benchmark[] and Private`makereportnb[]
  functions.
*)
BenchmarkReport::benchnotfnd = "The requested benchmark `1` could not be \
found in $Benchmarks";
BenchmarkReport::rptype = "Value of option in ReportType -> `1` is not \
\"Notebook\" or \"Text\".";
BenchmarkReport::vrbs = "Value of option in Verbose -> `1` is not True \
or False.";

BenchmarkReport[opts___Rule] :=
Module[{results, reporttypeopt, verboseopt, compare},
	{benchmarktorun, verboseopt} = {"BenchmarkName", "Verbose"} /. {opts}
		/. {"BenchmarkName" -> "WolframMark",
		"Verbose" -> True};
	reporttypeopt = "ReportType" /. {opts} /. If[$FrontEnd =!= Null,
		{"ReportType" -> "Notebook"}, {"ReportType" -> "Text"}];
	If[!MemberQ[$Benchmarks, benchmarktorun],
		Message[BenchmarkReport::benchnotfnd, benchmarktorun];
		Return[]];
	If[!MemberQ[{"Notebook", "Text"}, reporttypeopt],
		Message[BenchmarkReport::rptype, reporttypeopt];Return[]];
	If[!MemberQ[{True, False}, verboseopt],
		Message[BenchmarkReport::vrbs, verboseopt];Return[]];
	results = First[Benchmark["BenchmarkName" -> benchmarktorun,
		Verbose -> verboseopt]];
	compare = benchmarkcompare[Sequence @@ $BenchmarkLibrary, results,
		"Highlight" -> {{
			("MachineName" /. results),("System" /. results)
			}},
		"Report" -> reporttypeopt];
	Return[compare]
] (* BenchmarkReport["benchmark"]/Benchmark[]*)

BenchmarkReport[input:(_List | _InputForm).., opts___Rule]:=
Module[{benchmarksystems, benchmarkdata, benchmarklibrarydata={}},
	benchmarksystems = Union@@Cases[{input}, {__String}];
	benchmarkdata = Join[
		Cases[First /@ Cases[{input}, _InputForm], {__Rule}],
		Cases[{input}, {__Rule}]
		];
	reporttypeopt = "ReportType" /. {opts} /. If[$FrontEnd =!= Null,
		{"ReportType" -> "Notebook"}, {"ReportType" -> "Text"}];
	If[!MemberQ[{"Notebook", "Text"}, reporttypeopt],
		Message[BenchmarkReport::rptype, reporttypeopt];Return[]];
	If[Length[benchmarksystems] > 0,
		benchmarklibrarydata = getlibraryfromsystem[Sequence@@ \
			benchmarksystems];
		If[Length[benchmarklibrarydata] === 0,
			Message[BenchmarkReport::unknown, StringReplace[
				ToString[benchmarksystems],
				{"{"->"","}"->""}]];
			Return[$Failed]
		];
		If[Length[benchmarklibrarydata] === 1 &&
			Length[benchmarkdata] === 0,
			Message[BenchmarkReport::oneknown, (
				("MachineName" /. #) <> " (" <>
				("System" /. #) <> ")"
				)&[benchmarklibrarydata]];Return[$Failed]
		],
		If[Length[benchmarkdata] === 1,
			Message[BenchmarkReport::onedata,
				{benchmarksystems}];
			Return[$Failed]
		]
	];
	compare = benchmarkcompare[Sequence@@benchmarklibrarydata,
		Sequence@@benchmarkdata,
		"Highlight" -> If[Length[{benchmarkdata}] =!= 0,
			{("MachineName" /. #),("System" /. #)}& /@
			{benchmarkdata}, {{""},{""}}],
		"Report" -> reporttypeopt];
	Return[compare]
] (* BenchmarkReport[{data1, data2, ...}, {"system1", "system2"}, ...] *)

BenchmarkReport[input:(_String | _InputForm | _List).., opts___Rule]:=
Module[{benchmarklibrarydata, benchmarkdata, benchmarksystems,
		compare, reporttypeopt},
	benchmarksystems = Union[Cases[{input}, _String]];
	benchmarkdata = Join[
		Cases[First /@ Cases[{input}, _InputForm], {__Rule}],
		Cases[{input}, {__Rule}]
		];
	reporttypeopt = "ReportType" /. {opts} /. If[$FrontEnd =!= Null,
		{"ReportType" -> "Notebook"}, {"ReportType" -> "Text"}];
	If[!MemberQ[{"Notebook", "Text"}, reporttypeopt],
		Message[BenchmarkReport::rptype, reporttypeopt];Return[]];
	If[Length[benchmarksystems] > 0,
		benchmarklibrarydata = getlibraryfromsystem[Sequence@@ \
			benchmarksystems];
		If[Length[benchmarklibrarydata] === 0,
			Message[BenchmarkReport::unknown, StringReplace[
				ToString[benchmarksystems],
				{"{"->"","}"->""}]];
			Return[$Failed]
		];
		If[Length[benchmarklibrarydata] === 1 &&
			Length[benchmarkdata] === 0,
			Message[BenchmarkReport::oneknown, (
				("MachineName" /. #) <> " (" <>
				("System" /. #) <> ")"
				)&[benchmarklibrarydata]];Return[$Failed]
		]
	];
	compare = benchmarkcompare[Sequence@@benchmarklibrarydata,
		Sequence@@benchmarkdata,
		"Highlight" -> If[Length[{benchmarkdata}] =!= 0,
			{("MachineName" /. #),("System" /. #)}& /@
			{benchmarkdata}, {{""},{""}}],
		"Report" -> reporttypeopt];
	Return[compare]
] (* BenchmarkReport["system1", "system2", ...]/
	BenchmarkReport["system1", "system2", ... , data1, data2, ...] *)


(*
Benchmark[]:
  Benchmark[] is the user interface to BenchmarkTiming[] function. There is
  no input. There are two options, BenchmarkName: a string or some variation
  of a list of strings that correspond to the Keywords and TestName tags for
  the benchmark tests (See the comments gettests[] for more details) and
  Verbose (True/False) which toggles verbose reporting.
*)
Benchmark::benchnotfnd = "The requested benchmark `1` could not be found \
in $Benchmarks";
Benchmark::vrbs = "Value of option in Verbose -> `1` is not True \
or False.";
Benchmark[opts___Rule] :=
Module[{allbenchmarkstorun, benchmarkstorun, benchmarksran = {}, testresult,
		resultsran = {}, results, finaloutput, testid,
		months, verboseopt, checkpointopt, runinparallel,
		systemnames, machinename, totalruntime, pfactor},
	{benchmarktorun, verboseopt} = {"BenchmarkName", "Verbose"} /. {opts}
		/. {"BenchmarkName" -> "WolframMark",
			"Verbose" -> True};
	If[!MemberQ[$Benchmarks, benchmarktorun],
		Message[Benchmark::benchnotfnd, benchmarktorun];Return[]];
	If[!MemberQ[{True, False}, verboseopt],
		Message[BenchmarkReport::vrbs, verboseopt];Return[]];
	checkpointopt = "Checkpoint" /. {opts} /. {"Checkpoint" -> False};
	months = {"January", "February", "March", "April", "May",
		"June", "July", "August", "September", "October",
		"November", "December"};

	runinparallel = Positive[$KernelCount];

	If[runinparallel,
		If[checkpointopt,
			If[verboseopt, printTemporaryOrPrint["Disabling Checkpoint"]];
			checkpointopt = False
			]; (* If - checkpointopt *)
		ParallelNeeds["Benchmarking`"];
		pfactor = 3;
		systemnames = Union[Flatten[{$SystemID,
			ParallelEvaluate[$SystemID]}]];
		If[Length[systemnames] > 1,
			machinename = ToString[$KernelCount]
				<> "-node mixed cluster",
			machinename = ToString[$KernelCount]
				<> "-node homogeneous cluster"
			]; (* If *)
		systemnames = StringReplace[ToString[systemnames],
			{"{" -> "", "}" -> ""}],

		systemnames = $System;
		machinename = ToLowerCase[$MachineName]
		]; (* If - runinparallel *)

	allbenchmarkstorun = gettests[mungeinput[benchmarktorun]];
	If[allbenchmarkstorun == {},
		Message[Benchmark::nobnchmrks];Return[]];
	If[MemberQ[FileNames[#],#]&[$CheckpointFile],
		resultsran = ReadList[$CheckpointFile];
		benchmarksran = gettests[
			mungeinput[resultsran[[All,1]]]];
		benchmarkstorun = Complement[allbenchmarkstorun,
			benchmarksran],
		benchmarkstorun = allbenchmarkstorun;
		]; (* If *)
	If[runinparallel,
		benchmarkstorun = Flatten[Table[benchmarkstorun,
			{pfactor * $KernelCount}], 1];
		benchmarkstorun = benchmarkstorun[[unsortedunion[
			Flatten[Append[RandomInteger[{1, #}, {3, #}], Range[#]]]
			]]]&[Length[benchmarkstorun]]
		]; (* If *)

	If[verboseopt, If[resultsran != {},
		printTemporaryOrPrint["Cached results found for: "<>
			StringReplace[ToString[resultsran[[All,1]]],
			{"{"->"","}"->""}]]
		]]; (* If - If *)
	If[runinparallel,
	DistributeDefinitions[benchmarkstorun];
	DistributeDefinitions[BenchmarkTiming];
	totalruntime = ToExpression[formatresults[(AbsoluteTiming[
		results = ParallelMap[(
			testid = "TestName" /. #;
			Prepend["Code" /. #, testid]
			) &, benchmarkstorun]
		][[1]] /. {Second -> 1}), 3]], (* ParallelMap *)
	results = Map[
		(testid = "TestName" /. #;
		If[verboseopt,printTemporaryOrPrint[
			"Test " <> ToString[Position[allbenchmarkstorun,
				#][[1,1]]] <>
			" of " <> ToString[Length[allbenchmarkstorun]] <>
			": " <> testid <> " ..."]
			]; (* If *)
   		testresult = Prepend["Code" /. #, testid];
		If[checkpointopt, Write[$CheckpointFile, testresult]];
			testresult) &,
		benchmarkstorun
  		]; (* Map *)
	totalruntime = ToExpression[formatresults[
			(Plus @@ results[[All,2]]), 3]]
	]; (* If *)
	results = Join[resultsran, results];
	If[checkpointopt,
		Close[$CheckpointFile];
		DeleteFile[$CheckpointFile];
		];
	finaloutput = {
		"MachineName" -> ("MachineName" /. {opts} /.
			("MachineName" -> machinename)),
		"System" -> ("System" /. {opts} /. ("System" ->
			ToString[systemnames])),
		"BenchmarkName" -> benchmarktorun,
		"FullVersionNumber" -> First[StringSplit["ProductVersion" /. $ProductInformation]],
		"Date" -> (months[[#2]] <> " " <> ToString[#3] <> ", "
			<> ToString[#1]) & @@ Take[Date[], 3],
		If[runinparallel,
		"BenchmarkResult" -> ToExpression[formatresults[(
			(pfactor * $KernelCount *
				First["TotalTime" /. Cases[$BenchmarkLibrary,
				{__, "BenchmarkReference" -> True, ___}]])
				/ totalruntime
			), 3]],
		"BenchmarkResult" -> makescore[results, benchmarktorun]],
		"TotalTime" -> totalruntime
		};

	If[Not[runinparallel],
		AppendTo[finaloutput, "Results" -> results]
	];
	Return[InputForm[finaloutput]]
] (* Benchmark *)



(* === Benchmark Tests =================================================== *)

(*
benchmarktests:
  The set of test programs used in the benchmark. The format is a list of
  rules with the lhs always a string:

    {"TestName" -> "foo", "Keywords" -> {"keyword1", "keyword2", ...}, ...}

  The valid rules are:

    TestName -> A unique string that identifies the test.

    Keywords -> A list of strings that describe the test. The keywords, along
        with the TestName are searched by the Benchmark[] command to
        determine what tests to run. "All" is automatically added.

    Code -> The benchmark test itself. This must be wrapped in HoldForm[]
        to delay evaluation until called by Benchmark[]. It's also a good
        idea to use Modules for scoping.

  Some of the rules are not explicitly required, eg if the test doesn't use
  any variables, then the LocalVariables and Initialization rules don't need
  to be there. It's a good idea to put all of them in for good measure,
  though.
*)
benchmarktests = {

{
	"TestName" -> "Polynomial Expansion",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		BenchmarkTiming[
			Expand[Times @@ Table[(c + x)^3, {c, #}]];
		]&[350]
},
{
	"TestName" -> "Matrix Arithmetic",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{m}, BenchmarkTiming[
			SeedRandom[1];
			m = RandomReal[{}, {#, #}];
			(* Exponent must be small enough to avoid machine overflow *)
			Do[(1. + 0.5*m)^127,{50}];
		]]&[840]
},
{
	"TestName" -> "Matrix Multiplication",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{m1,m2}, BenchmarkTiming[
			SeedRandom[1];
			m1 = RandomReal[{}, {#, #}];
			m2 = RandomReal[{}, {#, #}];
			Do[Dot[m1, m2], {12}]
		]]&[1050]
},
{
	"TestName" -> "Eigenvalues of a Matrix",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{a,b,m}, BenchmarkTiming[
			SeedRandom[1];
			a = RandomReal[{}, {#, #}];
			b = DiagonalMatrix[RandomReal[{}, {#}]];
			m = a.b.Inverse[a];
			Do[Eigenvalues[m],{6}]
		]]&[420]
},
{
	"TestName" -> "Digits of Pi",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		BenchmarkTiming[N[Pi, #];]&[1000000]
},
{
	"TestName" -> "Large Integer Multiplication",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{a}, BenchmarkTiming[
			SeedRandom[1];
			a = RandomInteger[{10^#, 10^(#+1)}, {}];
			Do[a*(a+1), {20}]
		]]&[1100000]
},
{
	"TestName" -> "Gamma Function",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{a}, BenchmarkTiming[
			SeedRandom[1];
			a = RandomInteger[{80000, 90000}, {#}];
			Gamma[a]
		]]&[55]
},
{
	"TestName" -> "Discrete Fourier Transform",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{data}, BenchmarkTiming[
			SeedRandom[1];
			data = RandomReal[{}, {#}];
			Do[Fourier[data], {11}]
		]]&[1200000]
},
{
	"TestName" -> "Numerical Integration",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		BenchmarkTiming[NIntegrate[Sin[x^2+y^2],
			{x, -#*Pi, #*Pi}, {y, -#*Pi, #*Pi}];
			]&[2.6]
},
{
	"TestName" -> "Matrix Transpose",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{m}, BenchmarkTiming[
			SeedRandom[1];
			m = RandomReal[{}, {#, #}];
			Do[Transpose[m],{40}]
		]]&[2070]
},
{
	"TestName" -> "Random Number Sort",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{a}, BenchmarkTiming[
			SeedRandom[1];
			a = RandomInteger[{1, 50000}, {#}];
			Do[Sort[a], {15}]
		]]&[520000]
},
{
	"TestName" -> "Data Fitting",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{data}, BenchmarkTiming[
			data = Flatten[Table[{x, y, z, Log[120*x] -
			Abs[Cos[1/300*z]/(140*y)]}, {x, .2, 10, #},
				{y, .2, 10, #}, {z, .2, 10, #}], 2];
			FindFit[data, Log[a*x] - Abs[Cos[b*z]/(c*y)],
				{a, b, c}, {x, y, z}, AccuracyGoal -> 6];
		]]&[0.22]
},
{
	"TestName" -> "Singular Value Decomposition",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{m}, BenchmarkTiming[
			SeedRandom[1];
			m = RandomReal[{}, {#, #}];
			Do[SingularValueDecomposition[m],{2}]
		]] &[860]
},
{
	"TestName" -> "Solving a Linear System",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{m, v}, BenchmarkTiming[
			SeedRandom[1];
			m = RandomReal[{}, {#, #}];
			v = RandomReal[{}, {#}];
			Do[LinearSolve[m, v], {16}]
		]] &[1150]
},
{
	"TestName" -> "Elementary Functions",
	"Keywords" -> {"WolframMark"},
	"Code" :>
		Module[{m1, m2}, BenchmarkTiming[
			SeedRandom[1];
			m1 = RandomReal[{}, {#}];
			m2 = RandomReal[{}, {#}];
			Do[Exp[m1]; Sin[m1]; ArcTan[m1, m2], {30}]
		]] &[2.2*10^6]
}
}; (* benchmarktests *)

(* This adds "All" to the Keywords rule for each test in benchmarktests. *)
benchmarktests = Map[# /. ("Keywords" -> _) -> ("Keywords" -> (Join[{"All"},
	"Keywords" /. #])) &, benchmarktests];

(*
$BenchmarkLibrary:
  A collection of the benchmark run on various machines.
*)
$BenchmarkLibrary = {
	{
    "MachineName" -> "3.07 GHz Core i7-950 (8 Cores)",
 	"System" -> "Windows 7 Pro (64-bit) Desktop",
 	"BenchmarkName" -> "WolframMark",
 	"FullVersionNumber" -> "8.0.0",
 	"Date" -> "May 7, 2010",
 	"BenchmarkResult" -> 1,
 	"TotalTime" -> 13.842,
 	"Results" -> {
 		{"Data Fitting", 0.797},
 		{"Digits of Pi", 0.984},
 		{"Discrete Fourier Transform", 1.},
   		{"Eigenvalues of a Matrix", 0.797},
   		{"Elementary Functions", 0.844},
   		{"Gamma Function", 1.},
   		{"Large Integer Multiplication", 0.984},
   		{"Matrix Arithmetic", 1.},
   		{"Matrix Multiplication", 0.781},
   		{"Matrix Transpose", 1.062},
   		{"Numerical Integration", 0.953},
   		{"Polynomial Expansion", 0.89},
   		{"Random Number Sort", 0.969},
   		{"Singular Value Decomposition", 0.922},
   		{"Solving a Linear System", 0.859}},
   	"BenchmarkReference"->True
   	},{
   	"MachineName" -> "Intel(R) Core(TM) i5-3550 CPU @ 3.30GHz",
   	"System" -> "Microsoft Windows (64-bit)",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "10.0.0",
	"Date" -> "January 17, 2014",
	"BenchmarkResult" -> 1.659,
	"TotalTime" -> 8.344,
	"Results" -> {
		{"Data Fitting", 0.469},
		{"Digits of Pi", 0.344},
		{"Discrete Fourier Transform", 0.484},
		{"Eigenvalues of a Matrix", 0.531},
		{"Elementary Functions", 0.797},
		{"Gamma Function", 0.453},
		{"Large Integer Multiplication", 0.469},
		{"Matrix Arithmetic", 0.625},
		{"Matrix Multiplication", 0.438},
		{"Matrix Transpose", 0.859},
		{"Numerical Integration", 0.688},
		{"Polynomial Expansion", 0.109},
		{"Random Number Sort", 1.},
		{"Singular Value Decomposition", 0.547},
		{"Solving a Linear System", 0.531}}
    },{
   	"MachineName" -> "2.4 Ghz Core 2 Duo Mobile T8300 (2 Cores)",
   	"System" -> "MacBook OS X Snow Leopard (64-bit) Laptop",
 	"BenchmarkName" -> "WolframMark",
 	"FullVersionNumber" -> "8.0.0",
 	"Date" -> "May 7, 2010",
 	"BenchmarkResult" -> 0.465,
 	"TotalTime" -> 29.758,
 	"Results" -> {
 		{"Data Fitting", 1.781},
   		{"Digits of Pi", 1.402},
   		{"Discrete Fourier Transform", 2.44},
   		{"Eigenvalues of a Matrix", 1.404},
   		{"Elementary Functions", 3.031},
   		{"Gamma Function", 1.128},
   		{"Large Integer Multiplication", 1.219},
   		{"Matrix Arithmetic", 2.202},
   		{"Matrix Multiplication", 2.347},
   		{"Matrix Transpose", 2.308},
   		{"Numerical Integration", 1.626},
   		{"Polynomial Expansion", 1.572},
   		{"Random Number Sort", 2.062},
   		{"Singular Value Decomposition", 2.856},
   		{"Solving a Linear System", 2.38}}
   	},{
   	"MachineName" -> "2.80 GHz Core 2 Duo Mobile T9600 (2 Cores)",
 	"System" -> "Windows 7 Pro (64-bit) Laptop",
 	"BenchmarkName" -> "WolframMark",
 	"FullVersionNumber" -> "8.0.0",
 	"Date" -> "May 7, 2010",
 	"BenchmarkResult" -> 0.670,
 	"TotalTime" -> 20.671,
 	"Results" -> {
 		{"Data Fitting", 1.123},
   		{"Digits of Pi", 1.201},
   		{"Discrete Fourier Transform", 1.903},
   		{"Eigenvalues of a Matrix", 1.061},
   		{"Elementary Functions", 2.044},
   		{"Gamma Function", 1.17},
   		{"Large Integer Multiplication", 1.154},
   		{"Matrix Arithmetic", 1.607},
   		{"Matrix Multiplication", 1.529},
   		{"Matrix Transpose", 1.607},
   		{"Numerical Integration", 1.186},
   		{"Polynomial Expansion", 1.061},
   		{"Random Number Sort", 1.217},
   		{"Singular Value Decomposition", 1.248},
   		{"Solving a Linear System", 1.56}}
   	},{
   	"MachineName" -> "1.6 GHz Core 2 Duo Mobile L7500 (2 Cores)",
 	"System" -> "Windows 7 Pro (32-bit) Laptop",
 	"BenchmarkName" -> "WolframMark",
 	"FullVersionNumber" -> "8.0.0",
 	"Date" -> "May 7, 2010",
 	"BenchmarkResult" -> 0.316,
 	"TotalTime" -> 43.805,
 	"Results" -> {
 		{"Data Fitting", 2.168},
   		{"Digits of Pi", 2.746},
   		{"Discrete Fourier Transform", 3.744},
   		{"Eigenvalues of a Matrix", 2.262},
   		{"Elementary Functions", 4.228},
   		{"Gamma Function", 2.902},
   		{"Large Integer Multiplication", 3.136},
   		{"Matrix Arithmetic", 3.463},
   		{"Matrix Multiplication", 3.416},
   		{"Matrix Transpose", 2.73},
   		{"Numerical Integration", 2.48},
   		{"Polynomial Expansion", 1.919},
   		{"Random Number Sort", 2.106},
   		{"Singular Value Decomposition", 3.385},
   		{"Solving a Linear System", 3.12}}
   	},{
   	"MachineName" -> "2.93 GHz Core i7-940 (8 Cores)",
   	"System" -> "Linux Ubuntu (64-bit) Desktop",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "8.0.0",
	"Date" -> "May 7, 2010",
	"BenchmarkResult" -> 0.885,
	"TotalTime" -> 15.647,
	"Results" -> {
		{"Data Fitting", 0.945},
  		{"Digits of Pi", 0.989},
  		{"Discrete Fourier Transform", 1.142},
  		{"Eigenvalues of a Matrix", 0.916},
  		{"Elementary Functions", 0.804},
  		{"Gamma Function", 0.807},
  		{"Large Integer Multiplication", 0.881},
  		{"Matrix Arithmetic", 1.512},
  		{"Matrix Multiplication", 0.888},
  		{"Matrix Transpose", 1.308},
  		{"Numerical Integration", 1.157},
  		{"Polynomial Expansion", 1.137},
  		{"Random Number Sort", 1.411},
  		{"Singular Value Decomposition", 0.895},
  		{"Solving a Linear System", 0.855}}
  	},{
  	"MachineName" -> "Intel Core i7-3770 CPU @ 3.40GHz (8 cores)",
  	"System" -> "Linux x86 (64-bit)",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "10.0.0",
	"Date" -> "January 17, 2014",
	"BenchmarkResult" -> 1.886,
	"TotalTime" -> 7.34,
	"Results" -> {
		{"Data Fitting", 0.379},
		{"Digits of Pi", 0.313},
		{"Discrete Fourier Transform", 0.48},
		{"Eigenvalues of a Matrix", 0.423},
		{"Elementary Functions", 0.616},
		{"Gamma Function", 0.414},
		{"Large Integer Multiplication", 0.403},
		{"Matrix Arithmetic", 0.684},
		{"Matrix Multiplication", 0.4},
		{"Matrix Transpose", 0.746},
		{"Numerical Integration", 0.64},
		{"Polynomial Expansion", 0.09},
		{"Random Number Sort", 0.854},
		{"Singular Value Decomposition", 0.452},
		{"Solving a Linear System", 0.446}}
    },{
   	"MachineName" -> "2.60 GHz Core 2 Duo Mobile T7800 (2 Cores)",
	"System" -> "Windows XP Pro (32-bit) Laptop",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "8.0.0",
	"Date" -> "May 10, 2010",
	"BenchmarkResult" -> 0.436,
	"TotalTime" -> 31.768,
	"Results" -> {
		{"Data Fitting", 1.266},
  		{"Digits of Pi", 1.797},
  		{"Discrete Fourier Transform", 2.594},
  		{"Eigenvalues of a Matrix", 1.438},
  		{"Elementary Functions", 3.688},
  		{"Gamma Function", 1.922},
  		{"Large Integer Multiplication", 2.063},
  		{"Matrix Arithmetic", 2.406},
  		{"Matrix Multiplication", 3.25},
  		{"Matrix Transpose", 1.906},
  		{"Numerical Integration", 1.453},
  		{"Polynomial Expansion", 1.406},
  		{"Random Number Sort", 1.344},
  		{"Singular Value Decomposition", 2.516},
  		{"Solving a Linear System", 2.719}}
   },{
   	"MachineName" -> "2.13 GHz Core 2 Duo E6400 (2 Cores)",
   	"System" -> "Windows Vista (32-bit) Server",
 	"BenchmarkName" -> "WolframMark",
 	"FullVersionNumber" -> "8.0.0",
 	"Date" -> "May 10, 2010",
 	"BenchmarkResult" -> 0.363,
 	"TotalTime" -> 38.084,
 	"Results" -> {
 		{"Data Fitting", 2.49},
 		{"Digits of Pi", 2.176},
   		{"Discrete Fourier Transform", 3.389},
   		{"Eigenvalues of a Matrix", 1.746},
   		{"Elementary Functions", 3.182},
   		{"Gamma Function", 2.354},
   		{"Large Integer Multiplication", 2.544},
   		{"Matrix Arithmetic", 2.916},
   		{"Matrix Multiplication", 3.275},
   		{"Matrix Transpose", 2.521},
   		{"Numerical Integration", 2.331},
   		{"Polynomial Expansion", 1.654},
   		{"Random Number Sort", 1.779},
   		{"Singular Value Decomposition", 3.124},
   		{"Solving a Linear System", 2.603}}
   	},{
   	"MachineName" -> "3.5 GHz 6-Core Intel Xeon E5",
   	"System" -> "Mac OS X x86 (64-bit)",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "10.0.0",
	"Date" -> "February 20, 2014",
	"BenchmarkResult" -> 1.883,
	"TotalTime" -> 7.351,
	"Results" -> {
		{"Data Fitting", 0.608},
		{"Digits of Pi", 0.36},
		{"Discrete Fourier Transform", 0.277},
		{"Eigenvalues of a Matrix", 0.447},
		{"Elementary Functions", 0.297},
		{"Gamma Function", 0.465},
		{"Large Integer Multiplication", 0.425},
		{"Matrix Arithmetic", 0.993},
		{"Matrix Multiplication", 0.289},
		{"Matrix Transpose", 0.673},
		{"Numerical Integration", 0.694},
		{"Polynomial Expansion", 0.092},
		{"Random Number Sort", 0.885},
		{"Singular Value Decomposition", 0.425},
		{"Solving a Linear System", 0.421}}
    },{
   	"MachineName" -> "2 \[Times] 2.26 GHz Quad Core Xeon E5520 (8 Cores)",
   	"System" -> "Mac XServe OS X (64-bit) Server",
 	"BenchmarkName" -> "WolframMark",
 	"FullVersionNumber" -> "8.0.0",
 	"Date" -> "May 10, 2010",
 	"BenchmarkResult" -> 0.69,
 	"TotalTime" -> 20.061,
 	"Results" -> {
 		{"Data Fitting", 1.064},
 		{"Digits of Pi", 1.22},
   		{"Discrete Fourier Transform", 1.44},
   		{"Eigenvalues of a Matrix", 1.004},
   		{"Elementary Functions", 1.671},
   		{"Gamma Function", 1.049},
   		{"Large Integer Multiplication", 1.12},
   		{"Matrix Arithmetic", 1.713},
   		{"Matrix Multiplication", 0.672},
   		{"Matrix Transpose", 2.786},
   		{"Numerical Integration", 1.317},
   		{"Polynomial Expansion", 1.219},
   		{"Random Number Sort", 1.761},
   		{"Singular Value Decomposition", 0.944},
   		{"Solving a Linear System", 1.081}}
   	},{
   	"MachineName" -> "2 \[Times] 2.00 GHz G5 PowerPC (2 Cores)",
   	"System" -> "Mac OS X (32-bit) Desktop",
 	"BenchmarkName" -> "WolframMark",
 	"FullVersionNumber" -> "8.0.0",
 	"Date" -> "May 10, 2010",
 	"BenchmarkResult" -> 0.137,
 	"TotalTime" -> 100.911,
 	"Results" -> {
 		{"Data Fitting", 4.61},
   		{"Digits of Pi", 4.643},
   		{"Discrete Fourier Transform", 10.914},
   		{"Eigenvalues of a Matrix", 4.935},
   		{"Elementary Functions", 19.504},
   		{"Gamma Function", 5.162},
   		{"Large Integer Multiplication", 5.008},
   		{"Matrix Arithmetic", 5.703},
   		{"Matrix Multiplication", 5.208},
   		{"Matrix Transpose", 7.172},
   		{"Numerical Integration", 3.331},
   		{"Polynomial Expansion", 4.866},
   		{"Random Number Sort", 4.275},
   		{"Singular Value Decomposition", 9.745},
   		{"Solving a Linear System", 5.835}}
   	},{
   	"MachineName" -> "2.2 GHz Intel Core i7",
   	"System" -> "Mac OS X x86 (64-bit)",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "10.0.0",
	"Date" -> "January 17, 2014",
	"BenchmarkResult" -> 1.204,
	"TotalTime" -> 11.494,
	"Results" -> {
		{"Data Fitting", 0.842},
		{"Digits of Pi", 0.472},
		{"Discrete Fourier Transform", 0.491},
		{"Eigenvalues of a Matrix", 0.682},
   		{"Elementary Functions", 0.829},
		{"Gamma Function", 0.584},
		{"Large Integer Multiplication", 0.534},
		{"Matrix Arithmetic", 1.638},
		{"Matrix Multiplication", 0.726},
		{"Matrix Transpose", 0.824},
		{"Numerical Integration", 0.92},
		{"Polynomial Expansion", 0.124},
   		{"Random Number Sort", 1.198},
		{"Singular Value Decomposition", 0.881},
		{"Solving a Linear System", 0.749}}
    },{
    "MachineName" -> "ARMv6-compatible processor rev 7 (v6l)",
    "System" -> "Linux ARM (32-bit)",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "10.0.0",
	"Date" -> "January 18, 2014",
	"BenchmarkResult" -> 0.005,
	"TotalTime" -> 3010.747,
	"Results" -> {
		{"Data Fitting", 31.843},
		{"Digits of Pi", 15.552},
		{"Discrete Fourier Transform", 84.43},
		{"Eigenvalues of a Matrix", 144.463},
		{"Elementary Functions", 183.054},
		{"Gamma Function", 16.485},
		{"Large Integer Multiplication", 20.019},
		{"Matrix Arithmetic", 28.879},
		{"Matrix Multiplication", 1164.784},
		{"Matrix Transpose", 37.595},
		{"Numerical Integration", 36.452},
		{"Polynomial Expansion", 4.699},
		{"Random Number Sort", 25.726},
		{"Singular Value Decomposition", 444.153},
		{"Solving a Linear System", 772.613}}
	},{
   	"MachineName" -> "Intel Core i7 CPU",
   	"System" -> "Microsoft Windows (64-bit)",
	"BenchmarkName" -> "WolframMark",
	"FullVersionNumber" -> "10.0.0",
	"Date" -> "January 27, 2014",
	"BenchmarkResult" -> 0.841,
	"TotalTime" -> 16.46,
	"Results" -> {
		{"Data Fitting", 0.926},
		{"Digits of Pi", 0.719},
		{"Discrete Fourier Transform", 0.684},
		{"Eigenvalues of a Matrix", 1.232},
		{"Elementary Functions", 1.435},
		{"Gamma Function", 0.888},
		{"Large Integer Multiplication", 0.853},
		{"Matrix Arithmetic", 1.019},
		{"Matrix Multiplication", 1.403},
		{"Matrix Transpose", 1.326},
		{"Numerical Integration", 1.583},
		{"Polynomial Expansion", 0.152},
		{"Random Number Sort", 1.441},
		{"Singular Value Decomposition", 1.467},
		{"Solving a Linear System", 1.332}}
   	}
};

$BenchmarkSystems = (("MachineName" /. #) <> " (" <> ("System" /. #)
	<> ")")& /@ $BenchmarkLibrary

End[]; (* Benchmarking`Private` *)


(******* Experimental ******************************************************)

Begin["`Experimental`"]; (* Benchmarking`Experimental` *)

ConvertBenchmarkToXML::usage = "ConvertBenchmarkToXML[benchmark data]";
ConvertBenchmarkToXML[benchmarkdata_List] := Module[{xmlout},
	xmlout = XMLObject["Document"][{},
	XMLElement["BenchmarkMachineReport", {}, {

	XMLElement["BenchmarkMachineID", {}, {StringJoin @@ Table[
		Benchmarking`Private`$CharacterLookupTable[[
		RandomInteger[{1, Length[
			Benchmarking`Private`$CharacterLookupTable]
		}]]], {8}]}],
	XMLElement["WolframLanguageSystem", {}, {"System" /. benchmarkdata}],
	XMLElement["WolframLanguageFullVersionNumber", {},
		{"FullVersionNumber" /. benchmarkdata}],
	XMLElement["MachineClass", {}, {""}],
	XMLElement["MachineIDString", {}, {""}],
	XMLElement["MachineInterconnect", {}, {""}],
	XMLElement["MachineComponentSystems", {}, {

	XMLElement["MachineComponentSystem", {}, {
			XMLElement["SystemCount", {}, {""}],
			XMLElement["SystemMakeModel", {}, {""}],
			XMLElement["SystemProductID", {}, {""}],
			XMLElement["SystemOS", {}, {""}],
			XMLElement["SystemCPUCount", {}, {""}],
			XMLElement["SystemCPUName", {}, {""}],
			XMLElement["SystemCPUInfo", {}, {""}],
			XMLElement["SystemCPUSpeed", {}, {""}],
			XMLElement["SystemCPUCoreCount", {}, {""}],
			XMLElement["SystemCPUFeatures", {}, {""}],
			XMLElement["SystemMemorySize", {}, {""}],
			XMLElement["SystemMemoryBus", {}, {""}]
		}]}],

	XMLElement["BenchmarkName", {}, {"BenchmarkName" /. benchmarkdata}],
	XMLElement["BenchmarkVersion", {},
		{"BenchmarkVersion" /. benchmarkdata}],
	XMLElement["BenchmarkOverallResult", {},
		{ToString["BenchmarkResult" /. benchmarkdata]}],
	XMLElement["BenchmarkTotalTime", {},
		{ToString["TotalTime" /. benchmarkdata]}],
	XMLElement["BenchmarkTestResults", {},
		Block[{data = ("TestResults" /. benchmarkdata)},
		If[0 < Length[data],
			XMLElement["BenchmarkTestResult", {}, {

			XMLElement["BenchmarkTestName", {}, {#[[1]]}],
			XMLElement["BenchmarkTestTime", {},
				{ToString[#[[2]]]}]}] & /@ data,
			{""}]]
		]
	}], {}];
	Return[xmlout]
] (* ConvertBenchmarkToXML *)
End[]; (* Benchmarking`Experimental` *)

EndPackage[]; (* Benchmarking` *)

