(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Print["In streaming loader"] *)

PacletManager`Package`loadWolframLanguageCode["Streaming", "Streaming`", DirectoryName[$InputFileName], "Interface.m",
           "AutoUpdate" -> True,
           "AutoloadSymbols" -> {"Streaming`CachedObject"}
]


Streaming`PackageScope`$TestMode = False;

If[Streaming`PackageScope`$TestMode,
	Get[FileNameJoin[{DirectoryName[$InputFileName],"TestDSL.m"}]];
	Block[{$ContextPath},
		Get[FileNameJoin[{DirectoryName[$InputFileName],"Tests.m"}]]
	]
]
		


(* TODO: either learn how to control load ordering for files, or make all participating functions at least package-scope *)

Begin["Streaming`Temp`"]

ClearAll[registerWriter, registerReader];
registerWriter[name_ -> format_]:=
	Streaming`PackageScope`LazyListRegisterExportWriter[
		name, 
		Streaming`Drivers`PackagePrivate`makeTableWriter[format,##]&
	];
	
registerReader[name_ -> format_]:=
	Streaming`PackageScope`LazyListRegisterImportIterator[
		name, 
		Streaming`Drivers`PackagePrivate`makeTableIterator[format,##]&, "SkipInnerStructDetection" -> True
	];
	


registerWriter["CSV" -> "CSV"];
registerWriter["TSV" -> "TSV"];
registerWriter["Table" ->  "Table"];

registerReader["CSV" -> "CSV"];
registerReader["TSV" -> "TSV"];
registerReader["Table" ->  "Table"];

End[]

Remove["Streaming`Temp`*"];

(* This is also due to the lack of control over the loading order of files. We have to make packing 
rules be the last for UpValues of LazyList *)
UpValues[LazyList] = 
	With[{
		defs = 
			Cases[
				UpValues[LazyList], 
				def_/;!FreeQ[def, HoldPattern[Streaming`PackageScope`$LazyListPackingMode]]
			]
		},
		Join[
			DeleteCases[
				UpValues[LazyList], 
				def_/;!FreeQ[def, HoldPattern[Streaming`PackageScope`$LazyListPackingMode]]
			],
			defs
		]
	];
			

Streaming`PackageScope`StreamingCleanup[];