BeginPackage["CloudObject`"]

System`ExportForm;
System`UpdateInterval;
System`CloudExport;
System`CloudImport;

Begin["`Private`"]

Unprotect[CloudObject];

(* Formats *)

mimeToFormat := mimeToFormat = Quiet[
    DeleteCases[
        Flatten @ Map[
            Function[{format}, Function[{mime}, mime -> format] /@ ImportExport`GetMIMEType[format]], 
            $ExportFormats
        ],
        $Failed
    ], 
    FileFormat::fmterr
];

mimetypeToFormat[type_, filename_: None] := ToLowerCase @ Replace[
    ToUpperCase[type],
    Join[mimeToFormat, {_ -> If[filename =!= None, FileFormat[filename], "Text"]}]
]

fileFormatLookup := fileFormatLookup = Dispatch[MapAt[ToLowerCase, mimeToFormat, {All,1}]]

formatToMimeType := formatToMimeType = Composition[
    Replace @ Dispatch[
        Flatten[{
            None|_Missing|Automatic -> "text/plain",
            "wl" -> "text/plain",
            "htmlfragment" -> "text/html",
            "expressionjson"|"rawjson" -> "application/json",
            "base64" -> "application/base64",
            "jpg" -> "image/jpeg",  (* ImportExport`GetMIMEType lacks definition for "JPG", so add it here *)
            "svg" -> "image/svg+xml",
            "pdf" -> "application/pdf", (* Application/pdf is ordered correctly, hardcoding the decision here *)
            "gzip" -> "application/x-gzip",
            "bzip2" -> "application/x-bzip2",
            "csv" -> "text/csv", (* according to http://tools.ietf.org/html/rfc4180 *)
            "css" -> "text/css",
            Quiet[
                Map[
                    Replace[
                        ImportExport`GetMIMEType[#], {
                            $Failed|{} :> {},
                            (* Give non-"application/..." types precedence (e.g. image/png should be used instead of application/png). *)
                            types_ :> Rule[
                                ToLowerCase[#],
                                First @ SortBy[
                                    ToLowerCase[types], 
                                    StringMatchQ[#, "application/" ~~ __] &
                                ]
                            ]
                        }
                    ] &,
                    $ExportFormats            
                ], 
                FileFormat::fmterr
            ],
            mime_String?(StringMatchQ[#, __ ~~ "/" ~~ __]&) :> mime,
            _ -> "application/octet-stream"
        }]
    ],
    Replace[s_String :> ToLowerCase[s]]
]

formatToMimeMeta[fmt_, enc_:None] := 
    With[
        {mime = formatToMimeType[fmt], format = Replace[fmt, s_String :> ToLowerCase[s]]}, <|
            "ContentType" -> 
                If[
                    And[    
                        MatchQ[enc, _String],
                        StringMatchQ[mime, StringExpression[StartOfString, "text/", Repeated[Except[";"]]]]
                    ], 
                    StringJoin[mime, ";charset=", enc], 
                    mime
                ],
            "Headers" -> <|
                Replace[
                    mime, {
                        Alternatives[
                            "text/plain",
                            "text/html",
                            "text/xml",
                            "application/json",
                            "image/jpeg",
                            "image/png",
                            "application/vnd.wolfram.cloudcdf.html"
                        ] :> "Content-Disposition" -> "inline",
                        _ :> {}
                    }
                ],
                Replace[
                    format, {
                        "base64" :> "Content-Transfer-Encoding" -> "base64",
                        "gzip" :> "Content-Encoding" -> "gzip",
                        _ :> {}
                    }
                ]            
            |>
        |>
    ]

(* ExportForm *)

Unprotect[ExportForm];

(* Defining a global variable that can be overridden by block, FormFunction is doing that *)
$exportFormDefault = "HTMLCloudCDF";
$exportFormSoundDefault = "MP3";

$exportFormReplacements = Dispatch[{
    HoldPattern[_NotebookObject|_Notebook|_DocumentNotebook|_PaletteNotebook|_DialogNotebook] :> "CloudCDF",
    HoldPattern[_Sound] :> $exportFormSoundDefault,
    HoldPattern[_XMLElement|_XMLObject] :> {"XML", "text/html"},
    HoldPattern[_FormFunction|_APIFunction] :> "HTML",
    HoldPattern[_Manipulate|_Dynamic|_Graphics|_Graphics3D] :> "NBElement",
    (* By default, export as a notebook. Export will take care of wrapping in a Notebook expression. *)
    _ :> $exportFormDefault
}]

(* one-argument short forms, also used by CloudDeploy so it automatically chooses a proper format *)
ExportForm[expr_, Automatic, opts:OptionsPattern[]] := ExportForm[expr, Replace[expr, $exportFormReplacements], opts]
ExportForm[expr_, opts:OptionsPattern[]] := ExportForm[expr, Replace[expr, $exportFormReplacements], opts]

ExportForm /: ExportString[ExportForm[body_, fmt_, rest___], fmt1_:Inherited, rest1___] :=
    Replace[
        exportFormToString[ExportForm[body, Replace[fmt1, Inherited :> fmt], rest1, rest]],
        res:Except[$Failed] :> First[res]
    ]

ExportForm /: Export[path_, e_ExportForm, fmt_:Inherited, rest___] :=
    Export[path, ExportString[e, fmt, rest], "String"]

SetAttributes[ExportForm, ReadProtected];

Protect[ExportForm];

(*Import*)

Unprotect[CloudImport];

Options[cloudImportWrapper] = Options[Import]
Options[CloudImport] = Join[Options[Import], {CloudBase->Automatic}]

cloudImportWrapper[obj_CloudObject, importFn_Function, head_Symbol:CloudObject] := Module[
    {tempfilename, mimetype},
    {tempfilename, mimetype} = readObject[obj, head];
    If[tempfilename === $Failed, Return[$Failed]];
    cleanup[
        tempfilename,
        importFn[tempfilename, mimetype]
    ]
]

cloudImportWrapper[obj_CloudObject, Automatic, head_Symbol:CloudObject, o:OptionsPattern[]] := cloudImportWrapper[obj,
    Function[{fn, mimetypeIn}, Import[fn, mimetypeToFormat[mimetypeIn, fn], o]],
    head
]

cloudImportWrapper[obj_CloudObject, format_, head_Symbol:CloudObject, o:OptionsPattern[]] := cloudImportWrapper[obj,
    Function[{fn, mimetypeIn}, Import[fn, format, o]],
    head
]

CloudSemanticImport[obj_CloudObject, args___] := cloudImportWrapper[obj,
    Function[{fn, mimetype},
        SemanticImport[fn, args]
    ], CloudSemanticImport
]

CloudImport[obj_CloudObject,  format:(Automatic | _String) : Automatic, o:OptionsPattern[]] := 
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		cloudImportWrapper[obj, format, CloudImport, Sequence @@ FilterRules[{o}, Except[CloudBase]]]
    ]

CloudImport[uri_String,  format:(Automatic | _String) : Automatic, o:OptionsPattern[]] := 
	Block[{$CloudBase = handleCBase[OptionValue[CloudBase]]},
		CloudImport[CloudObject[uri], format, Sequence @@ FilterRules[{o}, Except[CloudBase]]]
    ]

CloudImport[URL[uri_String], format_ : Automatic, o:OptionsPattern[]] := CloudImport[uri, format, o]

CloudObject /: Import[obj_CloudObject, format_ : Automatic, o:OptionsPattern[]] := cloudImportWrapper[obj, format, Import, o]

CloudObject /: HoldPattern[SemanticImport][obj_CloudObject, args___] := cloudImportWrapper[obj,
    Function[{fn, mimetype},
        SemanticImport[fn, args]
    ], SemanticImport
]

SetAttributes[CloudImport, ReadProtected];
Protect[CloudImport];

(*Export*)

Unprotect[CloudExport];

Options[CloudExport] = objectFunctionOptionsJoin[$objectCreationOptions, {CloudBase -> Automatic}];

exportFormToString::usage = "exportFormToString[expr] exports an expression, automatically determine a format appropriate for the cloud, giving the export string and MIME type. It is similar to ExportString but supports some additional formats.
exportFormToString[expr, format] exports to a specific format.
exportFormToString[expr, format, options] exports an expression with Export options.";

SetAttributes[exportFormToString, HoldFirst];

$exportFormHandler = ExportString;

exportFormToByteArray[args___] := 
    Block[
        {$exportFormHandler = ExportByteArray},
        Replace[
            exportFormToString[args], 
            s_String :> StringToByteArray[s, "ISO8859-1"] (* string is already in 0-255 range *)
        ]
    ]

(* one argument version is doing the automatic behaviour *)
exportFormToString[ExportForm[args___], rest___] := exportFormToString[args, rest];
exportFormToString[expr_] := exportFormToString[Evaluate[ExportForm[expr, Automatic]]]
exportFormToString[expr_, Automatic, rest___] := exportFormToString[Evaluate[ExportForm[expr, Automatic]], rest]

(*should Inherited make its way down here we want to avoid splatting*)
exportFormToString[expr_, Inherited, rest___] := exportFormToString[expr, "WL", rest]

(* normalizing the export form to {format, mimetype} *)
exportFormToString[body_, format:Except[_List], rest___] := exportFormToString[body, {format, Automatic}, rest]

(* options in the list are becoming options in the export *)
exportFormToString[body_, {format_, mime:Except[_Rule|_RuleDelayed]:Automatic, opt__}, rest___] := exportFormToString[body, {format, mime}, opt, rest]
(* a list with one argument is becoming automatic *)
exportFormToString[body_, {format_:None}, rest___] := exportFormToString[body, {format, Automatic}, rest]  

(* Always export NotebookObjects and CellObjects as (interactive) CloudCDFs.
   The HTMLFragment result is just a static image of the whole notebook which doesn't look right. *)
exportFormToString[expr:_NotebookObject|_CellObject|_Manipulate|_Dynamic, {"HTMLCloudCDF", mime___}, rest___] :=
    exportFormToString[expr, {"CloudCDF", mime}, rest]

exportFormToString[expr_, {"HTMLCloudCDF", mime_}, rest___] :=
    With[
        {boxes = ToBoxes[expr, StandardForm]},
        If[
            interactiveBoxesQ[boxes],
            exportFormToString[
                expr, {
                    "CloudCDF", 
                    mime
                }, 
                rest
            ],
            exportFormToString[
                expr, {
                    "HTMLFragment", 
                    Replace[mime, Automatic -> expressionMimeType["HTMLCloudCDF"]]
                }, 
                rest
            ]
        ]
    ]

SetAttributes[interactiveBoxesQ, HoldAllComplete];
interactiveBoxesQ[_DynamicBox|_Graphics3DBox|_Dynamic|_DynamicModuleBox] = True;
interactiveBoxesQ[head_[args___]] := interactiveBoxesQ[head] || ReleaseHold[Map[interactiveBoxesQ, Hold[Or[args]], {2}]]
interactiveBoxesQ[other_] = False;

exportFormToString[expr_String, {"HTML", mime_}, rest___] :=
    exportFormToString[expr, {"HTMLFragment", mime}, rest]

exportFormToString[expr_, {"HTML", mime_}, rest___] :=
    exportFormToString[expr, {"HTMLFragment", mime}, rest, "FullDocument" -> True]

exportFormToString[expr_, {"NBElement"|"CloudCDFElement", Automatic}, rest___] := 
    exportFormToString[expr, {"NBElement", expressionMimeType["NBElement"]}, rest]

exportFormToString[expr_, {"CloudCDF", Automatic}, rest___] := 
    exportFormToString[expr, {"CloudCDF", expressionMimeType["CloudCDF"]}, rest]

exportFormToString[expr_, {"CloudCDF", mime_}, rest___] := {
    $exportFormHandler[wrapExportExpr[expr, "CloudCDF"], "NB", rest], 
    formatToMimeMeta[mime]
}

(* This is an internal thing it should be removed -riccardod *)

exportFormToString[expr_, {f:"API"|"Computation"|"Form"|"Task"|"Grammar"|"Expression", Automatic}, rest___] := 
    exportFormToString[expr, {"WL", expressionMimeType[f]}, rest]

exportFormToString[expr_, {"API"|"Computation"|"Form"|"Task"|"Grammar"|"Expression", mime_}, rest___] := 
    exportFormToString[expr, {"WL", mime}, rest]

exportFormToString[expr_, {None, Automatic|None}, rest___] :=
    exportFormToString[expr, {None, "text/html"}, rest]

exportFormToString[expr_, {"WL", Automatic}, rest___] := 
    exportFormToString[expr, {"WL", "text/plain;charset=utf-8"}, "Comments" -> None, rest]

exportFormToString[expr_, {"WL", mime_}, rest___] := {
    (* 
        this is a speed optimization for APIFunction, ExportByteArray is adding 3ms of overhead
        https://jira.wolfram.com/jira/browse/CLOUD-11913
    *)
    Replace[
        $exportFormHandler, {
            ExportString    :> ToString[expr, InputForm, CharacterEncoding -> "UTF8"],
            ExportByteArray :> StringToByteArray[ToString[expr, InputForm], "UTF8"],
            (* 
            this should never happen since we control $exportFormHandler.
            I added this so that we immediately recognize the issue 
            in case we start Block'ing $exportFormHandler with something else
            *)
            _ :> Throw[$exportFormHandler]
        }   
    ],
    formatToMimeMeta[mime, "utf-8"]
}

exportFormToString[expr_, {"CloudConnectorExcelForm", Automatic}, rest___] := {
    $exportFormHandler[CloudObject`cloudConnectorExcelForm[expr, rest], "JSON", "Compact" -> True],
    formatToMimeMeta["JSON"]
}

exportFormToString[expr_, {"CloudConnectorExcelForm", mime_}, rest___] := {
    $exportFormHandler[CloudObject`cloudConnectorExcelForm[expr, rest], "JSON", "Compact" -> True],
    mime
}

exportFormToString[expr_, {None|"String", mime_}, rest___] := {
    $exportFormHandler[expr, "String", CharacterEncoding -> "UTF8"],
    formatToMimeMeta[mime, "utf-8"]
}

exportFormToString[expr_, {format_?StringQ, Automatic}, rest___] := 
    exportFormToString[expr, {format, format}, rest]

exportFormToString[expr_, {format_?StringQ, None}, rest___] := 
    exportFormToString[expr, {format, "text/plain"}, rest]

exportFormToString[expr_, {"NB"|"NBElement", mime_}, rest___] := 
    {$exportFormHandler[expr, "NB", rest], formatToMimeMeta[mime]}

exportFormToString[expr_, {"Text", mime_}, rest___] :=
    {$exportFormHandler[expr, "Text", rest], formatToMimeMeta[mime, "utf-8"]}

exportFormToString[expr_, {format_?StringQ, mime_}, rest___] := {
    $exportFormHandler[wrapExportExpr[expr, format], format, rest], 
    formatToMimeMeta[mime]
}

exportFormToString[expr_, {func_, None|Automatic}, rest___] := 
    exportFormToString[expr, {func, "text/plain"}]

exportFormToString[expr_, {func_, mime_}, rest___] := {
    func[expr], 
    formatToMimeMeta[mime]
}

SetAttributes[wrapExportExpr, HoldFirst];
wrapExportExpr[expr:_Manipulate|_Dynamic, "NBElement"] :=
    toNotebookElement[Append[expr, IncludeDefinitions -> True]]
wrapExportExpr[expr_, "NBElement"] :=
    toNotebookElement[expr]
wrapExportExpr[expr_, "CloudCDF"] :=
    toNotebook[expr]
wrapExportExpr[expr:Alternatives[Sound[_SoundNote], Sound[{__SoundNote}]], "MP3"|"FLAC"|"WAV"|"OGG"]:=
    Sound`ToSampledSound[expr]
wrapExportExpr[expr_, form_] := 
    Unevaluated[expr]

notebookExprPattern = HoldPattern[_Notebook|_NotebookObject|_DocumentNotebook|_PaletteNotebook|_DialogNotebook];

toNotebook[expr : notebookExprPattern] := expr
toNotebook[expr_, cellopts___] := Notebook[{
    If[Head[expr] === Cell,
        Cell[First[expr], expr[[2]], cellopts],
    (* not a cell *)
        Cell[BoxData[ToBoxes[expr]], "Output", cellopts]
    ]
}]
toNotebookElement[expr_] := toNotebook[expr, TextAlignment->Center, ShowCellBracket->False]

CloudExport[expr_, format_, CloudObject[uri_, objopts:OptionsPattern[CloudObject]], opts:OptionsPattern[]] :=
    Quiet[
        Replace[
            exportFormToString[expr, format, FilterRules[{opts}, Except[Keys[Options[CloudExport]]]]], {
            {$Failed, ___} :> $Failed,
            {content_, mimetype_, ___} :> 
                Block[{$CloudBase = handleCBase @ OptionValue[CloudExport, {opts}, CloudBase]},
                    Replace[
                        writeObject[
                            CloudObject[uri, FilterRules[{objopts}, Except[CloudObjectNameFormat]]], 
                            content, 
                            mimetype["ContentType"], 
                            OptionValue[CloudExport, {opts, objopts}, Permissions],
                            OptionValue[CloudExport, {opts, objopts}, IconRules],
                            Unevaluated[expr],
                            OptionValue[CloudExport, {opts, objopts}, MetaInformation],
                            {},
                            CloudExport
                        ],
                        CloudObject[args___] :> 
                            CloudObject[
                                args,
                                CloudObjectNameFormat -> 
                                    OptionValue[CloudExport, {opts, objopts}, CloudObjectNameFormat]
                            ]
                    ]
                ]}
        ],
        OptionValue::nodef
    ]


CloudExport[expr_, format_, uri_String, opts:OptionsPattern[]] :=
    Block[
        {$CloudBase = handleCBase[Quiet[OptionValue[CloudExport, {opts}, CloudBase], OptionValue::nodef]]},
        CloudExport[Unevaluated[expr], format, Unevaluated[CloudObject[uri]], opts]
    ]
    
CloudExport[expr_, format_, URL[uri_String], opts___] :=
    CloudExport[Unevaluated[expr], format, uri, opts]    

CloudExport[expr_, format_, opts:OptionsPattern[]] :=
    Block[
        {$CloudBase = handleCBase[Quiet[OptionValue[CloudExport, {opts}, CloudBase], OptionValue::nodef]]},
        CloudExport[Unevaluated[expr], format, CloudObject[], opts]
    ]

CloudExport[args___] := (ArgumentCountQ[CloudExport, Length[DeleteCases[{args},_Rule,Infinity]],2,3];Null/;False)

CloudObject /: Export[obj_CloudObject, expr_, format_, rest___] :=
    CloudExport[Unevaluated[expr], format, obj, rest]
    
CloudObject /: Export[obj_CloudObject, expr_] := (Message[Export::argtu];HoldForm[Export[obj, expr]])

SetAttributes[CloudExport, ReadProtected];
Protect[CloudExport];

Protect[CloudObject];

End[]

EndPackage[]
