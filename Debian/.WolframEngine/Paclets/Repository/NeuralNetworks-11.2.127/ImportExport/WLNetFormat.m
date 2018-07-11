Package["NeuralNetworks`"]



PackageScope["$NeuralNetworksVersionNumber"]

(* get the version straight from the horses mouth (PacletInfo.m).
This is a string e.g. "11.1.1" *)
$NeuralNetworksVersionNumber := "11.2.0"


PackageScope["ToVersionString"]

(* only 11.0 and early 11.1 era nets used reals, now we use PacletInformation. *)
ToVersionString[r_Real | r_Integer] := Match[
	Round[r, 0.001], 
	11. :> "11.0.0", 
	11.1 :> "11.1.0",
	11.05 :> ThrowFailure["wlprerl"]
];
ToVersionString[s_String] := s;
ToVersionString[_] := corrupt[];


PackageExport["WLNetExport"]

Clear[WLNetExport];

General::invnet2 = "The second argument in `` is not a valid net."

WLNetExport[filename_String, head_Symbol[net_Association, meta_Association] ? ValidNetQ] := CatchFailureAsMessage[Export, Scope[
	CollectTo[{$tensors}, result = PrepareForExport[net]];
	Block[{$ContextPath = {"System`", "NeuralNetworks`"}, $Context = "Dummy`"}, 
		netstring = IOContextBlock @ Compress[result];
		metastring = IOContextBlock @ Compress[meta];
	];	
	ExportStructuredHDF5[filename, Association[
		"Version" -> $NeuralNetworksVersionNumber, 
		"Network" -> netstring, "Arrays" -> $tensors,
		"Metadata" -> metastring
	]];
	filename
]];

WLNetExport[a_, b_] := (
	Message[Export::invnet2, HoldForm[Export][a, Shallow[b]]];
	$Failed
);

WLNetExport[filename_String, net_, opts__] := 
	IOOptMessage[Export, filename, net, opts];

General::nonetioopts = "Invalid arguments provided in ``. \"WLNet\" format does not currently support any options or arguments."
IOOptMessage[f_Symbol, args___] := (
	Message[Export::nonetioopts, HoldForm[f] @@ Map[MsgForm, {args}]];  
	$Failed
);


PackageExport["ExportedArray"]


PackageScope["PrepareForExport"]

DeclareMethod[PrepareForExport, 
	PrepareLayerForExport, 
	PrepareContainerForExport,
	PrepareOperatorForExport
];

PrepareLayerForExport[assoc_] := 
	MapAtFields["Arrays", ExportTensor, assoc];

PrepareContainerForExport[assoc_] = 
	MapAtFields["Nodes", PrepareForExport, assoc];

PrepareOperatorForExport[assoc_] = (
	Scan[PrepareLayerForExport, GetSubNets[assoc]];
	PrepareLayerForExport[assoc]
);

ExportTensor[raw_RawArray] := (
	BagInsert[$tensors, raw]; 
	ExportedArray[BagLength[$tensors]]
);

ExportTensor[e_] := e;


PackageExport["WLNetImport"]

Import::wlprerl = "Cannot import networks saved during the 11.1 prerelease period."

corrupt[] := ThrowFailure["wlnetcorr", file];
General::wlnetcorr = "File is corrupt or is not a WLNet file.";
General::wlbadprop = "`` is not a valid property for a WLNet file."

WLNetImport[file_String, retType_:"Net"] := CatchFailureAsMessage[Import, Scope[

	Switch[retType, 
		"Net", arrayForm = "RawArrays",
		"UninitializedNet", arrayForm = "Placeholders",
		"ArrayList", arrayList = True; arrayForm = "Lists",
		"RawArrayList", arrayList = True; arrayForm = "RawArrays",
		"ArrayAssociation", arrayAssoc = True; arrayForm = "Lists",
		"RawArrayAssociation", arrayAssoc = True; arrayForm = "RawArrays",
		"WLVersion", arrayForm = "Placeholders",
		_, ThrowFailure["wlbadprop", retType]
	];

	result = Quiet @ CatchFailure @ ImportStructuredHDF5[file, "ArrayForm" -> arrayForm];

	If[FailureQ[result], corrupt[]];

	If[!MatchQ[result, KeyValuePattern[{"Version" -> _, "Network" -> _, "Arrays" -> _}]], corrupt[]];

	UnpackAssociation[result, network, arrays, version];

	If[retType === "WLVersion", Return[version]];
	If[arrayList, Return[arrays]];

	If[KeyExistsQ[result, "Metadata"],
		metadata = ReleaseHold @ toExpression @ result["Metadata"];
		If[!AssociationQ[metadata], corrupt[]];
		readerFunction = ReleaseHold @ toExpression @ result["ReaderFunction"];
		If[FailureQ[readerFunction], readerFunction = toExpression];
		KeyDropFrom[metadata, "ReaderFunction"];
	,
		metadata = <|"Version" -> version|>;
		readerFunction = toExpression;
	];
	constructor = GetConstructorFunction[metadata, "import"];
	network = readerFunction[network];
	If[FailureQ[network], ReturnFailed[]];

	If[arrayAssoc,
		arrpos = Position[ReleaseHold[network], _ExportedArray];
		specs = FromNetPath[arrpos /. Key[k_] :> k];
		Return[AssociationThread[specs, arrays]]
	];

	If[arrayForm === "Placeholders", arrays = arrays /. H5DatasetPlaceholder -> TensorT];
	network = network /. ExportedArray[id_] :> RuleCondition @ arrays[[id]];
	network = ReleaseHold[network];

	constructor[network, metadata]
]];

WLNetImport[filename_String, opts__] := 
	IOOptMessage[Import, filename, opts];

SetHoldAllComplete[IOContextBlock];
IOContextBlock[expr_] := Block[
	{$ContextPath = {"System`", "NeuralNetworks`"}, $Context = "NeuralNetworks`ImportContext`"}, 
	expr
];

toExpression[str_String] := IOContextBlock @ ToExpression[str, InputForm, Hold]
toExpression[str_String] /; StringStartsQ[str, "1:"] := IOContextBlock @ Uncompress[str, Hold];
toExpression[ba_ByteArray] /; NameQ["System`BinaryDeserialize"] := Symbol["System`BinaryDeserialize"][ba, Hold];
toExpression[_] := $Failed;


PackageExport["RenameLayers"]

NNSetUsage @ "
RenameLayers[net$, renamer$] renames layers in net by applying renamer$ to each name to produce a \
new name. renamer$ can be a function, an association, or a (list of) string replacement.
"

RenameLayers[NetP[net, meta], renamingFunction_] := Scope @ CatchFailureAsMessage[Export, 
	If[MatchQ[renamingFunction, _Rule | _RuleDelayed | {Repeated[_Rule | RuleDelayed]}],
		renamingFunction = StringReplace[renamingFunction]
	];
	$renamer = checkRename[renamingFunction];
	net2 = tryRename[net];
	ConstructLayer[net2, meta]
];

General::badrename = "Given renaming function applied to name `` returned the non-string ``."
General::duprename = "Given renaming function applied to name `` returned the already-produced name ``."

checkRename[renamer_][name_] := Scope[
	CacheTo[$renameCache, name, 
		newname = Replace[renamer[name], _Missing | Null | None -> name];
		If[!StringQ[newname], ThrowFailure["badrename", name, newname]];
		If[MemberQ[$renameCache, newname], ThrowFailure["duprename", name, newname]];
		newname
	]
];

tryRename[assoc_] := ReplaceAll[assoc, 
	container:<|"Type" -> ("Chain"|"Graph"), ___|> :> RuleCondition @ renameContainerLayers[container]
];

renameContainerLayers[assoc_] := Scope[
	$renameCache = Association[];
	MapAt[
		ReplaceAll[NetPath["Nodes", name_String, rest___] :> RuleCondition @ NetPath["Nodes", $renamer[name], rest]],
		MapAt[KeyMap[$renamer] /* tryRename, assoc, "Nodes"],
		"Edges"
	]
];
