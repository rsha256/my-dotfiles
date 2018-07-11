Package["NeuralNetworks`"]



PackageScope["$HDF5PackagePath"]

(* set this if you have a local checkout of HDF5Tools *)
$HDF5PackagePath = "HDF5Tools`";

If[!ValueQ[$HDF5LoadedQ], $HDF5LoadedQ = False]

LoadHDF5Tools[] := If[!TrueQ[$HDF5LoadedQ],
	Block[{$ContextPath = {"System`"}}, Get[$HDF5PackagePath]];
	HDF5Tools`HDF5ToolsInit[];
	$HDF5LoadedQ = True;
];


PackageScope["HDF5TrainingData"]


PackageScope["OpenHDF5TrainingData"]

NetTrain::invh5data = "`` is not an h5 that contains data suitable for training: there should be one dataset per port, where each dataset is composed of a single float32 or float64 array, and all arrays have the same length (first dimension)."

OpenHDF5TrainingData[path_String] := Scope[
	LoadHDF5Tools[];
	$path = ExpandFileName[path];
	file = HDF5Tools`HDF5OpenFile[path];
	groupContents = HDF5Tools`HDF5GetGroupContents[file, "/", False];
	columns = KeyValueMap[procColumn, groupContents];
	HDF5Tools`HDF5CloseGroup[group];
	lengths = columns[[All, 2, 1]];
	If[lengths === {} || Not[SameQ @@ lengths], ThrowFailure["invh5data", $path]];
	$fileID = First[file]; 
	colInfo = openColumn @@@ columns;
	HDF5TrainingData[$fileID, First[lengths], columns[[All, 1]], colInfo]
];

procColumn[name_, <|"DataFormat" -> "Real32"|"Real64", "Dimensions" -> dims_, "DataEncoding" -> _|>] :=
	{name, dims};

procColumn[_, _] := ThrowFailure["invh5data", $path];

openColumn[name_, dims_] := Scope[
	ds = ch @ HDF5Tools`h5dopen[$fileID, "/" <> name, HDF5Tools`H5PDEFAULT];
	fs = ch @ HDF5Tools`h5dgetspace[ds];
	ms = Unique["NeuralNetworks`Private`memoryspace$"];
	{ds, fs, Compose[Hold, ms], Rest @ dims}
];


PackageScope["CloseHDF5TrainingData"]

CloseHDF5TrainingData[HDF5TrainingData[file_, length_, cnames_, cinfo_]] := (
	Scan[closeColumn, cinfo];
	ch @ HDF5Tools`h5fclose[file];
)

closeColumn[{ds_, fs_, Hold[ms_], dims_}] := (
	If[ValueQ[ms], ch @ HDF5Tools`h5sclose[ms]];
	ch @ HDF5Tools`h5sclose[fs];
	ch @ HDF5Tools`h5dclose[ds];
)

SetHoldFirst[ch];
ch[x_] := Scope[
	If[!IntegerQ[r = x], Panic["HDF5Failure", "`` failed.", HoldForm[x]], r]
];


