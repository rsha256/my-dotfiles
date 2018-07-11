Output: ComputedType[
	TensorT[{SizeT}, RealTensorT], 
	TensorT[{$ColorChannels}, TensorT[Reverse[$ImageSize]]]
]

Parameters:
	$ImageSize: NormalizedT[SizeListT[2], rep2, {128, 128}]
	$ColorSpace: ComputedType[ColorSpaceT, ToColorSpace[$ColorSpace, $ColorChannels], {$ColorChannels}]
	$ColorChannels: ComputedType[SizeT, ToChannelCount[$ColorSpace]]
	$MeanImage: Defaulting[Nullable[EitherT[{ScalarT, ListT[$ColorChannels, ScalarT], ImageT[]}]]]
	$VarianceImage: Defaulting[Nullable[EitherT[{ScalarT, ListT[$ColorChannels, ScalarT], ImageT[]}]]]

PostInferenceFunction: Function[
	If[Not @ IntegerQ[$ColorChannels],
		PostSet[$ColorSpace, "RGB"];
		PostSet[$ColorChannels, 3];
		imgSize = $ImageSize;
		validateVarianceImage[$VarianceImage, imgSize, "RGB", 3];
		PostSet[NetPath["Output"], TensorT[{3}, TensorT[Reverse[imgSize]]]];
		,
		validateVarianceImage[$VarianceImage, $ImageSize, $ColorSpace, $ColorChannels];
	];
]


rep2[i_Integer] := {i, i};
rep2[i:{_Integer,_Integer}] := i;
rep2[_] := $Failed;

ToEncoderFunction: Function[
	toImageEncoderFunction[
		Join[<|"VarianceImage" -> None|>, #1], 
		#2
	]
]

toImageEncoderFunction = Function @ Block[{general, fast},
	general = makeWLEncoderFunction[#1];
	If[#2,
		fast = makeFileEncoderFunction[#1, general];
		makeEncoderFunction[general, fast],
		general
	]
];

RandomInstance: Function[
	RandomImage[{0, 1}, ImageSize -> #ImageSize, ColorSpace -> #ColorSpace]
]

MLType: Function["Image"]

EncoderToDecoder: Function[
	NetDecoder[{"Image",
		"ColorSpace" -> #ColorSpace, "MeanImage" -> #MeanImage, 
		"VarianceImage" -> Lookup[#, "VarianceImage", None],
		"$Channels" -> #ColorChannels, 
		"$Dimensions" -> Reverse[#ImageSize]
	}]
]

(******
makeEncoderFunction: an encoder function choosing between file and general paths
******)

makeEncoderFunction[slowEnc_, fastEnc_] := With[
	{fastEnc2 = fastEnc, slowEnc2 = slowEnc},
	If[fastPathQ[#],
		fastEnc2[#],
		Map[slowEnc2, #]
	]&
];

fastPathQ[{__File}] := True;
fastPathQ[_] := False;

(******
makeWLEncoderFunction: an image loader purely written in WL. Works on the 
	widest variety of image formats + incore WL images
******)

makeWLEncoderFunction = Function[
	finalizer = MakeFinalizer[#MeanImage, #VarianceImage, #ImageSize, #ColorSpace, #ColorChannels];
	LoadImage[#ImageSize] /* ConformImageColors[#ColorSpace] /* 
		ConformImageSize[#ImageSize] /* ConformImageAlpha[#ColorChannels] /*
		ToReal32Image /* finalizer /* If[#ColorChannels === 1, PadRank, Identity]
];

ToChannelCount["Grayscale"] = 1;
ToChannelCount["CMYK"] = 4;
ToChannelCount[Automatic] := SizeT;
ToChannelCount[_] := 3;

Clear[ToColorSpace];
ToColorSpace[space_String, _] := space;
ToColorSpace[_, 1] = "Grayscale";
ToColorSpace[_, 3] = "RGB";
ToColorSpace[_, _Integer] = Automatic;
ToColorSpace[_, SizeT] = "RGB";


(*-----------------------------------------------------------------------------*)

(* Note: it appears that using packed array division/subtraction is faster than 
Image`ArithmeticOperationsDump`imageSubtract. So will avoid using this, even 
though it should be faster in principle, due to lower precision arithmetic

im1 = RandomImage["ImageSize" -> {1000, 1000}, ColorSpace -> "RGB"];
im2 = RandomImage["ImageSize" -> {1000, 1000}, ColorSpace -> "RGB"];
Image`ArithmeticOperationsDump`imageSubtract[im1, im2]; // RepeatedTiming
Out: {0.00012, Null}
{data1, data2} = ImageData[#, Interleaving -> False] & /@ {im1, im2};
(data1 - data2); // RepeatedTiming
Out: {0.000070, Null}
*)

MakeFinalizer[mean_, var_, size_, space_, channels_] := With[
	{
		mean2 = getConformedImageData[mean, size, space, channels], 
		var2 = Sqrt @ getConformedImageData[var, size, space, channels]
	},
	(* return this func *)
	Function[input,
		Block[{tempIm},
			tempIm = ImageData[input, Interleaving -> False];
			(* Step 1: remove mean *)
			If[mean2 =!= None, tempIm -= mean2];
			(* Step 2: divide stddev mean *)
			If[var2 =!= Sqrt[None], tempIm /= var2];
			tempIm	
		] (* Block *)	
	] (* Function *)
]; (* With *) 

(*-----------------------------------------------------------------------------*)



(* If grayscale image, might need to add channel dim *)
PadRank[array_] := If[ArrayDepth[array] < 3, ArrayReshape[array, Prepend[Dimensions[array], 1]], array];

Clear[LoadImage];

LoadImage[isize_][File[ipath_String]] := Scope[
	(* infer from filename *)
	path = ExpandFileName[ipath];
	If[!FileExistsQ[path],
		path = FindFile[ipath];
		If[FailureQ[path], EncodeFail["path `` does not exist", ipath]];
	];
	image = Switch[
		ToLowerCase @ FileExtension[path],
		"jpg" | "jpeg",
			First @ Image`ImportExportDump`ImageReadJPEG[path],
		"png",
			First @ Image`ImportExportDump`ImageReadPNG[path],
		"tiff",
			First @ Image`ImportExportDump`ImageReadTIFF[path],
		_,
			Quiet @ Import[path]
	];
	If[!ImageQ[image], EncodeFail["couldn't load image"]];
	image
];

LoadImage[isize_][image_Image] := image;

LoadImage[isize_][other_] := If[
	TrueQ @ Internal`UnsafeQuietCheck[Image`PossibleImageQ[other], False], 
	Replace[
		Quiet @ Check[Image[other, "Real", ImageSize -> isize], $Failed],
		Except[_Image] :> EncodeFail["failed to rasterize expression with head ``", Head[other]]
	],
	EncodeFail["input is neither a 2D image or a File"]
];


(******
makeFileEncoderFunction: a parallel out-of-core image loader relying on OpenCV
	cv::imread (http://docs.opencv.org/3.1.0/d4/da8/group__imgcodecs.html).
	All the limitations of cv::imread apply:
	1) formats like GIF not supported. JPEG, PNG + TIFF supported
	2) Only subset of ColorSpace options supported: (RGB, XYZ, LAB, LUV, Grayscale)
******)

makeFileEncoderFunction[x_Association, safeEnc_] := With[
	{
		mean = imageToConformedRawArray[1, 
			x["MeanImage"], 
			Prepend[x["ImageSize"], x["ColorChannels"]], 
			x["ColorSpace"]
		],
		stddev = imageToConformedRawArray[0.5, 
			x["VarianceImage"], 
			Prepend[x["ImageSize"], x["ColorChannels"]], 
			x["ColorSpace"]
		],
		threads = $ProcessorCount,
		useMean = If[x["MeanImage"] === None, 0, 1],
		useStdDev = If[x["VarianceImage"] === None, 0, 1],
		safeEnc2 = safeEnc,
		(* careful: dims of ImageData is reverse of ImageSize *)
		arraySize = Prepend[Reverse[x["ImageSize"]], x["ColorChannels"]]
	},
	LoadFileImagesSafe[#, arraySize, 
		x["ColorSpace"], mean, useMean, stddev, 
		useStdDev, threads, safeEnc2
	]&
];


LoadOpenCV := Block[{$ContextPath = $ContextPath}, Needs["OpenCVLink`"]; Clear[LoadOpenCV]];


(* note space in message: prevents eval error during paclet loading!! *)
 NetEncoder::imgimprt = "Cannot load ``. Using random image instead.";

LoadFileImagesSafe[paths_List, arraySize_List, colorspace_String, 
	meanImage_RawArray, useMean_Integer, stddevImage_RawArray, useStdDev_Integer, 
	threadNum_Integer, safeImageLoaderFunc_] := Scope[

	LoadOpenCV;

	(* only support some color spaces. Currently, OpenCVLink`Private`$LoadImagesFromPathInto
		gives a LibraryError when invalid color space. 
		Future: change behaviour of $LoadImagesFromPathInto to still return random image,
			avoids the need of this check.
	*)
	fastColorSpaces = {"RGB", "XYZ", "LAB", "LUV", "Grayscale"};
	outArrayDim = Join[{Length[paths]}, arraySize];
	outputNormal = If[TrueQ @ MemberQ[fastColorSpaces, colorspace],
		output = RawArray["Real32", ConstantArray[0, outArrayDim]];
	  	fails = LoadFileImages[output, paths, colorspace, 
	  		meanImage, useMean, stddevImage, useStdDev, threadNum
	  	];
	  	If[Head[fails] === LibraryFunctionError, Panic[]];
	  	Normal[output],
	  	(* when invalid color space, create random tensor *)
	  	RandomReal[1, outArrayDim]
	];
  	(* Failure Mode: try load images with safe loader *)
  	If[Length[fails] > 0,
   		failedInfo = AssociationThread[fails -> paths[[fails]]];
   		ims = (Quiet @ Catch[safeImageLoaderFunc[#], EncodeFail])& /@ failedInfo;
   		(* check whether some images have still failed + print message *)
   		stillFailedPos = Flatten @ Position[Values @ ims, $Failed];
   		stillFailedPaths = Values[failedInfo][[stillFailedPos]];
   		Message[NetEncoder::imgimprt, #] & /@ stillFailedPaths;
   		(* for success imports, read into outputNormal *)
   		imsSuccess = Select[ims, (# =!= $Failed)&];
   		KeyValueScan[(outputNormal[[#1]] = #2) &, imsSuccess];
   	];
  	outputNormal
];

(* LoadFileImages: returns a list of indices indicating images failed to load *)
LoadFileImages[output_RawArray, paths_List, 
	colorspace_String, meanImage_RawArray, useMean_, stddevImage_RawArray, 
	useStdDev_, threadNum_Integer] := Scope[

	paths2 =  First /* ExpandFileName /@ paths;
	concatPath = StringJoin[paths2]; (*  assume paths are list of files *)
	pathLengths = Length[ToCharacterCode[#, "UTF8"]]& /@ paths2;

	OpenCVLink`Private`$LoadImagesFromPathInto[
		output, concatPath, pathLengths, 
		colorspace, useMean, meanImage, useStdDev, stddevImage, threadNum
	]

];