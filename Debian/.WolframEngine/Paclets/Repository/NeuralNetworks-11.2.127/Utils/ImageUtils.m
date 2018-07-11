Package["NeuralNetworks`"]


(*----------------------------------------------------------------------------*)
PackageScope["validateVarianceImage"]

validateVarianceImage[im_, size_, space_, channels_] := Scope[
	If[im === None, Return[True]];
	imNew = getConformedImageData[im, size, space, channels];
	(* check whether values are positive *)
	If[Not @ TrueQ[Positive[Min[imNew]]],
		(* zeros are easily introduced when resizing or converting color spaces.
			warn user about this *)
		If[ImageQ[im] && ((ColorSpace[im] =!= space) || (ImageSize[im] =!= size)),
			message = StringJoin["parameter \"VarianceImage\" contains non-positive ",
				"values when it is converted to the NetEncoder's color space ",
				ToString[space], " using ConvertColor and resized to ", 
				ToString[size], " using ImageResize." 
			]
			,
			message = "parameter \"VarianceImage\" contains non-positive values."
		];
		FailValidation[NetEncoder, message];
		];
	True
];

(*********************************)
PackageScope["getConformedImageData"]
getConformedImageData[im_Image, size_, space_, channels_] := Scope[
	conformer = ConformImageColors[space] /* ConformImageSize[size] /* 
	ConformImageAlpha[channels] /* ToReal32Image;
	ImageData[conformer[im], Interleaving -> False]
]

getConformedImageData[im_, ___] := im;

(*********************************)
PackageScope["imageToConformedRawArray"]

imageToConformedRawArray[pow_, None, ___] := RawArray["Real32", {0}];

imageToConformedRawArray[pow_, im_Real | im_Integer, dim_List, space_String] := 
	RawArray["Real32", Power[ConstantArray[im, dim], pow]];


imageToConformedRawArray[pow_, im_List, dim_List, space_String] := 
	RawArray["Real32", Power[im * ConstantArray[1., dim], pow]];

imageToConformedRawArray[pow_, im_Image, dim_List, space_String] := 
	RawArray["Real32", getConformedImageData[im, Rest[dim], space, First[dim]]]

(*********************************)
PackageScope["ToReal32Image"]
(* TODO: Investigate using ImageData[.., "Byte"] downstream *)
ToReal32Image[img: HoldPattern[Image[ra_RawArray /; Developer`RawArrayType[ra] =!= "Real32", ___]]] :=
	Image[img, "Real32"];

ToReal32Image[img_Image] := img;

ToReal32Image[_] := EncodeFail["couldn't load image"];

(*********************************)
PackageScope["ConformImageSize"]
ConformImageSize[size_][img_] := If[ImageDimensions[img] === size, img, ImageResize[img, size]];

PackageScope["ConformImageColors"]
ConformImageColors[colors_][img_] := 
	If[ImageColorSpace[img] === colors, img, ColorConvert[img, colors]];

PackageScope["ConformImageAlpha"]
ConformImageAlpha[channels_][img_] := Switch[
	ImageChannels[img],
	channels + 1, RemoveAlphaChannel[img, White], 
	channels, img,
	_, EncodeFail["image had wrong number of color channels (`` instead of ``)", ImageChannels[img], channels]
];