(* ::Package:: *)

GeoGraphics;

Begin["System`GeoGraphicsDump`"]


With[{entityq = If[Names["GeoGraphics`GeoEntityQ"] === {}, Symbol["System`GeoGraphicsDump`entityQ"], Symbol["GeoGraphics`GeoEntityQ"]]},
System`GeoGraphicsDump`getZoom1Image[body_?entityq, georange_, zoom_] := Module[{image, dims, xpixels, ypixels},
	image = GeoGraphics`CachedEntityValue[body, "CylindricalEquidistantTexture"];
	If[Head[image] === Graphics,
		(* We have an object of the form Graphics[raster] *)
		image = Quiet[ Image[image[[1]], "Byte"] ]
	];
	If[!ImageQ[image],
		Message[GeoGraphics::gmim, body];
		Return[$Failed]
	];
	If[zoom =!= Automatic && zoom =!= 1,
		Message[GeoGraphics::zoom1, zoom, body]
	];
	(* Cut the requested geo range *)
	If[georange =!= {{-90, 90}, {-180, 180}},
		dims = ImageDimensions[image];
		xpixels = Rescale[georange[[2]], {-180, 180}, {0, dims[[1]]}];
		ypixels = Rescale[georange[[1]], {-90, 90}, {0, dims[[2]]}];
		image = ImageTrim[image, Transpose[{xpixels, ypixels}]]
	];
	{image, Reverse@ georange}
];
];

End[]
