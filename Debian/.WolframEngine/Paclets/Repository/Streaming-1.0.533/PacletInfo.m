(* Mathematica package *)
Paclet[
	Name -> "Streaming",
Version -> "1.0.533",
MathematicaVersion -> "11.0+",
	Description -> "Streaming framework",
	Creator -> "Leonid Shifrin <leonids@wolfram.com>",
	Loading -> Automatic,
	Extensions -> {
		{
			"Kernel", 
			Context -> {"StreamingLoader`", {"Streaming`", "StreamingMain.m"}},
			Symbols -> {
				"Streaming`CachedObject"
			}
		}
	}
]