Begin["EntityFramework`DefaultEntityTypes`Private`"];

EntityFramework`LoadDefaultEntityTypes[] := (
	Unprotect[Internal`$DefaultEntityStores];
	Function[file,
		PrependTo[
			Internal`$DefaultEntityStores,
			Get[file]
		]
	] /@ FileNames[
		"*.wl",
		FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "DefaultEntityTypes"}]
	];
	Protect[Internal`$DefaultEntityStores];
);

End[];
