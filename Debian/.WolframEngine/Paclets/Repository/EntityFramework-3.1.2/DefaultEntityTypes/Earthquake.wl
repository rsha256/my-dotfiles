EntityStore["Earthquake" -> <|
	"EntityValidationFunction" -> Function[True],
	"Properties" -> Join[
		<|
			"Label" -> <|
				"DefaultFunction" -> CanonicalName
			|>
		|>,
		AssociationMap[
			<|
				"DefaultFunction" -> EntityFramework`BatchApplied[Function[ents, EarthquakeData[ents, #]]]
			|> &,
			EarthquakeData["Properties"]
		]
	]
|>]