Begin["EntityFramework`BatchApplied`Private`"];

b : EntityFramework`BatchList[Except[_List, x_], ___] := (Message[EntityFramework`BatchList::list, HoldForm[b], 1]; Null /; False);

Options[EntityFramework`BatchApplied] = {
	"BatchSize" -> Infinity
};

EntityFramework`BatchApplied[f_, OptionsPattern[]][HoldPattern[EntityFramework`BatchList][l_List], rest___] := With[
	{batchSize = OptionValue["BatchSize"]},
	If[! (batchSize === Infinity || (IntegerQ[batchSize] && Positive[batchSize])),
		Message[EntityFramework`BatchApplied::ioppf, "BatchSize", batchSize];
		Return[$Failed]
	];
	If[batchSize === Infinity || Length[l] <= batchSize,
		f[l, rest] // Replace[
			Except[_List?(Length[#] === Length[l] &)] :> (
				Message[EntityFramework`BatchApplied::listres, Length[l], HoldForm[f[l, rest]]];
				$Failed
			)
		],
		Module[{tag}, Catch[
			Join @@ Function[
				EntityFramework`BatchApplied[f][EntityFramework`BatchList[#], rest] // Replace[
					$Failed :> Throw[$Failed, tag]
				]
			] /@ Partition[l, UpTo[batchSize]],
			tag
		]]
	]
];
EntityFramework`BatchApplied[f_, OptionsPattern[]][x_, rest___] := f[{x}, rest] // Replace[{
	{res_} :> res,
	_ :> (
		Message[EntityFramework`BatchApplied::listres, 1, HoldForm[f[{x}, rest]]];
		$Failed
	)
}];

End[];
