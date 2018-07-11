(* Mathematica package *)
BeginPackage["CloudObject`"]

JoinURL;
JoinURLSearch;

Begin["`Private`"]

(*URL parsing*)

(* In older Mathematica versions (such as the one currently in the cloud, as of Apr 16),
 URLParse fails because URLUtilities is not there and, for some reason, OAuth does not auto-load. *)
If[FindFile["URLUtilities`"] === $Failed, Get["OAuth`"]];

urlParseList[url_] := With[{parsed = URLParse[url]},
	Replace[{
		Replace[parsed["Scheme"], s_String :> s <> ":"],
    	parsed["Domain"],
    	parsed["Path"],
    	parsed["Query"]
    }, None -> False, {1}]
]

JoinURL[items__] :=
    StringJoin[Riffle[StringTrim[#, "/"] & /@ DeleteCases[Flatten[{items}], ""], "/"]]

JoinURLSearch[values_] :=
    If[Length[values] > 0,
        "?" <> URLQueryEncode[Flatten[{values}]],
    (* else *)
        ""
    ]

End[]

EndPackage[]
