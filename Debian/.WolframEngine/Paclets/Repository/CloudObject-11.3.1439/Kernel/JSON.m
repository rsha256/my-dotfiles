BeginPackage["CloudObject`"]

Begin["`Private`"]

toJSON[s_String] :=
    "\"" <> StringJoin[
        If[32 <= # < 128 && ! MemberQ[ToCharacterCode["\"\\"], #],
            FromCharacterCode[#], "\\u" <> IntegerString[#, 16, 4]
        ] & /@ ToCharacterCode[s]
    ] <> "\""

toJSON[s_Symbol] := toJSON[SymbolName[s]]

toJSON[n_?NumberQ] := ToString[n]

isObject[list_] :=
    Length[list] > 0 && (And @@ (MatchQ[#, _ -> _] & /@ DeleteCases[list, Null]))

toJSON[list_List?isObject] :=
    "{" <> StringJoin @@ Riffle[DeleteCases[list, Null] /.
        (name_ -> value_) :> toJSON[name] <> ":" <> toJSON[value], ","
    ] <> "}"

toJSON[list_List] :=
    "[" <> StringJoin @@ Riffle[toJSON /@ DeleteCases[list, Null], ","] <> "]"

importFromJSON := (
                   Needs["JSONTools`"];
                   importFromJSON = JSONTools`FromJSON
			      )
exportToJSON := (
                 Needs["JSONTools`"];
                 exportToJSON = JSONTools`ToJSON
			    )

End[]

EndPackage[]
