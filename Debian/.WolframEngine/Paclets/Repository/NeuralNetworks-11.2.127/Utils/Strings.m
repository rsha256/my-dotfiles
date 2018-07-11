Package["NeuralNetworks`"]



PackageScope["SciString"]

SciString[r_] := SciString[r, 6];

SciString[None, _] := "---";
SciString[r_, n_] := Scope[
	r = N[r];
	If[!MachineRealQ[r], Return["?", Block]];
	n = n - 2;
	If[Abs[r] < 10*$MachineEpsilon, Return[addZeros["0.", n], Block]];
	{num, exp} = MantissaExponent[r]; num *= 10; exp--; 
	ds = DoubleToString[num, False, n - 1 - Boole[num < 0]]; dsl = StringLength[ds];
	ds = Switch[Sign[dsl - n],
		-1, addZeros[ds, n - dsl],
		 0, ds,
		 1, StringTake[ds, n]
	];
	StringJoin[ds, If[Negative[exp], "e-", "e+"], IntegerString[exp]]
];

addZeros[s_, n_] := StringJoin[s, Table["0", n]];


PackageScope["DimsString"]

DimsString[{n_}] := IntegerString[n];
DimsString[list_] := TextString @ Row[list, "\[Times]"];


PackageScope["RightAlignedRowString"]

RightAlignedRowString[spacings_, inputs_] := StringJoin[
	Riffle[MapThread[{Table[" ", #2 - StringLength[#1]], #1}&, {fmt /@ inputs, spacings}], " "]
];

fmt[a_String] := a;
fmt[a_List] := StringJoin[Map[fmt, a]];
fmt[None] := "---";
fmt[i_Integer] := TextString[i];
fmt[r_Real] := SciString[r];
fmt[_DirectedInfinity] := "\[Infinity]";
fmt[_] := "?";


PackageScope["QuotedStringList"]

QuotedStringList[{a_}] := QuotedString[q];
QuotedStringList[{a_, b_}] := Row[{a, b}, " or ", BaseStyle -> {ShowStringCharacters -> True}];
QuotedStringList[{a__,b_}] := 
	Row[{Row[{a}, ", ", BaseStyle -> {ShowStringCharacters -> True}], QuotedString[b]}, ", or "];

PackageScope["QuotedString"]

QuotedString[q_] := Style[q, ShowStringCharacters -> True];


