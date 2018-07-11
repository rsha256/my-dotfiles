BeginPackage["CloudObject`"]

Hold[System`$CloudSymbolBase];
System`CloudSymbol;

Begin["`Private`"]

Unprotect[CloudSymbol];

$CloudSymbolBase := CloudObject["/CloudSymbol"]

DownValues[CloudSymbol] = {
    HoldPattern[CloudSymbol[uri_, base_ : $CloudSymbolBase]] :>
        CloudGet[CloudObject[uri, base]]
};

UpValues[CloudSymbol] = {
    HoldPattern[Set[CloudSymbol[uri_, base_ : $CloudSymbolBase], expr_]] :>
        (CloudPut[Unevaluated[expr], CloudObject[uri, base]]; expr),
    HoldPattern[SetDelayed[CloudSymbol[uri_, base_ : $CloudSymbolBase], expr_]] :>
        (CloudPut[Unevaluated[expr], CloudObject[uri, base]];)
};

SetAttributes[CloudSymbol, {ReadProtected}];
Protect[CloudSymbol];

(* TODO: override more Set-related operations *)

End[]

EndPackage[]
