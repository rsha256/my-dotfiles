(* ::Package:: *)

(*
This package is used with the CloudConnectorForExcel (CCfE) project to convert Wolfram Language 
into a structure which can be read by the CCfE add-in, which is a specialised JSON format.
*)

BeginPackage["CloudObject`"] 

cloudConnectorExcelForm::usage = "Converts any expression into a specialised JSON that can be read by Cloud Connector for Excel add-in";

Begin["`Private`"]

(*
This function is designed to take any expression and convert it into something the 
CloudConnectForExcel can understand. 
Expressions are converted into something that Excel can handle. Every element in a 2 dimensional 
list is seperated into 2 key-value pairs, data and type. 
The data is limited to images, numbers and strings and type is split accordingly.
Anything which cannot be displayed in Excel is converted into an image.
The dimensions of the array are also specified in a seperated kvp and are used when unpacking the
json on the client side.
*)

CloudConnectorForExcel`$VersionNumber = "1.0.0";

$DefaultDepth = 2;
$AssociationsOn = True;
$KeyDensityThreshold = 0.75;
$ReturnImage = False;

$ExcelError = "#N/A";

Options[cloudConnectorExcelForm] = {
    "DepthLimit" :> $DefaultDepth,
    "KeyDensityThreshold" :> $KeyDensityThreshold,
    "Compact" -> True,
    "FailWithImages" :> $ReturnImage
};

(*Exports the structure created with the above code into a JSON*)
cloudConnectorExcelForm[evaluation_, opts: OptionsPattern[]] := restructureAndGetDimensions[evaluation, opts]

Options[restructureAndGetDimensions] = Options[cloudConnectorExcelForm];

restructureAndGetDimensions[data_, opts: OptionsPattern[]] :=
    With[
        {restructuredData = format[data, opts]},
        <| 
            "data" -> restructuredData, 
            "dimensions" -> Dimensions[restructuredData],
            "version" -> CloudConnectorForExcel`$VersionNumber 
        |>
    ]

(*Excel incompatible types are convered into PNGs*)
exportToPNGBase64[img_] := Developer`EncodeBase64[ExportString[img, "PNG"]]

custForm[num_] :=
    ToString[AccountingForm[num, NumberSigns -> {"-", ""}]]

dateObjectToExcelDate[date_] :=
    QuantityMagnitude[UnitConvert[Quantity[AbsoluteTime[date], "Seconds"], "Days"]] + 2

SetAttributes[dateObjectToExcelDate, Listable]

Options[tester] = Options[cloudConnectorExcelForm]

tester[data_, opts: OptionsPattern[]] := ReplaceAll[
    iFormat[data, OptionValue["DepthLimit"], True, OptionValue["KeyDensityThreshold"]],
    iAtom[x_] :> atom[OptionValue["FailWithImages"]][x]["data"]
]

Options[format] = Options[cloudConnectorExcelForm];

format[data_, opts: OptionsPattern[]] := ReplaceAll[
    iFormat[data, OptionValue["DepthLimit"], True, OptionValue["KeyDensityThreshold"]],
    iAtom -> atom[OptionValue["FailWithImages"]]
]

addKeys[depth_][data_?AssociationQ] /; depth > 1 := MapThread[
    Which[
        ListQ[#], Prepend[##],
        AssociationQ[#], Prepend[#1, "" -> #2],
        True, {#2, #1}
    ]&,
    {Values[data], iAtom /@ Keys[data]}
]

addKeys[1][data_?AssociationQ] := Transpose[Map[
    iAtom,
    {Keys[data], Values[data]},
    {2}
]]

iFormat[{}, rest___] := iFormat[{iAtom[""]}, rest]

iFormat[data_?AssociationQ /; !AllTrue[data, AssociationQ], depth_?Positive, col_, threshold_] := iFormat[
    addKeys[depth][data],
    Replace[depth, 1 -> 2],
    col,
    threshold
]

keyUnion[l:{__?AssociationQ}, rest___] := KeyUnion[l, rest]
keyUnion[ass_?AssociationQ /; AllTrue[ass, AssociationQ], rest___] := AssociationThread[Keys[ass], KeyUnion[Values[ass], rest]]

iFormat[data:(_?ListQ|_?AssociationQ) /; Length[data] > 0 && AllTrue[data, AssociationQ], depth_, col_, threshold_] /; depth > 1 := If[
    thresholdFailCheckQ[data, threshold],
    With[{newdata = Transpose[keyUnion[data, ""&], AllowedHeads -> All]},
        iFormat[
            Prepend[
                Transpose[Values[newdata], AllowedHeads -> All],
                If[AssociationQ[data], "" -> #, #]&[iAtom /@ Keys[newdata]]
            ],
            depth,
            col,
            threshold
        ]
    ],
    iFormat[
        addKeys[depth - 1] /@ data,
        Replace[depth, 2 -> 3],
        col,
        threshold
    ]
]
iFormat[data:{__}, depth_?Positive, False, threshold_] := With[
    {newdata = iFormat[#, depth - 1, True, threshold]& /@ data},
    With[{max = Max[Length /@ newdata]},
        Join[##, 2]& @@ Map[PadRight[#, max, {ConstantArray[iAtom[""], Length[First[#]]]}]&, newdata]
    ]
]
iFormat[data_List, depth_?Positive, True, threshold_] := Transpose[iFormat[data, depth, False, threshold]]
iFormat[data_, _, _, _] := {{iAtom[data]}}

iAtom[data_] /; !FreeQ[data, iAtom] := iAtom[data /. iAtom -> Identity]

thresholdFailCheckQ[data_, None] := True
thresholdFailCheckQ[data_, threshold_] := With[
    {keys = Keys /@ data},
    N[Mean[Length /@ keys] / Length[DeleteDuplicates[Join @@ keys]]] > threshold
]

atom[imgq_][x_String?StringQ] := If[tooLongQ[x],exprBehaviour[imgq][x],<|"data" -> x, "type" -> "String"|>];

(*

Numerics handling

*)

(* Do nothing its a Machine Integer*)

atom[imgq_][x_Integer?Developer`MachineIntegerQ] := <|"data" -> ToString[x], "type" -> "Integer"|>

(* If the Integer is too long format as a Real and let the client deal with it *)

atom[imgq_][x_Integer?NumericQ] := <|"data" -> If[ tooLongQ[x], excelExponentForm[N[x]], ToString[x]], "type" -> "Integer"|>

atom[_][x_Real?MachineNumberQ] := <|"data" -> excelExponentForm[x], "type" -> "Real"|>

atom[_][x_Real?NumericQ] := <|"data" -> excelExponentForm[x], "type" -> "Real"|>

(* Convert Rationals into a Real*)

atom[_][x_Rational?NumericQ] := <|"data" -> excelExponentForm[N[x]], "type" -> "Real"|>

(* In the case that the symbol is also a Numeric*)

atom[imgq_][x_?NumericQ] := <|"data" -> excelExponentForm[N[x]], "type" -> "Real"|>

(* To handle things like Pi*)

(*CONSIDER: Is there a symbol which is an Integer value?*)

atom[imgq_][x_Symbol?NumericQ] := <|"data" -> excelExponentForm[N[x]], "type" -> "Real"|>

atom[_][x_DateObject?DateObjectQ] := <|
    "data" -> dateObjectToExcelDate[x], 
    "type" -> "ExcelDate"
|>

(* 

Other Types

*)

atom[_][x_?BooleanQ] := <|"data" -> ToUpperCase[ToString[x]], "type" -> "Boolean"|>

(* No quantities are handled yet in Excel so just take the magnitude *)

atom[x_][HoldPattern[Quantity[magnitude_, unit_]]] := atom[x][magnitude]

(* Any graphical type expression*)

atom[_][x : _Graphics3D | _Graphics | _Image | _GeoGraphics] := <|
    "data" -> exportToPNGBase64[x],
    "type" -> ToString[Head[x]]
|>

atom[_][fullExpr : Legended[innerExpr : _Graphics3D | _Graphics | _Image | _GeoGraphics, ___]] := <|
    "data" -> exportToPNGBase64[fullExpr],
    "type" -> ToString[Head[innerExpr]]
|>

(* Any Symbol should be convert*)

atom[imgq_][x_Symbol] := atom[imgq][SymbolName[x]]

(* Default behaviours if all else fails is to treat as an expr*)

atom[imgq_][x_] := exprBehaviour[imgq][x];

(* 

If the result cannot be displayed natively in excel it will either return an Excel Error or 
make an image of the output limied expression
*)

exprBehaviour[imgq_][x_] := If[
    imgq,
    <|"data" -> exportToPNGBase64[limitOutput[x]], "type" -> "Expression"|>,
    <|"data" -> $ExcelError, "type" -> "String"|>
]


(*Limits

Functions which help access the limits of Excel
Defined from:
https://support.office.com/en-gb/article/Excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3?ui=en-US&rs=en-GB&ad=GB&fromAR=1

*)

$ExcelCharacterLimit = 32767;

$ExcelPrecision = 16;

$NumberSizeLimit = 308;

tooLongQ[str_String] := If[ StringLength[str] > $ExcelCharacterLimit, True, False]

tooLongQ[int_Integer] := If[ IntegerLength[int] > $NumberSizeLimit, True, False]



(*

This is used because numbers in between tooSmallQ numbers are displayed as unparsable exponents

ToString[NumberForm[N[10, 16],  NumberFormat -> (Row[{#1, "E", #3}] &)]]

"10.00000000000000E"

*)

exponentCorrectionCheck[number_] := number < -1000000 || number > 1000000 || -0.000001 < number <0.000001

(* Give the form 1.23456E78 for Excel to handle*)
excelExponentForm[number_?exponentCorrectionCheck] := ToString[ NumberForm[ number, $ExcelPrecision, NumberFormat -> (Row[{#1, "E", #3}]&)]]

(* Should be a number which doesn't meet the exponentCorrectionCheck criteria, which will just give a number to 16 digit precision*)
excelExponentForm[number_] := ToString[NumberForm[number, $ExcelPrecision]]

(* 

This is adapted from the output limiter code which exists in the Kernel package

https://stash.wolfram.com/projects/KERN/repos/kernel/browse/StartUp/OutputSizeLimit.m

Used to limit the output of an expression that would be rasterized and send to Excel

*)

limitOutput[x_] := 
With[{fmt = OptionValue[Options[$Output], FormatType],
        outputsizelimit =
            If[($OutputSizeLimit === Infinity) || NumericQ[$OutputSizeLimit], $OutputSizeLimit, 2^20]},
    If[
        (* if the expression isn't destined for boxes, do nothing *)
        !MemberQ[$BoxForms, fmt] ||
        MatchQ[HoldComplete[x], _[OutputSizeLimit`Dump`$unlimitedTextPattern]] ||
        MatchQ[HoldComplete[x], _[OutputSizeLimit`Dump`$unlimitedBoxPattern]],
        Unevaluated[x],
        
        (* otherwise, return the boxes, or encapsulate them if they're too big *)
        With[{boxes = Block[{$RecursionLimit = Typeset`$RecursionLimit}, MakeBoxes[x, fmt]]},
            OutputSizeLimit`Dump`loadSizeCountRules[];
            If[
                BoxForm`SizeCount[boxes, outputsizelimit] // TrueQ,
                BoxForm`AbsoluteRawBoxes[boxes],
                (*
                    If the boxes are too big, pass them through to the shortener. Note
                    that we pass the boxes and not the expression, because the first thing
                    that the shortener (i.e., Short) does is to typeset the expression. No
                    need to duplicate that effort, since we already have the boxes.
                    We wrap the boxes with FE`Boxes so that the encapsulateOutput utility
                    can tell the difference between pre-typeset boxes, and an expression
                    that still needs to be typeset.
                *)
                OutputSizeLimit`Short[x]] ],
        
        (* other-otherwise, do nothing *)
        Unevaluated[x] ] ]

(*Bypass if the output is just a string because it should be more than tooLongQ limit*)

limitOutput[x_String] := OutputSizeLimit`Short[x]


End[]

EndPackage[]
