BeginPackage["CloudObject`"]

(* 
  IMPORTANT: CloudObject`ToCharacterEncoding is used by the Java code as part of the kernel initalization. 
  is used to build $HTTPRequest(Data) so that the form field data can be decoded using the right character encoding. 
*)

ToCharacterEncoding::usage = "ToCharacterEncoding[{enc1, ...}, default] translates the first possible encN to an element of $CharacterEncodings, using default if no encN could be translated."
ToCharset::usage = "ToCharacterEncoding[{enc1, ...}, default] translates the first possible encN to a standard charset, using default if no encN could be translated."

Begin["`Private`"]

SetAttributes[{ToCharset, ToCharacterEncoding}, HoldRest]; (* don't prematurely evaluate the default *)

(* presumably, the encoding is in ASCII, so ToLowerCase makes sense *)
normalizeEncoding[enc_String] := 
    StringReplace[ToLowerCase[enc], "-"|"_"|Whitespace -> ""]

call:ToCharacterEncoding[_, _, __] := (
    System`Private`Arguments[call, {1,2}]; (* to issue a message for wrong number of args*)
    $Failed /; False (* always returns unevaluated *)
)

ToCharacterEncoding[enc_String|{encs___String}, default_:"Unicode"] := 
    Replace[
        Scan[
            Replace[
                $fromCharset[normalizeEncoding[#]],
                s:Except[Undefined] :> Return[s]
            ] &,
            (* per RFC-2045, etc., want it untranslated,
             i.e. ASCII, but perhaps more lenient.
             This could also be "ISO8859-1". *)
            {enc, encs}
        ],
        Null :> default
    ]


ToCharset[enc_String|{encs___String}, default_:"utf-8"] := 
    Replace[
        Scan[
            Replace[
                $toCharset[normalizeEncoding[#]],
                s:Except[Undefined] :> Return[s]
            ] &,
            {enc, encs}
        ],
        Null :> default
    ]

(* data source: http://www.iana.org/assignments/character-sets/character-sets.xml *)

$fromCharset := $fromCharset =
    Replace @ Dispatch @ Join[
        Map[
            normalizeEncoding[#] -> # &, 
            $CharacterEncodings
        ],
        Select[
            {
            "adobestandard"|"adobestandardencoding"|"adobestandardencoding"|"csadobestandardencoding" -> "AdobeStandard",
            "ansix341968"|"ansix341986"|"ascii"|"asci"|"cp367"|"csascii"|"ibm367"|"iso646irv"|"iso646us"|"isoir6"|"us"|"usascii" -> "ASCII",
            "cp936"|"cp936"|"csgbk"|"gbk"|"ms936"|"windows936" -> "CP936",
            "cseucpkdfmtjapanese"|"eucjp"|"eucjp"|"extendedunixcodepackedformatforjapanese" -> "EUC-JP",
            "850"|"cp850"|"cspc850multilingual"|"ibm850"|"ibm850" -> "IBM-850",
            "cp819"|"csisolatin1"|"ibm819"|"iso88591"|"iso88591"|"iso88591"|"iso88591"|"isoir100"|"l1"|"latin1" -> "ISO8859-1",
            "csisolatin6"|"iso885910"|"iso885910"|"iso885910"|"isoir157"|"l6"|"latin6" -> "ISO8859-10",
            "csiso885915"|"iso885915"|"iso885915"|"iso885915"|"latin9" -> "ISO8859-15",
            "csiso885916"|"iso885916"|"iso885916"|"iso885916"|"iso885916"|"isoir226"|"l10"|"latin10" -> "ISO8859-16",
            "csisolatin2"|"iso88592"|"iso88592"|"iso88592"|"iso88592"|"isoir101"|"l2"|"latin2" -> "ISO8859-2",
            "csisolatin3"|"iso88593"|"iso88593"|"iso88593"|"iso88593"|"isoir109"|"l3"|"latin3" -> "ISO8859-3",
            "csisolatin4"|"iso88594"|"iso88594"|"iso88594"|"iso88594"|"isoir110"|"l4"|"latin4" -> "ISO8859-4",
            "csisolatincyrillic"|"cyrillic"|"iso88595"|"iso88595"|"iso88595"|"iso88595"|"isoir144" -> "ISO8859-5",
            "arabic"|"asmo708"|"csisolatinarabic"|"ecma114"|"iso88596"|"iso88596"|"iso88596"|"iso88596"|"isoir127" -> "ISO8859-6",
            "csisolatingreek"|"ecma118"|"elot928"|"greek"|"greek8"|"iso88597"|"iso88597"|"iso88597"|"iso88597"|"isoir126" -> "ISO8859-7",
            "csisolatinhebrew"|"hebrew"|"iso88598"|"iso88598"|"iso88598"|"iso88598"|"isoir138" -> "ISO8859-8",
            "csisolatin5"|"iso88599"|"iso88599"|"iso88599"|"iso88599"|"isoir148"|"l5"|"latin5" -> "ISO8859-9",
            "cskoi8r"|"koi8r"|"koi8r" -> "koi8-r",
            "macarabic"|"macintosharabic" -> "MacintoshArabic",
            "csgb18030"|"csgb2312"|"gb18030"|"gb18030"|"gb180302000"|"gb2312"|"gb2312"|"gbk"|"macchinesesimplified"|"macintoshchinesesimplified" -> "MacintoshChineseSimplified",
            "macchinesetraditional"|"macintoshchinesetraditional" -> "MacintoshChineseTraditional",
            "maccroatian"|"macintoshcroatian" -> "MacintoshCroatian",
            "maccyrillic"|"macintoshcyrillic" -> "MacintoshCyrillic",
            "macgreek"|"macintoshgreek" -> "MacintoshGreek",
            "machebrew"|"macintoshhebrew" -> "MacintoshHebrew",
            "macicelandic"|"macintoshicelandic" -> "MacintoshIcelandic",
            "macintoshkorean"|"mackorean" -> "MacintoshKorean",
            "macintoshnoncyrillicslavic"|"macnoncyrillicslavic" -> "MacintoshNonCyrillicSlavic",
            "macintoshroman"|"macroman" -> "MacintoshRoman",
            "macintoshromanian"|"macromanian" -> "MacintoshRomanian",
            "macintoshromanpdfexport"|"macromanpdfexport" -> "MacintoshRomanPDFExport",
            "macintoshthai"|"macthai" -> "MacintoshThai",
            "macintoshturkish"|"macturkish" -> "MacintoshTurkish",
            "macintoshukrainian"|"macukrainian" -> "MacintoshUkrainian",
            "csshiftjis"|"mskanji"|"shiftjis"|"shiftjis"|"sjis" -> "ShiftJIS",
            "csutf8"|"utf8"|"utf8" -> "UTF8"
            },
            MemberQ[$CharacterEncodings, Last[#]] &
        ], 
        {_ :> Undefined}
    ]

$toCharset := $toCharset = Replace @ Dispatch @ Join[
    Apply[
        normalizeEncoding[#1] -> ToLowerCase[#2] &, {
                 "AdobeStandard" -> "Adobe-Standard-Encoding",
                         "ASCII" -> "US-ASCII",
                     "ISO8859-1" -> "ISO-8859-1",
                     "ISO8859-2" -> "ISO-8859-2",
                     "ISO8859-3" -> "ISO-8859-3",
                     "ISO8859-4" -> "ISO-8859-4",
                     "ISO8859-5" -> "ISO-8859-5",
                     "ISO8859-6" -> "ISO-8859-6",
                     "ISO8859-7" -> "ISO-8859-7",
                     "ISO8859-8" -> "ISO-8859-8",
                     "ISO8859-9" -> "ISO-8859-9",
                    "ISO8859-10" -> "ISO-8859-10",
              "ISOLatinCyrillic" -> "ISO-8859-5",
                     "ISOLatin4" -> "ISO-8859-4",
                     "ISOLatin3" -> "ISO-8859-3",
                     "ISOLatin2" -> "ISO-8859-2",
                     "ISOLatin1" -> "ISO-8859-1",
                "PrintableASCII" -> "ascii",
                      "ShiftJIS" -> "Shift_JIS",
                           "EUC" -> "EUC-JP",
                        "EUC-JP" -> "EUC-JP",
    "MacintoshChineseSimplified" -> "GB2312",
                        "koi8-r" -> "KOI8-R",
                          "UTF8" -> "utf-8"
        },
        {1}
    ],
    {_ :> Undefined}
]
End[]

EndPackage[]
