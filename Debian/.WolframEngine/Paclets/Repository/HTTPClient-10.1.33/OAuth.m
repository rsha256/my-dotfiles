(* Wolfram HTTPClient Package *)


BeginPackage["HTTPClient`OAuth`"]


Begin["`Private`"]


(* ========================================================================= ==

                        Exposed interface

    Comments:

    ToDo:

== ========================================================================= *)

(* OAuthAuthentication *)
$BlockingDialog=!TrueQ[$CloudEvaluation];

HTTPClient`OAuthAuthentication[rules__?OptionQ] :=
    Block[{token, auth,$BlockingDialog=Lookup[rules,"Blocking",!TrueQ[$CloudEvaluation]]},

        auth = MakeOAuthAuthentication[FilterRules[{rules},Except["Blocking"]]];
        (
            token = OAuthFlow[auth];
            If[token === $Canceled,
                $Canceled,
                HTTPClient`OAuthToken[auth, token]
            ] /; (token =!= $Failed)
        ) /; (auth =!= $Failed)

    ]


(* OAuthSignURL *)

HTTPClient`OAuthSignURL[url_, opts__?OptionQ] :=
    Block[{res},

        res = OAuthURLFetchList[
                url,
                Flatten[{opts}] /. HTTPClient`OAuthToken -> List
            ];
        res /; ListQ[res]

    ]


(* ========================================================================= ==

                        Messages

    Comments:

    ToDo:

== ========================================================================= *)

(* Initialization failures*)

HTTPClient`OAuthAuthentication::libload = "Failed to load OAuth libraries.";


(* OAuth configuration *)

HTTPClient`OAuthAuthentication::type = "`2` in `1` must be one of `3`.";
HTTPClient`OAuthAuthentication::stype = "`2` in `1` is not a string.";
HTTPClient`OAuthAuthentication::ntype = "Failed to automatically determine authorization type, please specify the `1` property.";
HTTPClient`OAuthAuthentication::prop = "The rules `2` give values for things that are not valid `1` properties.";
HTTPClient`OAuthAuthentication::sane = "`1` sanity checking failed for properties `2`.";


(* Get request token *)

HTTPClient`OAuthAuthentication::illreq = "The service `1` is of type `2` and does not use request tokens.";
HTTPClient`OAuthAuthentication::reqbdy = "The service `1` returned invalid request token response `2`.";


(* Assemble authorization URL *)

HTTPClient`OAuthAuthentication::sclst = "The scope `3` must be a list of strings for authorization type `1` used by `2`.";
HTTPClient`OAuthAuthentication::scnorlst = "The scope `3` must be None or a list of strings for authorization type `1` used by `2`.";
HTTPClient`OAuthAuthentication::scpar = "OAuth configuration for `1` needs a value for `2` in order to authorize with explicit scope.";
HTTPClient`OAuthAuthentication::stfmt = "The `1` state `3` for `2` must be a non-empty string of word characters or the symbol None.";


(* Get access token *)

HTTPClient`OAuthAuthentication::bdrt = "`3` is not a valid request token for authorization type `1` usd by `2`.";
HTTPClient`OAuthAuthentication::rtnone = "Request token None is not valid for authorization type `1` used by `2`.";
HTTPClient`OAuthAuthentication::rtenone = "Request token `3` should be None for authorization type `1` used by `2`.";
HTTPClient`OAuthAuthentication::accbdy = "The service `1` returned invalid access token response `2`.";
HTTPClient`OAuthAuthentication::bdstate = "The `1` access token from `2` was discarded because of a possible cross site request forgery attack. Make sure that the `3` belongs to the autorization request initiated by Mathematica.";


(* OAuth flow *)

HTTPClient`OAuthAuthentication::reqfail = "Failed to obtain OAuth request token for `1`.";
HTTPClient`OAuthAuthentication::bdver = "The authorization verifier `1` must be a string.";
HTTPClient`OAuthAuthentication::accfail = "Failed to obtain OAuth access token for `1`.";
HTTPClient`OAuthAuthentication::bdkey = "The authorization key `1` must be a string.";


(* Sign requests *)

HTTPClient`OAuthSignURL::nopt = "The option `2` is missing in `1`.";
HTTPClient`OAuthSignURL::stropt = "URL parameter options `2` for `1` must have string names and string values.";


(* ************************************************************************* *)
(*                             Library functions                             *)
(* ************************************************************************* *)

(* ========================================================================= ==

                        Library loading

    Comments:
        initializedQ is a precondition for using oAuth10SignURL.

    ToDo:

== ========================================================================= *)


(* initializedQ *)

initializedQ := (initializedQ = (loadLibOAuth[] =!= $Failed))


(* loadLibOAuth *)

loadLibOAuth[] :=
    Block[{path},

        path = FindLibrary["WolframAuthenticate"];
        (
            oAuth10SignURL = LibraryFunctionLoad[path, "oSignURL", {"UTF8String", "UTF8String", "UTF8String", "UTF8String", "UTF8String", "UTF8String", "UTF8String"}, "UTF8String"];
            oAuth10SignURL /; (Head[oAuth10SignURL] =!= LibraryFunctionLoad)

        ) /; (path =!= $Failed)
    ]

loadLibOAuth[___] := $Failed


(* ========================================================================= ==

                        URL encoding

    Comments: Rewritten based on the reference below.

    ToDo:

    References:
        http://stackoverflow.com/questions/3160924/how-do-i-url-escape-a-string-in-mathematica

== ========================================================================= *)

urlEscape[s_String]:=System`URLEncode[s]
(*urlEscape[s_String] :=
    StringReplace[s,
        {
            " "->"+",
            RegularExpression["[^\\w\\_\\.-]"] :>
                "%" <> ToUpperCase[IntegerString[ToCharacterCode["$0"][[1]],16]]
        }
    ]
*)
urlEscape[x_List] := urlEscape[StringJoin @@ (ToString /@ x)]

urlEncode[s_String] := urlEscape[s]

urlEncode[r:{_Rule...}, sortfun_:Sort] :=
    StringJoin[
        Riffle[
            StringJoin[ToString[#1], "=", urlEscape[#2]]& @@@ sortfun[r],
            "&"
        ]
    ]


(* ========================================================================= ==

                        Predicates and tests

    Comments:

    ToDo:

== ========================================================================= *)

(* nonEmptyString *)

nonEmptyStringQ[x_] := StringQ[x] && StringLength[x] != 0


(* Test FrontEnd availability *)

hasFrontEnd[] := ToString[Head[$FrontEnd]] === "FrontEndObject"


(* ************************************************************************* *)
(*                                OAuth Common                               *)
(* ************************************************************************* *)

(* ========================================================================= ==

                        Common auth properties

    Comments:
        "ServiceName" is the name identifying the OAuth service.
        "AutomaticScope" is the scope to use for auth unless provided explicitly.
        "VerifierLabel" is the label for the input field in the verifier dialog.
            Don't capitalize the label.

    ToDo:

== ========================================================================= *)

(* Defaults *)

defaultAuthCommon =
    {
        "ServiceName"          -> Automatic,
        "AutomaticScope"       -> None,
        "VerifyPeer"           -> Automatic,
        "VerifierLabel"        -> "verifier",
        "InputFieldOpts"       -> Automatic,
        "AuthenticationDialog" -> Automatic
    };


(* Sanity tests *)

commonSanityTests[auth_] :=
    {
        "OAuthVersion" ->
            MemberQ[OAuthService["OAuthVersion"], oAuthVersion[auth]],
        "ServiceName" ->
            nonEmptyStringQ[takeServiceName[auth]],
        "VerifyPeer" ->
            MatchQ[takeVerifyPeer[auth], Automatic | True | False],
        "VerifierLabel" ->
            nonEmptyStringQ[takeVerifierLabel[auth]],
        "InputFieldOpts" ->
            With[{ifopts = takeInputFieldOpts[auth]},
                ifopts === Automatic || OptionQ[ifopts]
            ],
        "AuthenticationDialog" ->
            !StringQ[takeAuthenticationDialog[auth]]
    }


(* ========================================================================= ==

                        OAuthAuthentication

    Comments: Each type of authorization should add downvalues for
        iOAuthAuthentication

    ToDo:

== ========================================================================= *)

(* MakeOAuthAuthentication *)

MakeOAuthAuthentication[rules___?OptionQ] :=
    Block[{auth},
        auth = oAuthAuthentication[rules];
        auth /; (auth =!= $Failed)
    ]

MakeOAuthAuthentication[___] := $Failed


(* oAuthAuthentication *)

oAuthAuthentication[rules___?OptionQ] :=
    Block[{param = "OAuthVersion", authtype, res},
        authtype = param /. Flatten[{rules}] /. {param -> Automatic};
        res = iOAuthAuthentication[authtype, FilterRules[Flatten[{rules}], Except[param]]];
        res /; (res =!= $Failed)
    ]

oAuthAuthentication[___] := $Failed

iOAuthAuthentication[args___] :=
    (messageOAuthAuthentication[args]; $Failed)


(* :message: *)

messageOAuthAuthentication[Automatic, ___] :=
    Message[HTTPClient`OAuthAuthentication::ntype, "OAuthVersion"]

messageOAuthAuthentication[authtype:Except[_String], ___] :=
    Message[HTTPClient`OAuthAuthentication::stype, "OAuthVersion" -> authtype, authtype]

messageOAuthAuthentication[authtype_, ___] :=
    Message[HTTPClient`OAuthAuthentication::type, "OAuthVersion" -> authtype, authtype, OAuthService["OAuthVersion"]]


(* ========================================================================= ==

                        OAuthFlow

    Comments: Each type of authorization should add downvalues for
        iOAuthFlow

    ToDo:

== ========================================================================= *)

(* OAuthFlow *)

OAuthFlow[auth_?AuthQ, args___] :=
    Block[{token},
        token = iOAuthFlow[auth, args];
        token /; (TokenSaneQ[auth, token] || token === $Canceled)
    ]

OAuthFlow[___] := $Failed


(* iOAuthFlow *)

iOAuthFlow[___] := $Failed


(* ========================================================================= ==

                        Typesetting

    Comments:

    ToDo:

== ========================================================================= *)

(* MakeBoxes for OAuthToken *)

HTTPClient`OAuthToken /: MakeBoxes[e:HTTPClient`OAuthToken[auth_, tok_], format_] :=
    With[{name = takeServiceName[auth], type = AuthType[tok], head = ToString[Head[e]]},
        With[{str = name <> " " <> type},
            TagBox[
                RowBox[{head, "[", "\[LeftSkeleton]",
                    TagBox[MakeBoxes[str, format], Editable -> False],
                    "\[RightSkeleton]", "]"}],
                InterpretTemplate[Hold[e]],
                Editable -> False, Selectable -> True, SelectWithContents -> True
            ]
        ] /; StringQ[type]
    ]


(* ========================================================================= ==

                        AuthType

    Comments:

    ToDo:

== ========================================================================= *)

(* AuthType *)

AuthType[ver_String] := "OAuth " <> ver

AuthType[obj_] :=
    Block[{ver},
        ver = oAuthVersion[obj];
        AuthType[ver] /; StringQ[ver]
    ]


(* ========================================================================= ==

                        Configuration and token predicates

    Comments:
        AuthQ gives True for any expression holding an authorization
        configuration.  Each type of authorization must overload AuthQ to
        recognize its particular form of configuration expression.

        TokenQ gives True for any expression that is used to hold authorization
        tokens.  Each type of authorization must overload TokenQ to recognize
        its particular form of token expression.

        TokenSaneQ tests whether an expression is in the form of a token
        matching the given authorization configuration.
        Each type of authorization must overload iTokenSaneQ to recognize its
        particular form of token expression.

    ToDo:

== ========================================================================= *)

(* AuthQ *)

AuthQ[_] := False


(* TokenQ *)

TokenQ[_] := False


(* TokenSaneQ *)

TokenSaneQ[_, _HTTPClient`OAuth`Private`temporaryOAuthToken] :=
    True/;!$BlockingDialog
    
TokenSaneQ[auth_, tok_] :=
    Block[{res},
        res = iTokenSaneQ[auth, tok];
        res /; MatchQ[res, True | False]
    ]

TokenSaneQ[_, _] := False


(* ========================================================================= ==

                        OAuthURLFetchList

    Comments:

    ToDo:

== ========================================================================= *)

(* OAuthURLFetchList *)

OAuthURLFetchList[args___, opts:OptionsPattern[]] :=
    Block[{res},
        res = oAuthURLFetchList[args, opts];
        res /; (ListQ[res] || AtomQ[res])
    ]

OAuthURLFetchList[expr___] :=
    (messageOAuthURLFetchList[expr]; Null /; False)


(* :message: *)

messageOAuthURLFetchList[args___, opts:OptionsPattern[]] /; (FilterRules[{opts}, "OAuthAuthentication"] === {}) :=
    Message[HTTPClient`OAuthSignURL::nopt, HoldForm[OAuthURLFetchList[args, opts]], "OAuthAuthentication"]


(* oAuthURLFetchList *)

oAuthURLFetchList[args___, opts:OptionsPattern[]] :=
    Block[{authrules, authopt, auth, token, otheropts, res},
        authrules = FilterRules[{opts}, "OAuthAuthentication"];
        (
            authopt = "OAuthAuthentication" /. authrules;
            auth = getAuthFromOpts[authopt];
            (
                token = getTokenFromOpts[auth, authopt];
                If[token === $Canceled,
                    Return[$Canceled]
                ];
                (
                    otheropts = FilterRules[{opts}, Except["OAuthAuthentication"]];
                    res = iOAuthURLFetchList[auth, token, args, Sequence @@ otheropts];
                    res /; ListQ[res]
                ) /; TokenQ[token]
            ) /; AuthQ[auth]
        ) /; authrules =!= {}
    ]


(* Different ways to specify the OAuth configuration in options *)

getAuthFromOpts[auth_?AuthQ] := auth

getAuthFromOpts[opts_List] :=
    Block[{res},
        res = Cases[opts, _?AuthQ];
        First[res] /; (Length[res] === 1)
    ]

getAuthFromOpts[rules:{___?OptionQ}] :=
    Block[{authrules, res},
        authrules = FilterRules[rules, Except["Token"]];
        res = MakeOAuthAuthentication @@ authrules;
        res /; (res =!= $Failed)
    ]

getAuthFromOpts[_] := $Failed


(* Different ways to specify authorization tokens in options *)

getTokenFromOpts[_, opts_List] :=
    Block[{res},
        res = Cases[opts, _?TokenQ];
        First[res] /; (Length[res] === 1)
    ]

getTokenFromOpts[auth_, _] := OAuthFlow[auth]


(* ========================================================================= ==

                        iOAuthURLFetchList

    Comments: Each type of authorization should add downvalues for
        iOAuthURLFetchList.  The definition in this file just copies
        non-options appearing after the URL to the result.

    ToDo:

== ========================================================================= *)

(* iOAuthURLFetchList *)

iOAuthURLFetchList[auth_, tok_, url_, nonopts:Longest[Except[_?OptionQ]..], opts:OptionsPattern[]] :=
    Block[{call},
        call = iOAuthURLFetchList[auth, tok, url, opts];
        (
            Replace[call, {newurl_, newopts___} :> {newurl, nonopts, newopts}, {0}]
        ) /; ListQ[call]
    ]


(* ========================================================================= ==

                        Verifier dialog

    Comments:

    ToDo:

== ========================================================================= *)

(* Text I/O verifier dialog for standalone kernel *)

verifierDialogStandalone[serviceName_, authorizeURL_, prompt1_, prompt2_, verifierLabel_] :=
    Block[{res},
        Print[prompt1];
        Print[prompt2 <> " " <> serviceName <> ","];
        WriteString[$Output, authorizeURL, "\n"];
        res = InputString[ToUpperCase[StringTake[verifierLabel, 1]] <> StringDrop[verifierLabel, 1] <> ": "];
        res /; StringQ[res]
    ]

verifierDialogStandalone[___] := $Failed


(* GUI verifier dialog for session with FrontEnd *)

verifierDialogFE[serviceName_, authorizeURL_, prompt1_, prompt2_, verifierLabel_, title_, inputFieldOpts_] :=
    Block[{windowWidth = 400, labelCell, lblbb, hspace = 10, ifopts, cells, verifier = "", res},
        labelCell = TextCell[ToUpperCase[StringTake[verifierLabel, 1]] <> StringDrop[verifierLabel, 1] <> ":", NotebookDefault, "DialogStyle", "ControlStyle"];
        ifopts = Which[
            inputFieldOpts === Automatic,
                lblbb = Rasterize[labelCell, "BoundingBox"];
                {ImageSize -> windowWidth - lblbb[[1]] - 20 - hspace},
            ListQ[inputFieldOpts],
               Flatten[inputFieldOpts],
            True,
               $Failed
        ];
        (

            cells = {
                TextCell[prompt1, NotebookDefault, "DialogStyle", "ControlStyle"],
                TextCell[Row[{prompt2 <> " ", Hyperlink[serviceName, authorizeURL, BaseStyle -> "ControlStyle"], "."}], NotebookDefault, "DialogStyle", "ControlStyle"],
                ExpressionCell[
                    Grid[{{
                            labelCell,
                            Spacer[hspace],
                            InputField[Dynamic[verifier, (verifier = StringReplace[#, RegularExpression["\\s"] -> ""]) &], String,
                                ContinuousAction -> True, BoxID -> "VerifierDialogInput", Sequence @@ ifopts]}},
                        Spacings -> {0, 0}],
                    "DialogStyle", "ControlStyle"],
                ExpressionCell[ChoiceButtons[
                        {Automatic, Automatic},
                        {DialogReturn[verifier], DialogReturn[$Canceled]},
                        {{Enabled -> Dynamic[verifier =!= ""]}, {}}],
                     TextAlignment->Right]
            };
            res = DialogInput[DialogNotebook[cells],
                WindowTitle -> title, WindowSize -> {windowWidth, FitAll}, Evaluator -> CurrentValue["Evaluator"],
                LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False},
                Initialization :> (MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[
                    FE`BoxReference[#, {{"VerifierDialogInput"}}, FE`BoxOffset -> {FE`BoxChild[1]}, FE`SearchStart -> "StartFromBeginning"]]]&)
            ];
            res /; (StringQ[res] || res === $Canceled)

        ) /; ListQ[ifopts]
    ]

verifierDialogFE[___] := $Failed


(* verifierDialog *)
nonblockingVerifierDialog[auth_, authorizeURL_, redirectFun_] :=
    Block[{customDialog, serviceName, verifierLabel, inputFieldOpts, title, prompt1, prompt2, temptoken=Unique[]},
        customDialog = takeAuthenticationDialog[auth];
        serviceName = takeServiceName[auth];
        verifierLabel = takeVerifierLabel[auth];
        inputFieldOpts = takeInputFieldOpts[auth];
        Which[
            !TrueQ[HTTPClient`Private`$allowDialogs],
                $Failed,
            customDialog =!= Automatic,
                customDialog[{authorizeURL, redirectFun,temptoken}],
            True,
                $Failed
        ];
        HTTPClient`OAuth`Private`temporaryOAuthToken[temptoken]
]

verifierDialog[auth_, authorizeURL_] :=
    Block[{customDialog, serviceName, verifierLabel, inputFieldOpts, title, prompt1, prompt2, res},
        customDialog = takeAuthenticationDialog[auth];
        serviceName = takeServiceName[auth];
        verifierLabel = takeVerifierLabel[auth];
        inputFieldOpts = takeInputFieldOpts[auth];
        res = Which[
            !TrueQ[HTTPClient`Private`$allowDialogs],
                $Failed,
            customDialog =!= Automatic,
                customDialog[authorizeURL],
            hasFrontEnd[],
                title = serviceName <> " Authorization";
                prompt1 = Row[{Style["Mathematica", Italic], " needs authorization to access your data at " <> serviceName <> "."}];
                prompt2 = "Obtain a " <> verifierLabel <> " by authenticating at";
                verifierDialogFE[serviceName, authorizeURL, prompt1, prompt2, verifierLabel, title, inputFieldOpts],
            True,
                prompt1 = "Mathematica needs authorization to access your data at " <> serviceName <> ".";
                prompt2 = "Obtain a " <> verifierLabel <> " by authenticating at";
                verifierDialogStandalone[serviceName, authorizeURL, prompt1, prompt2, verifierLabel]
        ];
        res /; (StringQ[res] || res === $Canceled)
    ]

verifierDialog[___] := $Failed


(* ========================================================================= ==

                        Wolfram key dialog

    Comments:

    ToDo:

== ========================================================================= *)

(* Text I/O verifier dialog for standalone kernel *)

keyDialogStandalone[wolframServerName_, authorizeURL_, prompt1_, prompt2_, suggestion_String] :=
    Block[{res},
        Print[prompt1];
        Print[prompt2 <> " " <> wolframServerName <> ","];
        WriteString[$Output, authorizeURL, "\n"];
        res = If[suggestion === "",
            InputString["Key: "],
            Replace[InputString["Key [", suggestion, "]: "], "" -> suggestion]
        ];
        res /; StringQ[res]
    ]

keyDialogStandalone[___] := $Failed


(* GUI verifier dialog for session with FrontEnd *)

keyDialogFE[wolframServerName_, authorizeURL_, prompt1_, prompt2_, title_, suggestion_String] :=
    Block[{windowWidth = 400, hspace = 10, cells, key = suggestion, res},
        cells = {
            TextCell[prompt1, NotebookDefault, "DialogStyle", "ControlStyle"],
            TextCell[Row[{prompt2 <> " ", Hyperlink[wolframServerName, authorizeURL, BaseStyle -> "ControlStyle"], "."}], NotebookDefault, "DialogStyle", "ControlStyle"],
            ExpressionCell[
                Grid[{{
                        TextCell["Key:", NotebookDefault, "DialogStyle", "ControlStyle"],
                        Spacer[hspace],
                        InputField[Dynamic[key, (key = StringReplace[#, RegularExpression["\\s"] -> ""]) &], String,
                            ContinuousAction -> True, BoxID -> "VerifierDialogInput", FieldSize -> Medium]}},
                    Spacings -> {0, 0}],
                "DialogStyle", "ControlStyle"],
            ExpressionCell[ChoiceButtons[
                    {Automatic, Automatic},
                    {DialogReturn[key], DialogReturn[$Canceled]},
                    {{Enabled -> Dynamic[key =!= ""]}, {}}],
                 TextAlignment->Right]
        };
        res = DialogInput[DialogNotebook[cells],
            WindowTitle -> title, WindowSize -> {windowWidth, FitAll}, Evaluator -> CurrentValue["Evaluator"],
            LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False},
            Initialization :> (MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[
                FE`BoxReference[#, {{"VerifierDialogInput"}}, FE`BoxOffset -> {FE`BoxChild[1]}, FE`SearchStart -> "StartFromBeginning"]]]&)
        ];
        res /; (StringQ[res] || res === $Canceled)
    ]

keyDialogFE[___] := $Failed


(* keyDialog *)

keyDialog[auth_, serviceID_String, state_String] :=
    Block[{authorizeURL, customDialog, serviceName, wolframServerName, title, prompt1, prompt2, serverEmulator, suggestion, res},
        authorizeURL = "https://oauthtokens.wolfram.com/key?service=" <> serviceID <> "&state=" <> state;
        wolframServerName = "Wolfram Research";
        suggestion = "";
        customDialog = takeAuthenticationDialog[auth];
        serviceName = takeServiceName[auth];
        serverEmulator = takeServerEmulator[auth];
        If[serverEmulator =!= None,
            suggestion = serverEmulator[authorizeURL]
        ];
        res = Which[
            !TrueQ[HTTPClient`Private`$allowDialogs],
                $Failed,
            customDialog =!= Automatic,
                customDialog[authorizeURL],
            hasFrontEnd[],
                title = serviceName <> " Authorization";
                prompt1 = Row[{Style["Mathematica", Italic], " needs authorization to access your data at " <> serviceName <> "."}];
                prompt2 = "Obtain a key by authenticating at";
                keyDialogFE[wolframServerName, authorizeURL, prompt1, prompt2, title, suggestion],
            True,
                prompt1 = "Mathematica needs authorization to access your data at " <> serviceName <> ".";
                prompt2 = "Obtain a key by authenticating at";
                keyDialogStandalone[wolframServerName, authorizeURL, prompt1, prompt2, suggestion]
        ];
        res /; (StringQ[res] || res === $Canceled)
    ]

keyDialog[___] := $Failed


(* ========================================================================= ==

             Helper functions common to all OAuth implementations

    Comments:

    ToDo:

== ========================================================================= *)

(* flowGetRequestToken *)

flowGetRequestToken[auth_] :=
    Block[{res, issued = False},
        Check[
            res = GetRequestToken[auth]
            ,
            issued = True
        ];
        If[res === $Failed && !issued,
            Message[HTTPClient`OAuthAuthentication::reqfail, takeServiceName[auth]]
        ];
        res
    ]


(* flowExtractCode *)

flowExtractCode[data_String, extractor_] :=
    Block[{code},
        code = extractor[data];
        code /; StringQ[code]
    ]

flowExtractCode[expr___] :=
    (messageFlowExtractCode[expr]; $Failed)


(* :message: *)

messageFlowExtractCode[data:Except[_String], extractor_] :=
    Message[HTTPClient`OAuthAuthentication::bdver, data]


(* flowGetAccessToken *)

flowGetAccessToken[auth_, requestToken_, code_, state___] :=
    Block[{res, issued = False},
        Check[
            res = GetAccessToken[auth, requestToken, code, state]
            ,
            issued = True
        ];
        If[res === $Failed && !issued,
            Message[HTTPClient`OAuthAuthentication::accfail, takeServiceName[auth]]
        ];
        res
    ]


(* getParameters *)

getParameters[opts:{___?OptionQ}, auth_] :=
    Block[{params},
        params = Replace["Parameters" /. opts, "Parameters" -> {}, {0}];
        params /; MatchQ[params, {(_String -> _String)...}]
    ]

getParameters[expr___] :=
    (messageGetParameters[expr]; $Failed)


(* :message: *)

messageGetParameters[opts:{___?OptionQ}, auth_] :=
    Block[{params},
        params = Replace["Parameters" /. opts, "Parameters" -> {}, {0}];
        (
            Message[HTTPClient`OAuthSignURL::stropt, takeServiceName[auth], Cases[params, Except[_String -> _String]]]
        ) /; !MatchQ[params, {(_String -> _String)...}]
    ]


(* verifyPeerOptions *)

verifyPeerOptions[Automatic] := Sequence[]

verifyPeerOptions[v:(True|False)] := "VerifyPeer" -> v

verifyPeerOptions[___] := $Failed


(* ************************************************************************* *)
(*                                  OAuth 1.0a                               *)
(* ************************************************************************* *)

(* ========================================================================= ==

                        OAuth 1.0a token data

    Comments:

    ToDo:

== ========================================================================= *)

(* Token expression *)

ruleheads10 = {"TokenKey", "TokenSecret"};

Token10[rules:{___?OptionQ}] :=
    Block[{badrules},
        badrules = FilterRules[rules, Except[ruleheads10]];
        (
            Token10 @@ (ruleheads10 /. rules)
        ) /; badrules === {}
    ]

tokenKey[Token10[key_, secret_]] := key

tokenSecret[Token10[key_, secret_]] := secret

tokenSaneQ10[tok_] := MatchQ[tok, Token10[_String, _String]]

TokenToRules[tok_Token10] := Thread[ruleheads10 -> (List @@ tok)]

AuthQ[_OAuth10Parameters] := True

TokenQ[_Token10] := True

iTokenSaneQ[_OAuth10Parameters, tok_] := tokenSaneQ10[tok]


(* extractToken10 *)

extractToken10[data_String, extractor_, auth_OAuth10Parameters, Hold[msg_MessageName]] :=
    Block[{tokens},
        tokens = Token10[extractor[data]];
        tokens /; tokenSaneQ10[tokens]
    ]

extractToken10[expr___] :=
    (messageExtractToken10[expr]; $Failed)


(* :message: *)

messageExtractToken10[data_String, extractor_, auth_OAuth10Parameters, Hold[msg_MessageName]] :=
    Message[msg, takeServiceName[auth], data]


(* ========================================================================= ==

                        OAuth 1.0a API properties

    Comments:
        "ConsumerKey" is the consumer key.
        "ConsumerSecret" is the consumer secret.
        "RequestEndpoint" is the request token endpoint URL.
        "RequestVerb" is the request HTTP verb.
        "AuthorizeEndpoint" is the authorization endpoint.
        "CodeExtractor" is the code extractor.
        "AccessEndpoint" is the access token endpoint.
        "AccessVerb" is the access HTTP verb.
        "URLSignService" is the URL signing service.
        "SignatureMethod" is the signature method.
        "RequestTokenExtractor" is the request token extractor.
        "AccessTokenExtractor" is the access token extractor.
        "RedirectURI" is the OAuth redirect URI.
        "ScopeParameter" is the parameter name for specifying the scope part of the authorization URL.

    ToDo:

== ========================================================================= *)

(* OAuth version string *)

$authKey10 = "1.0a";
oAuthVersion[_OAuth10Parameters] := $authKey10
oAuthVersion[_Token10] := $authKey10


(* Defaults *)

defaultOAuth10 =
    Join[defaultAuthCommon,
    {
        "ConsumerKey"           -> None,
        "ConsumerSecret"        -> None,
        "RequestEndpoint"       -> None,
        "RequestVerb"           -> "GET",
        "AuthorizeEndpoint"     -> None,
        "CodeExtractor"         -> "RemoveWhitespace",
        "AccessEndpoint"        -> None,
        "AccessVerb"            -> "GET",
        "URLSignService"        -> "HMAC-Sha1",
        "SignatureMethod"       -> "HMAC",
        "RedirectURI"           -> "oob",
        "RequestTokenExtractor" -> "Text/1.0",
        "AccessTokenExtractor"  -> "Text/1.0",
        "ScopeParameter"        -> None
    }];


(* Sanity tests *)

sanityTests[auth_OAuth10Parameters] :=
    Block[{commontests, moretests},
        commontests = commonSanityTests[auth];
        moretests = {
            "<<NumberOfElements>>" ->
                Length[auth] == Length[defaultOAuth10],
            "AutomaticScope" ->
                takeAutomaticScope[auth] === None,
            "RequestEndpoint" ->
                nonEmptyStringQ[takeRequestEndpoint[auth]],
            "RequestVerb" ->
                nonEmptyStringQ[takeRequestVerb[auth]],
            "AuthorizeEndpoint" ->
                nonEmptyStringQ[takeAuthorizeEndpoint[auth]],
            "CodeExtractor" ->
                MemberQ[OAuthService["CodeExtractor"], takeCodeExtractor[auth]],
            "AccessEndpoint" ->
                nonEmptyStringQ[takeAccessEndpoint[auth]],
            "AccessVerb" ->
                nonEmptyStringQ[takeAccessVerb[auth]],
            "URLSignService" ->
                MemberQ[OAuthService["SignatureService"], takeURLSignService[auth]],
            "SignatureMethod" ->
                nonEmptyStringQ[takeSignatureMethod[auth]],
            "RedirectURI" ->
                nonEmptyStringQ[takeRedirectURI[auth]],
            "RequestTokenExtractor" ->
                MemberQ[OAuthService["TokenExtractor"], takeRequestTokenExtractor[auth]],
            "AccessTokenExtractor" ->
                MemberQ[OAuthService["TokenExtractor"], takeAccessTokenExtractor[auth]],
            "ScopeParameter" ->
                MatchQ[takeScopeParameter[auth], None | _String]
        };
        Join[commontests, moretests]
    ]

SaneQ[auth_OAuth10Parameters] :=
    Block[{tests},
        tests = sanityTests[auth];
        TrueQ[And @@ (Last /@ tests)]
    ]


(* Part extractors *)

takeServiceName[auth_OAuth10Parameters]           := auth[[1]]
takeAutomaticScope[auth_OAuth10Parameters]        := auth[[2]]
takeVerifyPeer[auth_OAuth10Parameters]            := auth[[3]]
takeVerifierLabel[auth_OAuth10Parameters]         := auth[[4]]
takeInputFieldOpts[auth_OAuth10Parameters]        := auth[[5]]
takeAuthenticationDialog[auth_OAuth10Parameters]  := auth[[6]]

takeConsumerKey[auth_OAuth10Parameters]           := auth[[7]]
takeConsumerSecret[auth_OAuth10Parameters]        := auth[[8]]
takeRequestEndpoint[auth_OAuth10Parameters]       := auth[[9]]
takeRequestVerb[auth_OAuth10Parameters]           := auth[[10]]
takeAuthorizeEndpoint[auth_OAuth10Parameters]     := auth[[11]]
takeCodeExtractor[auth_OAuth10Parameters]         := auth[[12]]
takeAccessEndpoint[auth_OAuth10Parameters]        := auth[[13]]
takeAccessVerb[auth_OAuth10Parameters]            := auth[[14]]
takeURLSignService[auth_OAuth10Parameters]        := auth[[15]]
takeSignatureMethod[auth_OAuth10Parameters]       := auth[[16]]
takeRedirectURI[auth_OAuth10Parameters]           := auth[[17]]
takeRequestTokenExtractor[auth_OAuth10Parameters] := auth[[18]]
takeAccessTokenExtractor[auth_OAuth10Parameters]  := auth[[19]]
takeScopeParameter[auth_OAuth10Parameters]        := auth[[20]]


(* ========================================================================= ==

                        iOAuthAuthentication for OAuth 1.0a

    Comments: Implements OAuthAuthentication constructor for this OAuth version.

    ToDo:

== ========================================================================= *)

(* iOAuthAuthentication *)

iOAuthAuthentication[$authKey10, opts:{___?OptionQ}] :=
    Block[{defaults, badopts, res, tests, sane},
        defaults = Replace[defaultOAuth10, Verbatim[Rule]["ServiceName", Automatic] -> Unevaluated["ServiceName" :> automaticName10[opts]], {1}];
        badopts = FilterRules[opts, Except[defaults]];
        If[badopts =!= {},
            Message[HTTPClient`OAuthAuthentication::prop, AuthType[$authKey10], badopts]
        ];
        (
            res = OAuth10Parameters @@ ((First /@ defaults) /. opts /. defaults);
            tests = sanityTests[res];
            sane = TrueQ[And @@ (Last /@ tests)];
            If[!sane && ListQ[tests],
                Message[HTTPClient`OAuthAuthentication::sane, AuthType[$authKey10], Cases[tests, _[t_, Except[True]] :> t]]
            ];
            res /; sane
        ) /; (badopts === {})
    ]

iOAuthAuthentication[$authKey10, ___] := $Failed

iOAuthAuthentication[Automatic, opts:{___?OptionQ}] :=
    Block[{defaults, badopts, res, tests, sane},
        defaults = Replace[defaultOAuth10, Verbatim[Rule]["ServiceName", Automatic] -> Unevaluated["ServiceName" :> automaticName10[opts]], {1}];
        badopts = FilterRules[opts, Except[defaults]];
        (
            res = OAuth10Parameters @@ ((First /@ defaults) /. opts /. defaults);
            tests = sanityTests[res];
            sane = TrueQ[And @@ (Last /@ tests)];
            res /; sane
        ) /; (badopts === {})
    ]


(* Automatic service name *)

automaticName10[opts_] :=
    Block[{rules, authendp, res},
        rules = FilterRules[opts, "AuthorizeEndpoint"];
        (
            authendp = "AuthorizeEndpoint" /. rules;
            (
                res = StringCases[authendp, ("http"|"https")~~"://"~~name:RegularExpression["[^/]+"] :> name];
                First[res] /; Length[res] === 1
            ) /; StringQ[authendp]
        ) /; (rules =!= {})
    ]

automaticName10[___] := $Failed


(* ========================================================================= ==

                        Get request token for OAuth 1.0a

    Comments:

    ToDo:

== ========================================================================= *)

(* GetRequestToken *)

GetRequestToken[auth_OAuth10Parameters] :=
    Block[{res},
        res = getRequestToken10[auth];
        res /; (TokenQ[res] || res === $Failed)
    ]


(* getRequestToken10 *)

getRequestToken10[auth_OAuth10Parameters] :=
    Block[{requestEndpoint, requestVerb, consumerKey, consumerSecret, redirectURI,
            signatureMethod, urlSignService, tokenExtractor, verifyPeer,
            unsignedURL, signedURL, tokenBody, tokens},
        requestEndpoint = takeRequestEndpoint[auth];
        requestVerb = takeRequestVerb[auth];
        consumerKey = takeConsumerKey[auth];
        consumerSecret = takeConsumerSecret[auth];
        redirectURI = takeRedirectURI[auth];
        tokenExtractor = OAuthService["TokenExtractor", takeRequestTokenExtractor[auth]];
        signatureMethod = takeSignatureMethod[auth];
        urlSignService = OAuthService["SignatureService", takeURLSignService[auth]];
        verifyPeer = verifyPeerOptions[takeVerifyPeer[auth]];
        (

            unsignedURL = StringJoin[requestEndpoint, "?", urlEncode[{"oauth_callback" -> redirectURI}]];
            signedURL = urlSignService[unsignedURL, signatureMethod, requestVerb, consumerKey, consumerSecret, "", ""];

            tokenBody = URLFetch[signedURL, "Method" -> requestVerb, verifyPeer];
            (
                tokens = extractToken10[tokenBody, tokenExtractor, auth, Hold[HTTPClient`OAuthAuthentication::reqbdy]];
                tokens /; tokenSaneQ10[tokens]
            ) /; StringQ[tokenBody]

        ) /; (tokenExtractor =!= $Failed && urlSignService =!= $Failed && verifyPeer =!= $Failed)
    ]

getRequestToken10[expr___] := $Failed


(* ========================================================================= ==

                        Assemble authorization URL for OAuth 1.0a

    Comments: Expects None for <scope> in OAuth 1.0 flow, other uses are
        non-standard extensions.

    ToDo:

== ========================================================================= *)

(* AssembleAuthorizationURL *)

AssembleAuthorizationURL[auth_OAuth10Parameters, args__] :=
    Block[{res},
        res = assembleAuthorizationURL10[auth, args];
        res /; (StringQ[res] || res === $Failed)
    ]


(* assembleAuthorizationURL10 *)

assembleAuthorizationURL10[auth_OAuth10Parameters, requestToken_Token10, scope_:None] /; (scope === None) :=
    Block[{authorizeEndpoint, keyStr},
        authorizeEndpoint = takeAuthorizeEndpoint[auth];
        keyStr = tokenKey[requestToken];

        StringJoin[authorizeEndpoint, "?", "oauth_token=", keyStr]
    ]

assembleAuthorizationURL10[auth_OAuth10Parameters, requestToken_Token10, scope:{___String}] :=
    Block[{authorizeEndpoint, scopeParameter, keyStr, res},
        authorizeEndpoint = takeAuthorizeEndpoint[auth];
        scopeParameter = takeScopeParameter[auth];
        keyStr = tokenKey[requestToken];

        (
            res = StringJoin[
                authorizeEndpoint, "?",
                "oauth_token=", keyStr,
                "&", scopeParameter, "=", StringJoin @@ Riffle[scope, "%2C"]
            ];
            res /; StringQ[res]
        ) /; StringQ[scopeParameter]
    ]

assembleAuthorizationURL10[expr___] :=
    (messageAssembleAuthorizationURL10[expr]; $Failed)


(* :message: *)

messageAssembleAuthorizationURL10[auth_OAuth10Parameters, None, _] :=
    Message[AssembleAuthorizationURL::rtnone, AuthType[auth], takeServiceName[auth]]

messageAssembleAuthorizationURL10[auth_OAuth10Parameters, tok:Except[_Token10], _] :=
    Message[HTTPClient`OAuthAuthentication::bdrt, AuthType[auth], takeServiceName[auth], tok]

messageAssembleAuthorizationURL10[auth_OAuth10Parameters, _, sc:Except[None | {___String}]] :=
    Message[HTTPClient`OAuthAuthentication::scnorlst, AuthType[auth], takeServiceName[auth], sc]

messageAssembleAuthorizationURL10[auth_OAuth10Parameters, _, scope:{___String}] /; (takeScopeParameter[auth] === None) :=
    Message[HTTPClient`OAuthAuthentication::scpar, takeServiceName[auth], ScopeParameter]


(* ========================================================================= ==

                        Get access token for OAuth 1.0a

    Comments:

    ToDo:

== ========================================================================= *)

(* GetAccessToken *)

GetAccessToken[auth_OAuth10Parameters, requestToken_, verifier_] :=
    Block[{res},
        res = getAccessToken10[auth, requestToken, verifier];
        res /; (TokenQ[res] || res === $Failed)
    ]


(* getAccessToken10 *)

getAccessToken10[auth_OAuth10Parameters, requestToken_Token10, verifier_] :=
    Block[{consumerKey, consumerSecret, accessEndpoint, accessVerb,
            signatureMethod, urlSignService, tokenExtractor, verifyPeer,
            keyStr, secretStr, unsignedURL, signedURL, tokenBody, accessToken},
        consumerKey = takeConsumerKey[auth];
        consumerSecret = takeConsumerSecret[auth];
        accessEndpoint = takeAccessEndpoint[auth];
        accessVerb = takeAccessVerb[auth];
        tokenExtractor = OAuthService["TokenExtractor", takeAccessTokenExtractor[auth]];
        signatureMethod = takeSignatureMethod[auth];
        urlSignService = OAuthService["SignatureService", takeURLSignService[auth]];
        keyStr = tokenKey[requestToken];
        secretStr = tokenSecret[requestToken];
        verifyPeer = verifyPeerOptions[takeVerifyPeer[auth]];
        (

            unsignedURL = StringJoin[accessEndpoint, "?", urlEncode[{"oauth_verifier" -> verifier}]];
            signedURL = urlSignService[unsignedURL, signatureMethod, accessVerb, consumerKey, consumerSecret, keyStr, secretStr];

            tokenBody = URLFetch[signedURL, "Method" -> accessVerb, verifyPeer];
            (
                accessToken = extractToken10[tokenBody, tokenExtractor, auth, Hold[HTTPClient`OAuthAuthentication::accbdy]];
                accessToken /; tokenSaneQ10[accessToken]
            ) /; StringQ[tokenBody]

        ) /; (tokenExtractor =!= $Failed && urlSignService =!= $Failed && verifyPeer =!= $Failed)
    ]

getAccessToken10[expr___] :=
    (messageGetAccessToken10[expr]; $Failed)


(* :message: *)

messageGetAccessToken10[auth_OAuth10Parameters, None, _] :=
    Message[HTTPClient`OAuthAuthentication::rtnone, AuthType[auth], takeServiceName[auth]]

messageGetAccessToken10[auth_OAuth10Parameters, tok:Except[_Token10], _] :=
    Message[GetAccessToken::bdrt, AuthType[auth], takeServiceName[auth], tok]

messageGetAccessToken10[auth_OAuth10Parameters, _, ver:Except[_String]] :=
    Message[HTTPClient`OAuthAuthentication::bdver, ver]


(* ========================================================================= ==

                        Prepare OAuth URLFetch for OAuth 1.0a

    Comments:

    ToDo:

== ========================================================================= *)

(* iOAuthURLFetchList *)

iOAuthURLFetchList[auth_OAuth10Parameters, tokens_, url_, opts:OptionsPattern[]] :=
    Block[{res},
        res = oAuthURLFetchList10[auth, tokens, url, opts];
        res /; (ListQ[res] || res === $Failed)
    ]


(* oAuthURLFetchList10 *)

oAuthURLFetchList10[auth_OAuth10Parameters, tokens_Token10, url_, opts:OptionsPattern[]] :=
    Module[{consumerKey, consumerSecret, signatureMethod, urlSignService, verb,
            tokKey, tokSecret, fparams, params, unsignedURL, signedURL},
        consumerKey = takeConsumerKey[auth];
        consumerSecret = takeConsumerSecret[auth];
        signatureMethod = takeSignatureMethod[auth];
        urlSignService = OAuthService["SignatureService", takeURLSignService[auth]];
        tokKey = tokenKey[tokens];
        tokSecret = tokenSecret[tokens];
        (

            fparams = FilterRules[{opts}, Except["Parameters"]];
            verb = "Method" /. fparams /. {"Method" -> "GET"};
            params = getParameters[{opts}, auth];
            (
                unsignedURL = StringJoin[
                    url,
                    If[params === {},
                        "",
                        "?" <> urlEncode[params]
                    ]
                ];
                signedURL = urlSignService[unsignedURL, signatureMethod, verb, consumerKey, consumerSecret, tokKey, tokSecret];

                {signedURL, Sequence@@fparams} /; StringQ[signedURL]
            ) /; ListQ[params]

        ) /; (urlSignService =!= $Failed)
    ]

oAuthURLFetchList10[expr___] := $Failed


(* ========================================================================= ==

                        OAuth flow for OAuth 1.0a

    Comments: Scope must be None for true OAuth 1.0a.

    ToDo:

== ========================================================================= *)

(* iOAuthFlow *)

iOAuthFlow[auth_OAuth10Parameters, scope_:None] :=
    Block[{res},
        res = oAuthFlow10[auth, scope];
        res /; (res =!= $Failed)
    ]


(* oAuthFlow10 *)

oAuthFlow10[auth_OAuth10Parameters, scope_] :=
    Module[{requestToken, authorizeURL, res},
        requestToken = flowGetRequestToken[auth];
        (
            authorizeURL = AssembleAuthorizationURL[auth, requestToken, scope];
            With[{reqtoken=requestToken},
                res=nonblockingVerifierDialog[auth, authorizeURL,
                	Block[{$BlockingDialog=False},
                		iOAuthFlow10[auth, scope, reqtoken, #]
                	]&];
                res
            ]/; StringQ[authorizeURL]
        ) /; tokenSaneQ10[requestToken]
    ]/;!$BlockingDialog
    
oAuthFlow10[auth_OAuth10Parameters, scope_] :=
    Module[{requestToken, authorizeURL, input},
        requestToken = flowGetRequestToken[auth];
        (
            authorizeURL = AssembleAuthorizationURL[auth, requestToken, scope];
            (
                input = verifierDialog[auth, authorizeURL];
                iOAuthFlow10[auth, scope, requestToken, input]
            ) /; StringQ[authorizeURL]
        ) /; tokenSaneQ10[requestToken]
    ]

    
oAuthFlow10[expr___] := $Failed


(* iOAuthFlow10 *)

iOAuthFlow10[auth_, scope_, requestToken_, $Canceled] := $Canceled

iOAuthFlow10[auth_OAuth10Parameters, scope_, requestToken_, input_String] :=
    Module[{codeExtractor, accessToken, code},
        codeExtractor = OAuthService["CodeExtractor", takeCodeExtractor[auth]];
        (

            code = flowExtractCode[input, codeExtractor];
            (
                accessToken = flowGetAccessToken[auth, requestToken, code];
                accessToken /; tokenSaneQ10[accessToken]
            ) /; StringQ[code]

        ) /; (codeExtractor =!= $Failed)
    ]

iOAuthFlow10[expr___] := $Failed


(* ************************************************************************* *)
(*                                  OAuth 2.0                                *)
(* ************************************************************************* *)

(* ========================================================================= ==

                        OAuth 2.0 token data

    Comments:

    ToDo:

== ========================================================================= *)

(* Token expression *)

ruleheads20 = {"Token"};

Token20[rules:{___?OptionQ}] :=
    Block[{badrules},
        badrules = FilterRules[rules, Except[ruleheads20]];
        (
            Token20 @@ (ruleheads20 /. rules)
        ) /; badrules === {}
    ]

TokenToRules[tok_Token20] := Thread[ruleheads20 -> (List @@ tok)]

tokenToken[Token20[token_]] := token

tokenSaneQ20[tok_] := MatchQ[tok, Token20[_String]]

AuthQ[_OAuth20Parameters] := True

TokenQ[_Token20] := True

iTokenSaneQ[_OAuth20Parameters, tok_] := tokenSaneQ20[tok]


(* extractToken20 *)

extractToken20[data_String, extractor_, auth_OAuth20Parameters, Hold[msg_MessageName]] :=
    Block[{rules, tokens, state},
        rules = extractor[data];
        (
            tokens = Token20[FilterRules[rules, Except["State"]]];
            rules = FilterRules[rules, "State"];
            state = If[rules === {}, None, "State" /. rules];
            {tokens, state} /; (tokenSaneQ20[tokens] && MatchQ[state, _String | None])
         ) /; MatchQ[rules, {___Rule}]
    ]

extractToken20[expr___] :=
    (messageExtractToken20[expr]; $Failed)


(* :message: *)

messageExtractToken20[data_String, extractor_, auth_OAuth20Parameters, Hold[msg_MessageName]] :=
    Message[msg, takeServiceName[auth], data]


(* ========================================================================= ==

                        OAuth 2.0 API properties

    Comments:
        "ConsumerKey" is the consumer key.
        "ConsumerSecret" is the consumer secret.
        "AuthorizeEndpoint" is the authorization endpoint.
        "CodeExtractor" is the code extractor.
        "AccessEndpoint" is the access token endpoint.
        "AccessVerb" is the access HTTP verb.
        "RedirectURI" is the OAuth redirect URI.
        "AccessTokenExtractor" is the access token extractor.
        "ScopeDomain" is the complete set of known permissions.

    ToDo:

== ========================================================================= *)

(* OAuth version string *)

$authKey20 = "2.0";
oAuthVersion[_OAuth20Parameters] := $authKey20
oAuthVersion[_Token20] := $authKey20


(* Defaults *)

defaultOAuth20 =
    Join[
        Replace[defaultAuthCommon,
            {Verbatim[Rule][p:"AutomaticScope", _] :> (p -> {})},
            {1}],
        {
            "ConsumerKey"          -> None,
            "ConsumerSecret"       -> None,
            "AuthorizeEndpoint"    -> None,
            "CodeExtractor"        -> "RemoveWhitespace",
            "AccessEndpoint"       -> None,
            "AccessVerb"           -> "POST",
            "RedirectURI"          -> None,
            "AccessTokenExtractor" -> "JSON/2.0",
            "ScopeDomain"          -> None
        }
    ];


(* Sanity tests *)

sanityTests[auth_OAuth20Parameters] :=
    Block[{commontests, moretests},
        commontests = commonSanityTests[auth];
        moretests = {
            "<<NumberOfElements>>" ->
                Length[auth] == Length[defaultOAuth20],
            "AuthorizeEndpoint" ->
                nonEmptyStringQ[takeAuthorizeEndpoint[auth]],
            "CodeExtractor" ->
                MemberQ[OAuthService["CodeExtractor"], takeCodeExtractor[auth]],
            "AccessEndpoint" ->
                nonEmptyStringQ[takeAccessEndpoint[auth]],
            "AccessVerb" ->
                nonEmptyStringQ[takeAccessVerb[auth]],
            "RedirectURI" ->
                nonEmptyStringQ[takeRedirectURI[auth]],
            "AccessTokenExtractor" ->
                MemberQ[OAuthService["TokenExtractor"], takeAccessTokenExtractor[auth]],
            "AutomaticScope" ->
                MatchQ[takeAutomaticScope[auth], {_String...}],
            "ScopeDomain" ->
                With[{perm = takeScopeDomain[auth]},
                    perm === None ||
                    MatchQ[perm, {_String...}]
                ]
        };
        Join[commontests, moretests]
    ]

OAuth20Parameters /: SaneQ[auth_OAuth20Parameters] :=
    Block[{tests},
        tests = sanityTests[auth];
        TrueQ[And @@ (Last /@ tests)]
    ]


(* Part extractors *)

takeServiceName[auth_OAuth20Parameters]          := auth[[1]]
takeAutomaticScope[auth_OAuth20Parameters]       := auth[[2]]
takeVerifyPeer[auth_OAuth20Parameters]           := auth[[3]]
takeVerifierLabel[auth_OAuth20Parameters]        := auth[[4]]
takeInputFieldOpts[auth_OAuth20Parameters]       := auth[[5]]
takeAuthenticationDialog[auth_OAuth20Parameters] := auth[[6]]

takeConsumerKey[auth_OAuth20Parameters]          := auth[[7]]
takeConsumerSecret[auth_OAuth20Parameters]       := auth[[8]]
takeAuthorizeEndpoint[auth_OAuth20Parameters]    := auth[[9]]
takeCodeExtractor[auth_OAuth20Parameters]        := auth[[10]]
takeAccessEndpoint[auth_OAuth20Parameters]       := auth[[11]]
takeAccessVerb[auth_OAuth20Parameters]           := auth[[12]]
takeRedirectURI[auth_OAuth20Parameters]          := auth[[13]]
takeAccessTokenExtractor[auth_OAuth20Parameters] := auth[[14]]
takeScopeDomain[auth_OAuth20Parameters]          := auth[[15]]


(* ========================================================================= ==

                        iOAuthAuthentication for OAuth 2.0

    Comments: Implements OAuthAuthentication constructor for this OAuth version.

    ToDo:

== ========================================================================= *)

(* iOAuthAuthentication *)

iOAuthAuthentication[$authKey20, opts:{___?OptionQ}] :=
    Block[{defaults, badopts, res, tests, sane},
        defaults = Replace[defaultOAuth20, Verbatim[Rule]["ServiceName", Automatic] -> Unevaluated["ServiceName" :> automaticName20[opts]], {1}];
        badopts = FilterRules[opts, Except[defaults]];
        If[badopts =!= {},
            Message[HTTPClient`OAuthAuthentication::prop, AuthType[$authKey20], badopts]
        ];
        (
            res = OAuth20Parameters @@ ((First /@ defaults) /. opts /. defaults);
            tests = sanityTests[res];
            sane = TrueQ[And @@ (Last /@ tests)];
            If[!sane && ListQ[tests],
                Message[HTTPClient`OAuthAuthentication::sane, AuthType[$authKey20], Cases[tests, _[t_, Except[True]] :> t]]
            ];
            res /; sane
        ) /; (badopts === {})
    ]

iOAuthAuthentication[$authKey20, ___] := $Failed

iOAuthAuthentication[Automatic, opts:{___?OptionQ}] :=
    Block[{defaults, badopts, res, tests, sane},
        defaults = Replace[defaultOAuth20, Verbatim[Rule]["ServiceName", Automatic] -> Unevaluated["ServiceName" :> automaticName20[opts]], {1}];
        badopts = FilterRules[opts, Except[defaults]];
        (
            res = OAuth20Parameters @@ ((First /@ defaults) /. opts /. defaults);
            tests = sanityTests[res];
            sane = TrueQ[And @@ (Last /@ tests)];
            res /; sane
        ) /; (badopts === {})
    ]


(* Automatic service name *)

automaticName20[opts_] :=
    Block[{rules, authendp, res},
        rules = FilterRules[opts, "AuthorizeEndpoint"];
        (
            authendp = "AuthorizeEndpoint" /. rules;
            (
                res = StringCases[authendp, ("http"|"https")~~"://"~~name:RegularExpression["[^/]+"] :> name];
                First[res] /; (Length[res] === 1)
            ) /; StringQ[authendp]
        ) /; (rules =!= {})
    ]

automaticName20[___] := $Failed


(* ========================================================================= ==

                        Get request token for OAuth 2.0

    Comments: Not used in OAuth 2.0 flow.

    ToDo:

== ========================================================================= *)

(* GetRequestToken *)

GetRequestToken[auth_OAuth20Parameters] :=
    Block[{res},
        res = getRequestToken20[auth];
        res /; (res === None)
    ]

getRequestToken20[expr___] :=
    (messageGetRequestToken20[expr]; None)


(* :message: *)

messageGetRequestToken20[auth_OAuth20Parameters] :=
    Message[HTTPClient`OAuthAuthentication::illreq, takeServiceName[auth], AuthType[auth]]


(* ========================================================================= ==

                        Assemble authorization URL for OAuth 2.0

    Comments: Expects None for <requestToken> in OAuth 2.0 flow.

    ToDo:

== ========================================================================= *)

(* AssembleAuthorizationURL *)

AssembleAuthorizationURL[auth_OAuth20Parameters, requestToken_, scope_, state_:None] :=
    Block[{res},
        res = assembleAuthorizationURL20[auth, requestToken, scope, state];
        res /; (StringQ[res] || res === $Failed)
    ]


(* assembleAuthorizationURL20 *)

assembleAuthorizationURL20[auth_OAuth20Parameters, requestToken:None, scope:{___String}, state:None] :=
    Block[{authorizeEndpoint, consumerKey, redirectURI, res},
        authorizeEndpoint = takeAuthorizeEndpoint[auth];
        consumerKey = takeConsumerKey[auth];
        redirectURI = takeRedirectURI[auth];

        res = StringJoin[
            authorizeEndpoint,
            "?",
            "client_id=", consumerKey,
            "&redirect_uri=", redirectURI,
            "&scope=", StringJoin @@ Riffle[scope, "%2C"],
            "&response_type=code"
        ];

        res /; StringQ[res]
    ]

assembleAuthorizationURL20[auth_OAuth20Parameters, requestToken:None, scope:{___String}, state_String /; StringMatchQ[state, RegularExpression["\\w+"]]] :=
    Block[{authorizeEndpoint, consumerKey, redirectURI, res},
        authorizeEndpoint = takeAuthorizeEndpoint[auth];
        consumerKey = takeConsumerKey[auth];
        redirectURI = takeRedirectURI[auth];

        res = StringJoin[
            authorizeEndpoint,
            "?",
            "client_id=", consumerKey,
            "&redirect_uri=", redirectURI,
            "&scope=", StringJoin @@ Riffle[scope, "%2C"],
            "&state=", state,
            "&response_type=code"
        ];

        res /; StringQ[res]
    ]

assembleAuthorizationURL20[expr___] :=
    (messageAssembleAuthorizationURL20[expr]; $Failed)


(* :message: *)

messageAssembleAuthorizationURL20[auth_OAuth20Parameters, tok:Except[None], _, _] :=
    Message[AssembleAuthorizationURL::rtenone, AuthType[auth], takeServiceName[auth], tok]

messageAssembleAuthorizationURL20[auth_OAuth20Parameters, _, sc:Except[{___String}], _] :=
    Message[HTTPClient`OAuthAuthentication::sclst, AuthType[auth], takeServiceName[auth], sc]

messageAssembleAuthorizationURL20[auth_OAuth20Parameters, _, _, state:Except[_String | None]] :=
    Message[HTTPClient`OAuthAuthentication::stfmt, AuthType[auth], takeServiceName[auth], state]

messageAssembleAuthorizationURL20[auth_OAuth20Parameters, _, _, state_String /; !StringMatchQ[state, RegularExpression["\\w+"]]] :=
    Message[HTTPClient`OAuthAuthentication::stfmt, AuthType[auth], takeServiceName[auth], state]


(* ========================================================================= ==

                        Get access token for OAuth 2.0

    Comments: Expects None for <requestToken> in OAuth 2.0 flow.

    ToDo:

== ========================================================================= *)

(* GetAccessToken *)

GetAccessToken[auth_OAuth20Parameters, requestToken_, verifier_, state_:None] :=
    Block[{res},
        res = getAccessToken20[auth, requestToken, verifier, state];
        res /; (TokenQ[res] || res === $Failed)
    ]


(* getAccessToken20 *)

getAccessToken20[auth_OAuth20Parameters, requestToken:None, verifier_, state_] :=
    Block[{consumerKey, consumerSecret, accessEndpoint, accessVerb, redirectURI,
            tokenExtractor, verifyPeer, tokenBody, tmp, accessToken, state2},
        consumerKey = takeConsumerKey[auth];
        consumerSecret = takeConsumerSecret[auth];
        accessEndpoint = takeAccessEndpoint[auth];
        accessVerb = takeAccessVerb[auth];
        redirectURI = takeRedirectURI[auth];
        tokenExtractor = OAuthService["TokenExtractor", takeAccessTokenExtractor[auth]];
        verifyPeer = verifyPeerOptions[takeVerifyPeer[auth]];
        (

            tokenBody = URLFetch[accessEndpoint,
                "Method" -> accessVerb,
                "Parameters" -> {
                    "client_id" -> consumerKey,
                    "redirect_uri" -> redirectURI,
                    "client_secret" -> consumerSecret,
                    "code" -> verifier,
                    "grant_type" -> "authorization_code"
                },
                verifyPeer
            ];
            (
                tmp = extractToken20[tokenBody, tokenExtractor, auth, Hold[HTTPClient`OAuthAuthentication::accbdy]];
                (
                    {accessToken, state2} = tmp;
                    iGetAccessToken20[auth, accessToken, state, state2]
                ) /; MatchQ[tmp, {_, _}]
            ) /; StringQ[tokenBody]

        ) /; (tokenExtractor =!= $Failed && verifyPeer =!= $Failed)
    ]

getAccessToken20[expr___] :=
    (messageGetAccessToken20[expr]; $Failed)


(* :message: *)

messageGetAccessToken20[auth_OAuth20Parameters, tok:Except[None], _, _] :=
    Message[HTTPClient`OAuthAuthentication::rtenone, AuthType[auth], takeServiceName[auth], tok]

messageGetAccessToken20[auth_OAuth20Parameters, _, ver:Except[_String], _] :=
    Message[HTTPClient`OAuthAuthentication::bdver, ver]


(* iGetAccessToken20 *)

iGetAccessToken20[auth_OAuth20Parameters, accessToken_, state_, state2:None] :=
    Block[{},
        accessToken /; tokenSaneQ20[accessToken]
    ]

iGetAccessToken20[auth_OAuth20Parameters, accessToken_, state_String, state2_String] :=
    Block[{},
        (
            accessToken /; tokenSaneQ20[accessToken]
        ) /; state === state2
    ]

iGetAccessToken20[expr___] :=
    (messageIGetAccessToken20[expr]; $Failed)


(* :message: *)

messageIGetAccessToken20[auth_OAuth20Parameters, tok_, state_String, state2_String] /;
        state =!= state2 :=
    Message[HTTPClient`OAuthAuthentication::bdstate, AuthType[auth], takeServiceName[auth], takeVerifierLabel[auth]]


(* ========================================================================= ==

                        Prepare OAuth URLFetch for OAuth 2.0

    Comments:

    ToDo:

== ========================================================================= *)

(* iOAuthURLFetchList *)

iOAuthURLFetchList[auth_OAuth20Parameters, tokens_, url_, opts:OptionsPattern[]] :=
    Block[{res},
        res = oAuthURLFetchList20[auth, tokens, url, opts];
        res /; (ListQ[res] || res === $Failed)
    ]


(* oAuthURLFetchList20 *)

oAuthURLFetchList20[auth_OAuth20Parameters, tokens_Token20, url_, opts:OptionsPattern[]] :=
    Module[{tokenStr, fparams, params},
        tokenStr = tokenToken[tokens];

        fparams = FilterRules[{opts}, Except["Parameters"]];
        params = getParameters[{opts}, auth];
        (
            AppendTo[params, "access_token" -> tokenStr];

            {url, Sequence@@fparams, "Parameters" -> params}
        ) /; ListQ[params]
    ]

oAuthURLFetchList20[expr___] := $Failed


(* ========================================================================= ==

                        OAuth flow for OAuth 2.0

    Comments:

    ToDo:

== ========================================================================= *)

(* iOAuthFlow *)

iOAuthFlow[auth_OAuth20Parameters, scope_:Automatic] :=
    Block[{res},
        res = oAuthFlow20[auth, scope];
        res /; (res =!= $Failed)
    ]


(* oAuthFlow20 *)


oAuthFlow20[auth_OAuth20Parameters, scope_] :=
    Module[{scope2, requestToken, authorizeURL, state, res},
        requestToken = None;
        state = BlockRandom[
            SeedRandom[];
            IntegerString[
                Hash[{RandomInteger[{0, 2147483647}, {6}], $SessionID}, "SHA"],
                16, 40
            ]
        ];

        scope2 = Replace[scope, Automatic :> takeAutomaticScope[auth], {0}];

        authorizeURL = AssembleAuthorizationURL[auth, requestToken, scope2, state];   
        If[!StringQ[authorizeURL],Return[$Failed]];
        res= With[{st=state,gatDefinition=DownValues[HTTPClient`OAuth`Private`getAccessToken20]},
                nonblockingVerifierDialog[auth, authorizeURL,
                    (Block[{HTTPClient`OAuth`Private`getAccessToken20,$BlockingDialog=False},
                        DownValues[HTTPClient`OAuth`Private`getAccessToken20]=gatDefinition;
                        iOAuthFlow20[auth, scope, None, st, #]
                    ]&) 
                ]  
        ];
        res     
    ]/;!$BlockingDialog
    
oAuthFlow20[auth_OAuth20Parameters, scope_] :=
    Module[{scope2, requestToken, authorizeURL, state, input},
        requestToken = None;
        state = BlockRandom[
            SeedRandom[];
            IntegerString[
                Hash[{RandomInteger[{0, 2147483647}, {6}], $SessionID}, "SHA"],
                16, 40
            ]
        ];

        scope2 = Replace[scope, Automatic :> takeAutomaticScope[auth], {0}];

        authorizeURL = AssembleAuthorizationURL[auth, requestToken, scope2, state];
        (
            input = verifierDialog[auth, authorizeURL];
            iOAuthFlow20[auth, scope, requestToken, state, input]
        ) /; StringQ[authorizeURL]
    ]
oAuthFlow20[expr___] := $Failed


(* iOAuthFlow20 *)

iOAuthFlow20[auth_, scope_, requestToken_, state_, $Canceled] := $Canceled

iOAuthFlow20[auth_OAuth20Parameters, scope_, requestToken_, state_, input_String] :=
    Module[{codeExtractor, accessToken, code},
        codeExtractor = OAuthService["CodeExtractor", takeCodeExtractor[auth]];
        (

            code = flowExtractCode[input, codeExtractor];
            (
                accessToken = flowGetAccessToken[auth, requestToken, code, state];
                accessToken /; tokenSaneQ20[accessToken]
            ) /; StringQ[code]

        ) /; (codeExtractor =!= $Failed)
    ]

iOAuthFlow20[expr___] := $Failed


(* ************************************************************************* *)
(*                       Secretless OAuth 1.0a/2.0                           *)
(* ************************************************************************* *)

(* ========================================================================= ==

                        Secretless OAuth token data

    Comments:
       Uses Token10 and Token20 expressions for tokens.

    ToDo:

== ========================================================================= *)

(* Parameter expressions *)

AuthQ[_OAuthSLParameters] := True

iTokenSaneQ[_OAuthSLParameters, tok_] := TokenQ[tok]


(* extractTokenSL *)

extractTokenSL[data_String, extractor_, auth_OAuthSLParameters, Hold[msg_MessageName]] :=
    Block[{rules, tokens, state},
        rules = extractor[data];
        (
            tokens = iExtractTokenSL[FilterRules[rules, Except["State"]], auth, Hold[msg]];
            rules = FilterRules[rules, "State"];
            state = If[rules === {}, None, "State" /. rules];
            {tokens, state} /; (TokenQ[tokens] && MatchQ[state, _String | None])
         ) /; MatchQ[rules, {___Rule}]
    ]

extractTokenSL[expr___] :=
    (messageExtractTokenSL[expr]; $Failed)


(* :message: *)

messageExtractTokenSL[data_, extractor_, auth_OAuthSLParameters, Hold[msg_MessageName]] :=
    Message[msg, takeServiceName[auth], data]


(* iExtractTokenSL *)

iExtractTokenSL[rules:{___Rule}, auth_OAuthSLParameters, Hold[msg_MessageName]] :=
    Block[{tokens},
        tokens = Token10[rules];
        tokens /; tokenSaneQ10[tokens]
    ]

iExtractTokenSL[rules:{___Rule}, auth_OAuthSLParameters, Hold[msg_MessageName]] :=
    Block[{tokens},
        tokens = Token20[rules];
        tokens /; tokenSaneQ20[tokens]
    ]


(* ========================================================================= ==

                   Secretless OAuth API properties

    Comments:
        The common property "ServiceName" will be used as the unique identifier
        that tells the Wolfram server what social media service to connect to.

    ToDo:

== ========================================================================= *)

(* OAuth version string *)

$authKeySL = "secretless";
oAuthVersion[_OAuthSLParameters] := $authKeySL
oAuthVersion[_TokenSL] := $authKeySL


(* Defaults *)

defaultOAuthSL =
    Join[defaultAuthCommon,
    {
        "ServerEmulator"        -> None
    }];


(* Sanity tests *)

sanityTests[auth_OAuthSLParameters] :=
    Block[{commontests, moretests},
        commontests = commonSanityTests[auth];
        moretests = {
            "<<NumberOfElements>>" ->
                Length[auth] == Length[defaultOAuthSL],
            "AutomaticScope" ->
                takeAutomaticScope[auth] === None,
            "ServiceName" ->
                With[{name = takeServiceName[auth]},
                    StringQ[name] && StringMatchQ[name, RegularExpression["[\\w\\_\\.-]+"]]
                ]
        };
        Join[commontests, moretests]
    ]

SaneQ[auth_OAuthSLParameters] :=
    Block[{tests},
        tests = sanityTests[auth];
        TrueQ[And @@ (Last /@ tests)]
    ]


(* Part extractors *)

takeServiceName[auth_OAuthSLParameters]           := auth[[1]]
takeAutomaticScope[auth_OAuthSLParameters]        := auth[[2]]
takeVerifyPeer[auth_OAuthSLParameters]            := auth[[3]]
takeVerifierLabel[auth_OAuthSLParameters]         := auth[[4]]
takeInputFieldOpts[auth_OAuthSLParameters]        := auth[[5]]
takeAuthenticationDialog[auth_OAuthSLParameters]  := auth[[6]]

takeServerEmulator[auth_OAuthSLParameters]        := auth[[7]]


(* ========================================================================= ==

                  iOAuthAuthentication for Secretless OAuth

    Comments:
        Implements OAuthAuthentication constructor for this OAuth version.

        Unlike the other OAuth versions, "ServiceName" is a required
        parameter here since it cannot be deduced from other parameters

    ToDo:

== ========================================================================= *)

(* iOAuthAuthentication *)

iOAuthAuthentication[$authKeySL, opts:{___?OptionQ}] :=
    Block[{defaults, badopts, res, tests, sane},
        defaults = defaultOAuthSL;
        badopts = FilterRules[opts, Except[defaults]];
        If[badopts =!= {},
            Message[HTTPClient`OAuthAuthentication::prop, AuthType[$authKeySL], badopts]
        ];
        (
            res = OAuthSLParameters @@ ((First /@ defaults) /. opts /. defaults);
            tests = sanityTests[res];
            sane = TrueQ[And @@ (Last /@ tests)];
            If[!sane && ListQ[tests],
                Message[HTTPClient`OAuthAuthentication::sane, AuthType[$authKeySL], Cases[tests, _[t_, Except[True]] :> t]]
            ];
            res /; sane
        ) /; (badopts === {})
    ]

iOAuthAuthentication[$authKeySL, ___] := $Failed

iOAuthAuthentication[Automatic, opts:{___?OptionQ}] :=
    Block[{defaults, badopts, res, tests, sane},
        defaults = defaultOAuthSL;
        badopts = FilterRules[opts, Except[defaults]];
        (
            res = OAuthSLParameters @@ ((First /@ defaults) /. opts /. defaults);
            tests = sanityTests[res];
            sane = TrueQ[And @@ (Last /@ tests)];
            res /; sane
        ) /; (badopts === {})
    ]


(* ========================================================================= ==

                    Get access token for Secretless OAuth

    Comments:
        The OAuth flow is executed by the Wolfram server, which will store
        the access tokens for a short time or until they are retrieved
        in exchange for the key presented to the user by the Wolfram server.
        Note that the key presented to the user by the Wolfrm server is
        not the same as the OAuth verifier used in the OAuth flow.
        The OAuth version type is determined by the Wolfram servier, which will
        return a token of the appropriate type.

    ToDo:

== ========================================================================= *)

(* GetAccessToken *)

GetAccessToken[auth_OAuthSLParameters, requestToken_, wolframKey_, state_:None] :=
    Block[{res},
        res = getAccessTokenSL[auth, requestToken, wolframKey, state];
        res /; (TokenQ[res] || res === $Failed)
    ]


(* getAccessTokenSL *)

getAccessTokenSL[auth_OAuthSLParameters, requestToken:None, wolframKey_String, state_String] :=
    Block[{accessEndpoint, keyStr, secretStr, tokenBody, tmp, accessToken, state2, serverEmulator},
        accessEndpoint = "https://oauthtokens.wolfram.com/access";
        serverEmulator = takeServerEmulator[auth];

        tokenBody = If[serverEmulator === None,
            URLFetch[accessEndpoint, "Method" -> "GET", "VerifyPeer" -> False, "Parameters" -> {"key" -> wolframKey}],
            serverEmulator[accessEndpoint <> "?key=" <> wolframKey]
        ];
        (
            tmp = extractTokenSL[tokenBody, wolframTokenExtractor, auth, Hold[HTTPClient`OAuthAuthentication::accbdy]];
            (
                {accessToken, state2} = tmp;
                iGetAccessTokenSL[auth, accessToken, state, state2]
            ) /; MatchQ[tmp, {_, _}]
        ) /; StringQ[tokenBody]

    ]

getAccessTokenSL[expr___] :=
    (messageGetAccessTokenSL[expr]; $Failed)


(* :message: *)

messageGetAccessTokenSL[auth_OAuthSLParameters, tok:Except[None], _] :=
    Message[HTTPClient`OAuthAuthentication::rtenone, AuthType[auth], takeServiceName[auth], tok]

messageGetAccessTokenSL[auth_OAuthSLParameters, _, key:Except[_String]] :=
    Message[HTTPClient`OAuthAuthentication::bdkey, key]


(* iGetAccessTokenSL *)

iGetAccessTokenSL[auth_OAuthSLParameters, accessToken_, state_, state2:None] :=
    Block[{},
        accessToken /; TokenQ[accessToken]
    ]

iGetAccessTokenSL[auth_OAuthSLParameters, accessToken_, state_String, state2_String] :=
    Block[{},
        (
            accessToken /; TokenQ[accessToken]
        ) /; state === state2
    ]

iGetAccessTokenSL[expr___] :=
    (messageIGetAccessTokenSL[expr]; $Failed)


(* :message: *)

messageIGetAccessTokenSL[auth_OAuthSLParameters, tok_, state_String, state2_String] /;
        state =!= state2 :=
    Message[HTTPClient`OAuthAuthentication::bdstate, "OAuth", takeServiceName[auth], "key"]


(* ========================================================================= ==

                Prepare OAuth URLFetch for Secretless OAuth

    Comments:

    ToDo:

== ========================================================================= *)

(* iOAuthURLFetchList *)

iOAuthURLFetchList[auth_OAuthSLParameters, tokens_, url_, opts:OptionsPattern[]] :=
    Block[{res},
        res = oAuthURLFetchListSL[auth, tokens, url, opts];
        res /; (ListQ[res] || res === $Failed)
    ]


(* oAuthURLFetchListSL *)

oAuthURLFetchListSL[auth_OAuthSLParameters, tokens_Token10, url_, opts:OptionsPattern[]] :=
    Module[{signEndpoint, serviceName, tokKey, tokSecret, serverEmulator, fparams, verb, params, signParams, unsignedURL, signedURL},
        signEndpoint = "https://oauthtokens.wolfram.com/sign";
        serviceName = takeServiceName[auth];
        tokKey = tokenKey[tokens];
        tokSecret = tokenSecret[tokens];
        serverEmulator = takeServerEmulator[auth];

         fparams = FilterRules[{opts}, Except["Parameters"]];
         verb = "Method" /. fparams /. {"Method" -> "GET"};
         params = Join[getParameters[{opts}, auth], {}];
         (
             unsignedURL = StringJoin[url, If[params === {}, "", "?" <> urlEncode[params]]];
             signParams = {
                     "service" -> serviceName,
                     "verb" -> verb,
                     "url" -> unsignedURL,
                     "oauth_token" -> tokKey,
                     "oauth_token_secret" -> tokSecret
                 };
             signedURL = If[serverEmulator === None,
                 URLFetch[signEndpoint, "Method" -> "GET", "VerifyPeer" -> False, "Parameters" -> signParams],
                 serverEmulator[signEndpoint <> "?" <> urlEncode[signParams]]
             ];

             {signedURL, Sequence@@fparams} /; StringQ[signedURL]
         ) /; (StringQ[serviceName] && ListQ[params])

    ]

oAuthURLFetchListSL[auth_OAuthSLParameters, tokens_Token20, url_, opts:OptionsPattern[]] :=
    Module[{tokenStr, fparams, params},
        tokenStr = tokenToken[tokens];

        fparams = FilterRules[{opts}, Except["Parameters"]];
        params = getParameters[{opts}, auth];
        (
            AppendTo[params, "access_token" -> tokenStr];

            {url, Sequence@@fparams, "Parameters" -> params}
        ) /; ListQ[params]
    ]

oAuthURLFetchListSL[expr___] := $Failed


(* ========================================================================= ==

                        OAuth flow for secretless OAuth 1.0a

    Comments: Scope must be None.

    ToDo:

== ========================================================================= *)

(* iOAuthFlow *)

iOAuthFlow[auth_OAuthSLParameters, scope_:None] :=
    Block[{res},
        res = oAuthFlowSL[auth, scope];
        res /; (res =!= $Failed)
    ]


(* oAuthFlowSL *)

oAuthFlowSL[auth_OAuthSLParameters, scope_] :=
    Module[{serviceName, state, input},
        state = BlockRandom[
            SeedRandom[];
            IntegerString[
                Hash[{RandomInteger[{0, 2147483647}, {6}], $SessionID}, "SHA"],
                16, 40
            ]
        ];

        serviceName = takeServiceName[auth];
        (
            input = keyDialog[auth, serviceName, state];
            iOAuthFlowSL[auth, scope, state, input]
        ) /; StringQ[serviceName]
    ]

oAuthFlowSL[expr___] := $Failed


(* iOAuthFlowSL *)

iOAuthFlowSL[auth_, scope_, state_, $Canceled] := $Canceled

iOAuthFlowSL[auth_OAuthSLParameters, scope_, state_, input_String] :=
    Module[{key, accessToken},
        key = removeWhitespaceCodeExtractor[input];
        (
            accessToken = flowGetAccessToken[auth, None, key, state];
            accessToken /; TokenQ[accessToken]
        ) /; StringQ[key]
    ]


(* ************************************************************************* *)
(*                             Service functions                             *)
(* ************************************************************************* *)

(* ========================================================================= ==

                        Enumerations

    Comments:
        OAuthService[class] gives the names of entities in class.
        OAuthService[class, key] gives the entity named key in class.
        OAuthService["class"] gives the list of classes.

    ToDo:

== ========================================================================= *)

(* class *)

OAuthService["class"] :=
    {
        "CodeExtractor",
        "OAuthVersion",
        "SignatureService",
        "TokenExtractor"
    }


(* TokenExtractor *)

OAuthService["TokenExtractor"] :=
    {
        "JSON/2.0",
        "Text/1.0",
        "Text/2.0"
    }


(* CodeExtractor *)

OAuthService["CodeExtractor"] :=
    {
        "RemoveWhitespace",
        "URL",
        "Verbatim"
    }


(* SignatureService *)

OAuthService["SignatureService"] :=
    {
        "HMAC-Sha1"
    }


(* OAuthVersion *)

OAuthService["OAuthVersion"] :=
    {
        "1.0a",
        "2.0",
        "secretless"
    }


(* ========================================================================= ==

                        Export implementations

    Comments:

    ToDo:

== ========================================================================= *)

(* Token extractors *)

OAuthService["TokenExtractor", "Text/1.0"] := text10TokenExtractor
OAuthService["TokenExtractor", "Text/2.0"] := text20TokenExtractor
OAuthService["TokenExtractor", "JSON/2.0"] := jsonAccessTokenExtractor
OAuthService["TokenExtractor", "Wolfram"]  := wolframTokenExtractor


(* Code extractors *)

OAuthService["CodeExtractor", "RemoveWhitespace"] := removeWhitespaceCodeExtractor
OAuthService["CodeExtractor", "URL"]              := urlCodeExtractor
OAuthService["CodeExtractor", "Verbatim"]         := verbatimCodeExtractor


(* Signature services *)

OAuthService["SignatureService", "HMAC-Sha1"] := HMACSha1SignatureService


(* ========================================================================= ==

                        Token extractors

    Comments:

    ToDo:

== ========================================================================= *)

(* text10TokenExtractor *)

text10TokenExtractor[body_String] :=
    Block[{keys, secrets},
        keys = StringCases[body, "oauth_token=" ~~ x : RegularExpression["[^&#]*"] :> x];
        secrets = StringCases[body, "oauth_token_secret=" ~~ x : RegularExpression["[^&#]*"] :> x];
        {
            "TokenKey" -> First[keys],
            "TokenSecret" -> First[secrets]
        } /; (ListQ[keys] && Length[keys] == 1 && ListQ[secrets] && Length[secrets] == 1)
    ]

text10TokenExtractor[_] := $Failed


(* text20TokenExtractor *)

text20TokenExtractor[body_String] :=
    Block[{tokens, state},
        tokens = StringCases[body, "access_token="~~x:RegularExpression["[^&#]*"] :> x];
        tokens = If[MatchQ[tokens, {_String}], First[tokens], $Failed];
        state = StringCases[body, "state="~~x:RegularExpression["[^&#]*"] :> x];
        state = If[MatchQ[state, {_String}], First[state], None];
        {
            "Token" -> tokens,
            "State" -> state
        } /; (StringQ[tokens] && MatchQ[state, _String | None])
    ]

text20TokenExtractor[_] := $Failed


(* jsonAccessTokenExtractor *)

jsonAccessTokenExtractor[body_String] :=
    Block[{rules, tokens, state},
        rules = Quiet@ImportString[body, "JSON"];
        (
            tokens = FilterRules[rules, "access_token"];
            tokens = If[MatchQ[tokens, {_ -> _String}], tokens[[1, 2]], $Failed];
            state = FilterRules[rules, "state"];
            state = If[MatchQ[state, {_ -> _String}], state[[1, 2]], None];
            {
                "Token" -> tokens,
                "State" -> state
            } /; (StringQ[tokens] && MatchQ[state, _String | None])
        ) /; MatchQ[rules, {___Rule}]
    ]

jsonAccessTokenExtractor[_] := $Failed


(* wolframTokenExtractor *)

wolframTokenExtractor[body_String] :=
    Block[{tokens, state},
        tokens = StringCases[body, "access_token="~~x:RegularExpression["[^&#]*"] :> x];
        tokens = If[MatchQ[tokens, {_String}], First[tokens], $Failed];
        state = StringCases[body, "state="~~x:RegularExpression["[^&#]*"] :> x];
        state = If[MatchQ[state, {_String}], First[state], None];
        {
            "Token" -> tokens,
            "State" -> state
        } /; (StringQ[tokens] && MatchQ[state, _String | None])
    ]

wolframTokenExtractor[body_String] :=
    Block[{tokenKey, tokenSecret, state},
        tokenKey = StringCases[body, "oauth_token="~~x:RegularExpression["[^&#]*"] :> x];
        tokenKey = If[MatchQ[tokenKey, {_String}], First[tokenKey], $Failed];
        tokenSecret = StringCases[body, "oauth_token_secret="~~x:RegularExpression["[^&#]*"] :> x];
        tokenSecret = If[MatchQ[tokenSecret, {_String}], First[tokenSecret], $Failed];
        state = StringCases[body, "state="~~x:RegularExpression["[^&#]*"] :> x];
        state = If[MatchQ[state, {_String}], First[state], None];
        {
            "TokenKey" -> tokenKey,
            "TokenSecret" -> tokenSecret,
            "State" -> state
        } /; (StringQ[tokenKey] && StringQ[tokenSecret] && MatchQ[state, _String | None])
    ]

wolframTokenExtractor[_] := $Failed


(* ========================================================================= ==

                        Code extractors

    Comments:
        The URL code extractor is provided for Facebook, and allows verifiers
         both in the form of a URL and plain (whitespace tolerant).

    ToDo:

== ========================================================================= *)

(* removeWhitespaceCodeExtractor *)

removeWhitespaceCodeExtractor[input_String] :=
    Block[{code},
        code = StringReplace[input, RegularExpression["\\s"] -> ""];
        code /; (StringQ[code] && code =!= "")
    ]

removeWhitespaceCodeExtractor[_] := $Failed


(* verbatimCodeExtractor *)

verbatimCodeExtractor[code_String] := code

verbatimCodeExtractor[_] := $Failed


(* urlCodeExtractor *)

urlCodeExtractor[codeurl_String] :=
    Block[{codes, code},
        codes = StringCases[
            StringReplace[codeurl, RegularExpression["\\s"] -> ""],
            "code=" ~~ x : RegularExpression["[^&#]*"] :> x
        ];
        (
            code = First[codes];
            code /; (StringQ[code] && code =!= "")
        ) /; (ListQ[codes] && Length[codes] == 1)
    ]

urlCodeExtractor[input_String] :=
    Block[{code},
        (
            code = StringReplace[input, RegularExpression["\\s"] -> ""];
            code /; (StringQ[code] && code =!= "")
        )/; StringFreeQ[input, {"=", "&", "#"}]
    ]

urlCodeExtractor[_] := $Failed


(* ========================================================================= ==

                        Signature services

    Comments:

    ToDo:

== ========================================================================= *)

(* HMACSha1SignatureService *)

HMACSha1SignatureService[url_String, signatureMethod_String, httpVerb_String, consumerKey_String, consumerSecret_String, tokenKey_String, tokenSecret_String] /; initializedQ :=
    Block[{res},
        res = oAuth10SignURL[url, signatureMethod, httpVerb, consumerKey, consumerSecret, tokenKey, tokenSecret];
        res /; StringQ[res]
    ]

HMACSha1SignatureService[expr___] :=
    (messageHMACSha1SignatureService[expr]; $Failed)


(* :message: *)

messageHMACSha1SignatureService[expr___] /; !initializedQ :=
    Message[HTTPClient`OAuthAuthentication::libload]


(* ========================================================================= ==

                        Custom implementations

    Comments:

    ToDo:

== ========================================================================= *)

(* Services provided as functions *)

OAuthService[dummyDataClass_, f:Except[_String]] := f

OAuthService[expr___] := $Failed


(* ************************************************************************* *)

End[] (* OAuth`Private` *)

EndPackage[]
