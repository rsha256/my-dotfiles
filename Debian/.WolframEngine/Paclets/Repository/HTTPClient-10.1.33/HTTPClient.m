
(* Wolfram HTTPClient Package *)

BeginPackage["HTTPClient`"]

URLFetch::usage = "URLFetch[url, elements] return elements from url, for any accessible URL.";
URLSave::usage = "URLSave[url, file, elements] return elements from url for any accessible URL, and store the content in file. ";
URLFetchAsynchronous::usage = "URLFetchAsynchronous[url, eventFunction] asynchronously connect to a URL";
URLSaveAsynchronous::usage = "URLSaveAsynchronous[url, file, eventFunction] asynchronously connect to a URL, and store the content in a file.";
$HTTPCookies::usage = "Returns the list of globally shared cookies."



SetAttributes[URLFetch, {ReadProtected}];
SetAttributes[URLSave, {ReadProtected}];
SetAttributes[URLFetchAsynchronous, {ReadProtected}];
SetAttributes[URLSaveAsynchronous, {ReadProtected}];

Begin["`Private`"] (* Begin Private Context *) 

If[$VersionNumber <  9,
	Message[HTTPClient::enable,  "HTTPClient"]
]

Needs["HTTPClient`CURLLink`"];
Needs["HTTPClient`OAuth`"];
Needs["PacletManager`"];

(****************************************************************************)
$CACERT = FileNameJoin[{DirectoryName[System`Private`$InputFileName], "SSL", "cacert.pem"}];
$MessageHead = HTTPClient;

(****************************************************************************)
(* Default options for URLFetch *)
$StandardOptions = {	
	"Method" -> "GET", 
	"Parameters" -> {},
	"Body" -> "", 
	"MultipartElements" -> {},
	"VerifyPeer" -> True, 
	"Username" -> "", 
	"Password" -> "", 
	"UserAgent" -> Automatic, 
	"Cookies" -> Automatic, 
	"StoreCookies" -> True,
	"Headers" -> {},
	"CredentialsProvider"->Automatic,
	"ConnectTimeout"->0,
	"ReadTimeout"->0,
	"DisplayProxyDialog" -> True,
	"OAuthAuthentication" -> None,
	"FollowRedirects" -> True
}

$DeprecatedOptions = {
	"BodyData" -> "Body",
	"MultipartData" -> "MultipartElements"
}
	
Options[URLFetch] = $StandardOptions
Options[setStandardOptions] = $StandardOptions

(* Deprecated options fix *)

(* Uncomment those lines and the message inside deprecatedOptionFix to send a deprecation warning. *)
(* URLFetch::depropt             = "The option \"``\" is deprecated, please use \"``\""; *)
(* URLSave::depropt              = URLFetch::depropt; *)
(* URLFetchAsynchronous::depropt = URLFetch::depropt; *)
(* URLSaveAsynchronous::depropt  = URLFetch::depropt; *)

deprecatedOptionFix[sym_, options___] := Sequence @@ Replace[
	{options}, 
	head_[key:Alternatives @@ Keys[$DeprecatedOptions], value_] :> (
		(* uncomment the next line to send a deprecation warning *)
		(* Message[sym::depropt, key, Lookup[$DeprecatedOptions, key]]; *)
		head[Lookup[$DeprecatedOptions, key], value]
	),
	{1}
]
deprecatedOptionQ[options___] := Cases[Keys @ {options}, Alternatives @@ Keys[$DeprecatedOptions]] =!= {}

URLFetch[url_String,opts:OptionsPattern[]]/;InitializeQ[]:=(URLFetch[url,"Content",opts])	

URLFetch[url_String, res:(_String|_List|All), options___?OptionQ] /; deprecatedOptionQ[options] := 
	URLFetch[url, res, deprecatedOptionFix[URLFetch, options]] 

allowCredintalDialog[opts:OptionsPattern[]] := (
	"DisplayProxyDialog" /. Flatten[{opts}] /. "DisplayProxyDialog"->True
);
	
URLFetch[url_String, res:(_String|_List|All), opts:OptionsPattern[]] /; InitializeQ[] :=
	Module[{handle, output, error, stdOpts, elements, wellFormedURL, oauth, token, args},	
		setMessageHead[URLFetch];
		If[OptionValue["OAuthAuthentication"] =!= None,
			oauth = OptionValue["OAuthAuthentication"];
			token = If[Head[oauth] === HTTPClient`OAuthToken, oauth, HTTPClient`OAuthAuthentication[oauth]];
			stdOpts = FilterRules[Flatten[{opts}], Except["OAuthAuthentication"]];
			args = HTTPClient`OAuthSignURL[url, Join[stdOpts, {"CredentialsProvider" -> None, "OAuthAuthentication" -> token}]]; 
			Return[URLFetch @@ args]
		];
		
		stdOpts = Flatten[{opts, FilterRules[Options[URLFetch], Except[{opts}]]}];
		error = Catch[
			handle = commonInit[url, URLFetch, stdOpts];
			If[handle === $Failed,
				Return[$Failed]
			];
			
			setOutput[handle, "String"];
	
			If[handle["Proxies"] === {},
				handle["Return"] = CURLPerform[handle];
				If[CURLStatusCode[handle] === 401 && allowCredintalDialog[opts],
					If[credWrapper[handle, url, OptionValue["CredentialsProvider"]],
						handle["Return"] = CURLPerform[handle];	
					]
				]
			(*else*),
				Do[
					CURLOption[handle, "CURLOPT_PROXY", handle["Proxies"][[i]]];
					handle["Return"] = CURLPerform[handle];
				
					If[handle["Return"] === 0 && CURLStatusCode[handle] === 401 && allowCredintalDialog[opts],
						If[credWrapper[handle, url, OptionValue["CredentialsProvider"]],
							handle["Return"] = CURLPerform[handle];
						]
					];
					
					If[handle["Return"] === 0 && CURLStatusCode[handle] === 407 && allowCredintalDialog[opts],
						If[proxyCredentials[handle, url],
							handle["Return"] = CURLPerform[handle];
						]
					];
					
					(* these error codes indicate a problem with the proxy *)
					If[handle["Return"] =!= 5 && handle["Return"] =!= 7,
						Break[]
					];
	
				, {i, Length[handle["Proxies"]]}
				];		
			];
			
			If[CURLStatusCode[handle] === 401, 
				wellFormedURL = If[StringMatchQ[url, {"http://*", "https://*, ftp://*, ftps://*"}], 
					URIJoin[Flatten@{URISplit[url]}]
				(*else*), 
					URIJoin[Flatten@{URISplit["http://" <> url]}]
				];				
				
				sessionStore[wellFormedURL] := False
			];
			
			If[CURLStatusCode[handle] === 407,
				$proxyCache = False;
			];
			
			If[handle["Return"] =!= 0,
				CURLHandleUnload[handle];
				Message[URLFetch::invhttp, CURLError[handle["Return"]]];
				Return[$Failed]
			];
		
			If[OptionValue["StoreCookies"] && OptionValue["Cookies"] =!= Automatic,
				storeCookies[HTTPData[handle, "Cookies"]]
			]; 
		
			elements = If[handle["FTP"]===True, $FTPFetchElements, $URLFetchElements];
			(* Perhaps the user just wants to know the available output types. *)
			If[res === "Elements",
					Return[elements]
			];
		
			output = parseElements[handle, res, elements];
		
			CURLHandleUnload[handle];
			Clear[HTTPClient`CURLLink`CURLHandle];
		];
		
		If[error === $Failed,
			$Failed,
			output
		]
	]
	
URLFetch::invhttp = "`1`.";
URLFetch::noelem = "The element \"`1`\" is not allowed."
	
(****************************************************************************)
(* URLSave... *)
Options[URLSave] = Join[$StandardOptions, {BinaryFormat->True}]

URLSave[url_String, options___?OptionQ] := URLSave[url, Automatic, options]

URLSave[url_String, Automatic|None|Null, rest___] := 
	URLSave[url, FileNameJoin[{$TemporaryDirectory, CreateUUID[] <> ".tmp"}], rest]

URLSave[url_String, file_String, res:(_String|_List|All):"Content", options___?OptionQ] /; deprecatedOptionQ[options] := 
	URLSave[url, file, deprecatedOptionFix[URLSave, options]]  
	
URLSave[url_String, file_String, res:(_String|_List|All):"Content", opts:OptionsPattern[]] /; InitializeQ[] :=
	Module[{handle, output, error, stdOpts, elements, wellFormedURL, oauth, token, args},
		setMessageHead[URLSave];
		If[OptionValue["OAuthAuthentication"] =!= None,
			oauth = OptionValue["OAuthAuthentication"];
			token = If[Head[oauth] === HTTPClient`OAuthToken, oauth, HTTPClient`OAuthAuthentication[oauth]];
			stdOpts = FilterRules[Flatten[{opts}], Except["OAuthAuthentication"]];
			args = HTTPClient`OAuthSignURL[url, Join[stdOpts, {"CredentialsProvider" -> None, "OAuthAuthentication" -> token}]]; 
			Return[URLSave @@ {First[args], file, Rest[args]}]
		];
		stdOpts = Flatten[{opts, FilterRules[Options[URLSave], Except[{opts}]]}];
		error = Catch[
			handle = commonInit[url, URLSave, stdOpts];
			If[handle === $Failed,
				Return[$Failed]
			];
		
			setOutput[handle, "File", ExpandFileName[file], OptionValue[BinaryFormat]];
			
			If[handle["Proxies"] === {},
				handle["Return"] = CURLPerform[handle];
				If[CURLStatusCode[handle] === 401,
					If[credWrapper[handle, url, OptionValue["CredentialsProvider"]],
						handle["Return"] = CURLPerform[handle];	
					]
				]
			(*else*),
				Do[
					CURLOption[handle, "CURLOPT_PROXY", handle["Proxies"][[i]]];
					handle["Return"] = CURLPerform[handle];
					If[handle["Return"] === 0 && CURLStatusCode[handle] === 401,
						If[credWrapper[handle, url, OptionValue["CredentialsProvider"]],
							handle["Return"] = CURLPerform[handle];
						];
					];
				
					If[handle["Return"] === 0 && CURLStatusCode[handle] === 407,
						If[proxyCredentials[handle, url],
							handle["Return"] = CURLPerform[handle];
						]
					];
	
					(* these error codes indicate a problem with the proxy *)
					If[handle["Return"] =!= 5 && handle["Return"] =!= 7,
						Break[]
					]
					, {i, Length[handle["Proxies"]]}
				];		
			];
		
			If[CURLStatusCode[handle] === 401, 
				wellFormedURL = If[StringMatchQ[url, {"http://*", "https://*, ftp://*, ftps://*"}], 
					URIJoin[Flatten@{URISplit[url]}]
				(*else*), 
					URIJoin[Flatten@{URISplit["http://" <> url]}]
				];				
				
				sessionStore[wellFormedURL] := False
			];
			
			If[CURLStatusCode[handle] === 407,
				$proxyCache = False;
			];
			
			If[handle["Return"] =!= 0,
				CURLHandleUnload[handle];
				Message[URLSave::invhttp, CURLError[handle["Return"]]];
				Return[$Failed]
			];
		
			If[OptionValue["StoreCookies"] && OptionValue["Cookies"] =!= Automatic,
				storeCookies[HTTPData[handle, "Cookies"]]
			]; 
		
			elements = If[handle["FTP"]===True, $FTPSaveElements, $URLSaveElements];
			(* Perhaps the user just wants to know the available output types. *)
			If[res === "Elements",
				Return[elements]
			];
	
			output = 
				If[res === "Content",
					file
				(*else*), 
					parseElements[handle, res, elements]
				];
		
			CURLHandleUnload[handle];
			Clear[HTTPClient`CURLLink`CURLHandle];
		];
		
		If[error === $Failed,
			$Failed,
			output
		]
	]

URLSave::invhttp = "`1`.";
URLSave::noelem = "The element \"`1`\" is not allowed."
(****************************************************************************)
(* Useful functions for both URLFetch and URLSave *)

setMessageHead[head_] := $MessageHead = head;
curlMessage[head_, tag_, args___] := Message[MessageName[head, tag], args]

connectQ[] :=
	If[$AllowInternet,
		True
	(*else*),
		Message[URLFetch::offline];
		False
	];
	
(* Check all the options passed are valid. *)
validOptionsQ[opts_, func_] := 
	Module[{},
		If[opts === {},
			Return[True]
		];
		
		If[FilterRules[opts, Except[Options[func]]] =!= {},
			Message[General::optx, First[#], InString[$Line]] & /@ FilterRules[opts, Except[Options[func]]];
			Return[False];	
		];

		If[!StringQ[(Method /. opts)] || (Method /. opts) === "",
			If[!StringQ[("Method" /. opts)] || StringMatchQ[( "Method"/. opts), "Method"] || ("Method" /. opts) === "",
				Message[General::erropts, (Method /. opts /. Method -> "Method" ) /. opts, "Method"];
				Return[False];
			];
		];
		
		If[!MatchQ[("Headers" /. opts), List[Rule[_String, _String]...]],
			Message[General::erropts, "Headers" /. opts, "Headers"];
			Return[False];
		];
		
		If[!StringQ[("Username" /. opts)],
			Message[General::erropts, "Username" /. opts, "Username"];
			Return[False];
		];
		
		If[!StringQ[("Password" /. opts)],
			Message[General::erropts, "Password" /. opts, "Password"];
			Return[False];
		];
		
		If[("UserAgent" /. opts)=!= Automatic && !StringQ[("UserAgent" /. opts)],
			Message[General::erropts, "UserAgent" /. opts, "UserAgent"];
			Return[False];
		];
	
		If[!MatchQ[("VerifyPeer" /. opts), True|False],
			Message[General::erropts, "VerifyPeer" /. opts, "VerifyPeer"];
			Return[False];
		];
		
		If[!MatchQ[("StoreCookies" /. opts), True|False],
			Message[General::erropts, "StoreCookies" /. opts, "StoreCookies"];
			Return[False];
		];
		
		If[("Parameters" /. opts) === "Parameters",
			Return[True];
		];
		
		If[!MatchQ["Parameters" /. opts, List[Rule[_String, _String]...]],
			Message[General::erropts, "Parameters" /. opts, "Parameters"];
			Return[False];
		];
		
		If[("Body" /. opts) =!= "" && 
			!MatchQ[("Body" /. opts), _String|List[___Integer]],
			Message[General::erropts, "Body" /. opts, "Body"];
			Return[False];
		];
		
		If[("MultipartElements" /. opts) =!= {} && (
			!MatchQ[("MultipartElements" /. opts), {{_String, _String, {__Integer}}..}] &&
			!MatchQ[("MultipartElements" /. opts), {Rule[{_String, _String}, {__Integer}]..}] && 
			!MatchQ[("MultipartElements" /. opts), {Rule[{_String, _String}, _String]..}]),
			Message[General::erropts, "MultipartElements" /. opts, "MultipartElements"];
			Return[False];
		];
		
		If[!IntegerQ[("ConnectTimeout" /. opts)],
			Message[General::erropts, "ConnectTimeout" /. opts, "ConnectTimeout"];
			Return[False];
		];
		
		If[!IntegerQ[("ReadTimeout" /. opts)],
			Message[General::erropts, "ReadTimeout" /. opts, "ReadTimeout"];
			Return[False];
		];
	
		(* If we made it here, all the options should be valid. *)
		True
	]
	
(* Initialization routines common to both URLSave and URLFetch. *)
commonInit[url_String, func_, opts_List] := 
	Module[{handle},
		(* First determine if we're allowed to connect to the internet. *)	
		If[!connectQ[],
			Return[$Failed]
		];
		
		(* Now check all the options passed are valid. *)
		If[!validOptionsQ[Flatten[opts], func],
			Return[$Failed]	
		];
	
		handle = CURLHandleLoad[];
		handle["FTP"] = StringMatchQ[url, {"ftp://"~~___, "ftps://"~~___}];
		If[("UseProxy" /. $InternetProxyRules) =!= Automatic ,
			handle["Proxies"] = getProxies[url, "UseProxy" /. $InternetProxyRules];
			CURLSetProxies[handle, #] & /@ handle["Proxies"];
		];
		
		If[(("UseProxy" /. $InternetProxyRules) === Automatic) && (func === URLFetch || func === URLSave),
			handle["Proxies"] = getProxies[url, "UseProxy" /. $InternetProxyRules];
			CURLSetProxies[handle, #] & /@ handle["Proxies"];
		];
		
		If[(("UseProxy" /. $InternetProxyRules) === Automatic) && (func === URLFetchAsynchronous || func === URLSaveAsynchronous),
			CURLSetProxies[handle, "Automatic"];
		];
		
		CURLOption[handle, "CURLOPT_PROXYAUTH", 15];
		(* A bit mask passed to libcurl to indicated HTTP,HTTPS,FTP, and FTPS are the only allowed protocols *)
		CURLOption[handle, "CURLOPT_PROTOCOLS", 15]; 
		CURLOption[handle, "CURLOPT_NOSIGNAL", True];
		
		If[!("StoreCookies" /. opts) && ("Cookies" /. opts) === Automatic,
			setStandardOptions[handle, url, FilterRules[{Flatten[FilterRules[opts, Except["Cookies"]]], "Cookies"->$HTTPCookies}, $StandardOptions]]
		(*else*),
			setStandardOptions[handle, url, FilterRules[Flatten[opts], $StandardOptions]]
		]; 
		handle	
	]

(****************************************************************************)
(* Parse elements to return correct data *)
parseElements[handle_, out_, elements_] :=
	Module[{},
		Which[
			out === All, parseAll[handle, out, elements],
			StringQ[out], parseString[handle, out, elements],
			ListQ[out], parseList[handle, out, elements],
			True, Return[$Failed]
		]
	]

parseAll[handle_, All, elements_] := HTTPData[handle, #] & /@ elements;

parseString[handle_, "All", elements_] := parseAll[handle, All, elements] 
parseString[handle_, "Rules", elements_] := Rule @@@ Partition[Riffle[elements, (HTTPData[handle, #] & /@ elements)], 2]
parseString[handle_, str_String, elements_] := If[MemberQ[elements, str], HTTPData[handle, str],  curlMessage[$MessageHead, "noelem", ToString[str]]; Throw[$Failed]]

parseList[handle_, list_List, elements_] :=
	Module[{subList},
		If[Length[list] === 1,
			Which[
				StringQ[First[list]], parseString[handle, First[list], elements],
				ListQ[First[list]], 
					subList = First[list];
					If[Length[subList] === 1,
						If[StringQ[First[subList]], 
							parseString[handle, First[subList], elements]
						(*else*),
							Return[$Failed]
						]
					(*else*),
						HTTPData[handle, #] & /@ subList	
					]
				,
				True, Return[$Failed]
			]
		(*else*),
			(*Special Case: {"Rules", {...}}*)
			If[MatchQ[list, {"Rules", List[_String ...]}],
				parseString[handle, "Rules", Last[list]]
			(*else*),
				parseString[handle, #, elements] & /@ list
			]
		]
	]

(****************************************************************************)

buildData[handle_CURLHandle, data_List, method_String] /; InitializeQ[] := 
	Quiet[
		Check[
			StringExpression[ 
				Sequence @@ 
				Riffle[CURLEscape[ToString[First[#]]] <> "=" <> CURLEscape[ToString[Last[#]]] & /@ data, "&"]
			], 
			Throw[$Failed]
		]
	]

(****************************************************************************)

(* Return the headers of a CURLHandle as a list of rules. *)
HTTPData[handle_CURLHandle, "Headers"] /; errorQ[handle] := 
	Cases[StringSplit[StringSplit[CURLHeaderData[handle], "\r\n"], ": ", 2], {_, _}];
	

(****************************************************************************)
HTTPData[handle_CURLHandle, "Cookies"] /; errorQ[handle] := 
	Cases[
		{	"Domain"-> First[#], 
			If[#[[2]] === "FALSE",
				"MachineAccess"-> #[[2]]
			],
			"Path"->#[[3]], 
			"Secure"->#[[4]], 
			"Expires"->DateString[ToExpression[#[[5]]] + AbsoluteTime[{1970, 1, 1, 0, 0, 0}]], 
			"Name"->#[[6]], 
			"Value"->Last[#]
		}, 
		Except[Null]] & /@ StringSplit[StringSplit[CURLCookies[handle], "\n"], "\t"];

(****************************************************************************)
addHeaders[handle_CURLHandle, headers_List] :=
	CURLAddHeader[handle, StringReplace[ToString[First[#]] <> ": " <> ToString[Last[#]], "\n"->""]] & /@ headers


(****************************************************************************)
addCookies[handle_CURLHandle, cookies_List] := 
	Module[{errs},
		errs = Catch[
			If[cookies === {},
				CURLOption[handle, "CURLOPT_COOKIELIST", ""];
				Return[]		
			];
			
			CURLOption[handle, "CURLOPT_COOKIELIST", 
				StringJoin[
					ReleaseHold[{
							"Domain", "\t", 
							If[("MachineAccess" /. #) === "MachineAccess",
								"TRUE",
								"MachineAccess"
							], "\t",  			
							"Path", "\t", 
							"Secure", "\t", 
							Hold[ToString[AbsoluteTime["Expires"] - AbsoluteTime[{1970, 1, 1, 0, 0, 0}]]], "\t", 
							"Name", "\t", 
							"Value"
						} /. Rule @@@ #
					]
				]
			] & /@ cookies
		];
	]
	
storeCookies[cookies_List] /; InitializeQ[] :=
	Module[{handle},
		handle = CURLHandleLoad[];
		setStandardOptions[handle, ""];
		addCookies[handle, cookies];
		CURLHandleUnload[handle];
	]

$HTTPCookies /; InitializeQ[] :=
	Module[{cookies, handle, error},
		error = Catch[
			handle = CURLHandleLoad[];
			setStandardOptions[handle, ""];
			handle["Return"] = 0;
			cookies = HTTPData[handle, "Cookies"];
			CURLHandleUnload[handle];
		];
		If[error === $Failed, $Failed, cookies]
	]
	

(****************************************************************************)

(* Return the headers of a CURLHandle as a list of rules. *)
HTTPData[handle_CURLHandle, "Headers"] /; errorQ[handle] := 
	Cases[StringSplit[StringSplit[CURLHeaderData[handle], "\r\n"], ": ", 2], {_, _}];
	

(* Return the cookies used for by this CURLHandle. *)
HTTPData[handle_CURLHandle, "Cookies"] /; errorQ[handle] := 
	Cases[
		{	"Domain"-> First[#], 
			If[#[[2]] === "FALSE",
				"MachineAccess"-> #[[2]]
			],
			"Path"->#[[3]], 
			"Secure"->#[[4]], 
			"Expires"->DateString[ToExpression[#[[5]]] + AbsoluteTime[{1970, 1, 1, 0, 0, 0}]], 
			"Name"->#[[6]], 
			"Value"->Last[#]
		}, 
		Except[Null]] & /@ StringSplit[StringSplit[CURLCookies[handle], "\n"], "\t"];

(* Return the content as a list of bytes of a given CURLHandle. *)
HTTPData[handle_CURLHandle, "ContentData"] /; errorQ[handle] := 
		CURLRawContentData[handle]
	
(* Return the content as a String of a given CURLHandle. *)
HTTPData[handle_CURLHandle, "Content"] /; errorQ[handle] := 
	Module[{bytes, contentType, charset, mCharset }, 
		bytes = HTTPData[handle, "ContentData"];
		If[handle["FTP"] === True,
			Return[
				ImportString[
					ExportString[bytes, "Byte", "DataFormat"->{"Integer8"}],
					"Text"
				]			
			]	
		];
		
		contentType = Select[HTTPData[handle, "Headers"], 
		StringMatchQ[First[#], "Content-Type", IgnoreCase -> True] &];
		If[MatchQ[contentType, {{_String, _String}}], 
			charset = StringReplace[contentType[[1, 2]], 
				StartOfString ~~ ___ ~~ "charset=" ~~ c__ ~~ (WhitespaceCharacter | EndOfString) :> c ];
			
			If[StringMatchQ[charset, "text/html", IgnoreCase -> True],
				Return[
					ImportString[
						ExportString[bytes, "Byte", "DataFormat"->{"Integer8"}],
						{"HTML", "Source"}
					]
				];
			];
			
			(*It's OK if the charset part isn't in the header string;
			charsetToMCharset will just default to UTF8 when it gets whatever \
			garbage is in the charset variable at this point.*)
			mCharset = charsetToMCharset[ToUpperCase[charset]]
		(*else*), (*No content- type header field.Default to the most common case.*)
			mCharset = "ISO8859-1"
		];
		ImportString[
			ExportString[bytes, "Byte", "DataFormat"->{"Integer8"}],
			"Text",
			CharacterEncoding->mCharset
		]
	]

(*Convert HTTP header charset names (in uppercase) to Wolfram Language CharacterEncoding names.*)
charsetToMCharset["US-ASCII"] = "ASCII"
charsetToMCharset["ISO-8859-1"] = "ISO8859-1"
charsetToMCharset["UTF-8"] = "UTF8"
(*... etc.for others*)
charsetToMCharset[_] = "ISO8859-1"   (*Fallthrough to most common case.*)

		
(* Return the status code as an Integer of a given CURLHandle. *)
HTTPData[handle_CURLHandle, "StatusCode"] /; errorQ[handle] := 
	CURLStatusCode[handle]
	
(* Catch all for bad types *)
HTTPData[handle_CURLHandle, unknown_] := (curlMessage[$MessageHead, "noelem", ToString[unknown]]; Throw[$Failed])

(****************************************************************************)
URISplit[uri_String] := 
	Flatten[
		StringCases[
			uri, 
			RegularExpression[ "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"] -> 
			   	{"Scheme" -> "$2", "Authority" -> "$4"}
		]
	]

URIJoin[uri : List[_Rule, _Rule]] := 
	Module[{scheme, authority},
		If[!Fold[And, True, Map[MatchQ[#, Rule[_String, _String]] &, uri]], Return[$Failed]]; 
		{scheme, authority} = Map[Last, uri];
		StringJoin[
			Cases[{
				If[scheme =!= "", StringJoin[scheme, ":"]],
				Which[
					authority =!= "" && scheme =!= "", StringJoin["//", authority],
					authority === "" && scheme =!= "", authority
				]
			}, Except[Null]]
		]
	]

(****************************************************************************)

buildProxy[{scheme_String, url_String}] := If[StringMatchQ[url, scheme <> "://*"], url, scheme <> "://" <> url]
buildProxy[{url_String}] := url
buildProxy[url_String] := url	
	
getProxies[url_String, False] = {""}
getProxies[url_String, True] := Cases[Rest[$InternetProxyRules], Rule[scheme_, {proxy_, port_}] :> 
	If[StringMatchQ[proxy, scheme <> "://*", IgnoreCase -> True], "", ToLowerCase[scheme] <> "://"] <> proxy <> ":" <> ToString[port]]

getProxies[url_String, Automatic] :=
	getSystemProxies[url, $OperatingSystem]
	
getSystemProxies[url_String, "Windows"] :=
	Module[{rawProxies, proxies},
		rawProxies = If[StringMatchQ[url, {"http://*", "https://*, ftp://*, ftps://*"}],
			Quiet[Check[CURLGetProxies[url], {}], LibraryFunction::strnull]
		(*else*),
			Quiet[Check[CURLGetProxies["http://" <> url], {}], LibraryFunction::strnull]
		];
		
		proxies = StringSplit[StringSplit[rawProxies, ";"], "=", 2];
		buildProxy[#] & /@ proxies
	]	
	
getSystemProxies[url_String, "MacOSX"] := 
	Module[{},	
		If[StringMatchQ[url, {"http://*", "https://*, ftp://*, ftps://*"}],
			Flatten@{Quiet[Check[CURLGetProxies[URIJoin[Flatten@{URISplit[url]}]], {}]]}
		(*else*),
			Flatten@{Quiet[Check[CURLGetProxies[URIJoin[Flatten@{URISplit["http://" <> url]}]], {}]]}
		]
	]
	
getSystemProxies[url_String, _] := {}

setProxies[handle_CURLHandle] := CURLSetProxies[handle, StringJoin@Riffle[handle["Proxies"], "\n"]] 
(****************************************************************************)
setOutput[handle_CURLHandle, "String"] := 
	(
		If[!(handle["FTP"]===True),
			CURLOption[handle, "CURLOPT_HEADERFUNCTION", "WRITE_MEMORY"];
			CURLOption[handle, "CURLOPT_WRITEHEADER", "MemoryPointer"];
		];
		CURLOption[handle, "CURLOPT_WRITEFUNCTION", "WRITE_MEMORY"];
		CURLOption[handle, "CURLOPT_WRITEDATA", "MemoryPointer"];
	)
	
setOutput[handle_CURLHandle, "File", fileName_String, format:(True|False)] := 
	(
		If[!(handle["FTP"]===True),
			CURLOption[handle, "CURLOPT_HEADERFUNCTION", "WRITE_MEMORY"];
			CURLOption[handle, "CURLOPT_WRITEHEADER", "MemoryPointer"];
		];
		
		CURLFileInfo[handle, fileName, format];
		CURLOption[handle, "CURLOPT_WRITEFUNCTION", "WRITE_FILE"];
		CURLOption[handle, "CURLOPT_WRITEDATA", "FilePointer"];
	)
	
setOutput[handle_CURLHandle, "WriteFunction", func_String] :=
	(
		If[!(handle["FTP"]===True),
			CURLOption[handle, "CURLOPT_HEADERFUNCTION", "WRITE_MEMORY"];
			CURLOption[handle, "CURLOPT_WRITEHEADER", "MemoryPointer"];	
		];
		CURLWriteInfo[handle, func];
		CURLOption[handle, "CURLOPT_WRITEFUNCTION", "WRITE_USER"];
	)

(****************************************************************************)

setStandardOptions[handle_CURLHandle, url_String, opts:OptionsPattern[]] := 
	Module[{finalURL = url, method = ToUpperCase[OptionValue["Method"]], baseURL}, 
		If[OptionValue["UserAgent"] === Automatic,
			CURLOption[handle, "CURLOPT_USERAGENT", "Wolfram HTTPClient " <> ToString[$VersionNumber]],	
			CURLOption[handle, "CURLOPT_USERAGENT", OptionValue["UserAgent"]];
		];
			
		CURLOption[handle, "CURLOPT_CAINFO", $CACERT];
		CURLOption[handle, "CURLOPT_SSL_VERIFYPEER", OptionValue["VerifyPeer"]]; 
		CURLOption[handle, "CURLOPT_FOLLOWLOCATION", OptionValue["FollowRedirects"]];
		CURLOption[handle, "CURLOPT_POSTREDIR", HTTPClient`CURLInfo`Private`$CURLPostRedir]; 
		CURLOption[handle, "CURLOPT_TIMEOUT", OptionValue["ReadTimeout"]];
		CURLOption[handle, "CURLOPT_CONNECTTIMEOUT", OptionValue["ConnectTimeout"]];
		(* Always ensure that a Method is set intially, at Top Level. *)
		If[StringQ[method] && StringMatchQ[method, ""],
			CURLOption[handle, "CURLOPT_CUSTOMREQUEST", "GET"],
			CURLOption[handle, "CURLOPT_CUSTOMREQUEST", method]
		];

		If[OptionValue["Username"] =!= "",
			CURLOption[handle, "CURLOPT_USERNAME", OptionValue["Username"]]
		];
		If[OptionValue["Password"] =!= "",
			CURLOption[handle, "CURLOPT_PASSWORD", OptionValue["Password"]]
		];
		Switch[OptionValue["Cookies"],
			Automatic, CURLAutoCookies[handle], 
			_, addCookies[handle, OptionValue["Cookies"]]
		];
		If[OptionValue["Headers"] =!= {},
			addHeaders[handle, OptionValue["Headers"]]
		];

		If[method === "POST",
			CURLOption[handle, "CURLOPT_POST", True];	
		];
		
		If[method === "HEAD",
			CURLOption[handle, "CURLOPT_NOBODY", True];	
		];
		
		If[method =!= "GET" && method =!= "POST" && method =!= "HEAD",
			CURLOption[handle, "CURLOPT_CUSTOMREQUEST", method]
		];
		
		If[OptionValue["Parameters"] =!= {},
			If[method === "GET",
				finalURL  = url <> "?" <> buildData[handle, OptionValue["Parameters"], method],
				CURLOption[handle, "CURLOPT_COPYPOSTFIELDS", buildData[handle, OptionValue["Parameters"], method]]
			]
		];
		(* If the Parmeters are set then, we don't want to set the body. *)
		If[StringQ[OptionValue["Body"]] && ( OptionValue["Parameters"] ==={} ), 
			CURLOption[handle, "CURLOPT_COPYPOSTFIELDS", OptionValue["Body"]];
		];
		
		CURLCredentialsProvider[handle, ToString[OptionValue["CredentialsProvider"]]];

		(*Handles the old List cases of Multipart Requests*)
		If[MatchQ[OptionValue["MultipartElements"], {{_String, _String, {__Integer}}..}],
			CURLForm[handle, 
					#[[1]], 
					#[[2]], 
					#[[3]], 
					Length[#[[3]]],
					""
			] & /@ OptionValue["MultipartElements"]
		];	

		(*Handles a List of Rules of MutipartData*)
		Which[MatchQ[OptionValue["MultipartElements"], {Rule[{_String, _String}, _String]..}], 
				CURLForm[handle, 
					#[[1]][[1]], 
					#[[1]][[2]], 
					#[[2]], 
					Length[#[[2]]],
					""
			] & /@ ((Rule[#[[1]],ToCharacterCode[#[[2]]]])& /@ OptionValue["MultipartElements"]),
			MatchQ[OptionValue["MultipartElements"], {Rule[{_String, _String}, {__Integer}]..}],
				CURLForm[handle, 
					#[[1]][[1]], 
					#[[1]][[2]], 
					#[[2]], 
					Length[#[[2]]],
					""
			] & /@ OptionValue["MultipartElements"]
		];

		(* If the Parmeters are set then, we don't want to set the body. *)
		If[MatchQ[OptionValue["Body"], {__Integer}|{}]&& ( OptionValue["Parameters"] ==={}) ,
			CURLOption[handle, "CURLOPT_POSTFIELDSIZE", Length[OptionValue["Body"]]];
			CURLOption[handle, "CURLOPT_COPYPOSTFIELDS", OptionValue["Body"]]
		];
			
		handle["URL"] = finalURL;
		CURLSetURL[handle, finalURL];
		CURLOption[handle, "CURLOPT_URL", finalURL];	
		
		baseURL = If[StringMatchQ[url, {"http://*", "https://*, ftp://*, ftps://*"}], 
			URIJoin[Flatten@{URISplit[url]}]
		(*else*), 
			URIJoin[Flatten@{URISplit["http://" <> url]}]
		];				
		handle["BaseURL"] = baseURL;
		CURLSetBaseURL[handle, baseURL];
	]

(****************************************************************************)
(* helper functions for HTTP streams *)
streamInit[url_String, opts_List] :=
	Module[{stdOpts, error, handle},
		Quiet[
			stdOpts = FilterRules[Flatten[{opts, FilterRules[Options[URLFetch], Except[opts]]}], Except[BinaryFormat]];
			error = Catch[
				handle = commonInit[url, URLFetch, stdOpts];
				If[handle === $Failed,
					Return[$Failed]
				];
				
				If[TrueQ[$proxyCache],
					CURLProxyCache[handle];
				];
		
				setOutput[handle, "String"];
			]
		];
		handle["OPTIONS"] = stdOpts;
		
		If[error === $Failed,
			$Failed
		(*else*),
			First[handle]
		]
	]
	
Options[streamCookies] = $StandardOptions
streamCookies[id_Integer] :=
	streamCookies[id, Sequence@@CURLHandle[id]["OPTIONS"]]

streamCookies[id_Integer, opts:OptionsPattern[]] :=
	Module[{error},
		Quiet[
			error = Catch[
				If[OptionValue["StoreCookies"] && OptionValue["Cookies"] =!= Automatic,
					storeCookies[HTTPData[CURLHandle[id], "Cookies"]]
				] 
			]
		];
		
		If[error === $Failed,
			$Failed,
			True
		]
	]
	
streamStore[id_Integer] := 
	Module[{wellFormedURL, handle},
		handle = CURLHandle[id];
		wellFormedURL = If[StringMatchQ[handle["URL"], {"http://*", "https://*, ftp://*, ftps://*"}], 
			URIJoin[Flatten@{URISplit[handle["URL"]]}]
		(*else*), 
			URIJoin[Flatten@{URISplit["http://" <> handle["URL"]]}]
		];
		sessionStore[wellFormedURL] := False
	]
(****************************************************************************)
sessionStore[_] := False;

credWrapper[handle_CURLHandle, url_String, func_] :=
	credWrapper[First[handle], url, func]
	
credWrapper[id_Integer, url_String, func_] :=
	Module[{credProvider, wellFormedURL, defaultQ, handle = CURLHandle[id], res},
		defaultQ = func === Automatic; 
		credProvider = If[defaultQ, HTTPClient`Private`passwordDialog, func];
		
		wellFormedURL = If[StringMatchQ[url, {"http://*", "https://*, ftp://*, ftps://*"}], 
			URIJoin[Flatten@{URISplit[url]}]
		(*else*), 
			URIJoin[Flatten@{URISplit["http://" <> url]}]
		];
		

		If[defaultQ && sessionStore[wellFormedURL],
			CURLReset[handle];
			CURLSessionCache[handle, wellFormedURL];
			Return[True]
		];
		
		res = credProvider[url];
		Which[
			res === $Canceled, False,
			MatchQ[res, List[_String, _String]], 
				CURLReset[handle];
				If[defaultQ,
					sessionStore[wellFormedURL] := True;
					CURLStore[handle, wellFormedURL, First[res], Last[res]];
				(*else*),
					CURLOption[handle, "CURLOPT_USERNAME", First[res]];
					CURLOption[handle, "CURLOPT_PASSWORD", Last[res]];	
				];
				True	
		]
	]
	
$proxyCache = False;
proxyCredentials[id_Integer, url_String] :=
	proxyCredentials[CURLHandle[id], url]
	
proxyCredentials[handle_CURLHandle, url_String] :=	
	Module[{result},
		If[$proxyCache === True,
			CURLReset[handle];
			CURLProxyCache[handle];
			Return[True];	
		];
		
		result = proxyDialog[url];
		Which[
			res === $Canceled, False,
			MatchQ[result, List[_String, _String]],
				CURLReset[handle];
				CURLOption[handle, "CURLOPT_PROXYUSERNAME", First[result]];
				CURLOption[handle, "CURLOPT_PROXYPASSWORD", Last[result]];
				$proxyCache = True;
				True
		]
	]
	
(* Old default Wolfram System password dialog *)
If[!ValueQ[$allowDialogs], $allowDialogs = True]
hasFrontEnd[] := ToString[Head[$FrontEnd]] === "FrontEndObject"
$pwdDlgResult;

passwordDialogStandalone[prompt1_, prompt2_, prompt3_] :=
(
	Print[prompt1];
	Print[prompt2];
	Print[prompt3];
	{InputString["username: "], InputString["password (will echo as cleartext): "]}
)

passwordDialogFE[title_, prompt1_, prompt2_, prompt3_] :=
	Module[{cells, uname = "", pwd = "", createDialogResult},
		cells = {
			TextCell[prompt1, NotebookDefault, "DialogStyle", "ControlStyle"],
			TextCell[prompt2, NotebookDefault, "DialogStyle", "ControlStyle"],
			ExpressionCell[Grid[{ {TextCell["Username:  "], InputField[Dynamic[uname], String, ContinuousAction -> True, 
         		ImageSize -> 200, BoxID -> "UserNameField"]}, {TextCell["Password:  "], 
					InputField[Dynamic[pwd], String, ContinuousAction -> True, 
						ImageSize -> 200, FieldMasked -> True]}}], "DialogStyle", "ControlStyle"],
				TextCell[prompt3, NotebookDefault, "DialogStyle", "ControlStyle"],
                
				ExpressionCell[ Row[{DefaultButton[$pwdDlgResult = {uname, pwd}; 
					DialogReturn[], ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]]], Spacer[{2.5`, 42, 16}],
				CancelButton[$pwdDlgResult = $Canceled; DialogReturn[], 
					ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]]]}], TextAlignment -> Right] };
			createDialogResult = DialogInput[DialogNotebook[cells], 
				WindowTitle -> title, WindowSize -> {400, FitAll}, Evaluator -> CurrentValue["Evaluator"], 
				LineIndent -> 0, PrivateFontOptions -> {"OperatorSubstitution" -> False} ];
			If[createDialogResult === $Failed,
				Null,
			(* else *)
				MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[ FE`BoxReference[createDialogResult, {{"UserNameField"}}, 
					FE`BoxOffset -> {FE`BoxChild[1]}]]];
				$pwdDlgResult
			]
	]
	
coreDialog[url_String, prompt2_String] :=
	Module[{title, prompt1, prompt3},
	    title = "Authentication Required";
        Clear[$pwdDlgResult];
        Which[
            !TrueQ[$allowDialogs],
                Null,
            hasFrontEnd[],
                (* Use FE dialog box *)
                prompt1 = Row[{"You are attempting to read from the URL:\n", Hyperlink[url, BaseStyle -> "ControlStyle"]}];
                prompt3 = "(These values are kept for this session only.)";
                passwordDialogFE[title, prompt1, prompt2, prompt3],
            True,
                prompt1 = "You are attempting to read from the URL:\n" <> url;
                prompt3 = "(These values are kept for this session only.)";
                passwordDialogStandalone[prompt1, prompt2, prompt3]
        ]
	]
	
passwordDialog[url_String] := coreDialog[url, "The server is requesting authentication."]
proxyDialog[url_String] := coreDialog[url, "The proxy server is requesting authentication."]


(****************************************************************************)
$AsyncEnum = {
	"Progress" -> 0,
	"Transfer" -> 1	
}

callBackWrapper[obj_, "headers", data_] := {obj, "headers", Cases[StringSplit[StringSplit[FromCharacterCode[First[data]], "\r\n"], ": ", 2], {_, _}]}
callBackWrapper[obj_, "cookies", data_] := 
	{obj, "cookies",      	
		Cases[
			{	"Domain"-> First[#], 
				If[#[[2]] === "FALSE",
					"MachineAccess"-> #[[2]]
				],
				"Path"->#[[3]], 
				"Secure"->#[[4]], 
				"Expires"->DateString[ToExpression[#[[5]]] + AbsoluteTime[{1970, 1, 1, 0, 0, 0}]], 
				"Name"->#[[6]], 
				"Value"->Last[#]
			}, 
			Except[Null]] & /@ StringSplit[StringSplit[FromCharacterCode[First[data]], "\n"], "\t"]
	}
	
callBackWrapper[obj_, "credentials", data_] := 
	Module[{error, credProvider, handleID, url, output},
		Catch[error,
			handleID = data[[1]];
			url = data[[2]];
			credProvider = data[[3]];	
			CURLReset[CURLHandle[handleID]];
			output = ToExpression[credProvider][handleID, url];
			If[MatchQ[output, {_String, _String}],
				CURLOption[CURLHandle[handleID], "CURLOPT_USERNAME", output[[1]]];
				CURLOption[CURLHandle[handleID], "CURLOPT_PASSWORD", output[[2]]];
			];
			CURLSetCheckQ[CURLHandle[handleID], True];
		];
		
		If[error === $Failed,
			{obj, "credentials", {False}}
		(*else*),
			{obj, "credentials", {True}} 
		]
	]
	
callBackWrapper[obj_, name_, data_] := {obj, name, data}

(****************************************************************************)
Options[URLFetchAsynchronous] = Join[$StandardOptions, {"Progress"->False, "Transfer"->Automatic, "UserData"->None}];

URLFetchAsynchronous[url_String, func:Except[_Rule|_RuleDelayed|_String], options___?OptionQ] /; deprecatedOptionQ[options] := 
	URLFetchAsynchronous[url, func, deprecatedOptionFix[URLFetchAsynchronous, options]] 

URLFetchAsynchronous[url_String, func:Except[_Rule|_RuleDelayed|_String], opts:OptionsPattern[]] /; InitializeQ[] := 
	Module[{handle, stdOpts, error, oauth, token, args, output},
		If[OptionValue["OAuthAuthentication"] =!= None,
			oauth = OptionValue["OAuthAuthentication"];
			token = If[Head[oauth] === HTTPClient`OAuthToken, oauth, HTTPClient`OAuthAuthentication[oauth]];
			stdOpts = FilterRules[Flatten[{opts}], Except["OAuthAuthentication"]];
			args = HTTPClient`OAuthSignURL[url, Join[stdOpts, {"CredentialsProvider" -> None, "OAuthAuthentication" -> token}]]; 
			Return[URLFetchAsynchronous @@ {First[args], func, Rest[args]}]
		];
		stdOpts = Flatten[{opts, FilterRules[Options[URLFetchAsynchronous], Except[{opts}]]}];	
		error = Catch[
			(* handle is freed in c code *)
			handle = commonInit[url, URLFetchAsynchronous, stdOpts];
			If[handle === $Failed,
				Return[$Failed]
			];
		
			CURLSetAsync[handle, True];
			setOutput[handle, "String"];
		
			If[OptionValue["StoreCookies"] && OptionValue["Cookies"] =!= Automatic,
				CURLAsyncCookies[handle, True]
			]; 
		
			(* Set which async events will be raised. *)
			CURLAsyncOption[handle, "Progress" /. $AsyncEnum, OptionValue["Progress"]];
			Switch[OptionValue["Transfer"],
				Automatic, CURLAsyncOption[handle, "Transfer" /. $AsyncEnum, True];,
				"Chunks", CURLAsyncOption[handle, "Transfer" /. $AsyncEnum, False];
			];
		];
		output = If[error === $Failed,
			$Failed
		(*else*),
			Internal`CreateAsynchronousTask[
				HTTPClient`CURLLink`Private`curlAsyncObj, {First@handle}, 
				func[Sequence@@callBackWrapper[##]] &,
				"TaskDetail"->url, 
				"UserData"->OptionValue["UserData"]]
		];
		Clear[HTTPClient`CURLLink`CURLHandle];
		output
	]
	

(****************************************************************************)
Options[URLSaveAsynchronous] = Join[$StandardOptions, {"Progress"->False, BinaryFormat->True, "UserData"->None}];

URLSaveAsynchronous[url_String, func:Except[_Rule|_RuleDelayed|_String], options___?OptionQ] := 
	URLSaveAsynchronous[url, Automatic, options]

URLSaveAsynchronous[url_String, Automatic|None|Null, rest___] := 
	URLSaveAsynchronous[url, FileNameJoin[{$TemporaryDirectory, CreateUUID[] <> ".tmp"}], rest]

URLSaveAsynchronous[url_String, file_String, func:Except[_Rule|_RuleDelayed|_String], options___?OptionQ] /; deprecatedOptionQ[options] := 
	URLSaveAsynchronous[url, file, func, deprecatedOptionFix[URLSaveAsynchronous, options]] 

URLSaveAsynchronous[url_String, file_String, func:Except[_Rule|_RuleDelayed|_String], opts:OptionsPattern[]] /; InitializeQ[] := 
	Module[{handle, stdOpts, error, oauth, token, args, output},
		If[OptionValue["OAuthAuthentication"] =!= None,
			oauth = OptionValue["OAuthAuthentication"];
			token = If[Head[oauth] === HTTPClient`OAuthToken, oauth, HTTPClient`OAuthAuthentication[oauth]];
			stdOpts = FilterRules[Flatten[{opts}], Except["OAuthAuthentication"]];
			args = HTTPClient`OAuthSignURL[url, Join[stdOpts, {"CredentialsProvider" -> None, "OAuthAuthentication" -> token}]]; 
			Return[URLSaveAsynchronous @@ {First[args], file, func, Rest[args]}]
		];
		stdOpts = Flatten[{opts, FilterRules[Options[URLSaveAsynchronous], Except[{opts}]]}];
		error = Catch[
			(* handle is freed in c code *)
			handle = commonInit[url, URLSaveAsynchronous, stdOpts];
			If[handle === $Failed,
				Return[$Failed]
			];
		
			CURLSetAsync[handle, True];
			setOutput[handle, "File", ExpandFileName[file], OptionValue[BinaryFormat]];
		
			If[OptionValue["StoreCookies"] && OptionValue["Cookies"] =!= Automatic,
				CURLAsyncCookies[handle, True]
			]; 
		
			(* Set which async events will be raised. *)
			CURLAsyncOption[handle, "Progress" /. $AsyncEnum, OptionValue["Progress"]];
		];
		
		output = If[error === $Failed,
			$Failed
		(*else*),
			Internal`CreateAsynchronousTask[
				HTTPClient`CURLLink`Private`curlAsyncObj, {First@handle}, 
				func[Sequence@@callBackWrapper[##]] &, 
				"TaskDetail"->url, 
				"UserData"->OptionValue["UserData"]]
		];
		Clear[HTTPClient`CURLLink`CURLHandle];
		output
	]

(****************************************************************************)
Initialize[] := Initialize[] = 
	Catch[	
		CURLInitialize[];
		CURLSetCert[$CACERT];
	] =!= $Failed

InitializeQ[] := Initialize[];
	
(****************************************************************************)
(* List of all possible output types *)
$URLFetchElements = {
	"Content",
	"ContentData",
	"Headers",
	"Cookies",
	"StatusCode"
}

$FTPFetchElements = {
	"Content",
	"ContentData",
	"StatusCode"
}

$URLSaveElements = {
	"Headers",
	"Cookies",
	"StatusCode"	
}

$FTPSaveElements = {
	"StatusCode"
};

(****************************************************************************)
errorQ[obj_CURLHandle] := obj["Return"] === 0

(****************************************************************************)
End[] (* End Private Context *)
EndPackage[]
