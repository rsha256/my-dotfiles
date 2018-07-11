BeginPackage["MailReceiver`"]

System`MailReceiverFunction
System`MailResponseFunction
System`ReturnReceiptFunction

Begin["`Private`"]


$UserCloudObjectURLs=True;
$MailReceiverAddressFunction=Automatic;
$MailReceiverAddressTemplate:=$MailReceiverAddressTemplate = StringTemplate["receiver+`1`@wolframcloud.com"]

(****************** Cloud Deploy **************************)
System`MailReceiverFunction/:CloudDeploy[System`MailReceiverFunction[mrfargs___]]:=Catch[cloudDeployMRF[{mrfargs}]]
System`MailReceiverFunction/:CloudDeploy[System`MailReceiverFunction[mrfargs___],cdargs__]:=Catch[cloudDeployMRF[{mrfargs},cdargs]]

cloudDeployMRF[receiver_List,cdargs___]:=clouddeployMRF[createReceiver@@receiver, cdargs]

clouddeployMRF[receiver_Association,cdargs___]:=Block[{co, cdargs1, info, id, shortid,url,email},
    If[!$CloudConnected,CloudConnect[]];
    If[!$CloudConnected,Message[System`MailReceiverFunction::cloudc];Throw[$Failed]];
    cdargs1=checkCDargs[cdargs];
    co=CloudDeploy[APIFunction[{"data"->"String"},
          (handlemail[receiver,importmailString[#data]]&)
       ],
       Sequence@@cdargs1,
       Permissions->{All->"Execute"}
    ];
    If[Head[co]=!=CloudObject,
        Message[System`MailReceiverFunction::noco];Throw[$Failed]
    ];
    email=createMRFEmail[co];
    co=setcloudinfo[co,email];
    setMRFTypesetting[co,email];
    co
]

clouddeployMRF[first_,___]:=(Message[MailReceiverFunction::noco];$Failed)

createMRFEmail[co_]:=If[$MailReceiverAddressFunction===Automatic,
    $MailReceiverAddressTemplate[createShortID[co]]
    ,
    $MailReceiverAddressFunction[co]
]

createShortID[co_]:=Block[{url, id,shortid},
    (* always use UUID ? *)
    If[$UserCloudObjectURLs,
        url=First[co];
        ,   
        id=getclouduuid[co];
        If[StringQ[id],
            Message[System`MailReceiverFunction::noco];Throw[$Failed]
        ];
        url=$CloudBase <> "/objects/" <> id
    ];
    shortid=URLShorten[url];
    If[StringQ[shortid],
        FileNameTake[shortid],
        getclouduuid[co]
    ]
    
]

setcloudinfo[co_, email_]:=Block[{res},
    res=SetOptions[co,MetaInformation->{"EmailAddress"->email}];
    If[res===Null||ListQ[res],
        CloudObject[First[co],MetaInformation->{"EmailAddress"->email}]
        ,
        $Failed
    ]   
]

getclouduuid[co_]:=getclouduuid[co]=With[{info=System`CloudObjectInformation[co]},
    If[Head[info]=!=System`CloudObjectInformationData,
        Message[System`MailReceiverFunction::noco];Throw[$Failed]
    ];
    First[info]["UUID"]
]

checkCDargs[args___]:={args}/;FreeQ[{args},Permissions]
checkCDargs[___]:=(Message[System`MailReceiverFunction::perms];Throw[$Failed])
(****************** LOCAL OPERATION ************************)

System`MailReceiverFunction[mrfargs___][args___]:=Catch[handleMail[{mrfargs},{args}]]

Options[System`MailReceiverFunction]={System`MailResponseFunction->Automatic,System`ReturnReceiptFunction->Automatic}

handleMail[receiver_List,mail_List]:=handlemail[createReceiver@@receiver,importmail[First[mail],Rest[mail]]]/;Length[mail]>0
handleMail[receiver_List,mail_List]:=(Message[System`MailReceiverFunction::invmail,mail];$Failed)

handleMail[___]:=$Failed

handlemail[receiver_,{mail_, rawmail_}]:=Block[{res,raw,prepped, co, receiveraddress=Missing["NotAvailable"],slots},
    co=$EvaluationCloudObject;
    If[Head[co]===CloudObject,
        receiveraddress = Replace[
            Lookup[Options[co, MetaInformation][[1, -1]], "EmailAddress", Missing["NotAvailable"]],
            ns_ /; !StringQ[ns] :> Missing["NotAvailable"]
        ]
    ];
    (* import mail *)
    raw=Lookup[receiver,"Import",Identity][mail];
    If[raw===$Failed,Throw[$Failed]];
    (* prefix? Interpreters? *)
    prepped=Lookup[receiver,"Prefix",Identity][raw];
    If[prepped===$Failed,Throw[$Failed]];
    slots=Select[Cases[{Lookup[receiver,{"Receive","Response","ReturnReceipt"},Identity]}, Slot[n_] | SlotSequence[n_] :> n, Infinity], StringQ];
    prepped=addMailReceiverKeys[slots,prepped,rawmail,receiveraddress];
    (* receiver *)
    res=Lookup[receiver,"Receive",Identity][prepped];
    If[res===$Failed,Throw[$Failed]];
    (* interpret results *)
    res=Lookup[receiver,"Interpret",Identity][res];
    If[res===$Failed,Throw[$Failed]];
    (* response *)
    Lookup[receiver,"Response",First][Join[prepped,Association["Result"->res]], rawmail];
    (* receipt *)
    handleReceipt[Lookup[receiver,"ReturnReceipt",Identity],Join[prepped,Association["Result"->res]]];
    (* returned results *)
    Lookup[receiver,"Result",Identity][res]
]

handlemail[___]:=$Failed

createReceiver[fun_Function,opts___?OptionQ]:=createReceiver[{},fun, opts]
createReceiver[fun_]:=createReceiver[{},fun]

(* Future: interpretation in first argument *)
createReceiver[{},fun_Function, opts:OptionsPattern[System`MailReceiverFunction]]:=Block[{responsefun,receiptfun},
    responsefun=createResponseFunction[OptionValue[System`MailResponseFunction]];
    receiptfun=createReceiptFunction[OptionValue[System`ReturnReceiptFunction]];
    Association[
        "Receive"->fun,
        "Response"->responsefun ,
        "ReturnReceipt"->receiptfun
    ]
]


$predefinedfunctions={}

predefinedfunction[_]:=Throw[$Failed]

createReceiver[interp_,str:(Alternatives@@$predefinedfunctions), opts___]:=Block[{},
    createReceiver[interp,predefinedfunction[str],opts]
]

createReceiver[_,expr_,___]:=(Message[System`MailReceiverFunction::invfun,expr];Throw[$Failed])

createResponseFunction[f_Function]:=customresponseFunction[f];
createResponseFunction[True]=defaultResponseFun;
createResponseFunction[False]=Null;
createResponseFunction[Automatic]=Null/;!$CloudEvaluation;
createResponseFunction[Automatic]=(If[humanSenderQ[#2],defaultResponseFun[##],Null]&);
createResponseFunction[expr_]:=(Message[System`MailResponseFunction::invopt, expr];Throw[$Failed]);


humanSenderRegex=
    RegularExpression[
        "(?m)(^(((Resent-)?(From|Sender)|X-Envelope-From):|>?From)([^>]*[^(.%@a-z0-9])?(Post(ma(st(er)?|n)|office)|(send)?Mail(er)?|daemon|mmdf|n?uucp|ops|r(esponse|oot)|(bbs\\.)?smtp(error)?|s(erv(ices?|er)|ystem)|A(dmin(istrator)?|MMGR))(([^).!:a-z0-9][-_a-z0-9]*)?[%@>\\t ][^<)]*(\\(.*\\).*)?)?$([^>]|$))"]

humanSenderQ[rawmail_]:=StringFreeQ[rawmail,humanSenderRegex,IgnoreCase->True]

mrfAddressQ[address_String]:=(!StringFreeQ[address,"receiver"])&&(!StringFreeQ[address,"wolfram"])
mrfAddressQ[address_]:=False

defaultResponseFun[as_,raw_]:=
    With[{from=getReplyAddress[as],
        subj=ToString[Lookup[as,"Subject",Missing["NotAvailable"]]], mrf=Lookup[as,"ReceiverAddress",Missing["NotAvailable"]]},
        If[!mrfAddressQ[from],
            sendmailmrf[][Association[
                
                "To"->from,
                Sequence@@If[StringQ[mrf],
                    {"From"->mrf},
                    {}
                ],
                "Subject"->"Automatic Response re: "<>subj,
                
                "Body"->"Your message was received."<>
                "\n\nDestination: "<>ToString[mrf]<>
                "\n\nSubject: "<>subj<>
                "\n\nSize: "<>ToString[Round[ByteCount[raw]/1000]]<>" kb"<>
                "\n\nMessage-ID: "<>ToString[Lookup[as,"MessageID",Missing["NotAvailable"]]]
            ]
            ]
        ]
    ]
    
customresponseFunction[fun_]:=(With[{
    from=getReplyAddress[#],
    mrf=Lookup[#,"ReceiverAddress",Missing["NotAvailable"]]},
    If[!mrfAddressQ[from],
        If[StringQ[mrf],
            sendmailmrf[][
                fun[#],
                "To"->from,
                "From"->mrf
            ]
            ,
            sendmailmrf[][
                fun[#],
                "To"->from
            ]
        ]
    ]
]&)

createReceiptFunction[True]=(True&);
createReceiptFunction[False]=(False&);
createReceiptFunction[Automatic]=False/;!$CloudEvaluation;
createReceiptFunction[Automatic]=createReceiptFunction[True];
createReceiptFunction[f_Function]:=f;
createReceiptFunction[expr_]:=(Message[System`ReturnReceiptFunction::invopt, expr];Throw[$Failed]);

sendreceipt[to_String, as_]:=With[{mrf=Lookup[as,"ReceiverAddress",Missing["NotAvailable"]]},
    sendmailmrf[][Association["To" -> to, 
    
        Sequence@@If[StringQ[mrf],
            {"From"->mrf},
            {}
        ],
    
        "Subject" -> "Read Receipt: "<>Lookup[as,"Subject",""], 
        "Body" -> 
            "The message sent on "<>DateString[Lookup[as,"OriginatingDate",DateObject[]]]<>" to "<>
            ToString[Lookup[as,"ReceiverAddress",""]]<>" with subject \""<>ToString[Lookup[as,"Subject",""]]<>"\" has been received."]]
]
   
sendreceipt[_]:=Null

handleReceipt[fun_, as_]:=Block[{from, res},
    res=fun[as];
    If[TrueQ[res],
       from=getReturnReceiptAddress[as];
       sendreceipt[from,as]
    ]
]

getReplyAddress[as_]:=Block[{address},
    address=Lookup[as,"ReplyTo"];
    If[StringQ[address],
        address=Quiet[takeAddress[address]]
    ];
    If[!StringQ[address],
        address=Lookup[as,"ReturnPath"],
        If[StringQ[address],
            address=Quiet[takeAddress[address]]
        ]
    ];
    If[!StringQ[address],
        address=Lookup[as,"From"],
        If[StringQ[address],
            address=Quiet[takeAddress[address]]
        ]
    ];
    address
]

getReturnReceiptAddress[as_]:=Block[{address},
    address=checkHeaders[as,"DispositionNotificationTo"];
    If[StringQ[address],
        address=Quiet[takeAddress[address]]
    ];
    If[!StringQ[address],
        address=checkHeaders[as,"ReturnReceiptTo"],
        If[StringQ[address],
            address=Quiet[takeAddress[address]]
        ]
    ];
    address
]

mbox11Q:=(mbox11Q=TrueQ[$VersionNumber>=10.5])
importMBOX[file_]:=With[{wdf=Import[file, {"MBOX", {"MessageElements", "HeaderRules"},  1}]},
	If[ListQ[wdf],
		{Association[wdf],Import[file, "String"]}
		,
		Throw[$Failed]
	]
]/;mbox11Q

importMBOX[file_]:=With[{wdf=Import[file,"MBOX"]},
	If[Length[wdf]>0,
		{Association[First[wdf]],Import[file,"String"]}
		,
		Throw[$Failed]
	]
]
importStringMBOX[str_]:=With[{wdf=ImportString[str, {"MBOX", {"MessageElements", "HeaderRules"}, 1}]},
	If[ListQ[wdf],
		{Association[wdf],ImportString[str, "String"]}
		,
		Throw[$Failed]
	]
]/;mbox11Q

importStringMBOX[str_]:=With[{wdf=ImportString[str,"MBOX"]},
	If[Length[wdf]>0,
		{Association[First[wdf]],str}
		,
		Throw[$Failed]
	]
]

(* importing *)
importmail[$Failed,_]:=$Failed
importmail[as_Association,_]:={as,Missing["NotAvailable"]}/;KeyExistsQ[as,"From"]
importmail[as_Association,_]:=(Message[System`MailReceiverFunction::nfrom];Throw[$Failed])
importmail[file_System`File,rest_]:=importmailfile[First[file],rest]
importmailfile[file_String,rest_]:=If[StringQ[FindFile[file]],
    With[{res=importMBOX[file]},
        If[AssociationQ[res[[1]]],
            {First[importmail[res[[1]],rest]],res[[2]]},
            (Message[System`MailReceiverFunction::nomail,file];Throw[$Failed])
        ]
    ]
    ,
    (Message[System`MailReceiverFunction::nfile, file];Throw[$Failed])
]/;MatchQ[FileExtension[file],"MBOX"|"mbox"]
    
importmail[str_String,rest_]:=importmailString[str]

importmailString[str_]:=With[{res=importStringMBOX[str]},
        If[AssociationQ[res[[1]]],
            {res[[1]],res[[2]]},
            (Message[System`MailReceiverFunction::invmail,str];Throw[$Failed])
        ]
    ]



importmail[l:{_Rule...},rest_]:=importmail[Association[l],rest]
importmail[mail_,_]:=(Message[System`MailReceiverFunction::invmail,mail];Throw[$Failed])


addMailReceiverKeys[slots_,as0_, rawmail_,receiveraddress_]:=Block[{as=as0},
	as["Attachments"]=takeattachments[as0["Attachments"]];
	as["AttachmentNames"]=takenames[as0["Attachments"]];
	If[!KeyExistsQ[as,#],
		(as[#]=getMailReceiverValue[#,as, rawmail, receiveraddress])
	]&/@slots;
	as	
]

getMailReceiverValue["ToList",as_, _, receiveraddress_]:=If[KeyExistsQ[as,"To"],
                DeleteCases[Flatten[{as["To"]}],receiveraddress],
                "ToList"->{}
            ]

getMailReceiverValue["CcList",as_, _, receiveraddress_]:=If[KeyExistsQ[as,"Cc"],
                DeleteCases[Flatten[{as["Cc"]}],receiveraddress],
                toemaillist[checkHeaders[as,{"Cc","CC"}]]
            ]    
getMailReceiverValue["Body",as_, _, _]:=as["Data"]            
getMailReceiverValue["ReceiverAddress",_, _, receiveraddress_]:=receiveraddress
getMailReceiverValue["FromAddress",as_, _, _]:=takeAddress[as["From"]]
getMailReceiverValue["FromName",as_, _, _]:=takeName[as["From"]]
getMailReceiverValue["AttachmentData",as_, rawmail_, _]:=Replace[
	Quiet[ImportString[rawmail,{"MBOX","AttachmentData",1}]],
	Except[_List]->Missing["NotAvailable"],{0}]

getMailReceiverValue["AttachmentAssociations",as_, rawmail_, _]:=Replace[
	Quiet[ImportString[rawmail,{"MBOX","AttachmentAssociations",1}]],
	Except[_List]->{},{0}]

getMailReceiverValue["ToAddressList",as_, _, receiveraddress_]:=
	If[KeyExistsQ[as,"ToList"],
		DeleteCases[takeAddress[as["ToList"]],receiveraddress]
		,
		{}
	]

getMailReceiverValue["ToNameList",as_, _, receiveraddress_]:=
	If[KeyExistsQ[as,"ToList"],
		takeName[as["ToList"]]
		,
		{}
	]

getMailReceiverValue["CcAddressList",as_, _, receiveraddress_]:=
	If[KeyExistsQ[as,"CcList"],
		DeleteCases[takeAddress[as["CcList"]],receiveraddress]
		,
		{}
	]
	
getMailReceiverValue["CcNameList",as_, _, receiveraddress_]:=
	If[KeyExistsQ[as,"CcList"],
		takeName[as["CcList"]]
		,
		{}
	]
getMailReceiverValue["ReturnPath",as_, _, _]:=checkHeaders[as,"Return-Path"]
getMailReceiverValue["ReplyTo",as_, _, _]:=If[
	KeyExistsQ[as,"ReplyToList"],
        as["ReplyToList"],
        checkHeaders[as,"Reply-To"]
	]

getMailReceiverValue["ReplyToAddress",as_, _, _]:=If[
	KeyExistsQ[as,"ReplyToAddressList"],
        as["ReplyToAddressList"],
        takeAddress[checkHeaders[as,"Reply-To"]]
	]

getMailReceiverValue["ReplyToMessageID",as_, _, _]:=checkHeaders[as, "In-Reply-To"]
getMailReceiverValue["MessageID",as_, _, _]:=If[KeyExistsQ[as,"MessageId"],
	as["MessageId"],
	checkHeaders[as, "Message-ID"]
]

getMailReceiverValue["ReferenceMessageIDList",_, _, _]:={}
getMailReceiverValue["ReturnReceiptRequested",as_, _, _]:=StringQ[getReturnReceiptAddress[as]]
getMailReceiverValue["Precedence",as_, _, _]:=Lookup[Lookup[as,"Headers",Association[]],"Precedence",Missing["NotAvailable"]]
getMailReceiverValue["DeliveryChainHostnames",as_, _, _]:=takeHostnames[Flatten[{checkHeaders[as, "Received"]}]]

getMailReceiverValue["DeliveryChainRecords",as_, _, _]:=checkHeaders[as, "Received"]
getMailReceiverValue["HeaderString",as_, rawmail_, _]:=If[StringQ[rawmail],
	If[mbox11Q,
   		ImportString[rawmail,{"MBOX","HeaderString",1}]
   		,
   		First[StringSplit[rawmail,"\n\n"]],Missing["NotAvailable"]
	]
]

getMailReceiverValue["HeaderRules",as_, rawmail_, _]:=If[KeyExistsQ[as,"Headers"],
   Normal[as["Headers"]],
   If[mbox11Q,
   		ImportString[rawmail,{"MBOX","HeaderRules",1}],
	   If[StringQ[as["HeaderString"]],
	       headerrules[as["HeaderString"]]
	       ,
	       Replace[
				Quiet[headerrules[If[StringQ[rawmail],First[StringSplit[rawmail,"\n\n"]],Missing["NotAvailable"]]]],
			Except[_List]->Missing["NotAvailable"],{0}]
	                          
	   ]
   ]
]
            
getMailReceiverValue["CharacterEncoding",as_, _, _]:=takecharset[checkHeaders[as, "ContentType"]]
getMailReceiverValue["ContentType",as_, _, _]:=takecontenttype[checkHeaders[as, "Content-Type"]]

getMailReceiverValue["OriginatingMailClient",as_, _, _]:=checkHeaders[as, {"User-Agent","x-mailer"}]
getMailReceiverValue["OriginatingIPAddress",as_, _, _]:=takelast[checkHeaders[as, "Received"],"IPAddress"]
getMailReceiverValue["OriginatingHostname",as_, _, _]:=Last[takeHostnames[Flatten[{checkHeaders[as, "Received"]}]]]
getMailReceiverValue["OriginatingLocation",as_, _, _]:=If[KeyExistsQ[as,"OriginatingIPAddress"],
	findGeoLocation[as["OriginatingIPAddress"]],
	findGeoLocation[takelast[checkHeaders[as, "Received"],"IPAddress"]]
]
getMailReceiverValue["OriginatingCountry",as_, _, _]:=If[KeyExistsQ[as,"OriginatingIPAddress"],
	geoCountry[as["OriginatingIPAddress"]],
	geoCountry[takelast[checkHeaders[as, "Received"],"IPAddress"]]
]


getMailReceiverValue["OriginatingDate",as_, _, _]:=Interpreter["DateTime"][checkHeaders[as, "Date"]]
getMailReceiverValue["OriginatingTimezone",as_, _, _]:=takeTimeZone[checkHeaders[as, "Date"]]
getMailReceiverValue["ServerOriginatingDate",as_, _, _]:=takelast[Flatten[{checkHeaders[as, "Received"]}],"DateTime"]
getMailReceiverValue["ServerOriginatingTimezone",as_, _, _]:=takelast[Flatten[{checkHeaders[as, "Received"]}],"TimeZone"]

getMailReceiverValue["ReceiverRoutingType",as_, _, receiveraddress_]:=getroutingtype[receiveraddress, as]
getMailReceiverValue["Association",as_, rawmail_, _]:=If[mbox11Q,
	Replace[Quiet[ImportString[rawmail,{"MBOX","Association",1}]],
		Except[_Association]->as,{0}],
	as]



getMailReceiverValue[elem_,as_, rawmail_, _]:=importMBOXElement[elem, rawmail]/;!KeyExistsQ[as,elem]&&mboxElementQ[elem, rawmail]

$deprecatedMBOXelements={"RawData","ReplyTo","To","Cc","CC","Bcc","BCC","Date","Data","RawAttachments","EmailClient","MessageSummary","AttachmentSummary"};
$unsupportedMBOXelements={"MessageCount"};

mboxElementQ[elem_, rawmail_]:=True/;MemberQ[$deprecatedMBOXelements,elem]&&mbox11Q
mboxElementQ[elem_, rawmail_]:=False/;MemberQ[$unsupportedMBOXelements,elem]

mboxElementQ[elem_, rawmail_]:=MemberQ[ImportString[rawmail,{"MBOX","Elements"}],elem]/;mbox11Q
mboxElementQ[__]:=False

importMBOXElement[elem_, rawmail_]:=With[{res=ImportString[rawmail,{"MBOX",elem,1}]},
	If[FailureQ[res],
		Missing["NotAvailable"],
		res
	]
]

getMailReceiverValue[___]:=Missing["NotAvailable"]

$mrfParameterOrder={"From","FromAddress","Subject","Body","Attachments","AttachmentData",
       "ReceiverAddress","ReceiverRoutingType","FromName","ToList","ToAddressList","ToNameList",
       "CcList","CcAddressList","CcNameList","ReturnPath","ReplyTo","ReplyToAddress","DoNotReply",
       "Answered","NewBodyContent","QuotedContent","ContentList","ContentAssociationList",
       "ForwardedContent","ThreadFromList","ThreadFromAddressList","Attachments","AttachmentNames",
       "AttachmentAssociations","MessageID","ReplyToMessageID","ReferenceMessageIDList","Precedence",
       "ReturnReceiptRequested","DeliveryChainHostnames","DeliveryChainRecords","HeaderString",
       "HeaderRules","CharacterEncoding","ContentType","OriginatingMailClient","OriginatingIPAddress",
       "OriginatingHostname","OriginatingCountry","OriginatingDate","OriginatingTimezone",
       "ServerOriginatingDate","ServerOriginatingTimezone","Document","Association"};

sortMRFKeys[as_Association] := Join[KeyTake[as, $mrfParameterOrder], KeyDrop[as, $mrfParameterOrder]]
sortMRFKeys[expr_]:=Missing["NotAvailable"]
(* typesetting *)
setMRFTypesetting[co_, email_]:=(
    Unprotect[CloudObject];
    With[{c = co,link=ToBoxes[Hyperlink[email,"mailto:"<>email]] }, c /: MakeBoxes[c, form : StandardForm | TraditionalForm] := 
        InterpretationBox[RowBox[{"CloudObject", "[", "\"mailto:\"",
            link
            , "]"}], co]];
    Protect[CloudObject]
)

(* parameter utilities *)
takeAddress[addresses:(_List|_String)]:=Interpreter["EmailAddress"][addresses]
takeAddress[expr_]:=expr

takeName[str_String]:=First[takeName[{str}]]
takeName[addresses_List]:=With[{split=StringSplit[addresses,"<"]},
    If[Length[#]>1,StringTrim[First[#]],Missing["NotAvailable"]]&/@split]
takeName[expr_]:=expr

takeHostnames[str_]:=takeHostnames[StringSplit[str,"Received"]]
takeHostnames[l_List]:=DeleteDuplicates[takeHostnames1/@Flatten[StringSplit[l,"\n"]]]
takeHostnames[expr_]:=expr

takeHostnames1[line_]:=StringTrim[StringReplace[line,{"from"->"","by"->"",("("~~___~~EndOfString)->""}]]

takecharset[str_String]:=If[StringFreeQ[str,"charset"],Missing["NotAvailable"],
    StringTrim[StringTrim[First[StringCases[str,"charset"~~x:(__)~~(Whitespace|EndOfString):>x]]],"="]
]
takecharset[expr_]:=expr

takecontenttype[str_String]:=First[StringSplit[str,";"]]
takecontenttype[expr_]:=expr

takelast[str_, type_]:=takelast[StringSplit[str,"from"], type]
takelast[l_List, type_]:=takelast1[Last[l], type]
takelast[expr_, _]:=expr

takelast1[received_, type_]:=With[{res=takelast2[Switch[type,"IPAddress",First,_,Last][StringSplit[received,{"\n", "by", "for"}]], type]},
    If[Length[res]>0,First[res],Missing["NotAvailable"]]
]
takelast2[line_, "IPAddress"]:=Interpreter["IPAddress"][StringCases[line,(ip : ((HexadecimalCharacter | "." | ":") ...) /; (stringLength[ip] > 5)):>ip]]
takelast2[line_, "DateTime"]:= Replace[
    Interpreter["DateTime"][StringCases[line,";"~~date:(__)~~("("|EndOfString):>date]],
    _Failure->Missing["NotAvailable"]
]
takelast2[line_, "TimeZone"]:= Replace[
    Interpreter["TimeZone"][StringCases[line,tz : (("-" | "+") ~~ (DigitCharacter ...)):>tz]], 
    _Failure->Missing["NotAvailable"]
]


takelastTimestamp[str_]:=takelastTimestamp[StringSplit[str,"from"]]
takelastTimestamp[l_List]:=takelastTimestamp1[Last[l]]
takelastTimestamp[expr_]:=expr

takelastTimestamp1[received_]:=With[{res=takelastTimestamp2[First[StringSplit[received,"\n"]]]},
    If[Length[res]>0,First[res],Missing["NotAvailable"]]
]
takelastTimestamp2[line_]:=Interpreter["DateTime"][StringCases[line,";"~~date:(__)~~("("|EndOfString):>date]]

takeTimeZone[do_DateObject]:=Lookup[Options[do, TimeZone], TimeZone, Missing["NotAvailable"]]
takeTimeZone[str_String]:=takelast2[str, "TimeZone"]
takeTimeZone[_]:=Missing["NotAvailable"]

takeattachments[l:{_Rule...}]:=Last/@l
takeattachments[l_List]:=l
takeattachments[as_Association]:=Values[as]
takeattachments[l:{_Association...}]:=Flatten[Lookup[l,"Content",{}]]
takeattachments[expr_]:=expr
takeattachments[expr_]:={}

takenames[l:{_Rule...}]:=First/@l
takenames[as_Association]:=Keys[as]
takenames[l:{_Association...}]:=Flatten[Lookup[l,"Name",{}]]
takenames[___]:={}

findGeoLocation[str_String]:=FindGeoLocation[str]
findGeoLocation[expr_]:=Missing["NotAvailable"]

geoCountry[HoldPattern[pos_GeoPosition]]:=GeoNearest[Entity["Country"],pos]
geoCountry[_]:=Missing["NotAvailable"]

getAttachments[attachmentdata_]:=Missing["NotAvailable"] (* TODO *)
getroutingtype[receiveraddress_String, as_]:=Which[
    !(And@@StringFreeQ[Lookup[as,"ToList",{}],receiveraddress]),"To",
    !(And@@StringFreeQ[Lookup[as,"CcList",{}],receiveraddress]),"Cc",
    True,"Bcc"
]

getroutingtype[_, _]:=Missing["NotAvailable"]

checkHeaders[as_,headers_List]:=Catch[(If[KeyExistsQ[as,#],Throw[as[#],"checkheaders"]]&/@headers;
    If[KeyExistsQ[as,"Headers"],checkHeaders[as["Headers"],headers],Missing["NotAvailable"]]),"checkheaders"]
checkHeaders[as_,header_]:=as[header]/;KeyExistsQ[as,header]
checkHeaders[as_,header_]:=Lookup[as["Headers"],header]/;KeyExistsQ[as,"Headers"]
checkHeaders[__]:=Missing["NotAvailable"]

headerrules[headerstr_]:=With[{rules=Quiet[Rule @@@ StringTrim/@StringSplit[
    StringJoin /@  Partition[Rest[StringSplit[headerstr, 
        "\n" ~~ (x : Except[WhitespaceCharacter]) :> x]], 2], ":", 2]]},
    If[MatchQ[rules,{_Rule...}],
        rules,
        Missing["NotAvailable"]
    ]
]
  
sendmailmrf[]:=If[(Length[DownValues[CloudSystem`SendMail`Private`sendMailMRF]]>0)&&$CloudEvaluation,
    CloudSystem`SendMail`Private`sendMailMRF,
    SendMail    
]

toemaillist[str_String]:=StringTrim[StringSplit[str,","]]
toemaillist[_]:={}

stringLength[str_String]:=StringLength[str]
stringLength[_]:=0

End[]

EndPackage[]
