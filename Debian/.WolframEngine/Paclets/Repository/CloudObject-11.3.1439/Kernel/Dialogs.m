(* ::Package:: *)

(* ::Section::Closed:: *)
(*Notes*)


(* ::Text:: *)
(*For v11, the loginDialog has been redesigned and the FrontEndResources are being moved into the CloudObject paclet. And because the v10.x.x FrontEnd does not support the new v11 TextResource file TextResources/CloudLoginStrings.tr, Dialogs.m must include the Legacy (i.e., pre-v11) dialog as well as v11's. *)


(* ::Text:: *)
(*For FrontEnd versions that are v11 or newer, Dialogs.m will launch the v11 dialog. Otherwise, the Legacy dialog appears. So both v10 and v11 versions of the dialog are supported in this package.  --larrya, 2016/06/28*)


(* ::Section::Closed:: *)
(*Header*)


BeginPackage["CloudObject`"];

Begin["`Private`"];


(* ::Section::Closed:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*Initial Settings*)


(* ::Subsubsection::Closed:: *)
(*Colors*)


(* ::Subsubsubsection::Closed:: *)
(*Dialog*)


(*Background color for the dialog*)
$DialogBackgroundColor = RGBColor[1., 1., 1.];



(* ::Subsubsubsection::Closed:: *)
(*Banners*)


(*Title Banner*)
$TitleBannerBackgrounColor = RGBColor[0.2, 0.2, 0.2];

(*Error Banner*)
$ErrorPanelBackgrounColor = RGBColor[1., 0.9254901960784314, 0.5176470588235293];


(* ::Subsubsubsection::Closed:: *)
(*Label Text*)


(*Text*)
$SubheaderColor = RGBColor[0.8666666666666667, 0.06666666666666667, 0.];

$DefaultColor = RGBColor[0.39215686274509803, 0.39215686274509803, 0.39215686274509803];
$HoverColor = RGBColor[0.996078431372549, 0., 0.];

(*Error message colors*)
$ErrorMessageColorInControlArea = $SubheaderColor;
$ErrorMessageColorInErrorPanel = RGBColor[0.2, 0.2, 0.2];

$RememberMeTextColor = $DefaultColor;

$ForgotPasswordDefaultTextColor = $DefaultColor;
$ForgotPasswordHoverTextColor = $HoverColor;

$NoWolframIDTextColor = $DefaultColor;
$CreateWolframIDDefaultTextColor = RGBColor[0.8666666666666667, 0.06666666666666667, 0.];
$CreateWolframIDHoverTextColor = RGBColor[0.996078431372549, 0., 0.];


(* ::Subsubsubsection::Closed:: *)
(*InputField*)


(*Color of Text in InputFields*)
$InputTextColor = RGBColor[0.2, 0.2, 0.2];


(* ::Subsubsubsection::Closed:: *)
(*Buttons*)


(*SignIn button text*)
$SignInButtonTextColor = RGBColor[1., 1., 1.];
$SignInButtonDisabledTextColor = RGBColor[0.8, 0.8, 0.8];

(*Cancel button text*)
$CancelButtonTextColor = $DefaultColor;
$CancelButtonPressedTextColor = RGBColor[1., 1., 1.];
(*$CancelButtonDisabledTextColor = RGBColor[1., 1., 1., 0.5];*)


(* ::Subsubsubsection::Closed:: *)
(*Cloud Server Selector (unfinished)*)


(*Color of text in CloudServer pull-down-menu when hovered over.*)
$MenuHoverTextColor = RGBColor[0.6509803921568628, 0.6509803921568628, 0.6509803921568628];


(* ::Subsubsubsection::Closed:: *)
(*Divider line*)


$DividerColor = RGBColor[0.8980392156862745, 0.8980392156862745, 0.8980392156862745];


(* ::Subsubsection::Closed:: *)
(*Sizes and Spacings*)


(* ::Subsubsubsection::Closed:: *)
(*Dialog (overall)*)


$DialogWidth = 392;
$LeftDialogMargin = 41;
$RightDialogMargin = $LeftDialogMargin;

$DividerWidth = 330;
$LeftDividerMargin = 31;
$RightDividerMargin = $LeftDialogMargin;


(* ::Subsubsubsection::Closed:: *)
(*Font Sizes*)


$SubheaderFontSize = 18;

$InputFieldHeaderFontSize = 14;
$InputFieldFontSize = $InputFieldHeaderFontSize;

$ErrorFontSizeInControlArea = 10;
$ErrorFontSizeInPanelArea = 12;

$RememberMeFontSize = 12;

$ButtonFontSize = 12;

$ForgotPasswordFontSize = $RememberMeFontSize;

$CloudUserIDFontSize = 14;


(* ::Subsubsubsection::Closed:: *)
(*Font Family*)


$BaseFontFamily = "Source Sans Pro";


(* ::Subsubsubsection::Closed:: *)
(*Title Banner*)


$TitleBannerHeight = 44;
$IconToTextGutterSpace = 14;


(* ::Subsubsection::Closed:: *)
(*Spacing utilities*)


spacr[vertspc_] := Spacer[{30, vertspc}]


(* ::Subsection::Closed:: *)
(*FE Resources*)


(* ::Subsubsection::Closed:: *)
(*Load the Paclet's TextResources (FlushTextResourceCaches)*)


(* ::Text:: *)
(*MathLink`CallFrontEnd[FrontEnd`FlushTextResourceCaches[]] should only evaluate in a Desktop FrontEnd whose version is >= 11. It should not evaluate in standalone kernel or a cloud FE.*)


If[TrueQ[$Notebooks && Not[$CloudEvaluation] && BoxForm`sufficientVersionQ[11]],
	MathLink`CallFrontEnd[FrontEnd`FlushTextResourceCaches[]]
]; (*Flush the TextResource caches in order to load the paclet's TR. *)


(* ::Subsubsection::Closed:: *)
(*Locations*)


(* ::Subsubsubsection::Closed:: *)
(*Bitmap Directory Path*)


(*Relative directory path to bitmaps*)
$CloudLoginBitmapsDirectory = If[BoxForm`sufficientVersionQ[11], {"Dialogs", "CloudLogin", "v2.0"}, {"Dialogs", "CloudLogin"}];


(* ::Subsubsubsection::Closed:: *)
(*Strings (@@resource)*)


(*Relative directory path to bitmaps*)
$CloudLoginTextResource = If[BoxForm`sufficientVersionQ[11], "CloudLoginDialogStrings", "CloudLoginDialog"];


(* ::Subsubsection::Closed:: *)
(*Text Resources*)


tr[resource_String, id_] := Dynamic@RawBoxes@FEPrivate`FrontEndResource[resource, id];

tr[id_] := tr[$CloudLoginTextResource, id]


(* ::Subsubsection::Closed:: *)
(*Image Resources*)


imgr[relPth_List, flNm_String] := FrontEnd`FileName[relPth, flNm];

imgr[flNm_String] := imgr[$CloudLoginBitmapsDirectory, flNm];


imgimportr[relPth_List, flNm_String] := 
  Dynamic[RawBoxes@
    FEPrivate`ImportImage[FrontEnd`ToFileName[relPth, flNm]]];
    
imgimportr[flNm_String] := imgimportr[$CloudLoginBitmapsDirectory, flNm]



CloudDialogImage["CheckboxOn"] := imgimportr["CheckboxOn.png"];

CloudDialogImage["CheckboxOff"] := imgimportr["CheckboxOff.png"];

CloudDialogImage["TopBanner"] := imgr["Banner.9.png"];

CloudDialogImage["SigninButton","Default"] := imgr["SigninButton-Default.9.png"];
                    
CloudDialogImage["SigninButton","Hover"] := imgr["SigninButton-Hover.9.png"];
                    
CloudDialogImage["SigninButton","Pressed"] := imgr["SigninButton-Pressed.9.png"];

CloudDialogImage["SigninButton","Disabled"] := imgr["SigninButton-Disabled.9.png"];

CloudDialogImage["CancelButton","Default"] := imgr["CancelButton-Default.9.png"];
                    
CloudDialogImage["CancelButton","Hover"] := imgr["CancelButton-Hover.9.png"];
                    
CloudDialogImage["CancelButton","Pressed"] := imgr["CancelButton-Pressed.9.png"];

CloudDialogImage["CancelButton","Disabled"] := imgr["CancelButton-Disabled.9.png"];


(*Used in Legacy FrontEnd Only*)

CloudDialogImage["CloudLogoIcon"] := imgimportr["CloudLogoIcon.png"];

CloudDialogImage["BackgroundImage"] := imgr["Background.9.png"];

CloudDialogImage["JoinNowButton","Default"] := imgr["JoinNowButton-Default.9.png"];
                
CloudDialogImage["JoinNowButton","Hover"] := imgr["JoinNowButton-Hover.9.png"];
                
CloudDialogImage["JoinNowButton","Pressed"] := imgr["JoinNowButton-Pressed.9.png"];



(* ::Subsection::Closed:: *)
(*Labels*)


(* ::Subsubsection::Closed:: *)
(*Inline Styles*)


(* ::Subsubsubsection::Closed:: *)
(*Specific Styles that Are Derived from the following "Base Text Styles"*)


styledtxt[txt_, "subheader"] := styledtxt[txt, "basefont", FontSize -> $SubheaderFontSize, FontColor -> $SubheaderColor];

styledtxt[txt_, "errorInControlArea"] := styledtxt[txt, "errormssg", FontSize -> $ErrorFontSizeInControlArea, FontColor -> $ErrorMessageColorInControlArea];
styledtxt[txt_, "errorInErrorPanel"] := styledtxt[txt, "errormssg", FontSize -> $ErrorFontSizeInPanelArea, FontColor -> $ErrorMessageColorInErrorPanel];

styledtxt[txt_, "rememberme"] := styledtxt[txt, "basefont", FontSize -> $RememberMeFontSize, FontColor -> $RememberMeTextColor, LineBreakWithin -> False];

styledtxt[txt_, "forgotpassword", "default"] := styledtxt[txt, "forgotpassword", $ForgotPasswordDefaultTextColor];
styledtxt[txt_, "forgotpassword", "hover"] := styledtxt[txt, "forgotpassword", $ForgotPasswordHoverTextColor];

styledtxt[txt_, "wolframid", "leadin"] := styledtxt[txt, "wolframid", $NoWolframIDTextColor];
styledtxt[txt_, "wolframidlink", "default"] := styledtxt[txt, "wolframid", $CreateWolframIDDefaultTextColor];
styledtxt[txt_, "wolframidlink", "hover"] := styledtxt[txt, "wolframid", $CreateWolframIDHoverTextColor];


(* ::Subsubsubsection::Closed:: *)
(*Base Text Styles*)


styledtxt[txt_, "errormssg", opts___] := styledtxt[txt, "basefont", LineBreakWithin -> Automatic, LineIndent -> 0, TextAlignment -> Center, Hyphenation -> False, opts];

styledtxt[txt_, "forgotpassword", colr_] := styledtxt[txt, "basefont", FontSize -> $ForgotPasswordFontSize, LineBreakWithin -> False, FontColor -> colr];

styledtxt[txt_, "buttons", colr_] := styledtxt[txt, "basefont", FontSize -> $ButtonFontSize, LineBreakWithin -> False, FontColor -> colr];

styledtxt[txt_, "wolframid", colr_] := styledtxt[txt, "basefont", FontSize -> $CloudUserIDFontSize, LineBreakWithin -> False, FontColor -> colr];

styledtxt[txt_, opts___] := styledtxt[txt, "basefont", FontSize -> $InputFieldHeaderFontSize, FontColor -> $DefaultColor, LineBreakWithin -> False, opts];

styledtxt[txt_, "basefont", opts___] := Style[txt, FontFamily -> $BaseFontFamily, FontWeight -> "Regular", LineSpacing -> {1, 0}, AutoSpacing -> False, opts];


(* ::Subsubsection::Closed:: *)
(*Mouseover Styles*)


mouseovertxt[txt_, styleID_:"forgotpassword"] := PaneSelector[
	{
		False -> styledtxt[txt, styleID, "default"],
		True -> styledtxt[txt, styleID, "hover"]
	},
	FrontEnd`CurrentValue["MouseOver"]
];


(* ::Subsection::Closed:: *)
(*Divider*)


dividerLine = Item[Graphics[{$DividerColor, Rectangle[]}, ImageSize -> {$DividerWidth, 1}, AspectRatio -> Full], Alignment -> Center];


(* ::Subsection::Closed:: *)
(*Controls*)


(* ::Subsubsection::Closed:: *)
(*InputFields*)


inptFld[dyn:Dynamic[expr_], boxid_, fldtype_, opts:OptionsPattern[]] := InputField[Dynamic[expr], fldtype, 
	ContinuousAction -> True,  
	System`BoxID -> boxid, 
	ImageSize -> {Full, Automatic},
	BaseStyle -> {
		FontFamily -> $BaseFontFamily,
		FontWeight -> "Regular",
		FontColor -> $InputTextColor,
		FontSize -> $InputFieldFontSize 
	},
	opts];

loginFld[dyn:Dynamic[expr_], boxid_, opts:OptionsPattern[]] := inptFld[dyn, boxid, String, opts];

loginFld[dyn:Dynamic[uname_], boxid_, "username"] := loginFld[dyn, boxid];

loginFld[dyn:Dynamic[pwd_], boxid_, "password"] := loginFld[dyn, boxid, FieldMasked -> True];


(* ::Subsubsection::Closed:: *)
(*Buttons*)


button[label_, "hyperlink", events_] := MouseAppearance[button[label, events], "LinkHand"];

button[label_, events_] := EventHandler[
	label,
	events,
	PassEventsDown -> True
]


SetAttributes[loginbtn, HoldRest];

loginbtn[label_, appearance_List, function_, opts:OptionsPattern[]] := Button[label,
	function,
	Appearance -> appearance,
	FrameMargins -> {{10, 10}, {0, 0}},
	ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
	Alignment -> {Center, Center},
	opts
];

loginbtn["SignInButton", btnfunction_, btnenabledfunction_] := loginbtn[enabledColorToggle[tr["SignInButtonLabel"], $SignInButtonTextColor, $SignInButtonDisabledTextColor, btnenabledfunction], 
	{
		"Default" -> CloudDialogImage["SigninButton", "Default"], 
		"Hover" -> CloudDialogImage["SigninButton", "Hover"], 
		"Pressed" -> CloudDialogImage["SigninButton", "Pressed"],
		"Disabled" -> CloudDialogImage["SigninButton","Disabled"],
		"ButtonType" -> "Default"
	}, 
	btnfunction,
	Enabled -> btnenabledfunction
];

loginbtn["CancelButton", btnfunction_] := loginbtn[styledtxt[tr["CancelButtonLabel"], "buttons", $CancelButtonTextColor], 
	{
		"Default" -> CloudDialogImage["CancelButton", "Default"], 
		"Hover" -> CloudDialogImage["CancelButton", "Hover"], 
		"Pressed" -> CloudDialogImage["CancelButton", "Pressed"],
		(*"Disabled" -> CloudDialogImage["CancelButton","Disabled"],*)
		"ButtonType" -> "Cancel"
	}, 
	btnfunction
]


SetAttributes[enabledColorToggle, HoldAll];
enabledColorToggle[label_, enabledColr_, disabledColr_, btnenabledfunction_] := PaneSelector[
	{
		True -> styledtxt[label, "buttons", enabledColr],
		False -> styledtxt[label, "buttons", disabledColr]
	},
	btnenabledfunction
]


signinbtns[Dynamic[logincreds_], btnenabledfunction_] := Grid[
	{
		{
			loginbtn["SignInButton", DialogReturn[logincreds], btnenabledfunction],
			loginbtn["CancelButton", DialogReturn[MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionCancelled[]; logincreds = $Canceled]]
		}
	}
]


(* ::Subsection::Closed:: *)
(*Composites*)


inFldGrid[header_, inFld_, opts:OptionsPattern[]] := Grid[{
		{header},
		{inFld}
	},
	Alignment -> Left
];

inFldGrid[dyn:Dynamic[symbol_], header_, type:("password" | "username"), boxid_, opts:OptionsPattern[]] := inFldGrid[header, loginFld[dyn, boxid, type], opts]


chkbox[dyn_Dynamic] := Toggler[dyn,
		{
			True -> CloudDialogImage["CheckboxOn"],
			False -> CloudDialogImage["CheckboxOff"]
		}
]


(* ::Subsubsection::Closed:: *)
(*Messages*)


(*Certain error messages contain styled ButtonBoxes. To support both the legacy and v11+ styles, I added duplicate error messages for those messages that contain BBs. The style of the BB changed for v11. In v11+ errmssgTR[] converts the errorCode to a string and appends "v11" to any message that contains a button. The new ids are represented in FE/SystemFiles/TextResources/ErrorMessages.tr. At present, only error codes 1500 and 1600 contain BBs. So there replacement ids are "1500v11" and "1600v11", respectively.*)
SetAttributes[errmssgTR, HoldFirst];

errmssgTR[errorCode_] := PaneSelector[
	{
		True -> Dynamic[FrontEndResource["WolframCloudLoginErrors", "1600v11"], DestroyAfterEvaluation -> True],
		False -> Dynamic[FrontEndResource["WolframCloudLoginErrors", errorCode], DestroyAfterEvaluation -> True]
	},
	Dynamic[
		errorCode === 1600 && BoxForm`sufficientVersionQ[11]
	],
	Alignment -> {Center, Center},
	ImageSize -> {Full, Automatic}
]


SetAttributes[errmssg, HoldAll];
errmssg[message_, isWCUILogin_, stylesuffix_String, opts:OptionsPattern[]] := PaneSelector[
					{
						True -> errmssg[message, stylesuffix],
						False -> ""
					},
					Dynamic[isWCUILogin && errorCode != 0],
					(*ImageSize -> {Full, All},*)
					opts
				];

errmssg[message_, stylesuffix_String] := styledtxt[message, "errorIn"<>stylesuffix];


(* ::Subsubsection::Closed:: *)
(*Create Wolfram ID*)


createWolframID := Framed[
	Style[
		Row[
			{
				styledtxt[tr["CloudAccountQuery"], "wolframid", "leadin"], " ", button[mouseovertxt[tr["JoinNowButtonLabel"], "wolframidlink"], "hyperlink", "MouseClicked" :> FE`hyperlinkCoded["https://account.wolfram.com/create-account", "source=cloudlogindialog"]]
			}
		],
		LineBreakWithin -> False
	],
	ImageSize -> {Full, Automatic},
	Alignment -> {Center, Center},
	ImageMargins -> {{31, 31}, {0, 0}},
	FrameStyle -> None
]


(* ::Subsection::Closed:: *)
(*Title-Error Banner*)


titleBanner := Panel[Pane["", Full, Alignment -> Left], Appearance -> {"Default" -> CloudDialogImage["TopBanner"]}, Alignment -> Left];


SetAttributes[errorPanel, HoldFirst];
errorPanel[errorMessage_] := Framed[errorMessage,
	ImageSize -> {Full, Automatic},
	Alignment -> {Center, Center},
	FrameMargins -> {{20, 20}, {16, 16}},
	FrameStyle -> None,
	Background -> $ErrorPanelBackgrounColor
];

errorPanel[errorCode_, styleSuffix_] := errorPanel[errmssg[errorCode, styleSuffix]]


SetAttributes[errorTitleColumn, HoldFirst];

errorTitleColumn[message_] := Column[
	{
		errorPanel[message],
		titleBanner
	},
	ItemSize -> {Automatic, Automatic},
	Spacings -> {{0, 0}, {0, 0}}
]

errorTitleColumn[errorCode_?(IntegerQ[#] && # =!= 0&), _] := errorTitleColumn[errmssg[errmssgTR[errorCode], "ErrorPanel"]]
errorTitleColumn[_?(# === 0&), notifications_List?(Length[#] > 0&)] := errorTitleColumn[Column[errmssg[#, "ErrorPanel"]& /@ notifications]
	]
errorTitleColumn[_, _] := titleBanner


SetAttributes[titlePlusErrorPanel, HoldAll];
titlePlusErrorPanel[errorCode_, isWCUILogin_, notifications_] := PaneSelector[
	{
		False -> titleBanner,
		True -> errorTitleColumn[errorCode, notifications]
	},
	Dynamic[(isWCUILogin && errorCode != 0) || Length[notifications] > 0],
	ImageSize -> {Full, Automatic}
]



(* ::Section::Closed:: *)
(*Dialog*)


(* ::Subsection::Closed:: *)
(*FrontEnd Version >= 11*)

loginDialog[username_String] := loginDialog[username, {}] /; BoxForm`sufficientVersionQ[11]
loginDialog[username_String, notificationIds_List] := With[{boxid = "username", pwdboxid =" passwd", cloudLoginTR = $CloudLoginTextResource},
	Block[
		{
			$loginCredentials
		},
		Module[
			{
				uname = username,
				pwd = "",
				ctrlCol
			},

			Clear[$loginCredentials];

			errorCode = CurrentValue["WolframCloudLoginError"];
			isWCUILogin = TrueQ[CurrentValue["WolframCloudUILogin"]];
			notifications = Dynamic[FrontEndResource[$CloudLoginTextResource, #], DestroyAfterEvaluation -> True]& /@ notificationIds;
			

			If[Developer`UseFrontEnd[CurrentValue["UserInteractionEnabled"]],
						
				ctrlCol = Column[
					{
						styledtxt[tr["CloudSubheaderText"], "subheader"],

						spacr[26](*errmssg[errorCode, isWCUILogin, "ControlArea"]*),

						inFldGrid[Dynamic[uname],
							styledtxt[
								tr["WolframIDLabel"]
							],
							"username",
							boxid
						],

						spacr[28](*errmssg[errorCode, isWCUILogin, "ControlArea"]*),

						inFldGrid[Dynamic[pwd],
							styledtxt[tr["PasswordLabel"]],
							"password",
							pwdboxid
						],

						spacr[28](*errmssg[errorCode, isWCUILogin, "ControlArea"]*),

						DynamicWrapper[
							Grid[
								{
									{
										Grid[
											{
												{
													chkbox[Dynamic[CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}, True]]],
													button[styledtxt[tr["RemeberMeLabel"], "rememberme"],
														"MouseClicked" :> (
															FEPrivate`Set[
																FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}],
																FEPrivate`Not[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}, True]]
															]
														)
													]

												}
											},
											Alignment -> {Automatic, Center}
										]
									},
									{
										Grid[
											{
												{
													signinbtns[Dynamic[$loginCredentials],
														Dynamic[uname =!= "" && pwd =!= ""]
													],
													Pane[
														button[mouseovertxt[tr["ForgotPasswordLabel"]], "hyperlink",
															"MouseClicked" :> (FrontEndExecute[FrontEnd`NotebookLocate[{URL["https://account.wolfram.com/auth/forgot-password"], None}]])
														], 
														Full, 
														FrameMargins -> {{0, 1}, {0, 0}}, 
														Alignment -> {Right, Baseline}]
												}
											},
											ItemSize -> {{Scaled[0.58], Scaled[0.42]}, Automatic},
											Spacings -> {0, 0},
											Alignment -> {{Left, Right}, Automatic}
										]
									}
								},
								Alignment -> {Left, Automatic},
								Spacings -> {Automatic, 1}
							],
							$loginCredentials = {uname, pwd}
						]
					},
					Alignment -> Left,
					Spacings -> {0, 0},
					ItemSize -> {Automatic, Automatic}
				];

				DialogInput[
					ExpressionCell[
						Framed[
							Column[
								{
									CloudObject`Private`titlePlusErrorPanel[CloudObject`Private`errorCode, CloudObject`Private`isWCUILogin, CloudObject`Private`notifications](*titleBanner*),
									spacr[34],
									Pane[ctrlCol, {Full, All},
										FrameMargins -> {{$LeftDialogMargin, $RightDialogMargin}, {0, 0}}
									],
									spacr[26],
									dividerLine,
									spacr[13],
									createWolframID,
									spacr[26]
								},
								Spacings -> {0, 0}
							],
							ImageSize -> {Full, Full},
							FrameMargins -> 0,
							ImageMargins -> {{0, 0}, {-3, -1}},
							FrameStyle -> None
						],
						CellMargins -> {{-1, -5}, {0, -2}},
						CellFrameMargins -> 0
					],
					Background -> $DialogBackgroundColor,
					CellContext -> Notebook,
					DynamicUpdating -> True,
					DynamicEvaluationTimeout -> 100.,
					WindowTitle :> Dynamic[FEPrivate`FrontEndResource[cloudLoginTR, "WindowTitle"]],
					WindowSize -> {$DialogWidth, All}, 
					Modal -> True,
					NotebookDynamicExpression :> (
						Refresh[FrontEnd`MoveCursorToInputField[EvaluationNotebook[], If[uname === "", boxid, pwdboxid]], None]
					)
				];
				$loginCredentials,
				Return[$Canceled](*Else, no interactive FE*)
			]
		]
	]
] /; BoxForm`sufficientVersionQ[11];



(* ::Subsection::Closed:: *)
(*Legacy FrontEnd*)

loginDialog[username_String, _] := loginDialog[username] 
loginDialog[username_String, _] := loginDialog[username] 
loginDialog[username_String] := With[{btnlblStyle = {"DialogStyle", "ControlStyle", FontSize -> (Inherited*0.95)}, boxid = "username", pwdboxid="passwd", cloudLoginTR = $CloudLoginTextResource},
  Block[{$loginCredentials}, 
   Module[{uname = username, pwd = "", leftCol, rightCol, columns, errorCode = CurrentValue["WolframCloudLoginError"], isWCUILogin = TrueQ[CurrentValue["WolframCloudUILogin"]]
}, 
   	Clear[$loginCredentials];
    If[Developer`UseFrontEnd[CurrentValue["UserInteractionEnabled"]],
     leftCol = Column[{
        Column[{
          ExpressionCell[Row[{
             tr["WolframIDLabel1"](*"Wolfram ID "*)," ",
             Style[ tr["WolframIDLabel2"](*"(your email address)"*),FontSize -> (Inherited*0.85)]}], "DialogStyle","ControlStyle", FormatType -> TextForm],
             InputField[Dynamic[uname], String, ContinuousAction -> True,  System`BoxID -> boxid]
          }],
        Column[{
          TextCell[tr["PasswordLabel"](*"Password"*), "DialogStyle", "ControlStyle"],
          
          InputField[Dynamic[pwd], String, ContinuousAction -> True, 
           FieldMasked -> True, System`BoxID -> pwdboxid]
          }
         ],
		 DynamicWrapper[Grid[{
             {
			 EventHandler[
				PaneSelector[
					{
						False -> Style[tr["ForgotPasswordLabel"](*"Forgot your password?"*), btnlblStyle, Gray],
						True -> Style[tr["ForgotPasswordLabel"](*"Forgot your password?"*), btnlblStyle, RGBColor[0.878431,0.513725,0.133333]]
					},
					FrontEnd`CurrentValue["MouseOver"]
				],
				"MouseClicked" :> (FrontEndExecute[FrontEnd`NotebookLocate[{URL["https://account.wolfram.com/auth/forgot-password"], None}]])			
			 ]           
             },
             {
             Grid[{{
				Toggler[Dynamic[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"},True]],
				{
					True -> CloudDialogImage["CheckboxOn"],
					False -> CloudDialogImage["CheckboxOff"]
				}],
				EventHandler[Style[tr["RemeberMeLabel"], "DialogStyle", "ControlStyle"],
					"MouseClicked" :> (
						FEPrivate`Set[
							FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"}],
							FEPrivate`Not[FrontEnd`CurrentValue[$FrontEnd,{PrivateFrontEndOptions,"DialogSettings","Login","RememberMe"},True]]
						]
					)]
				}},
			 Alignment -> {Automatic, Center}]
             },
			{PaneSelector[
					{
						True -> Pane[
									Style[Dynamic[FrontEndResource["WolframCloudLoginErrors", errorCode], DestroyAfterEvaluation -> True], 
									LineIndent -> 0, RGBColor[0.9, 0.55, 0.32], LineSpacing -> {1, 0}], {Full, Full}, 
									Alignment -> {Left, Center},
									Scrollbars -> {False, Automatic},
									AppearanceElements -> {}             
								],
						False -> ""
					},
					Dynamic[isWCUILogin && errorCode != 0],
					ImageSize -> {Full(*220*), 50},
					ImageMargins -> {{0, 10}, {5, 0}}
				]
			}, (*Login error message display*)
			{
			 Grid[{{
             Button[
              
              Pane[Style[tr["SignInButtonLabel" ](*"Sign In"*), btnlblStyle, White], 
               ImageMargins -> {{10, 10}, {0, 0}}],
              DialogReturn[$loginCredentials],
              ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
              Appearance -> {
              	"Default" -> CloudDialogImage["SigninButton","Default"], 
              	"Hover" -> CloudDialogImage["SigninButton","Hover"], 
                "Pressed" -> CloudDialogImage["SigninButton","Pressed"],
				"ButtonType" -> "Default"}],

             Button[
              
              Pane[Style[tr["CancelButtonLabel" ](*"Sign In"*), btnlblStyle, RGBColor[0.266667,0.266667,0.266667]], 
               ImageMargins -> {{10, 10}, {0, 0}}],
              DialogReturn[MathLink`CallFrontEnd@FrontEnd`WolframCloud`ConnectionCancelled[]; $loginCredentials = $Canceled],
              ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
              Appearance -> {
              	"Default" -> CloudDialogImage["CancelButton","Default"], 
              	"Hover" -> CloudDialogImage["CancelButton","Hover"], 
                "Pressed" -> CloudDialogImage["CancelButton","Pressed"],
				"ButtonType" -> "Cancel"}]
			 }}]

			}},
		  Alignment -> {Left, Automatic},
		  Spacings -> {Automatic, 1}],
          $loginCredentials = {uname, pwd}]
        },
       Alignment -> Left,
       Spacings -> .5];
     rightCol = Pane[Column[{CloudDialogImage["CloudLogoIcon"],
        Style[tr["CloudAccountQuery"](*"Don't have a Wolfram Cloud account yet?"*), 
         "DialogStyle", "ControlStyle", FontSize -> (Inherited*0.9), LineSpacing -> {1, 0}],
		Button[
          
          Pane[Style[tr["JoinNowButtonLabel"](*"Join Now"*), btnlblStyle, White], 
           ImageMargins -> {{10, 10}, {0, 0}}],
          FE`hyperlinkCoded["https://account.wolfram.com/create-account", "source=cloudlogindialog"],
          ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
          Appearance -> {
          	"Default" -> CloudDialogImage["JoinNowButton","Default"], 
          	"Hover" -> CloudDialogImage["JoinNowButton","Hover"],
          	"Pressed" -> CloudDialogImage["JoinNowButton","Pressed"]
          	}]
        },
       Alignment -> Center,
       ItemSize -> Scaled[.6],
       Spacings -> 2.5],
	 FrameMargins -> {{0, 0}, {0, 5}}];
     
     
     columns = Grid[{
        {"", Pane[
          leftCol,
          Full,
          Alignment -> Left
          ], Pane[
          rightCol,
          Full,
          Alignment -> Center
          ]}
       } ,
       Dividers -> {{None, {3 -> Directive[RGBColor[0.74, 0.74, 0.74]]}}, None},
       Spacings -> {0, 1},
       ItemSize -> {{Automatic, 
          1 -> FEPrivate`If[
            FEPrivate`SameQ[FEPrivate`$OperatingSystem, "MacOSX"], 4, 
            3]}, Automatic},
       Alignment -> {{Left, {-1 -> Center}}, Top}];
     
     DialogInput[
      ExpressionCell[
       Framed[
        Column[{Panel["", Appearance -> {"Default" -> CloudDialogImage["TopBanner"]}, ImageSize -> {Full, Automatic},
           FrameMargins -> {{10, 10}, {0, 0}}, 
           Alignment -> {Left, Center}],
          
          Panel[
           Pane[
            columns,
            {Full,All},
            ImageMargins -> {{0, 0}, {10, 15}}
            ],
           Appearance -> {"Default" -> CloudDialogImage["BackgroundImage"]}
           ]
          },
         Spacings -> {0, 0}
         ],
        ImageSize -> {Full, Full},
        FrameMargins -> 0,
        ImageMargins -> {{0, 0}, {-3, -1}},
        FrameStyle -> None
        ],
       CellMargins -> {{-1, -5}, {0, -2}},
       CellFrameMargins -> 0
       ],
      CellContext -> Notebook,
      WindowTitle :> Dynamic[FEPrivate`FrontEndResource[cloudLoginTR,"WindowTitle"]](*"Enter Login Credentials"*),
      WindowSize -> {520, FitAll}, 
      Modal -> True,
      NotebookDynamicExpression :> (
         Refresh[FrontEnd`MoveCursorToInputField[EvaluationNotebook[], If[uname === "", boxid, pwdboxid]], None]
 )
      ];
     $loginCredentials,
     (*Else,no interactive FE*)Return[$Canceled];]]
     ]
  ]


(* ::Section::Closed:: *)
(*Footer*)


End[];

EndPackage[];
