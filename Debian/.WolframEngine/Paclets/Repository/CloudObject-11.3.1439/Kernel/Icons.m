BeginPackage["CloudObject`"]

Begin["`Private`"]

listIcons[obj_CloudObject, head_Symbol] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]), Message[head::cloudnf, obj]; Return[$Failed]];
        listIcons[cloud, uuid, head]
    ]

listIcons[cloud_, uuid_, head_] :=
    (
        If[!(StringQ[cloud] && UUIDQ[uuid]), Return[$Failed]];
        Replace[
            execute[cloud, "GET", {"files", uuid, "icons"}],
            {
                {_, bytes_List} :> ImportString[FromCharacterCode[bytes], "Lines"],
                other_ :> (checkError[other, head]; $Failed)
            }
        ]
    )

(*{
"Default", DONE
"API", DONE
"WebComputation", DONE
"EditPage",
"EmbedCode",
"EmbedContent",
"ExportDoc",
"Mobile",
"Publish",
"Report",
"ScheduledProg", DONE
"WebForm"}; DONE
*)
iconDeploymentType["API" | "application/vnd.wolfram.expression.api", _] := "API"
iconDeploymentType["Form" | "application/vnd.wolfram.expression.form", _] := "WebForm"
iconDeploymentType["WebComputation" | "application/vnd.wolfram.expression.computation", _] := "WebComputation"
iconDeploymentType["Task" | "application/vnd.wolfram.expression.task", _] := "ScheduledProg"
iconDeploymentType[mimetype_, permissions_] := "Default"

cloudIconFormat[name_] := "image/png"

cloudIconSize["mobile" | "Android" | "IOS"] := Large

cloudIconSize[name_] := Small

(* $cloudIconNames - the list of icon environments to generate by default *)
$cloudIconNames = {"FileBrowser", "IOS"};

(* $validIconNames - the list of documented valid icon names *)
$validIconNames = {"Android", "FileBrowser", "IOS", "MacOSX", "WebPage"};

$automaticIconRules = Map[# -> Automatic&, $cloudIconNames]

Options[SetCloudIcons] = {Asynchronous -> False, "Deployment" -> "Default",
    "Content" :> None, "DeleteUnmentionedIcons" -> False};

SetCloudIcons[obj_CloudObject, iconName_String -> iconContent_,
    opts:OptionsPattern[]] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]), Return[$Failed]];
        setCloudIcon[iconContent, cloud, uuid, iconName, opts]
    ]

SetCloudIcons[obj_CloudObject, rules:{_Rule ...}, opts:OptionsPattern[]] :=
    Module[{cloud, uuid, contentExpr, deployment, iconsToSet, existingIcons,
        iconsToDelete},
        {cloud, uuid} = getCloudAndUUID[obj];
        If[!(StringQ[cloud] && UUIDQ[uuid]), Return[$Failed]];

        (* here's where we could have an optimization that calls Iconize once,
            on the largest requested size, and then downsize the other images
        *)

        (* We only need to get object content if IconRules specifies Automatic,
            and we only want to get object content once (as for SetOptions,
            where the content must be loaded) and then re-used. *)
        If[MemberQ[Map[Last, rules], Automatic],
        (* We want to fetch the content and deployment only once, not
                once each time in the Map.
             *)
            contentExpr = OptionValue["Content"];
            deployment = OptionValue["Deployment"];
        (* Else *),
        (* these option values will not be used, but we need to pass them *)
            contentExpr = None;
            deployment = "Default";
        ];

        (* delete icons not explicitly mentioned here *)
        If[TrueQ[OptionValue["DeleteUnmentionedIcons"]],
            iconsToSet = Map[First, rules];
            existingIcons = Replace[listIcons[cloud, uuid, SetOptions], $Failed :> iconsToSet];
            iconsToDelete = Complement[existingIcons, iconsToSet];
            $lastIconDeleteResult =
                Map[deleteCloudIcon[cloud, uuid, #]&, iconsToDelete];
        ];

        Map[setCloudIcon[Last[#], cloud, uuid, First[#],
            "Content" -> contentExpr, "Deployment" -> deployment, opts]&, rules]
    ]

SetCloudIcons[obj_CloudObject, Automatic, opts:OptionsPattern[]] :=
    SetCloudIcons[obj, $automaticIconRules, opts]

SetCloudIcons[obj_CloudObject, expr_, opts:OptionsPattern[]] :=
    SetCloudIcons[obj, Map[Rule[#, expr]&, $cloudIconNames], opts]

setCloudIcon::usage = "setCloudIcon[iconexpr, cloud, uuid, environment]"

Options[setCloudIcon] = Options[SetCloudIcons];

(* used for setting icon content according to object contents via Iconize *)
setCloudIcon[Automatic, cloud_, uuid_, iconname_, OptionsPattern[]] :=
    Module[{contentExpr, deployment, iconImage},
        contentExpr = Check[OptionValue["Content"], Return[$Failed]];
        deployment = Check[OptionValue["Deployment"], Return[$Failed]];
        iconImage = Iconize`Iconize[Evaluate@contentExpr, deployment,
            ImageSize -> cloudIconSize[iconname],
            Iconize`Iconize`PackagePrivate`Platform -> iconname (* Iconize needs to use a string for this or export the Platform symbol *)
        ];
        putCloudIcon[iconImage, cloud, uuid, iconname,
            Asynchronous -> OptionValue["Asynchronous"]]
    ]

(* used for manually setting icon content *)
setCloudIcon[iconContent_, cloud_, uuid_, iconname_, OptionsPattern[]] :=
    Module[{iconImage},
        iconImage = Rasterize[iconContent, "Image",
            ImageSize -> cloudIconSize[iconname]];
        putCloudIcon[iconImage, cloud, uuid, iconname,
            Asynchronous -> OptionValue["Asynchronous"]]
    ]

Options[putCloudIcon] = {Asynchronous -> False};

putCloudIcon[iconImage_Image, cloud_, uuid_, iconname_, OptionsPattern[]] :=
    Module[{iconTempFile, iconFileContent, result},
        iconTempFile = CreateTemporary[]; (* Note: doing this in memory is actually slower; e.g. ToCharacterCode[ExportString[..., "JPG"]] *)
        Export[iconTempFile, iconImage, cloudIconFormat[iconname]];
        iconFileContent = BinaryReadList[iconTempFile];
        DeleteFile[iconTempFile];
        result =  $lastSetCloudIconResult =
            execute[cloud, "PUT", {"files", uuid, "icon", iconname},
                Body -> iconFileContent, Type -> cloudIconFormat[iconname],
                Asynchronous -> OptionValue[Asynchronous]];
        If[TrueQ[OptionValue[Asynchronous]],
            iconname -> iconImage,
        (* Else *)
            Replace[
                result,
                {
                    {_, _} :> iconname -> iconImage,
                    _ :> iconname -> $Failed
                }
            ]
        ]
    ]

deleteCloudIcon[cloud_, uuid_, iconname_] :=
    execute[cloud, "DELETE", {"files", uuid, "icon", iconname}];

End[]

EndPackage[]
