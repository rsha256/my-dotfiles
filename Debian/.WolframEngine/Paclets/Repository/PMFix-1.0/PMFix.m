(* See PACMAN-1 JIRA issue. *)
If[$VersionNumber == 11.2,
    Options[PacletManager`PacletInstall] = 
        Options[PacletManager`PacletInstallQueued] = {"IgnoreVersion" -> False, "DeletePacletFile" -> False, "Site" -> Automatic, 
            "UpdateSites" -> Automatic, "Asynchronous" :> MathLink`IsPreemptive[], "CompletionFunction" -> None};
    Options[PacletManager`PacletUpdate] = {"KeepExisting" -> Automatic, "Site" -> Automatic,
        "UpdateSites" -> Automatic, "Asynchronous" :> MathLink`IsPreemptive[]}
]
