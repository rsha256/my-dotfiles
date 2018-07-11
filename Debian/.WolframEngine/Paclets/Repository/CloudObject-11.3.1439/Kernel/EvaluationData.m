BeginPackage["CloudObject`"]

System`EvaluationData;

Begin["`Private`"]

(* EvaluationData *)
Unprotect[EvaluationData];
SetAttributes[{iEvaluationData, iHold, EvaluationData}, HoldAllComplete];
SetAttributes[iCatch, SequenceHold]

$DisableEvaluationDataToString = False;

iEvaluationData[expr_] := Replace[
	AbsoluteTiming[Timing[Catch[Catch[iCatch[expr], _, iHold]]]], {
		{walltime_, {cputime_, iCatch[res_]}} :> 
			HoldComplete[walltime, cputime, res],
		{walltime_, {cputime_, iHold[res___]}} :> (
			Message[Throw::nocatch, HoldForm[Throw[res]]]; 
			HoldComplete[walltime, cputime, Hold[Throw[res]]]
		),
		{walltime_, {cputime_, res_}} :> (
			Message[Throw::nocatch, HoldForm[Throw[res]]]; 
			HoldComplete[walltime, cputime, Hold[Throw[res]]]
		),
		{walltime_, {cputime_, res___}} :> (
			Message[Throw::nocatch, HoldForm[Throw[Sequence[res]]]]; 
			HoldComplete[walltime, cputime, Hold[Throw[Sequence[res]]]]
		)
	}
]

EvaluationData[expr_] :=
	Module[{
		$messages, 
		$printOutput, 
		handleMessage, 
		handlePrint,
		messageList,
		data
		},
		$messages = Internal`Bag[];
		handleMessage = logMessage[$messages, #]&;
		Internal`AddHandler["Message", handleMessage];
		
		$printOutput = Internal`Bag[];
		handlePrint = logOutput[$printOutput, #]&;
		Internal`AddHandler["Wolfram.System.Print", handlePrint];

		data = Replace[
			iEvaluationData[expr],
			(* we use HoldComplete because expr could evaluate to a Sequence, 
				since Rule is SequenceHold this will fix the bug
				see https://jira.wolfram.com/jira/browse/SAAS-11630
				and https://jira.wolfram.com/jira/browse/CLOUD-3841 *)
			HoldComplete[walltime_, cputime_, result_] :> (
				messageList = Internal`BagPart[$messages, All];
				<|
					"Result" :> result,
					"Success" -> messageList === {},
					"FailureType" -> If[messageList === {}, None, "MessageFailure"], (* this needs to be designed *)
					"OutputLog" -> Internal`BagPart[$printOutput, All],
					Thread[
						{"Messages", "MessagesText", "MessagesExpressions"} ->
						If[
							messageList === {},
							{{}, {}, {}},
							Transpose[messageList]
						]
					],
					"Timing" -> Round[cputime, 0.001],
					"AbsoluteTiming" -> Round[walltime, 0.001],
					If[
						! TrueQ[$DisableEvaluationDataToString],
						"InputString" -> ToString[Unevaluated[expr], InputForm],
						{}
					]
				|>
			)
		];
		
		Internal`RemoveHandler["Message", handleMessage];
		Internal`RemoveHandler["Wolfram.System.Print", handlePrint];

		data
	];

EvaluationData[args___] := (
	ArgumentCountQ[EvaluationData, Length[DeleteCases[{args}, _Rule|_RuleDelayed, Infinity]], 1, 1];
	$Failed
)

Protect[EvaluationData];

logMessage[bag_, Hold[Message[___], False]] := Null; (* skip *)

(* always skip General::newsym, it is on in webMathematica and is way too noisy for us *)
logMessage[bag_, Hold[Message[General::newsym, ___], _]] := Null;

logMessage[bag_, expr:Hold[Message[tag_, ___], True]] :=
	With[{heldmsg = Delete[expr, -1]},
		Internal`StuffBag[bag, {tag, messageToString[heldmsg], heldmsg}]
	];

messageToString[Hold[Message[msgname_, args___]]] :=
	(* if msgname is an unevaluated MessageName, it means the message
		template is defined as General::tag *)
	ToString[Row[{ToString[Unevaluated[msgname]], " : ",
		StringForm[If[Head[msgname] === MessageName,
			ReplacePart[msgname, 1 -> General], msgname], args]
    }]];

logOutput[bag_, res_HoldComplete] := 
	 Internal`StuffBag[bag, outputToString[res]]; 
	

outputToString[HoldComplete[output__]] := ToString[Row[{output}]]    

End[]

EndPackage[]