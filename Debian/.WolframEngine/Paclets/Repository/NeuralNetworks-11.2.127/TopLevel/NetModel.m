Package["NeuralNetworks`"]



PackageExport["NetModel"]

LoadResourceSystemClient := Block[{$ContextPath = $ContextPath}, 
	Needs["ResourceSystemClient`"];
	Clear[LoadResourceSystemClient]];

$ROCache = <||>;

NetModel::invhttp = "Couldn't connect to server."
NetModel::model = "No model with name `1` could be found."
NetModel::invprop = "The argument `1` at position 2 is not a known property."

ValidROQ = ResourceSystemClient`Private`resourceObjectQ;

NetModel[model_String, elem_String] := Scope[
	LoadResourceSystemClient;
	Block[{PrintTemporary}, Quiet[
		ro = CacheTo[$ROCache, model, ResourceObject["NeuralNet" -> model]];
		If[FailureQ[ro], KeyDropFrom[$ROCache, model]];
		messages = $MessageList;
	]];
	If[!ValidROQ[ro],
		Which[
			MemberQ[messages, HoldForm[URLFetch::offline] | HoldForm[ResourceObject::offline]],
				Message[NetModel::offline],
			MemberQ[messages, HoldForm[URLFetch::invhttp]],
				Message[NetModel::invhttp],
			MemberQ[messages, HoldForm[ResourceAcquire::apierr]],
				Message[NetModel::model, model]
		];
		Return[$Failed]
	];
	Which[
		elem === "ResourceObject",                              ro,
		MemberQ[$defaultContentElements, elem],                 ResourceData[ro, elem],
		MemberQ[$defaultProperties, elem],                      ro[elem],
		MemberQ[getROMetadata[ro, "ContentElements"], elem],    ResourceData[ro, elem],
		MemberQ[getROMetadata[ro, "Properties"], elem],         ro[elem],
		True,
			Message[NetModel::invprop, elem];
			$Failed
	]
]

$defaultContentElements = {"EvaluationNet", "UninitializedEvaluationNet"};
$defaultProperties = {"ByteCount", "InputDomains", "SourceMetadata", "TaskType"};

$ROMetadataCache = <||>;
getROMetadata[ro_, meta_] := CacheTo[$ROMetadataCache, {ro, meta}, ro[meta]];
	
NetModel[model_String] := NetModel[model, "EvaluationNet"]

NetModel::invarg1 = "Parameter `1` at position 1 in `2` should be a string."
NetModel::invarg2 = "Parameter `1` at position 2 in `2` should be a string."
NetModel::invarg3 = 
	"Parameter `1` at position 1 and parameter `2` at position 2 in `3` should be Strings."

NetModel[x_] := (Message[NetModel::invarg1, x, HoldForm@NetModel[x]]; $Failed)
NetModel[x_, elem_String] := 
	(Message[NetModel::invarg1, x, HoldForm@NetModel[x, elem]]; $Failed)
NetModel[net_String, x_] := 
	(Message[NetModel::invarg2, x, HoldForm@NetModel[net, x]]; $Failed)
NetModel[x_, y_] := 
	(Message[NetModel::invarg3, x, y, HoldForm@NetModel[x, y]]; $Failed)

DeclareArgumentCount[NetModel, {1, 2}];


PackageScope["NetModelRemove"]

NNSetUsage @ "
NetModelRemove[] removes all locally cached NetModels.
"

NetModelRemove[] := Scope[
	LoadResourceSystemClient;
	res = Select[
		ResourceSystemClient`Private`$localResources, 
		(ResourceObject[#]["ResourceType"] === "NeuralNet")&
	];
	ResourceRemove /@ res
]
