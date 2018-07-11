Package["NeuralNetworks`"]



PackageExport["GetOutputLayers"]

GetOutputLayers[net_NetP] := finalOutput[net];

DeclareMethod[finalOutput, layerFinalOutput, containerFinalOutput];

layerFinalOutput[net_] := 
	<|First[OutputNames[net]] -> net|> (* %TEMP there are no layers with more than one output, for NOW *)

containerFinalOutput[net_] := Switch[
	net["Type"],
	"Chain", 
		finalOutput @ net[["Nodes", -1]],
	"Graph",
		outputs = OutputNames[net];
		outputPorts = Thread[NetPath["Outputs", outputs]];
		outputSources = Lookup[net["Edges"], outputPorts];
		outputLayers = Lookup[net["Nodes"], outputSources[[All, 2]]];
		AssociationThread[outputs, First[finalOutput[#]]& /@ outputLayers],
	_,
		$Unreachable
];

PackageScope["HasLossPortQ"]

HasLossPortQ[net_] := MemberQ[OutputNames[net], "Loss"];


PackageExport["NetAttachLoss"]

(* for trying things out we don't want to have a fully specified net *)
$enforceFullySpecified = True;
NetAttachLoss::arg1 = "First argument to NetAttachLoss should be a valid net.";
NetAttachLoss[net_, spec_:Automatic, enforce_:False] := CatchFailureAsMessage @ Scope[
	If[!ValidNetQ[net], ThrowFailure["arg1"]];
	$enforceFullySpecified = enforce;
	{lossNet, ports, wrapped} = iAttachLoss[net, spec];
	lossNet
];


PackageScope["iAttachLoss"]

Clear[iAttachLoss];

iAttachLoss[net_, All] := iAttachLoss[net, OutputNames[net]];

iAttachLoss[net_, spec_String] := iAttachLoss[net, {spec}];

(* %BLOCKED when support for unconnected graph nodes comes, add ability to mix specs. *)

iAttachLoss[net_, ports_List ? StringVectorQ] := Scope[
	outputs = Outputs[net];
	General::involoss = "The net has no output port called \"``\" to use as a loss.";
	ALookup[outputs, ports, ThrowFailure["involoss", #]&];
	{net, ports, False}
];

iAttachLoss[net_, Automatic] := Scope[
	If[HasLossPortQ[net], 
		Return[{net, {"Loss"}, False}]
	];
	outputLayers = GetOutputLayers[net];
	If[AllTrue[outputLayers, TrueQ @ NProperty[#, "IsLoss"]&],
		(* net is already a fully set up loss network *)
		Return[{net, Keys[outputLayers], False}]
	];
	$automode = True;
	autoLossLayers = KeyValueMap[#1 -> ChooseLossLayer[#1, #2]&, outputLayers];
	iAttachLoss[net, autoLossLayers]
];

General::invlossptype = "Cannot attach a loss layer to integer-valued port ``."

ChooseLossLayer[portName_, outputLayer_] := Scope[
	outputType = First @ outputLayer["Outputs"];
	stripped = StripCoders[outputType];
	If[MatchQ[TType[outputType], _IndexIntegerT], ThrowFailure["invlossptype", outMsgForm[portName]]];
	res = Quiet @ Switch[
		outputLayer["Type"],
		"Softmax",
			CrossEntropyLossLayer["Index", "Input" -> stripped, "Target" -> toIndexTensor[outputType]],
		"Elementwise" /; outputLayer["Parameters", "Function"] === ValidatedParameter[LogisticSigmoid],
			CrossEntropyLossLayer["Binary", "Input" -> stripped, "Target" -> toTargetType[outputType]],
		"NetPairEmbedding",
			ContrastiveLossLayer["Input" -> stripped],
		type_ /; $LayerData[type, "IsLoss"],
			$Verbatim,
		_,
			MeanSquaredLossLayer["Input" -> stripped, "Target" -> toTargetType[outputType]]
	];
	If[FailureQ[res], Panic["AutoLossLayerFailed", "Failed to create auto layer for `` based on layer ``. Message was ``.", portName, MsgForm[outputLayer], MsgForm[res]]];
	res
];

iAttachLoss[net_, lossLayer_ ? NetLayerQ] := Scope[
	outputs = OutputNames[net];
	If[Length[outputs] =!= 1,
		General::plossmultiout = "Cannot attach single loss layer `` when there are multiple outputs: ``. Use \"port\" -> losslayer to specify only a specific output be used.";
		ThrowFailure["plossmultiout", MsgForm[lossLayer], MsgForm[outputs]];
	];
	General::plossdup = "Cannot attach loss layer ``: net  already has an output port called \"Loss\".";
	If[HasLossPortQ[net], ThrowFailure["plossdup", MsgForm[lossLayer]]];	
	iAttachLoss[net, {First[outputs] -> lossLayer}]
];

iAttachLoss[net_, rule_Rule] := iAttachLoss[net, {rule}];

iAttachLoss[net_NetP, rules:{Repeated[_Rule | _String]}] := Scope[
	$otypes = Outputs[net]; $itypes = InputNames[net];
	$layers = <|"Net" -> net|>; 
	$outputLayers := $outputLayers = GetOutputLayers[net]; (* <- for Automatic *)
	$lossOutputs = {}; $edges = {}; $lid = 1;
	Scan[procRule, rules];
	lossNet = CatchFailure[NetGraph, toNetGraph[$layers, Flatten @ $edges, {}]];
	If[FailureQ[lossNet], 
		General::invlossnet = "Could not construct loss network for ``: ``";
		ThrowFailure["invlossnet", MsgForm[net], MsgForm[lossNet]]
	];
	{lossNet, $lossOutputs, True}
];

Clear[procRule];

procRule[portName_String] := procRule[checkOPort[portName] -> $Verbatim];

$scaling = 1.0;
procRule[portName_String -> spec_ -> Scaled[r_ ? NumericQ]] := Scope[
	$scaling = N[r]; 
	procRule[portName -> spec];
];

General::nelossport = "Can't attach a loss to a non-existent output port \"``\".";
checkOPort[portName_] := If[KeyExistsQ[$otypes, portName], portName, ThrowFailure["nelossport", portName]];

procRule[portName_String -> Automatic] := Scope[
	outLayer = Lookup[$outputLayers, checkOPort @ portName];
	$automode = True;
	procRule[portName -> ChooseLossLayer[portName, outLayer]]
];

procRule[portName_String -> Scaled[r_ ? NumericQ]] := Scope[
	$scaling = N[r];
	procRule[checkOPort[portName] -> $Verbatim]
];

procRule[portName_String -> $Verbatim] := Scope[
	src = NetPort["Net", portName]; 
	If[$scaling =!= 1.0,
		mid = portName <> "Scaled";
		sowScaled[mid];
		AppendTo[$edges, src -> mid]; src = mid;
	];
	AppendTo[$edges, src -> NetPort[portName]];
	AppendTo[$lossOutputs, portName];
];

sowScaled[scaleName_] := (
	$layers[scaleName] = NData @ ElementwiseLayer[# * $scaling&];
);

procRule[portName_String -> layer_ ? NetLayerQ] := Scope[
	General::nelossport2 = "Can't attach loss `` to non-existent output port \"``\".";
	otype = Lookup[$otypes, portName, ThrowFailure["nelossport2", MsgForm[layer], portName]];
	layer = AttachLossLayer[portName, otype, NData @ layer];
	name = "LossNet" <> IntegerString[$lid++]; hasTarget = KeyExistsQ[layer["Inputs"], "Target"];
	$layers[name] = layer;
	If[hasTarget && MemberQ[$itypes, portName], (* weird but possible edge case *)
		General::lossioclash = "Cannot attach target loss `` to the output port \"``\" as it has the same name as an input port.";
		ThrowFailure["lossioclash", layer, portName];
	];
	outputName = portName <> "Loss";
	AppendTo[$lossOutputs, outputName];
	If[$scaling == 1.0, oname = name,
		oname = portName <> "Scaled";
		sowScaled[oname];
		AppendTo[$edges, name -> oname]; 
	];	
	AppendTo[$edges, {
		NetPort[1, portName] -> NetPort[name, "Input"], 
		If[hasTarget, NetPort[portName] -> NetPort[name, "Target"], Nothing],
		oname -> NetPort[outputName]
	}];
];

procRule[portName_String -> layer_] := (
	General::invlosslayer = "Cannot use `` as a loss layer for output port \"``\" as it is not a valid net layer.";
	ThrowFailure["invlosslayer", Shallow[layer], portName];
);

procRule[r_] := (
	General::invlspecel = "Loss rules element `` should be a rule mapping a port name to a loss layer.";
	ThrowFailure["invlspecel", MsgForm[r]];
)

General::invlspec = "Loss specification `` should be one of All, Automatic, a loss layer or net, \"port\", \"port\" -> spec, \"port\" -> spec -> Scaled[s], or a list of these.";
iAttachLoss[net_, spec_] := ThrowFailure["invlspec", spec /. n_ ? ValidNetQ :> MsgForm[n]];


toTargetType[type_] := ReplaceAll[type, dec_NetDecoder :> DecoderToEncoder[dec]];

AttachLossLayer[portName_, outputType_, customLayer_] := Scope[
	ldata = customLayer; $pname = portName; $otype = outputType;
	If[ldata["Type"] === "CrossEntropyLoss" && MatchQ[ldata["Parameters", "TargetForm"], "Index" | _EnumT],
		ldata["Parameters", "TargetForm"] = "Index"; 
		ldata["Parameters", "TargetForm"];
		targetType = toIndexTensor[outputType]; (* lower the target rank *)
	,
		targetType = toTargetType[outputType];
	];
	(* check loss layer has the right inputs *)
	inputs = Inputs[ldata];
	inputType = Lookup[inputs, "Input", plossPanic["loss layer does not have an \"Input\" port."]];
	If[!SubsetQ[{"Input", "Target"}, Keys[inputs]],
		plossPanic["loss layer should have either a single \"Input\" port or an \"Input\" port and a \"Target\" port."]
	];
	(* check we can attach loss layer to output of net *)
	newInputType = UnifyTypes[inputType, outputType];
	If[FailureQ[newInputType], plossEdgePanic[]];
	ldata["Inputs", "Input"] = newInputType;
	(* attach the reflected output type to the target *)
	If[KeyExistsQ[inputs, "Target"],
		newTargetType = UnifyExternalTypes[inputs["Target"], targetType];
		If[!FailureQ[newTargetType],
			ldata["Inputs", "Target"] = newTargetType 
		];
	];
	(* reinfer the loss layer to make sure the new input types do their thing *)
	ldata2 = InferNData[ldata];
	(* if that fails or was incomplete, complain *)
	If[FailureQ[ldata], plossPanic[TextString @ ldata2]];
	If[$enforceFullySpecified && !ConcreteNetQ[ldata2], plossPanic @ StringForm["`` is not fully specified", UnspecifiedPathString[ldata2]]];
	ldata2
];

General::invploss1 = "Cannot attach `` loss layer `` to ``: ``";
$automode = False;
plossPanic[reason_] := ThrowFailure["invploss1", If[$automode, "automatically chosen", "provided"], MsgForm[ldata], outMsgForm[$pname], TextString @ reason];

General::invploss2 = "`` loss layer ``, which expects ``, is incompatible with ``, which produces ``."
plossEdgePanic[] := ThrowFailure["invploss2", If[$automode, "Automatically chosen", "Provided"], MsgForm[ldata], MsgForm[First @ ldata["Inputs"]], outMsgForm[$pname], MsgForm[$otype]];

toIndexTensor = MatchValues[
	TensorT[{}, _] := Automatic; (* softmax rejects size-1 tensors *)
	TensorT[dims_List, RealT] := TensorT[Most[dims], IndexIntegerT[Last[dims]]];
	TensorT[{n_}, dec_NetDecoder] := TensorT[{n}, DecoderToEncoder[dec]];
	enc_NetDecoder := DecoderToEncoder[enc];
	_ := Automatic;
];

outMsgForm[name_] := MsgForm[NetPath["Outputs", name]];



PackageScope["ToLossFunction"]

General::lfnotrealout = "Output of custom loss function should be a single real number."
General::lfnotsymbolic = "Custom loss function must support symbolic differentiation."

(* this is an experimental WIP, not used anywhere *)
ToLossFunction[f_Function, sz_] := Scope[
    vars = Array[Subscript[x, #]&, sz];
    testres = UnsafeQuietCheck[f[RandomReal[1, sz]]];
    If[!MachineRealQ[testres], ThrowFailure["lfnotrealout"]];
    res = UnsafeQuietCheck[f[vars]];
    If[FailureQ[res], ThrowFailure["lfnotsymbolic"]];
    derivative = D[res, {vars}];
    derivative = Compose[Hold, derivative] /. Subscript[x, i_] :> Part[cvar, i];
    cdfunc = Compile @@ Prepend[derivative, {{cvar, _Real, 1}}];

    {Compile @@ cdargs, Function @@ Prepend[derivative, cvar]}
];