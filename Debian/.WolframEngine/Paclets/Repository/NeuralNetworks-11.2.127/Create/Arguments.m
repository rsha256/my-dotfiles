Package["NeuralNetworks`"]



PackageScope["ParseArguments"]


ParseArguments[head_, layerq_, definition_, args_] := Scope[

	$parseinputhead = head;

	UnpackAssociation[definition,
		$params:"Parameters", 
		$coercions:"ParameterCoercions",
		$defaults:"ParameterDefaults",
		$maxargs:"MaxArgCount", 
		$minargs:"MinArgCount"
	];

	If[layerq,
		UnpackAssociation[definition,  
			$arrays:"Arrays", 
			$inputs:"Inputs", 
			$outputs:"Outputs",
			$states:"States"
		],
		$arrays = $inputs = $outputs = $states = <||>;
	];

	$userparams = Association[];
	$args = Keys[$params]; 
	$i = 0; 
	
	If[$strictLayerArgs && badArgCountQ[head, Length @ Cases[args, Except[_Rule]], $minargs, $maxargs],
		ThrowRawFailure[$Failed]
	];

	Scan[scanArg, args];

	$params = Association @ KeyValueMap[
		#1 -> Lookup[$userparams, #1,
			def = Lookup[$defaults, #1];
			If[MissingQ[def], #2, parseParam[#1, def]]
		]&,
		$params
	];

	ret = If[layerq, {$arrays, $params, $inputs, $outputs, $states}, $params];

	(* fill in syntatic length vars with actual random ones, except for negative IDs,
	which are for debugging and we don't want them to change *)
	$sids = <||>;
	ret /. s_LengthVar :> RuleCondition @ CacheTo[$sids, s, If[Negative @ First[s], s, NewLengthVar[]]]
];

badArgCountQ[head_, c_, min_, max_] := !ArgumentCountQ[head, c, min, max];

badArgCountQ[sf_StringForm, c_, min_, max_] := 
	UnsafeQuietCheck[
		ArgumentCountQ[ZZZ, c, min, max]; False,
		With[{sym = sf[[2]]}, Message[MessageName[sym, "argcnterr"], EvaluateChecked[
			ArgumentCountQ[ZZZ, c, min, max],
			rewriteBadArgMsg[TextString @ sf]
		]]]; True
	];

rewriteBadArgMsg[name_String][failure_] := Scope[
	headstr = SymbolName[head];
	newmsg = StringReplace[
		TextString[failure],
		{"ZZZ" -> name, "called" -> "provided", "argument" -> "parameter"}
	];
	newmsg
];

General::argcnterr = "``";

scanArg[value_] := 
	If[++$i > $maxargs, Panic["TooManyArgs"],
		parseParam[$args[[$i]], value];
	];

General::netinvrule = "`` is not a valid option for ``."
scanArg[r_Rule] := ThrowFailure["netinvrule", r, $parseinputhead];

scanArg[Rule[sym_Symbol, value_]] /; Context[sym] === "System`" :=
	scanArg[Rule[SymbolName[sym], value]];

scanArg[Rule[key_String, value_]] := 
	Which[
		KeyExistsQ[$params, key],
			parseParam[key, value];,
		KeyExistsQ[$arrays, key],
			$arrays[key] = CoerceParam[key, value, $arrays[key]];,
		KeyExistsQ[$inputs, key],
			$inputs[key] = ParseInputSpec[key, $inputs[key], value];,
		KeyExistsQ[$outputs, key],
			$outputs[key] = ParseOutputSpec[key, $outputs[key], value];,
		True,
			ThrowFailure["netinvkey", $parseinputhead, key]
	];

parseParam[key_, value_] := Scope[
	value = Lookup[$coercions, key, Identity] @ value;
	$userparams[key] = CoerceParam[key, value, $params[key]]
];



PackageScope["ParseInputSpec"]

Clear[ParseInputSpec, ParseOutputSpec];

General::invlistinlen1 = "Specification for input port `` should be a list of tensor dimensions."
General::invlistinlen2 = "Specification for input port `` should be a list of tensor dimensions of length ``."
ParseInputSpec[param_, type:ListT[n_, t_], sizes_List] := Scope[
	If[IntegerQ[n] && Length[sizes] =!= n, ThrowFailure["invlistinlen2", param, n]];
	result = CatchFailure[General, checkType[t, ToT[#]]& /@ sizes];
	If[FailureQ[result], 
		If[IntegerQ[n], 
			ThrowFailure["invlistinlen2", param, n],
			ThrowFailure["invlistinlen1", param]
		]
	];
	result
];

ParseInputSpec[param_, type_, spec_] := 
	Block[{$name = param, $type = type, $spec = spec, $preserveCoders = True}, 
		checkType[type, ToT[spec, NetEncoder]]
	];


PackageScope["ParseOutputSpec"]

ParseOutputSpec[param_, type_, spec_] := 
	Block[{$name = param, $type = type, $spec = spec, $preserveCoders = True},
		checkType[type, ToT[spec, NetDecoder]]
	]


Clear[checkType, checkType2];

checkType[t1_TensorT, p:CoderP /; !SequenceCoderQ[p]] := Scope[
	res = checkType2[t1, p];
	(* automatically raise type descriptions that aren't sequence-level to be sequence-level *)
	If[FailureQ[res], res = checkType2[t1, TensorT[{SizeT}, p]]];
	If[FailureQ[res], failType[type, utype]];
	res
];

checkType[_Missing, _] :=
	ThrowFailure["netinvkey", $CatchFailureHead, $name];

checkType[t1_, t2_] := Scope[
	res = checkType2[t1, t2];
	If[FailureQ[res], failType[t1, t2]];
	res
];

checkType2[type_, utype_] := Scope[
	(* %GENERALITY we work around no reasoning about variance of tensor inputs / outputs by just weakening the user type *)
	If[MatchQ[TType[type], _IndexIntegerT], utype = utype /. RealT -> AtomT];
	res = UnifyTypes[type, utype]; 
	res
];

General::invspenc = "NetEncoder of type ``, which produces ``, cannot be attached to port ``, which must be ``."
failType[type_, enc_NetEncoder] :=
	ThrowFailure["invspenc", CoderName[enc], TypeString[CoderType[enc]], $name, TypeString[StripCoders @ type]];

General::invspdec = "NetDecoder of type ``, which expects ``, cannot be attached to port ``, which produces ``."
failType[type_, dec_NetDecoder] :=
	ThrowFailure["invspdec", CoderName[dec], TypeString[CoderType[dec]], $name, TypeString[StripCoders @ type]];

General::invportshape = "Specification `` is not compatible with port \"``\", which must be ``."
failType[_, u_] := ThrowFailure["invportshape", 
	ReplaceAll[$spec, (head:NetEncoder|NetDecoder)[type_, args___] :> HoldForm[head][type, "\[Ellipsis]"]], 
	$name,
	TypeString[$type]
];

