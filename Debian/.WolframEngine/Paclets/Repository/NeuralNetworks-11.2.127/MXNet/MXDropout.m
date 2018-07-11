Package["NeuralNetworks`"]



PackageScope["RecurrentDropoutMethodT"]

NNSetUsage @ "
RecurrentDropoutMethodT is a shortcut for a fairly complex ValidatedParameterT that handles the \
various possible dropout techniques for input and recurrent states in GRU etc.
"

RecurrentDropoutMethodT = ValidatedParameterT[parseRDO, None];

$allowedDOMethods = {"VariationalInput", "VariationalState", "StateUpdate"};
parseRDO[None] := None;
parseRDO[p_ ? NumericQ] := checkDP[p];
parseRDO[list_List] := Association @ Map[parseRDO1, list];
parseRDO[spec_] := invDropSpec[];

checkDP[p_] := Replace[N[p], {r_Real /; Between[r, {0,1}] :> r, _ :> ThrowFailure["invdrppval"]}];
General::invdrppval = "Dropout probabilities should be numeric values between 0 and 1.";

parseRDO1[method_String -> p_] /; MemberQ[$allowedDOMethods, method] := method -> checkDP[p];
parseRDO1[_] := invDropSpec[];

General::invdrpspc = "Specification for \"Dropout\" should be either None, a numeric probability, or a list of rules mapping methods to probabilities, where the allowed methods are ``."
invDropSpec[] := ThrowFailure["invdrpspc", QuotedStringList[$allowedDOMethods]];


PackageScope["MakeRNNDropoutData"]

(* special cased single real so the default looks compact in standardform *)
MakeRNNDropoutData[_, n_] /; !$TMode := iMakeRNNDropoutData[{0., 0., 0.}, n];
MakeRNNDropoutData[None, n_] :=   iMakeRNNDropoutData[{0., 0., 0.}, n];
MakeRNNDropoutData[p_Real, n_] := iMakeRNNDropoutData[{p, p, 0.}, n];
MakeRNNDropoutData[assoc_Association, n_] := 
	iMakeRNNDropoutData[Lookup[assoc, {"VariationalInput", "VariationalState", "StateUpdate"}, 0.], n];

iMakeRNNDropoutData[{0., 0., 0.}, n_] := {False, Table[{Identity, Identity}, n], Identity};
iMakeRNNDropoutData[{ip_, sp_, sup_}, n_] := 
	{ip > 0 || sp > 0, Table[{MakeVarDropoutFunction[ip], MakeVarDropoutFunction[sp]}, n], MakeDropoutFunction[sup]};

MakeVarDropoutFunction[prob_] := If[
	!$TMode || prob == 0, Identity,
	Module[{mask = None}, Function[in, 
		If[mask === None, 
			Block[{ret = SowNode["Dropout", in, "p" -> prob]}, mask = ret; mask[[2]] = 1; ret],
			SowHad[mask, in]
		]
	]]
];

MakeDropoutFunction[prob_] := If[
	!$TMode || prob == 0, Identity,
	Function[in, SowNode["Dropout", in, "p" -> prob]]
];


