Package["NeuralNetworks`"]


PackageScope["SingletonCached"]

(* 
SingletonCached will clear itself whenever a query is made against a net that is not 
the same as the previous query's net. 

SingletonCached is used to cache executors. A similar mechanism is used for the 
fast path cache (see NetApply), which is basically a special purpose cache that
bypasses all ordinary logic for simple nets to make them as low-overhead as possible
when doing plan evaluation (e.g. inside a Plot or Table). 

The point of using SingletonCached is to reduce memory bloat from lots of different
nets. SingletonCached will still result in an executor being reused in e.g. a Plot or
Table, but as soon as another net is used the first executor is thrown away. So there's
only ever one executor in the cache at any given time.

MXPlans are still cached permanently, via the old-fashioned Cached, whic has no 
occupancy limit. However, this doesnt result in much resource usage AFAIK -- MXSymbols
dont have much overhead until they are MXSymbolBind'd.
*)

Clear[$SingletonCache];
$SingletonCache = Language`NewExpressionStore["SingletonNNCache"];

ClearAll[SingletonCached];

$LastSingletonUUID = 0;
SingletonCached[func_, net_, args__] := Scope[
	id = NetUUID[net];
	If[id =!= $LastSingletonUUID,
		Scan[$SingletonCache["remove"[First[#]]]&, $SingletonCache["listTable"[]]];
		ClearCachedNDArrays[]; (* a bit unclean to couple this general-purpose caching
		mechanism to clearing ndarrays, but does the job, which is to fully wipe out
		any residual impact of the previous cached net *)
		$LastSingletonUUID ^= id;
	];
	Replace[$SingletonCache["get"[net, {func, args}]], Null :>
		Replace[func[net, args],
			res:Except[_ ? FailureQ] :> 
			($SingletonCache["put"[net, {func, args}, res]]; res)
		]
	]
]

PackageScope["Cached"]
PackageScope["NetUUID"]

Clear[$Cache];
$Cache = Language`NewExpressionStore["NNCache"];

(* TODO: Replace this with a System`Private` function that just returns the raw pointer of an expression *)
NetUUID[net_] := Cached[getUUID, net];
$UUIDCount = 0;
getUUID[net_] := ++$UUIDCount;

ClearAll[Cached];

Cached[func_, net_, args__] := 
	Replace[$Cache["get"[net, {func, args}]], Null :>
		Replace[func[net, args],
			res:Except[_ ? FailureQ] :> 
			($Cache["put"[net, {func, args}, res]]; res)
		]
	];

Cached[func_, net_] := 
	Replace[$Cache["get"[net, func]], Null :>
		Replace[func[net],
			res:Except[_ ? FailureQ] :> 
			($Cache["put"[net, func, res]]; res)
		]
	];


PackageScope["CachedIf"]
	
CachedIf[True, args___] := Cached[args];
CachedIf[_, args___] := Construct[args];

PackageScope["CachedValues"]

CachedValues[] := $Cache["listTable"[]];
CachedValues[net_NetP] := FirstCase[$Cache["listTable"[]], {Verbatim[net], vals_} :> vals];

PackageExport["ClearCache"]

ClearCache[] := Scan[$Cache["remove"[First[#]]]&, CachedValues[]];


PackageScope["ToCachedNDArray"]

Clear[$NDArrayCache];
$NDArrayCache = Language`NewExpressionStore["NNNDArrayCache"];

ToCachedNDArray[ra_, cont_] := 
	Replace[$NDArrayCache["get"[ra, cont]], Null :> Block[
		{res = MXNetLink`NDArrayCreate[ra, cont]},
		$NDArrayCache["put"[ra, cont, res]]; res
	]];


PackageScope["CachedNDArrays"]

CachedNDArrays[] := $NDArrayCache["listTable"[]];


PackageExport["ClearCachedNDArrays"]

ClearCachedNDArrays[] := Scope[
	keys = CachedNDArrays[][[All, 1]];
	Scan[$NDArrayCache["remove"[#]]&, keys];
];
