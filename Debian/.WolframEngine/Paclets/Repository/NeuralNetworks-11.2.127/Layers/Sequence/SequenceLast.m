Input: ChannelT[SizeT, $Output]

Output: AnyTensorT

AllowDynamicDimensions: True

Writer: Function[
	input = GetInputMetaNode["Input"];
	last = SowMetaLast[input];
	SetOutput["Output", last];
]

Tests: {
	{"Input" -> 4} -> "_IWqOWEDnRsY_JnzXy27PbJU",
	{"Input" -> {3, 5}} -> "5_N6sEq6rJ9TI_EDX3DcMW7R4",
	{"Input" -> {3, 2, 2}} -> "2*2_HpsfqpCDCtI_WMYO2gH/cV8",
	{"Input" -> "Varying"} -> "_akQbzL2Wru4_GrrrizPh1LI",
	{"Input" -> {"Varying", 1}} -> "1_ISPdreHzTcg_BncZwl5gfBY",
	{"Input" -> {"Varying", 2, 2}} -> "2*2_HpsfqpCDCtI_A3KOoAQHp3A"
}