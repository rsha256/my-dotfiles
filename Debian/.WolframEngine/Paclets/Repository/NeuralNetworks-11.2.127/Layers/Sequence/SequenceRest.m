InheritsFrom: "SequenceMost"

Writer: Function[
	input = GetInputMetaNode["Input"];
	rest = SowMetaDrop[input, #$LengthOut, True];
	SetOutput["Output", rest];
]

Tests: {
	{"Input" -> 4} -> "3_cwgaM9SDvGc_fcjOSo+tB00",
	{"Input" -> {3, 5}} -> "2*5_GiF+/cMRWSs_RhCCsyun4m8",
	{"Input" -> "Varying"} -> "2_bvslIpK6uGM_ae+ykiAfrcM",
	{"Input" -> {"Varying", 2}} -> "2*2_ZfKlaeefEj0_eVbWKPW+omU",
	{"Input" -> {"Varying", 2, 2}} -> "2*2*2_f3sk5xoc1A8_XevAIhv4GY0"
}