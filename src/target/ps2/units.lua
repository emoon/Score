Program {
	Name = "score_ee_target",
	Sources = {
		"ee/score_connection.c",
		"ee/socket.c",
		"ee/target_ps2_ee.c",
	},

	Libs = {
		{ "c", "gcc", "debug", "kernel", "syscall", "ps2snd", "pad", "m", "fileXio"; Config = "ps2-*-*-*" },
	}
	
}

Default "score_ee_target"

