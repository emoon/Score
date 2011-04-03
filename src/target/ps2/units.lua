Program {
	Name = "score_ee_target",
	Sources = {
		-- tinylibc
		"ee/libc/init.c",
		"ee/libc/sbrk.c",
		"ee/libc/string/memcpy.c",
		"ee/libc/string/memset.c",
		"ee/libc/string/memcmp.c",
		"ee/libc/string/strlen.c",
		"ee/libc/string/strcpy.c",
		"ee/libc/string/sprintf.c",
		"ee/libc/string/ctype.c",
		"ee/libc/stdio/printf.c",
		"ee/libc/stdlib/abort.c",
		"ee/libc/stdlib/init.c",
		-- main code
		"ee/score_connection.c",
		"ee/socket.c",
		"ee/target_ps2_ee.c",
	},

	Libs = {
		-- { "c", "gcc", "debug", "kernel", "syscall", "ps2snd", "pad", "m", "fileXio"; Config = "ps2-*-*-*" },
		{ "gcc", "scf", "ent_smap", "eenet", "eenetctl", "graph", "kernl", "m", "sn", "sneetcp"; Config = "ps2-*-*-*" },
	}
	
}

Default "score_ee_target"

