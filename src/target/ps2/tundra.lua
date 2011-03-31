
local ps2_common = {

	Env = {
		PROGOPTS = "-T Shared/External/ps2sdk/ee/startup/linkfile"
	},

	ReplaceEnv = {
		PROGCOM = "$(LD) $(PROGOPTS) $(LIBPATH:p-L) -o $(@) -Wl,--start-group $(LIBS:p-l) $(<) -Wl,--end-group"
	}
}

Build {
	Units = "units.lua",
	Configs = {
			{
				Name = "ps2-gcc",
				Virtual = true,
				Inherit = ps2_common,
				Tools = { "gcc" },
				ReplaceEnv = {
				PROGSUFFIX = ".elf",
				CC = "ee-gcc",
				AR = "ee-ar",
				LD = "ee-gcc",
			},
		},	
	},
}

