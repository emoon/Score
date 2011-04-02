
local ps2_common = {

	Env = {
		PS2_ROOT = "c:/dev/ps2root",
		LIBPATH = "$(PS2_ROOT)/ps2sce/sce/ee/lib",
		CPPDEFS = { "PS2_SCE" },
		CPPPATH = { "$(PS2_ROOT)/ps2dev/usr/local/ps2dev/ee/ee/include",
					"$(PS2_ROOT)/ps2dev/usr/local/ps2dev/ee/ee/sys-include",
					"$(PS2_ROOT)/ps2sce/sce/common/include",
					"$(PS2_ROOT)/ps2sce/sce/ee/include" },

		PROGOPTS = "-T $(PS2_ROOT)/ps2dev/usr/local/ps2dev/ee/startup/linkfile"
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

