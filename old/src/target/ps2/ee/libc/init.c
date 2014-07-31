extern void _ps2sdk_stdlib_init();
extern void _ps2sdk_stdlib_deinit();


void _ps2sdk_libc_init()
{
	_ps2sdk_stdlib_init();
}

void _ps2sdk_libc_deinit()
{
	_ps2sdk_stdlib_deinit();
}

void _ps2sdk_args_parse(int argc, char ** argv)
{
}

