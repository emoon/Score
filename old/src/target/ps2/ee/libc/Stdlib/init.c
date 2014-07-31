void                 (* __stdlib_exit_func[32])(void);
int                  __stdlib_exit_index = 0;

void _ps2sdk_stdlib_init()
{
}

void _ps2sdk_stdlib_deinit()
{
        int i;

        for (i = (__stdlib_exit_index - 1); i >= 0; --i)
        {
                (__stdlib_exit_func[i])();
        }
}
