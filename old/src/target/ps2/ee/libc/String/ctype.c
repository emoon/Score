int isupper(int c)
{
	return ((c >= 'A') && (c <= 'Z')) ? 1 : 0;
}

int islower(int c)
{
	return ((c >= 'a') && (c <= 'z')) ? 1 : 0;
}

int isalpha(int c)
{
	return (islower(c)||isupper(c)) ? 1 : 0;
}

int isdigit(int c)
{
	return ((c >= '0') && (c <= '9')) ? 1 : 0;
}

int isalnum(int c)
{
	return (isalpha(c)||isdigit(c)) ? 1 : 0;
}

int isspace(int c)
{
	return ((c == '\f') || (c == '\n') || (c == '\r') || (c == ' ') || (c == '\t') || (c == '\v')) ? 1 : 0;
}

int ispunct(int c)
{
	return (isalnum(c)||isspace(c)) ? 1 : 0;
}

int iscntrl(int c)
{
	return c < 32;
}

int isxdigit(int c)
{
	return (isdigit(c) && ((islower(c) && (c <= 'f')) || (isupper(c) && (c <= 'F')))) ? 1 : 0;
}

int isblank(int c)
{
	return ((c == ' ')||(c == '\t')) ? 1 : 0;
}

enum
{
	UpperCase = 1,
	LowerCase = 2,
	Digit = 4,
	Space = 8,
	Punctuation = 16,
	Control = 32,
	Hexadecimal = 64,
	Blank = 128
};

int	look_ctype_table(unsigned int c)
{
	int state = 0;

	if (isupper(c))
		state |= UpperCase;
	if (islower(c))
		state |= LowerCase;
	if (isdigit(c))
		state |= Digit;
	if (isspace(c))
		state |= Space;
	if (ispunct(c))
		state |= Punctuation;
	if (iscntrl(c))
		state |= Control;
	if (isxdigit(c))
		state |= Hexadecimal;
	if (isblank(c))
		state |= Blank;

	return state;
}

char _ctype_[]= {    0,   32,   32,   32,   32,   32,   32,   32,
                    32,   32,   40,   40,   40,   40,   40,   32,
                    32,   32,   32,   32,   32,   32,   32,   32,
                    32,   32,   32,   32,   32,   32,   32,   32,
                    32, -120,   16,   16,   16,   16,   16,   16,
                    16,   16,   16,   16,   16,   16,   16,   16,
                    16,   68,   68,   68,   68,   68,   68,   68,
                    68,   68,   68,   16,   16,   16,   16,   16,
                    16,   16,   65,   65,   65,   65,   65,   65,
                     1,    1,    1,    1,    1,    1,    1,    1,
                     1,    1,    1,    1,    1,    1,    1,    1,
                     1,    1,    1,    1,   16,   16,   16,   16,
                    16,   16,   66,   66,   66,   66,   66,   66,
                     2,    2,    2,    2,    2,    2,    2,    2,
                     2,    2,    2,    2,    2,    2,    2,    2,
                     2,    2,    2,    2,   16,   16,   16,   16,
                    32,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0,    0,    0,    0,    0,    0,    0,    0,
                     0
                };
