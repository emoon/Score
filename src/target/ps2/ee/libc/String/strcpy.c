#include <stddef.h>
#include <_ansi.h>

char* strcpy(char* to, const char* from)
{
	char* save = to;


	for (; (*to = *from) != '\0'; ++from, ++to)
		;

	return save;
}


char *_EXFUN(strncpy,(char* s1, const char* s2, size_t n))
{
	char* os1 = s1;

	n++;

	while ((--n != 0) && ((*s1++ = *s2++) != '\0'))
		;

	if (n != 0)
		while (--n != 0)
			*s1++ = '\0';

	return os1;
}

