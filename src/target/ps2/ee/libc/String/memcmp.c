#include <stddef.h>

int memcmp(const void* s1, const void* s2, size_t n)
{
	if (n)
	{
		const unsigned char* p1 = (const unsigned char*)(s1);
		const unsigned char* p2 = (const unsigned char*)(s2);

		do
		{
			if (*p1++ != *p2++)
				return (*--p1 - *--p2);
		}
		while (--n != 0);
	}

	return 0;
}
