#include <stddef.h>

void* memset(void* dest, int c0, size_t length)
{
	unsigned char* d = (unsigned char*)(dest);
	unsigned char c = c0;

	while (length)
	{
		*d++ = c;
		--length;
	}
	return dest;
}
