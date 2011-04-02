#include <stddef.h>

void* memcpy(void* dest, const void* src, size_t length)
{
	unsigned char* d = (unsigned char*)(dest);
	const unsigned char* s = (const unsigned char*)(src);

	while (length)
	{
		*d++ = *s++;
		--length;
	}
	return dest;
}
