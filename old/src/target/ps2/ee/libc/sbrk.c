#include <stddef.h>
#include <stdlib.h>

extern void* _end;
void* sbrk(size_t incr)
{
	static void* current_heap = &_end;
	void* data = current_heap;

	current_heap = (void*)(((unsigned char*)(current_heap)) + incr);
	if ((unsigned int)(current_heap) > (32 * 1024 * 1024))
	{
		//printf("Out of memory\n");
		abort();
	}

	return data;
}
