#include <stdlib.h>

#if defined(PS2_SCE)
#include <eekernel.h>
#else
#include <kernel.h>
#endif

void abort()
{
	SleepThread();
	while (1) {}
}
