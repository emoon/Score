#if !defined(ZENIC_PS2_SCE)
#include <fileio.h>

int (*_ps2sdk_close)(int) = fioClose;
int (*_ps2sdk_open)(const char*, int) = fioOpen;
int (*_ps2sdk_read)(int, void*, int) = fioRead;
int (*_ps2sdk_lseek)(int, int, int) = fioLseek;
int (*_ps2sdk_write)(int, const void*, int) = fioWrite;

#endif
