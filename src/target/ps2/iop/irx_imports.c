#include "irx_imports.h"

stdio_IMPORTS_start
I_printf
stdio_IMPORTS_end

sysclib_IMPORTS_start
I_memset
I_memcmp
I_memcpy
I_strlen
I_strcmp
I_strcpy
I_strncpy
I_strncmp
I_strtok
I_strchr
I_tolower
I_strcat
I_strrchr
I_strtol
I_index
sysclib_IMPORTS_end

thbase_IMPORTS_start
I_CreateThread 
I_StartThread 
I_SleepThread 
I_GetThreadId 
I_ExitDeleteThread
I_DelayThread 
thbase_IMPORTS_end

ps2ip_IMPORTS_start
I_lwip_send
I_lwip_socket
I_lwip_listen
I_lwip_recv
I_lwip_close
I_lwip_bind
I_lwip_accept
I_lwip_select
I_lwip_getsockopt
I_lwip_setsockopt
I_lwip_connect
I_lwip_getsockname
ps2ip_IMPORTS_end

