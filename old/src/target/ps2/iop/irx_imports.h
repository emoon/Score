#ifndef PS2_TARGET_IOP_IRX_IMPORTS_H
#define PS2_TARGET_IOP_IRX_IMPORTS_H

#include "irx.h"
#include "intrman.h"
#include "iomanX.h"
#include "ps2ip.h"
#include "sifcmd.h"
#include "sifman.h"
#include "stdio.h"
#include "sysclib.h"
#include "sysmem.h"
#include "thbase.h"
#include "dns.h"
#include "ioman_mod.h"
#include "iopmgr.h"

#define getsockopt lwip_getsockopt
#define setsockopt lwip_setsockopt
#define getsockname lwip_getsockname

#endif 

