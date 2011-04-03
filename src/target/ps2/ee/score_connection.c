
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "score_connection.h"
#include "socket.h"
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <sifrpc.h>

#if defined(PS2_SCE)
#include <eekernel.h>
#include <sifdev.h>

#undef R3000

#ifndef R5900
#define R5900
#endif

#include <sntypes.h>       
#include <sneeutil.h>     
#include <snsocket.h>    
#include <sntcutil.h>   
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define MAX_CONNECTIONS 8

static struct Socket* s_listenSocket = 0;
static struct Socket* s_clients[MAX_CONNECTIONS];
static uint32_t s_clientCount = 0;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static bool Network_create()
{
#if defined(PS2_SCE)
	sceSifInitRpc(0);

	if (sceSifLoadModule("host:modules/dev9.irx", 0, 0) < 0) 
	{
		printf("dev9.irx cannot be loaded\n");
		return false;
	}

	if ((sceSifLoadModule("host:modules/snstkrel.irx", 0, 0)) < 0) 
	{
		printf("snstkdbg.irx cannot be loaded.\n");
		return false;
	}

	int res = sockAPIinit(1);

	if (res != 0)
	{
		printf("EE:sockAPIinit() failed %d\n", res);
		return false;
	}

	res = sockAPIregthr();

	if (res != 0)
	{
		printf("EE:sockAPIregthr() failed %d\n", res);
		return false;
	}

	if ((res = sceSifLoadModule("host:modules/sndrv100.irx", 0, 0)) < 0) 
	{
		printf("sndrv100.irx cannot be loaded %d\n", res);
		return false;
	}

	if ((res = sceSifLoadModule("host:modules/smap.irx", 0, 0)) < 0) 
	{
		printf("smap.irx cannot be loaded %d\n", res);
		return false;
	}

	int deviceAttached = SN_DEV_TYPE_NONE;
	short vendor = 0;
	short product = 0;

    while (deviceAttached == SN_DEV_TYPE_NONE)
    {
        res = sndev_get_attached(0, &deviceAttached, &vendor, &product);

        if (res != 0)
        {
            printf("EE:sndev_get_attached() failed %d", res);
            return false;
        }

        if (deviceAttached == SN_DEV_TYPE_NONE)
        {
            printf("EE:Waiting for device to be ready\n");
            sn_delay(1000);
        }
    }

	// Set up the IP address, mask and gateway (hardcoded for now)

	sndev_set_ether_ip_type etherOpt;
	memset(&etherOpt, 0, sizeof(etherOpt));
	inet_aton("192.168.0.8", (struct in_addr*)&etherOpt.ip_addr);
	inet_aton("255.255.255.0", (struct in_addr*)&etherOpt.sub_mask);
	inet_aton("192.168.0.1", (struct in_addr*)&etherOpt.gateway);

	res = sndev_set_options(0, SN_DEV_SET_ETHER_IP, &etherOpt, sizeof(etherOpt));

	if (res != 0)
	{
		printf("EE:sndev_set_options(SN_DEV_SET_ETHER_IP) failed %d\n", res);
		return false;
	}

	sn_int32 stackState;
    res = sn_stack_state(SN_STACK_STATE_START, &stackState);
  
    if (res != 0)
    {
        printf("EE:sn_stack_sate() failed %d\n", res);
        return res;
    }

    printf("EE:Waiting for socket API to be readyn\n");
    {
        while (sn_socket_api_ready() == SN_FALSE)
        {
            // Delay to avoid hogging the processor
            sn_delay(500);
        }
    }

	return true;
#else
	return false;
#endif
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool ScoreConnection_create()
{
	struct Socket* sock;
	struct Address* address;

	if (!Network_create())
	{
		printf("Failed to create network\n");
		return false;
	}

	// Lookup

	if (!Socket_nameToAddressPort(&address, "192.168.0.8", 1339))
	{
		printf("Failed Socket_nameToAddressPort\n");
		return false;
	}

	// Create socket

	sock = Socket_createTcp(address);

	if (!sock)
	{
		printf("Failed to setup socket\n");
		return false;
	}

	printf("connection created 0x%x\n", sock);

	Socket_setBlocking(sock, false);

	s_listenSocket = sock;

	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void (*runTest)(void);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void ScoreConnection_safePoint()
{
    uint32_t i, count;

    sn_delay(1000);

	if (!s_listenSocket)
		return;

	if (Socket_listen(s_listenSocket, 5))
	{
		struct Socket* connection = Socket_accept(s_listenSocket);

		if (connection)
		{
			printf("got connection!\n");
			s_clients[s_clientCount++] = connection;
			printf("connection %x\n", connection);
		}
	}

    for (i = 0, count = s_clientCount; i < count; ++i)
    {
        int size;
        static char s_buffer[1400] __attribute__ ((aligned (16)));
        void (*foo)();

        struct Socket* client = s_clients[i];

		printf("client %x i %d\n", client, i);

        if (!client)
            continue;

        memset(s_buffer, 0, sizeof(s_buffer));

        size = Socket_receive(client, s_buffer, sizeof(s_buffer));

        if (size < 0)
        {
            printf("Lost connection with client\n");
            s_clients[i] = 0;
            continue;
        }

        if (size <= 0)
            continue;

        foo = (void*)&s_buffer[0];

        FlushCache(0);
        foo();

        // TODO: Handle the data here

    }
}


