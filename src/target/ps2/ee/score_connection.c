
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

#define MAX_CONNECTIONS 16

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
		ZENIC_INFO("dev9.irx cannot be loaded");
		return false;
	}

	if ((sceSifLoadModule("host:modules/snstkrel.irx", 0, 0)) < 0) 
	{
		ZENIC_INFO("snstkdbg.irx cannot be loaded.\n");
		return false;
	}

	int res = sockAPIinit(1);

	if (res != 0)
	{
		ZENIC_INFO("EE:sockAPIinit() failed %d", res);
		return false;
	}

	res = sockAPIregthr();

	if (res != 0)
	{
		ZENIC_INFO("EE:sockAPIregthr() failed %d", res);
		return false;
	}

	if (sceSifLoadModule("host:modules/sndrv100.irx", 0, 0) < 0) 
	{
		ZENIC_INFO("sndrv100.irx cannot be loaded.");
		return false;
	}

	if (sceSifLoadModule("host:modules/smap.irx", 0, 0) < 0) 
	{
		ZENIC_INFO("smap.irx cannot be loaded");
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
            ZENIC_INFO("EE:sndev_get_attached() failed %d", res);
            return false;
        }

        if (deviceAttached == SN_DEV_TYPE_NONE)
        {
            ZENIC_INFO("EE:Waiting for device to be ready\n");
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
		ZENIC_INFO("EE:sndev_set_options(SN_DEV_SET_ETHER_IP) failed %d", res);
		return false;
	}

	sn_int32 stackState;
    res = sn_stack_state(SN_STACK_STATE_START, &stackState);
  
    if (res != 0)
    {
        ZENIC_INFO("EE:sn_stack_sate() failed %d", res);
        return res;
    }

    ZENIC_INFO("EE:Waiting for socket API to be readyn");
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

static void networkInit()
{

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

	if (!Socket_nameToAddressPort(&address, "127.0.0.1", 1339))
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

	Socket_setBlocking(sock, false);

	s_listenSocket = sock;

	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void ScoreConnection_safePoint()
{
    uint32_t i, count;

	if (!s_listenSocket)
		return;

	if (Socket_listen(s_listenSocket, 5))
	{
		struct Socket* connection = Socket_accept(s_listenSocket);

		if (connection)
		{
			//sendCommands(connection);
			s_clients[s_clientCount++] = connection;
		}
	}

    for (i = 0, count = s_clientCount; i < count; ++i)
    {
        int size;
        char buffer[1400];

        struct Socket* client = s_clients[i];

        if (!client)
            continue;

        memset(buffer, 0, sizeof(buffer));

        size = Socket_receive(client, buffer, sizeof(buffer));

        if (size < 0)
        {
            printf("Lost connection with client\n");
            s_clients[i] = 0;
            continue;
        }

        if (size <= 0)
            continue;

        // TODO: Handle the data here

    }
}


