#include "Socket.h"
#include <string.h>

#if defined(PS2_SCE)
#undef R3000

#ifndef R5900
#define R5900
#endif

#include <sntypes.h>       
#include <sneeutil.h>     
#include <snsocket.h>    
#include <sntcutil.h>   

#else

#include <tamtypes.h>
#include <ps2ip.h>
#include <tcpip.h>

#endif 

#include <stdio.h>

#define MAX_SOCKETS 16

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct Address
{
  uint8_t data[4096];
  uint32_t size;
} Address;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct Socket
{
	Address address;
	int socket;
} Socket;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static Socket s_sockets[MAX_SOCKETS];
static uint32_t s_socketCount = 0;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Socket* Socket_createTcp(const Address* address)
{
    int socketHandle = socket(AF_INET, SOCK_STREAM, PF_INET);
    if (socketHandle < 0)
    {
        printf("EE:Error Opening socket\n");
        return 0;
    }
	
	struct sockaddr* saddr = (struct sockaddr*)address->data;
	uint32_t size = address->size; 
	int err = bind(socketHandle, saddr, size);
	if (err != 0)
	{
		printf("Unable to bind TcpSocket %d\n", err);
#if defined(PS2_SCE)
		closesocket(socketHandle);
#else
		disconnect(socketHandle);
#endif
		return 0;
	}

	Socket* sock = &s_sockets[s_socketCount++];
	sock->address = *address;
	sock->socket = socketHandle;

	//ASSERT(s_socketCount < MAX_SOCKETS);

	return sock;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool Socket_setBlocking(Socket* sock, bool blocking)
{
#if defined(PS2_SCE)
	int block = blocking ? 0 : 1;

    if (setsockopt(sock->socket, SOL_SOCKET, SO_NONBLOCK, (char*)&block, sizeof(block))!=0)
    {
        printf("EE:setsockopt(SO_NONBLOCK) failed: %s\n", sn_errno(sock->socket));
        return false;
    }
#endif
	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Socket* Socket_connect(const Address* address)
{
	uint32_t size;
	struct sockaddr* saddr;
	Socket* sock;

    int socketHandle = socket(AF_INET, SOCK_STREAM, PF_INET);
    if (socketHandle < 0)
    {
        printf("EE:Error Opening socket\n");
        return 0;
    }

	saddr = (struct sockaddr*)address->data;
	size = address->size;

	if (connect(socketHandle, (const struct sockaddr*)address->data, address->size) < 0)
	{
        printf("EE:Error connecting\n");
		return 0;
	}

	sock = &s_sockets[s_socketCount++];
	sock->address = *address;
	sock->socket = socketHandle;

	if (s_socketCount >= MAX_SOCKETS)
	{
		printf("EE:out of max sockets\n");
		return 0;
	}

	return sock;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool Socket_listen(Socket* sock, uint32_t backlog)
{
	int err = listen(sock->socket, backlog);

	if (err < 0)
	{
		printf("EE:listen failed\n");
		return false;
	}

	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool Socket_close(Socket* sock)
{
#if defined(PS2_SCE)
	int error = closesocket(sock->socket);
#else
	int error = disconnect(sock->socket);
#endif

	sock->socket = 0;

	if (error < 0)
		return false;

	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Socket* Socket_accept(Socket* sock)
{
	Socket* newSocket = 0;

	int s = accept(sock->socket, 0, 0);
	if (s < 0)
		return 0;

	newSocket = &s_sockets[s_socketCount++];
	newSocket->socket = s;

	//ASSERT(s_socketCount < MAX_SOCKETS);

	return newSocket;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool Socket_send(Socket* sock, const void* buffer, uint32_t length)
{
#if defined(PS2SCE)
	const sockaddr* address = (const struct sockaddr*)sock->address.data;
#else
	struct sockaddr* address = (struct sockaddr*)sock->address.data;
#endif

#if defined(PS2SCE)
	int error = sendto(sock->socket, (const char*)buffer, length, 0, address, sizeof(struct sockaddr_in));
#else
	int error = sendto(sock->socket, (char*)buffer, length, 0, address, sizeof(struct sockaddr_in));
#endif
	if (error >= 0)
		return true;

	return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int Socket_receive(Socket* sock, void* buffer, uint32_t length)
{
	int readSize = recv(sock->socket, (char*)(buffer), length, 0);

	if (readSize >= 0)
	{
		return readSize;
	}

	if (readSize < 0)
	{
		// ...
	}		

	return 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool Socket_nameToAddressPort(Address** outAddress, const char* hostname, uint16_t port)
{
	static Address s_address;
	Address* address = &s_address;
	*outAddress = address;
#if defined(PS2_SCE)		
	struct sockaddr_in saddr;

	memset(&saddr, 0, sizeof(saddr));

	inet_aton(hostname, &saddr.sin_addr);

	saddr.sin_family = AF_INET;
	saddr.sin_port = htons(port);

	memcpy(address->data, &saddr, sizeof(saddr));
	address->size = sizeof(saddr);

	return true;
#else
	(void)address;
	(void)hostname;
	(void)port;
	return false;
#endif
}

