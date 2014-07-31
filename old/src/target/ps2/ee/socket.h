///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifndef SCORE_SOCKET_H
#define SCORE_SOCKET_H

#include "types.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct Socket;
struct Address;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct Socket* Socket_createTcp(const struct Address* address);
struct Socket* Socket_accept(struct Socket* socket);
struct Socket* Socket_connect(const struct Address* address);

bool Socket_setBlocking(struct Socket* socket, bool blocking);
bool Socket_listen(struct Socket* socket, unsigned int backlog);
bool Socket_close(struct Socket* socket);

bool Socket_send(struct Socket* socket, const void* buffer, unsigned int length);
int Socket_receive(struct Socket* socket, void* buffer, unsigned int length);

bool Socket_stringToAddress(struct Address* address, const char* string);
bool Socket_nameToAddressPort(struct Address** address, const char* hostname, uint16_t port);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif

