
bool ScoreConnection_create()
{
	Socket* sock;
	Address address;

	memset(s_nameHashes, 0, sizeof(s_nameHashes));
	memset(s_functions, 0, sizeof(s_functions));

	Network_create();

	// Lookup

	if (!Socket_nameToAddressPort(&address, "127.0.0.1", REMOTE_CONSOLE_PORT))
	{
		printf("Failed Socket_nameToAddressPort\n");
		return false;
	}

	// Create socket

	sock = Socket_createTcp(&address);

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
    uint i, count;

	if (!s_listenSocket)
		return;

	if (Socket_listen(s_listenSocket, 5))
	{
		Socket* connection = Socket_accept(s_listenSocket);

		if (connection)
		{
			sendCommands(connection);
			s_clients[s_clientCount++] = connection;
		}
	}

    for (i = 0, count = s_clientCount; i < count; ++i)
    {
        uint iter = 0, functionCount; 
        uint32_t hash;
        uint pos;
        int size;
        char buffer[1400];

        Socket* client = s_clients[i];

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


