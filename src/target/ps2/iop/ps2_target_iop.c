#include "irx_imports.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void ScoreMain()
{
	// ... nothing here yet
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

s32 _start(int argc, char* argv[])
{
	iop_thread_t mythread;
	int pid;
	int i;

	printf("Score: target server\n");

	// Start socket server thread

	mythread.attr = 0x02000000; 
	mythread.option = 0; 
	mythread.thread = (void*)ScoreMain; 
	mythread.stacksize = 0x1000;
	mythread.priority = 0x43; // just above ps2link

	pid = CreateThread(&mythread);

	if (pid > 0) 
	{
		if ((i = StartThread(pid, NULL)) < 0) 
		{
			printf("StartThread failed (%d)\n", i);
			return MODULE_NO_RESIDENT_END;
		}
	} 
	else 
	{
		printf("CreateThread failed (%d)\n", pid);
		return MODULE_NO_RESIDENT_END;
	}

	return MODULE_RESIDENT_END;
}

