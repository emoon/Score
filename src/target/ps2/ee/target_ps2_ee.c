//
//
//

#include "score_connection.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main()
{
	ScoreConnection_create();
	
	while (1)
	{
		//ScoreConnection_update();
		ScoreConnection_safePoint();
	}

	return 0;
}

