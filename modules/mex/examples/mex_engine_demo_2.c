//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "engine.h"
//=============================================================================
int main(int argc, char *argv[])
{
 	Engine *ep = NULL;
	char buffer[256], cmd[256];
	if (!(ep = engOpen(""))) {
		fprintf(stderr, "\nCan't start Nelson engine.\n");
		exit(-1);
	}

	engOutputBuffer(ep, buffer, 256);
	
	while (strcmp(cmd, "exit\n") != 0) {
		printf("\nEnter a Nelson command to evaluate.\n");
		printf("For example: X = 1:5\n");
		printf("To finish: exit\n");
		printf("--> ");
		fgets(cmd, 255, stdin);
	
		engEvalString(ep, cmd);
		printf("%s", buffer);
	}
	engClose(ep);
	return EXIT_SUCCESS;
}
//=============================================================================
