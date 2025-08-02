//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "engine.h"
//=============================================================================
int
main(int argc, char* argv[])
{
    Engine* ep = NULL;
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
        printf(">> ");
        fgets(cmd, 255, stdin);

        engEvalString(ep, cmd);
        printf("%s", buffer);
    }
    engClose(ep);
    return EXIT_SUCCESS;
}
//=============================================================================
