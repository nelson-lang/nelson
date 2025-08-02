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
    mxArray* v = NULL;
    mxArray* result = NULL;
    double v_values[10] = { 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0 };

    if (!(ep = engOpen(""))) {
        fprintf(stderr, "\nCan't start NELSON engine.\n");
        return EXIT_FAILURE;
    }
    v = mxCreateDoubleMatrix(1, 10, mxREAL);
    memcpy((void*)mxGetPr(v), (void*)v_values, sizeof(v_values));
    if (engPutVariable(ep, "v", v) != 0) {
        fprintf(stderr, "\nCan't not put variable into NELSON engine.\n");
        return EXIT_FAILURE;
    }
    if (engEvalString(ep, "v2 = v * 2;") != 0) {
        fprintf(stderr, "\nCan't not evaluate string into NELSON engine.\n");
        return EXIT_FAILURE;
    }
    if ((result = engGetVariable(ep, "v2")) == NULL) {
        printf("v2 variable does not exist.\n");
    } else {
        printf("v2 type is %s\t\n", mxGetClassName(result));
    }
    mxDestroyArray(result);
    engClose(ep);
    return EXIT_SUCCESS;
}
//=============================================================================
