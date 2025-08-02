//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string.h>
#include <mex.h>
//=============================================================================
#if MX_IS_NELSON
#define GRAPHICS_OBJECT_TYPE "graphics_object"
#else
#define GRAPHICS_OBJECT_TYPE "graphic"
#endif
#define BLUE_INDEX 2
#define GREEN_INDEX 1
#define RED_INDEX 0
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    const char* objectClassName;
    mxArray* color_ptr = NULL;
    mxArray* value = NULL;
    double* color = NULL;
    const char* colorProperty = "Color";

    if (nlhs > 1) {
        mexErrMsgTxt("Too many output arguments.");
    }

    if (nrhs != 1) {
        mexErrMsgTxt("Not enough input arguments.");
    }

    objectClassName = mxGetClassName(prhs[0]);
    if (strncmp(objectClassName, GRAPHICS_OBJECT_TYPE, 15)) {
        mexErrMsgTxt("Graphics object expected.");
    }

    color_ptr = mxGetProperty(prhs[0], 0, colorProperty);
    if (color_ptr == NULL) {
        mexErrMsgTxt("Cannot get property.");
    }

    value = mxDuplicateArray(color_ptr);
    color = mxGetPr(value);

    color[RED_INDEX] = color[RED_INDEX] / 4;
    color[GREEN_INDEX] = color[GREEN_INDEX] / 4;
    color[BLUE_INDEX] = color[BLUE_INDEX] / 4;

    plhs[0] = mxDuplicateArray(prhs[0]);
    mxSetProperty(plhs[0], 0, colorProperty, value);
}
