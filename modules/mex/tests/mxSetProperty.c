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
#include <string.h>
#include <mex.h>
//=============================================================================
#if MX_IS_NELSON
#define GRAPHIC_OBJECT_TYPE "graphic_object"
#else
#define GRAPHIC_OBJECT_TYPE "graphics"
#endif
#define BLUE_INDEX  2
#define GREEN_INDEX 1
#define RED_INDEX   0
//=============================================================================
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    const char *objectClassName;
    mxArray *color_ptr = NULL;
    mxArray *value = NULL;
    double *color = NULL; 
    const char* colorProperty = "Color";
    
    if (nlhs > 1) {
        mexErrMsgTxt("Too many output arguments.");
    }

    if (nrhs != 1) {
        mexErrMsgTxt("Not enough input arguments.");
    }

    objectClassName = mxGetClassName(prhs[0]);
    if (strncmp(objectClassName, GRAPHIC_OBJECT_TYPE, 15)) {
        mexErrMsgTxt("Graphic object expected.");
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
