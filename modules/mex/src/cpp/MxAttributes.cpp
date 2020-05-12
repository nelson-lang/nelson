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
#include "mex.h"
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
bool
mxIsNumeric(const mxArray* pm)
{
    if (pm == nullptr) {
        return false;
    }
    return (pm->classID == mxDOUBLE_CLASS || pm->classID == mxSINGLE_CLASS
        || pm->classID == mxINT8_CLASS || pm->classID == mxUINT8_CLASS
        || pm->classID == mxINT16_CLASS || pm->classID == mxUINT16_CLASS
        || pm->classID == mxINT32_CLASS || pm->classID == mxUINT32_CLASS
        || pm->classID == mxINT64_CLASS || pm->classID == mxUINT64_CLASS);
}
//=============================================================================
bool
mxIsComplex(const mxArray* pm)
{
    if (pm == nullptr) {
        return false;
    }
    return (pm->iscomplex);
}
//=============================================================================
mwSize
mxGetNumberOfDimensions(const mxArray* pm)
{
    if (pm == nullptr) {
        return 0;
    }
    return (pm->number_of_dims);
}
//=============================================================================
size_t
mxGetElementSize(const mxArray* pm)
{
    if (pm == nullptr) {
        return 0;
    }
    return sizeFromClass(pm->classID);
}
//=============================================================================
const mwSize*
mxGetDimensions(const mxArray* pm)
{
    if (pm == nullptr) {
        return nullptr;
    }
    return pm->dims;
}
//=============================================================================
int
mxSetDimensions(mxArray* pm, const mwSize* dims, mwSize ndim)
{
    if (pm == nullptr) {
        return 1;
    }
    while ((ndim > 2) && (dims[ndim - 1] == 1)) {
        ndim--;
    }
    pm->number_of_dims = ndim;
    free(pm->dims);
    pm->dims = copyDims(ndim, dims);
    return 0;
}
//=============================================================================
size_t
mxGetNumberOfElements(const mxArray* array_ptr)
{
    return (size_t)countElements(array_ptr->number_of_dims, array_ptr->dims);
}
//=============================================================================
mwIndex
mxCalcSingleSubscript(const mxArray* pm, mwSize nsubs, mwIndex* subs)
{
    mwIndex index = 0;
    mwIndex iMult = 1;
    if (pm != nullptr) {
        const mwSize* dims = mxGetDimensions(pm);
        for (mwSize i = 0; i < nsubs; i++) {
            index += subs[i] * iMult;
            iMult *= dims[i];
        }
    }
    return index;
}
//=============================================================================
size_t
mxGetM(const mxArray* pm)
{
    if (pm != nullptr) {
        return (size_t)pm->dims[0];
    }
    return 0;
}
//=============================================================================
size_t
mxGetN(const mxArray* pm)
{
    if (pm != nullptr) {
        return (size_t)pm->dims[1];
    }
    return 0;
}
//=============================================================================
void
mxSetM(mxArray* pm, mwSize m)
{
    if (pm != nullptr) {
        pm->dims[0] = m;
    }
}
//=============================================================================
void
mxSetN(mxArray* pm, mwSize n)
{
    if (pm != nullptr) {
        pm->dims[1] = n;
    }
}
//=============================================================================
bool
mxIsEmpty(const mxArray* pm)
{
    return (mxGetNumberOfElements(pm) == 0);
}
//=============================================================================
bool
mxIsFromGlobalWS(const mxArray* pm)
{
    mexErrMsgTxt("mxIsFromGlobalWS() is not yet unimplemented.");
    return false;
}
//=============================================================================
