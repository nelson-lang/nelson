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
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
bool
mxIsScalar(const mxArray* array_ptr)
{
    return (mxGetNumberOfElements(array_ptr) == 1);
}
//=============================================================================
double
mxGetScalar(const mxArray* pm)
{
    if (pm == nullptr) {
        return 0;
    }
    if (mxIsEmpty(pm)) {
        return 0;
    }
    switch (pm->classID) {
    case mxCELL_CLASS:
        return 0;
    case mxSTRUCT_CLASS:
        return 0;
    case mxLOGICAL_CLASS:
        return static_cast<double>(((mxLogical*)pm->realdata)[0]);
    case mxCHAR_CLASS:
        return ((mxChar*)pm->realdata)[0];
    case mxDOUBLE_CLASS:
        return ((mxDouble*)pm->realdata)[0];
    case mxSINGLE_CLASS:
        return ((mxSingle*)pm->realdata)[0];
    case mxINT8_CLASS:
        return ((mxInt8*)pm->realdata)[0];
    case mxUINT8_CLASS:
        return ((mxUint8*)pm->realdata)[0];
    case mxINT16_CLASS:
        return ((mxInt16*)pm->realdata)[0];
    case mxUINT16_CLASS:
        return ((mxUint16*)pm->realdata)[0];
    case mxINT32_CLASS:
        return ((mxInt32*)pm->realdata)[0];
    case mxUINT32_CLASS:
        return ((mxUint32*)pm->realdata)[0];
    case mxINT64_CLASS:
        return (mxDouble)((mxInt64*)pm->realdata)[0];
    case mxUINT64_CLASS:
        return (mxDouble)((mxUint64*)pm->realdata)[0];
    default:
        return 0;
    }
    return 0;
}
//=============================================================================
bool
mxIsDouble(const mxArray* pm)
{
    if (pm != nullptr) {
        return (pm->classID == mxDOUBLE_CLASS);
    }
    return false;
}
//=============================================================================
mxDouble*
mxGetDoubles(const mxArray* pa)
{
    return nullptr;
}
//=============================================================================
int
mxSetDoubles(mxArray* pa, mxDouble* dt)
{
    return 0;
}
//=============================================================================
bool
mxIsSingle(const mxArray* pm)
{
    if (pm != nullptr) {
        return (pm->classID == mxSINGLE_CLASS);
    }
    return false;
}
//=============================================================================
mxSingle*
mxGetSingles(const mxArray* pa)
{
    return nullptr;
}
//=============================================================================
int
mxSetSingles(mxArray* pa, mxSingle* dt)
{
    return 0;
}
//=============================================================================
mxDouble*
mxGetPr(const mxArray* pm)
{
    if (pm != nullptr) {
        return (mxDouble*)pm->realdata;
    }
    return nullptr;
}
//=============================================================================
void
mxSetPr(mxArray* pm, double* pr)
{
    if (pm != nullptr) {
        pm->realdata = pr;
    }
}
//=============================================================================
void*
mxGetData(const mxArray* pm)
{
    if (pm != nullptr) {
        return (void*)pm->realdata;
    }
    return nullptr;
}
//=============================================================================
