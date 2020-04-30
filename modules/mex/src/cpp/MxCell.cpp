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
mxArray*
mxCreateCellArray(mwSize ndim, const mwSize* dims)
{
    return mxAllocateRealArray(ndim, dims, sizeof(void*), mxCELL_CLASS);
}
//=============================================================================
mxArray*
mxCreateCellMatrix(mwSize m, mwSize n)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateCellArray(2, dims);
}
//=============================================================================
bool
mxIsCell(const mxArray* pm)
{
    if (pm) {
        return (pm->classID == mxCELL_CLASS);
    }
    return false;
}
//=============================================================================
mxArray*
mxGetCell(const mxArray* pm, mwIndex index)
{
    if (pm) {
        return (((mxArray**)pm->realdata)[index]);
    }
    return nullptr;
}
//=============================================================================
void
mxSetCell(mxArray* pm, mwIndex index, mxArray* value)
{
    if (pm == nullptr) {
        return;
    }
    if (!mxIsCell(pm)) {
        return;
    }
    ((mxArray**)pm->realdata)[index] = value;
}
//=============================================================================
