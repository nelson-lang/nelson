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
#include "MxArrayOf.hpp"
//=============================================================================
mxArray*
mxCreateStructArray(mwSize ndim, const mwSize* dims, int nfields, const char** fieldnames)
{
    mxArray* ret = mxNewArray();
    if (ret) {
        Nelson::stringVector _fieldnames;
        for (size_t k = 0; k < nfields; ++k) {
            _fieldnames.push_back(fieldnames[k]);
        }
        Nelson::Dimensions _dims;
        for (mwSize k = 0; k < ndim; ++k) {
            _dims[k] = dims[k];
        }
        Nelson::ArrayOf* st = (Nelson::ArrayOf*)Nelson::ArrayOf::allocateArrayOf(
            Nelson::NLS_STRUCT_ARRAY, _dims.getElementCount(), _fieldnames, true);
        Nelson::ArrayOf s
            = Nelson::ArrayOf(Nelson::NLS_STRUCT_ARRAY, _dims, st, false, _fieldnames);

        mwSize num_dim;
        mwSize* dim_vec = GetDimensions(s, num_dim);
        ret->number_of_dims = num_dim;
        ret->dims = dim_vec;
        ret->classID = mxSTRUCT_CLASS;
        ret->issparse = false;
        ret->iscomplex = false;
        ret->imagdata = nullptr;
        ret->realdata = nullptr;
        Nelson::ArrayOf* ptr = new Nelson::ArrayOf(s);
        ptr->ensureSingleOwner();
        ret->ptr = (uint64_t*)ptr;
    }
    return ret;
}
//=============================================================================
mxArray*
mxCreateStructMatrix(mwSize m, mwSize n, int nfields, const char** fieldnames)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateStructArray(2, dims, nfields, fieldnames);
}
//=============================================================================
bool
mxIsStruct(const mxArray* pm)
{
    if (pm) {
        return (pm->classID == mxSTRUCT_CLASS);
    }
    return false;
}
//=============================================================================
int
mxGetNumberOfFields(const mxArray* pm)
{
    if (mxIsStruct(pm)) {
        Nelson::ArrayOf* ptr = (Nelson::ArrayOf*)pm->ptr;
        Nelson::stringVector fieldnames = ptr->getFieldNames();
        return (int)fieldnames.size();
    }
    return 0;
}
//=============================================================================
mxArray*
mxGetFieldByNumber(const mxArray* pm, mwIndex index, int fieldnumber)
{
    if (!mxIsStruct(pm)) {
        return nullptr;
    }
    if (index >= mxGetNumberOfElements(pm) || index < 0) {
        return nullptr;
    }
    if (fieldnumber >= mxGetNumberOfFields(pm) || fieldnumber < 0) {
        return nullptr;
    }
    Nelson::ArrayOf* ptr = (Nelson::ArrayOf*)pm->ptr;
    const Nelson::ArrayOf* qp = (const Nelson::ArrayOf*)ptr->getDataPointer();
    size_t fieldCount = ptr->getFieldNames().size();
    Nelson::ArrayOf field = qp[index * fieldCount + fieldnumber];
    return Nelson::ArrayOfToMxArray(field);
}
//=============================================================================
