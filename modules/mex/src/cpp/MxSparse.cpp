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
#include "MxSparse.h"
#include "MxHelpers.hpp"
//=============================================================================
bool
mxIsSparse(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->issparse;
    }
    return false;
}
//=============================================================================
mxArray*
mxCreateSparseLogicalMatrix(mwSize m, mwSize n, mwSize nzmax)
{
    mxArray* pa = mxNewArray();
    if (pa != nullptr) {
        mwSize num_dim = 2;
        auto* dim_vec = new mwSize[num_dim];
        dim_vec[0] = m;
        dim_vec[1] = n;
        pa->number_of_dims = num_dim;
        pa->dims = dim_vec;
        pa->classID = mxLOGICAL_CLASS;
        pa->issparse = true;
        pa->iscomplex = false;
        pa->imagdata = nullptr;
        pa->realdata = nullptr;
        pa->interleavedcomplex = false;
        pa->ptr = nullptr;
        pa->Ir = nullptr;
        pa->Jc = nullptr;
        pa->nzmax = nzmax;
        pa->nIr = 0;
        pa->nJc = 0;
    }
    return pa;
}
//=============================================================================
static mxArray*
mxCreateSparseInternal(
    mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag, bool interleavedcomplex)
{
    mxArray* pa = mxNewArray();
    if (pa != nullptr) {
        mwSize num_dim = 2;
        auto* dim_vec = new mwSize[num_dim];
        dim_vec[0] = m;
        dim_vec[1] = n;
        pa->number_of_dims = num_dim;
        pa->dims = dim_vec;
        pa->classID = mxDOUBLE_CLASS;
        pa->issparse = true;
        pa->iscomplex = ComplexFlag == mxCOMPLEX;
        pa->imagdata = nullptr;
        pa->realdata = nullptr;
        pa->interleavedcomplex = interleavedcomplex;
        pa->ptr = nullptr;
        pa->Ir = nullptr;
        pa->Jc = nullptr;
        pa->nzmax = nzmax;
        pa->nIr = 0;
        pa->nJc = 0;
    }
    return pa;
}
//=============================================================================
mxArray*
mxCreateSparseInterleavedComplex(mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag)
{
    return mxCreateSparseInternal(m, n, nzmax, ComplexFlag, true);
}
//=============================================================================
mxArray*
mxCreateSparseSeparatedComplex(mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag)
{
    return mxCreateSparseInternal(m, n, nzmax, ComplexFlag, false);
}
//=============================================================================

mwIndex*
mxGetJc(const mxArray* pm)
{
    mwIndex* Jc = nullptr;
    if (mxIsSparse(pm)) {
        Jc = pm->Jc;
    }
    return Jc;
}
//=============================================================================
mwIndex*
mxGetIr(const mxArray* pm)
{
    mwIndex* Ir = nullptr;
    if (mxIsSparse(pm)) {
        Ir = pm->Ir;
    }
    return Ir;
}
//=============================================================================
mwSize
mxGetNzmax(const mxArray* pm)
{
    if (mxIsSparse(pm)) {
        return pm->nzmax;
    }
    return 0;
}
//=============================================================================
void
mxSetNzmax(mxArray* pm, mwSize nzmax)
{
    if (mxIsSparse(pm)) {
        if (nzmax == 0) {
            pm->nzmax = 1;
        } else {
            pm->nzmax = nzmax;
        }
        pm->nIr = pm->nzmax;
    }
}
//=============================================================================
void
mxSetIr(mxArray* pm, mwIndex* ir)
{
    if (mxIsSparse(pm)) {
        pm->Ir = ir;
    }
}
//=============================================================================
void
mxSetJc(mxArray* pm, mwIndex* jc)
{
    if (mxIsSparse(pm)) {
        pm->Jc = jc;
    }
}
//=============================================================================
