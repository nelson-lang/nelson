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
#include "MxArrayOf.hpp"
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
    mxArray* pm = mxNewArray();
    if (pm) {
        pm->classID = mxLOGICAL_CLASS;
        pm->issparse = true;
        pm->iscomplex = false;
        pm->number_of_dims = 2;
        pm->dims = (mwSize*)mxCalloc((mwSize)2, sizeof(mwSize));
        pm->dims[0] = m;
        pm->dims[1] = n;
        pm->realdata = (mxDouble*)mxCalloc(nzmax, sizeof(mxDouble));
        pm->nIr = nzmax;
        pm->Ir = (mwIndex*)mxCalloc(nzmax, sizeof(mwIndex));
        pm->nJc = n + 1;
        pm->Jc = (mwIndex*)mxCalloc(n + 1, sizeof(mwIndex));
        pm->nzmax = nzmax;
        pm->ptr = nullptr;
    }
    return pm;
}
//=============================================================================
static mxArray*
mxCreateSparseInternal(
    mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag, bool interleavedcomplex)
{
    mxArray* pm = mxNewArray();
    if (pm) {
        pm->classID = mxDOUBLE_CLASS;
        pm->issparse = true;
        if (ComplexFlag == mxCOMPLEX) {
            pm->iscomplex = true;
        } else {
            pm->iscomplex = false;
        }
        pm->number_of_dims = 2;
        pm->dims = (mwSize*)mxCalloc((mwSize)2, sizeof(mwSize));
        pm->dims[0] = m;
        pm->dims[1] = n;
        pm->interleavedcomplex = interleavedcomplex;
        if (ComplexFlag == mxCOMPLEX) {
            if (interleavedcomplex) {
                pm->realdata = (mxDouble*)mxCalloc(nzmax * 2, sizeof(mxDouble));
            } else {
                pm->realdata = (mxDouble*)mxCalloc(nzmax, sizeof(mxDouble));
                pm->imagdata = (mxDouble*)mxCalloc(nzmax, sizeof(mxDouble));
            }
        } else {
            pm->realdata = (mxDouble*)mxCalloc(nzmax, sizeof(mxDouble));
        }
        pm->nIr = nzmax;
        pm->Ir = (mwIndex*)mxCalloc(nzmax, sizeof(mwIndex));
        pm->nJc = n + 1;
        pm->Jc = (mwIndex*)mxCalloc(n + 1, sizeof(mwIndex));
        pm->nzmax = nzmax;
        pm->ptr = nullptr;
    }
    return pm;
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
