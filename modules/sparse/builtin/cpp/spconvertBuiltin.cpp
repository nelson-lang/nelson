//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "spconvertBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SparseConstructors.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::spconvertBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf D(argIn[0]);
    if (D.isEmpty()) {
        ArrayOfVector retval;
        retval << SparseConstructor(D);
        return retval;
    }
    if (!D.is2D() || D.columns() < 3 || D.isSparse()) {
        Error(L"Nelson:spconvert:invalidInputSize", L"Input must be an m-by-3 or m-by-4 matrix.");
    }
    D.promoteType(NLS_DOUBLE);
    indexType rows = D.rows();
    indexType cols = D.columns();
    double* ptrD = (double*)D.dataPointer();

    double* ptrI = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, rows);
    double* ptrJ = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, rows);
    for (indexType k = 0; k < rows; ++k) {
        ptrI[k] = ptrD[k];
        ptrJ[k] = ptrD[k + rows];
    }
    ArrayOf I(NLS_DOUBLE, Dimensions(rows, 1), ptrI);
    ArrayOf J(NLS_DOUBLE, Dimensions(rows, 1), ptrJ);

    ArrayOf V;
    if (cols == 3) {
        double* ptrV = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, rows);
        for (indexType k = 0; k < rows; ++k) {
            ptrV[k] = ptrD[k + 2 * rows];
        }
        V = ArrayOf(NLS_DOUBLE, Dimensions(rows, 1), ptrV);
    } else {
        double* ptrV = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, rows);
        for (indexType k = 0; k < rows; ++k) {
            ptrV[2 * k] = ptrD[k + 2 * rows];
            ptrV[2 * k + 1] = ptrD[k + 3 * rows];
        }
        V = ArrayOf(NLS_DCOMPLEX, Dimensions(rows, 1), ptrV);
    }

    ArrayOfVector retval;
    retval << SparseConstructor(I, J, V);
    return retval;
}
//=============================================================================
