//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparseBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseConstructors.hpp"
#include "CheckIJV.hpp"
#include "SparseType.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
sparseBuiltinOneRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf A(argIn[0]);
    if (A.isSparse()
        || (A.is2D()
            && ((A.dataClass() == NLS_DOUBLE) || (A.dataClass() == NLS_DCOMPLEX)
                || (A.dataClass() == NLS_SINGLE) || (A.dataClass() == NLS_SCOMPLEX)
                || (A.dataClass() == NLS_LOGICAL)))) {
        retval << SparseConstructor(A);
    } else {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"sparse constructor");
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
sparseBuiltinTwoRhs(int nLhs, const ArrayOfVector& argIn)
{
    /*
    S = sparse(m, n)
    */
    ArrayOf M = argIn[0];
    ArrayOf N = argIn[1];
    indexType m = M.asScalarIndex();
    indexType n = N.asScalarIndex();
    ArrayOfVector retval(1);
    retval << SparseConstructor(m, n);
    return retval;
}
//=============================================================================
static ArrayOfVector
sparseBuiltinThreeRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf I(argIn[0]);
    ArrayOf J(argIn[1]);
    ArrayOf V(argIn[2]);
    if (I.isComplex() || J.isComplex()) {
        Error(L"Nelson:sparsfcn:nonPositiveIndicesOfSparse",
            L"Sparse matrix indices must be positive integers.");
    }
    if ((V.dataClass() == NLS_DOUBLE || V.dataClass() == NLS_DCOMPLEX || V.dataClass() == NLS_SINGLE
            || V.dataClass() == NLS_SCOMPLEX || V.dataClass() == NLS_LOGICAL)) {
        if (I.isVector() && J.isVector() && V.isVector()
            || I.isScalar() && J.isScalar() && V.isScalar() || I.isEmpty() || J.isEmpty()
            || V.isEmpty()) {
            retval << SparseConstructor(I, J, V);
        } else {
            Error(L"Nelson:sparse:ijvSizeMismatch",
                L"in I, J, V format, all three vectors must be the same size or be scalars.");
        }
    } else {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"sparse constructor");
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
sparseBuiltinFiveOrSixRhs(int nLhs, const ArrayOfVector& argIn)
{
    /*
    S = sparse(i, j, v, m, n)
    S = sparse(i, j, v, m, n, nz)
    */
    ArrayOfVector retval;
    ArrayOf I(argIn[0]);
    ArrayOf J(argIn[1]);
    ArrayOf V(argIn[2]);
    ArrayOf M(argIn[3]);
    ArrayOf N(argIn[4]);
    indexType m = M.asScalarIndex();
    indexType n = N.asScalarIndex();
    indexType nnz = 0;
    if (argIn.size() == 6) {
        ArrayOf NNZ(argIn[5]);
        nnz = NNZ.asScalarIndex(true);
    }
    if ((V.dataClass() == NLS_DOUBLE || V.dataClass() == NLS_DCOMPLEX || V.dataClass() == NLS_SINGLE
            || V.dataClass() == NLS_SCOMPLEX || V.dataClass() == NLS_LOGICAL)) {
        if (argIn.size() == 6) {
            retval << SparseConstructor(I, J, V, m, n, nnz);
        } else {
            retval << SparseConstructor(I, J, V, m, n);
        }
    } else {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"sparse constructor");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 6);
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 1:
        return sparseBuiltinOneRhs(nLhs, argIn);
    case 2:
        return sparseBuiltinTwoRhs(nLhs, argIn);
    case 3:
        return sparseBuiltinThreeRhs(nLhs, argIn);
    case 5:
    case 6:
        return sparseBuiltinFiveOrSixRhs(nLhs, argIn);
    default: {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
