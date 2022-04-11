//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparseBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadUnaryOperator.hpp"
#include "SparseConstructors.hpp"
#include "CheckIJV.hpp"
#include "SparseType.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
sparseBuiltinOneRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "sparse", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf A(argIn[0]);
        if (A.isSparse()
            || (A.is2D()
                   && ((A.getDataClass() == NLS_DOUBLE) || (A.getDataClass() == NLS_DCOMPLEX)
                          || (A.getDataClass() == NLS_LOGICAL)))) {
            retval << SparseConstructor(A);
        } else {
            retval << OverloadUnaryOperator(eval, A, "sparse");
        }
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
sparseBuiltinTwoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    /*
    S = sparse(m, n)
    */
    ArrayOf M = argIn[0];
    ArrayOf N = argIn[1];
    indexType m = M.getContentAsScalarIndex();
    indexType n = N.getContentAsScalarIndex();
    ArrayOfVector retval(1);
    retval << SparseConstructor(m, n);
    return retval;
}
//=============================================================================
static ArrayOfVector
sparseBuiltinThreeRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf I(argIn[0]);
    ArrayOf J(argIn[1]);
    ArrayOf V(argIn[2]);
    if ((V.getDataClass() == NLS_DOUBLE || V.getDataClass() == NLS_DCOMPLEX
            || V.getDataClass() == NLS_LOGICAL)
        && !V.isSparse()) {
        if (I.isVector() && J.isVector() && V.isVector()
            || I.isScalar() && J.isScalar() && V.isScalar() || I.isEmpty() || J.isEmpty()
            || V.isEmpty()) {
            retval << SparseConstructor(I, J, V);
        } else {
            Error(_W("in I, J, V format, all three vectors must be the same size or be scalars."));
        }
    } else {
        retval << OverloadUnaryOperator(eval, argIn[2], "sparse");
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
sparseBuiltinFiveOrSixRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
    indexType m = M.getContentAsScalarIndex();
    indexType n = N.getContentAsScalarIndex();
    indexType nnz = 0;
    if (argIn.size() == 6) {
        ArrayOf NNZ(argIn[5]);
        nnz = NNZ.getContentAsScalarIndex(false);
    }
    if ((V.getDataClass() == NLS_DOUBLE || V.getDataClass() == NLS_DCOMPLEX
            || V.getDataClass() == NLS_LOGICAL)
        && !V.isSparse()) {
        if (argIn.size() == 6) {
            retval << SparseConstructor(I, J, V, m, n, nnz);
        } else {
            retval << SparseConstructor(I, J, V, m, n);
        }
    } else {
        retval << OverloadUnaryOperator(eval, argIn[2], "sparse");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparseBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 1:
        return sparseBuiltinOneRhs(eval, nLhs, argIn);
    case 2:
        return sparseBuiltinTwoRhs(eval, nLhs, argIn);
    case 3:
        return sparseBuiltinThreeRhs(eval, nLhs, argIn);
    case 5:
    case 6:
        return sparseBuiltinFiveOrSixRhs(eval, nLhs, argIn);
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
