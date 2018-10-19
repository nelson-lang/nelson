//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
            retval.push_back(SparseConstructor(A));
        } else {
            retval.push_back(OverloadUnaryOperator(eval, A, "sparse"));
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
    ArrayOfVector retval;
    retval.push_back(SparseConstructor(m, n));
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
            || I.isScalar() && J.isScalar() && V.isScalar()) {
            retval.push_back(SparseConstructor(I, J, V));
        } else {
            Error(_W("in I, J, V format, all three vectors must be the same size or be scalars."));
        }
    } else {
        ArrayOf r = OverloadUnaryOperator(eval, argIn[2], "sparse");
        retval.push_back(r);
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
            retval.push_back(SparseConstructor(I, J, V, m, n, nnz));
        } else {
            retval.push_back(SparseConstructor(I, J, V, m, n));
        }
    } else {
        ArrayOf r = OverloadUnaryOperator(eval, argIn[2], "sparse");
        retval.push_back(r);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparseBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
