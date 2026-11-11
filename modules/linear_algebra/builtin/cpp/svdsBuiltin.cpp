//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "svdsBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "SparseArpack.hpp"
#include "SparseArpackFallback.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isSparseSingleArpackClass(NelsonType dataClass)
{
    return dataClass == NLS_SINGLE || dataClass == NLS_SCOMPLEX;
}
//=============================================================================
static ArrayOfVector
convertSparseArpackSingleOutput(ArrayOfVector retval)
{
    for (size_t k = 0; k < retval.size(); ++k) {
        if (retval[k].dataClass() == NLS_DOUBLE) {
            retval[k].promoteType(NLS_SINGLE);
        } else if (retval[k].dataClass() == NLS_DCOMPLEX) {
            retval[k].promoteType(retval[k].allReal() ? NLS_SINGLE : NLS_SCOMPLEX);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::svdsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 4);
    const ArrayOf& A = argIn[0];
    if (!(A.isNumeric() || A.isLogical()) || A.isIntegerType()) {
        OverloadRequired("svds");
    }
    NelsonType inputClass = A.dataClass();
    ArrayOfVector promotedArgIn(argIn);
    if (inputClass == NLS_SINGLE || inputClass == NLS_LOGICAL) {
        promotedArgIn[0].promoteType(NLS_DOUBLE);
    } else if (inputClass == NLS_SCOMPLEX) {
        promotedArgIn[0].promoteType(NLS_DCOMPLEX);
    }
    // the sparse solver needs k below the smallest dimension and a text option
    indexType limit = A.is2D() ? std::min(A.rows(), A.columns()) : 0;
    bool textOption = argIn.size() < 3 || argIn[2].isRowVectorCharacterArray()
        || argIn[2].isScalarStringArray();
    bool useSparseSolver
        = A.isSparse() && textOption && nLhs <= 3 && SvdsRequestedK(argIn, limit) < limit;
    ArrayOfVector retval
        = useSparseSolver ? SvdsSparseDouble(nLhs, promotedArgIn) : SvdsDense(nLhs, promotedArgIn);
    return isSparseSingleArpackClass(inputClass) ? convertSparseArpackSingleOutput(retval) : retval;
}
//=============================================================================
