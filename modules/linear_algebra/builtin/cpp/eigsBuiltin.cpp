//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "eigsBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "SparseArpack.hpp"
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
Nelson::LinearAlgebraGateway::eigsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isSparse()) {
        OverloadRequired("eigs");
    }
    NelsonType inputClass = argIn[0].dataClass();
    if (!isSparseSingleArpackClass(inputClass)) {
        return EigsSparseDouble(nLhs, argIn);
    }
    ArrayOfVector promotedArgIn(argIn);
    promotedArgIn[0].promoteType(inputClass == NLS_SINGLE ? NLS_DOUBLE : NLS_DCOMPLEX);
    return convertSparseArpackSingleOutput(EigsSparseDouble(nLhs, promotedArgIn));
}
//=============================================================================
