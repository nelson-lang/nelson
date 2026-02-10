//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "luBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "LUMatrixFactorization.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::luBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 3);

    ArrayOf A = argIn[0];
    if (!A.is2D()) {
        raiseError(L"Nelson:linear_algebra:ERROR_VECTOR_OR_MATRIX_2D_EXPECTED",
            ERROR_VECTOR_OR_MATRIX_2D_EXPECTED);
    }
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            raiseError(
                L"Nelson:linear_algebra:ERROR_NOT_YET_IMPLEMENTED", ERROR_NOT_YET_IMPLEMENTED);
        }
        retval = LUMatrixFactorizationDoubleReal(A, nLhs);
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            raiseError(
                L"Nelson:linear_algebra:ERROR_NOT_YET_IMPLEMENTED", ERROR_NOT_YET_IMPLEMENTED);
        }
        retval = LUMatrixFactorizationDoubleComplex(A, nLhs);
    } break;
    case NLS_SINGLE: {
        retval = LUMatrixFactorizationSingleReal(A, nLhs);
    } break;
    case NLS_SCOMPLEX: {
        retval = LUMatrixFactorizationSingleComplex(A, nLhs);
    } break;
    default: {
        raiseError(L"Nelson:linear_algebra:ERROR_SINGLE_OR_DOUBLE_TYPE_EXPECTED",
            ERROR_SINGLE_OR_DOUBLE_TYPE_EXPECTED);
    } break;
    }
    return retval;
}
//=============================================================================
