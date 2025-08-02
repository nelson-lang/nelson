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
#include "Error.hpp"
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
        Error(_W("Vector or matrix 2D expected."));
    }
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            Error(_W("Not yet implemented."));
        }
        retval = LUMatrixFactorizationDoubleReal(A, nLhs);
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            Error(_W("Not yet implemented."));
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
        Error(_W("single or double type expected."));
    } break;
    }
    return retval;
}
//=============================================================================
