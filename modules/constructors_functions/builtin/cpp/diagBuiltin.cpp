//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "diagBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::diagBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    ArrayOf b;
    int64* dp;
    int64 diagonalOrder;
    if (argIn.size() == 1) {
        diagonalOrder = 0;
    } else {
        b = argIn[1];
        if (!b.isScalar()) {
            Error(_W("Second argument must be a scalar."));
        }
        b.promoteType(NLS_INT64);
        dp = (int64*)b.getDataPointer();
        diagonalOrder = dp[0];
    }

    ArrayOf a = argIn[0];
    if (!a.is2D()) {
        Error(_W("First argument to 'diag' function must be 2D."));
    }
    ArrayOf diag;
    if ((a.getDimensionLength(1) == 1) || (a.getDimensionLength(0) == 1)) {
        diag = ArrayOf::diagonalConstructor(a, diagonalOrder);
    } else {
        diag = a.getDiagonal(diagonalOrder);
    }
    retval << diag;
    return retval;
}
//=============================================================================
