//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "corrcoefBuiltin.hpp"
#include "CorrelationCoefficients.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::corrcoefBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool needToOverload;
    ArrayOf res;
    if (argIn.size() == 2) {
        // corrcoef(x, y) is equivalent to corrcoef([x(:) y(:)])
        ArrayOf X = argIn[0];
        ArrayOf Y = argIn[1];
        if (X.elementCount() != Y.elementCount()) {
            Error(L"Nelson:statistics:xyLengthMismatch", L"%s must have the same length.",
                L"X and Y");
        }
        X.promoteType(NLS_DOUBLE);
        Y.promoteType(NLS_DOUBLE);
        indexType n = X.elementCount();
        Dimensions dimsM(n, 2);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsM.elementCount());
        ArrayOf M = ArrayOf(NLS_DOUBLE, dimsM, ptr);
        memcpy(ptr, X.dataPointer(), n * sizeof(double));
        memcpy(ptr + n, Y.dataPointer(), n * sizeof(double));
        res = CorrelationCoefficients(M, needToOverload);
    } else {
        res = CorrelationCoefficients(argIn[0], needToOverload);
    }
    if (needToOverload) {
        OverloadRequired("corr");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
