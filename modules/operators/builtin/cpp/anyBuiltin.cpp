//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <vector>
#include "anyBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "OverloadRequired.hpp"
#include "Any.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
tryGetDimensionVector(const ArrayOf& arg, std::vector<indexType>& dims)
{
    if (arg.isRowVectorCharacterArray() || arg.isStringArray() || arg.isScalar() || !arg.isNumeric()
        || arg.isComplex() || !arg.isVector()) {
        return false;
    }
    ArrayOf values(arg);
    values.promoteType(NLS_DOUBLE);
    const double* ptr = (const double*)values.dataPointer();
    indexType n = values.elementCount();
    dims.clear();
    for (indexType k = 0; k < n; ++k) {
        double v = ptr[k];
        if (v < 1 || v != std::floor(v)) {
            Error(L"Nelson:operators:invalidDimensionArg",
                L"Dimension argument must be a positive integer scalar or vector of positive "
                L"integers.");
        }
        dims.push_back((indexType)v);
    }
    return true;
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::anyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    indexType d = 0;
    ArrayOf arg1 = argIn[0];
    bool isAll = false;
    bool useVecDim = false;
    std::vector<indexType> vecDims;
    if (argIn.size() > 1) {
        ArrayOf arg2 = argIn[1];

        if (arg2.isRowVectorCharacterArray() || (arg2.isStringArray() && arg2.isScalar())) {
            std::wstring paramAsString = arg2.asWideString();
            if (paramAsString != L"all") {
                Error(L"Nelson:operators:invalidValue", L"Wrong value for #%d argument.", 2);
            } else {
                isAll = true;
            }
        } else if (tryGetDimensionVector(arg2, vecDims)) {
            useVecDim = true;
        } else {
            d = arg2.asScalarIndex(false);
        }
    }
    bool needToOverload = false;
    ArrayOf res;
    if (isAll) {
        res = AnyAll(arg1, needToOverload);
    } else if (useVecDim) {
        res = arg1;
        for (indexType dm : vecDims) {
            res = Any(res, dm, needToOverload);
            if (needToOverload) {
                break;
            }
        }
    } else {
        res = Any(arg1, d, needToOverload);
    }
    if (needToOverload) {
        OverloadRequired("any");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
