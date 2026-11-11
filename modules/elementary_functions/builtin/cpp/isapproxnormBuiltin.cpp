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
#include "isapproxnormBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsApproxNorm.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
needsOverload(const ArrayOf& value)
{
    return value.isCell() || value.isHandle() || value.isStruct() || value.isClassType();
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isapproxnormBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    if (needsOverload(argIn[0]) || needsOverload(argIn[1])) {
        OverloadRequired("isapproxnorm");
    }
    if (!argIn[0].isNumeric() || !argIn[1].isNumeric()) {
        Error(L"Nelson:elementary_functions:numericsTypesExpected", L"Numerics types expected.");
    }
    double precision = 0.;
    if (argIn.size() == 3) {
        const ArrayOf& param = argIn[2];
        bool valid = param.isNumeric() && !param.isComplex() && param.isScalar();
        if (valid) {
            precision = param.asDoubleScalar();
        }
        if (!valid || !std::isfinite(precision) || precision < 0.) {
            Error(L"Nelson:isapproxnorm:invalidPrecision",
                L"Precision must be a nonnegative finite real scalar.");
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(IsApproxNorm(argIn[0], argIn[1], precision));
    return retval;
}
//=============================================================================
