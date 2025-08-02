//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "doubleBuiltin.hpp"
#include "ToDouble.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::doubleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    bool needToOverload;
    ArrayOf res = ToDouble(argIn[0], needToOverload);
    if (needToOverload) {
        OverloadRequired("double");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
