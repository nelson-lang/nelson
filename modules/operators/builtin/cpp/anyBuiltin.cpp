//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "anyBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "Any.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::anyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    indexType d = 0;
    ArrayOf arg1 = argIn[0];
    if (argIn.size() > 1) {
        ArrayOf arg2 = argIn[1];
        d = arg2.getContentAsScalarIndex(false);
    }
    bool needToOverload = false;
    ArrayOf res = Any(arg1, d, needToOverload);
    if (needToOverload) {
        OverloadRequired("any");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
