//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "log2Builtin.hpp"
#include "Logarithm2.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::log2Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool needToOverload;
    if (nLhs < 2) {
        retval << Logarithm2(argIn[0], needToOverload);
    } else {
        retval = Frexp(argIn[0], needToOverload);
    }
    if (needToOverload) {
        OverloadRequired("log2");
    }
    return retval;
}
//=============================================================================
