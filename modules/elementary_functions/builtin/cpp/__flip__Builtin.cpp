//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__flip__Builtin.hpp"
#include "Flip.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::__flip__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    indexType dim = (indexType)argIn[1].asInt64Scalar();
    bool needToOverload = false;
    ArrayOf res = Flip(argIn[0], dim, needToOverload);
    if (needToOverload) {
        OverloadRequired("flip");
    }
    retval << res;
    return retval;
}
//=============================================================================
