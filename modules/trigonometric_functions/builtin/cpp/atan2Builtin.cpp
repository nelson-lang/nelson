//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "atan2Builtin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "Atan2.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TrigonometricGateway::atan2Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    bool needToOverload;
    ArrayOf res = Atan2(argIn[0], argIn[1], needToOverload);
    if (needToOverload) {
        OverloadRequired("atan2");
    } else {
        retval << res;
    }

    return retval;
}
//=============================================================================
