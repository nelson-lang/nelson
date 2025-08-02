//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismemberBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "IsMember.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::ismemberBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    bool needToOverload = false;
    ArrayOf res = IsMember(argIn[0], argIn[1], needToOverload);
    if (needToOverload) {
        OverloadRequired("ismember");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
