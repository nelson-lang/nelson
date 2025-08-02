//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "replaceBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringReplace.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::replaceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    bool needToOverload;
    ArrayOf res = Replace(argIn[0], argIn[1], argIn[2], needToOverload);
    if (needToOverload) {
        Error(_W("Invalid input argument(s): cell or string expected."));
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
