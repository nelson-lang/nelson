//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "extractAfterBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringModern.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::extractAfterBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    bool needToOverload = false;
    ArrayOf res = StringExtractAfter(argIn[0], argIn[1], needToOverload);
    if (needToOverload) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    ArrayOfVector retval;
    retval << res;
    return retval;
}
//=============================================================================
