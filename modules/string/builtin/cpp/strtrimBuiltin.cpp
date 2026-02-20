//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strtrimBuiltin.hpp"
#include "Error.hpp"
#include "StringTrim.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strtrimBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf A = argIn[0];
    bool needToOverload;
    ArrayOf res = StringTrim(A, needToOverload);
    if (needToOverload) {
        raiseError2(_E("nelson:validators:mustBeTextAtPosition"), 1);
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
