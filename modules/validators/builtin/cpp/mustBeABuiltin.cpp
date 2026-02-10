//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mustBeABuiltin.hpp"
#include "ValidatorsInternal.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::mustBeABuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 3);
    wstringVector classNames = argIn[1].getContentAsWideStringVector();
    int argPos = -1;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        argPos = param3.getContentAsInteger32Scalar();
        if (argPos < 1) {
            raiseError(L"Nelson:validators:ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER",
                ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER);
        }
    }
    mustBeA(argIn[0], classNames, argPos, true);
    return retval;
}
//=============================================================================
