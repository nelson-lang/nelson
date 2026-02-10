//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mustBePositiveBuiltin.hpp"
#include "ValidatorsInternal.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::mustBePositiveBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    int argPos = -1;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        argPos = param2.getContentAsInteger32Scalar();
        if (argPos < 1) {
            raiseError(L"Nelson:validators:ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER",
                ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER);
        }
    }
    mustBePositive(argIn[0], argPos, true);
    return retval;
}
//=============================================================================
