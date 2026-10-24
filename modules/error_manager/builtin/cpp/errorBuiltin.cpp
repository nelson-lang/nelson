//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "errorBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ErrorFunction.hpp"
#include "StringPrintf.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::errorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 0);

    ErrorParseResult parsed = ParseErrorFunctionArguments(argIn);
    if (parsed.needsFormatting) {
        std::wstring message;
        std::wstring errorMessage;
        if (!StringPrintf(message, errorMessage, eval, parsed.formatArguments)) {
            Error(L"Nelson:error:formatFailed", errorMessage);
        }
        parsed.exception.setMessage(message);
    }
    ThrowParsedError(eval, parsed.exception, parsed.useCurrentStack);
    return retval;
}
//=============================================================================
