//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MExceptionBuiltin.hpp"
#include "MException.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Exception.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MExceptionBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    std::wstring identifier = argIn[0].getContentAsWideString();
    if (argIn.size() == 1) {
        if (identifier == L"reset") {
            eval->resetLastErrorException();
            return retval;
        }
        if (identifier == L"last") {
            retval << ExceptionToArrayOf(eval->getLastErrorException());
            return retval;
        }
        raiseError(L"Nelson:error_manager:ERROR_RESET_OR_LAST_VALUE_EXPECTED",
            ERROR_RESET_OR_LAST_VALUE_EXPECTED);
    }
    if (!isValidMExceptionIdentifier(identifier)) {
        raiseError(
            L"Nelson:error_manager:ERROR_FIRST_INPUT_ARGUMENT_MUST_BE_A_VALID_MESSAGE_IDENTIFIER",
            ERROR_FIRST_INPUT_ARGUMENT_MUST_BE_A_VALID_MESSAGE_IDENTIFIER);
    }
    std::wstring message = argIn[1].getContentAsWideString();
    retval << ExceptionToArrayOf(Exception(message, identifier));
    return retval;
}
//=============================================================================
