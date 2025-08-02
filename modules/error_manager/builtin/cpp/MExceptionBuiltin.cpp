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
        Error("'reset' or 'last' value expected.");
    }
    if (!isValidMExceptionIdentifier(identifier)) {
        Error(_W("First input argument must be a valid message identifier."));
    }
    std::wstring message = argIn[1].getContentAsWideString();
    retval << ExceptionToArrayOf(Exception(message, identifier));
    return retval;
}
//=============================================================================
