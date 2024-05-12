//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "errorBuiltin.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "IsErrorStruct.hpp"
#include "MException.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::errorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    std::wstring message;
    std::wstring identifier;

    if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
            message = argIn[0].getContentAsWideString();
        } else {
            Exception e;
            if (IsErrorStruct(argIn[0], e)) {
                eval->setLastErrorException(e);
                throw e;
            }
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        // nargin == 2
        identifier = argIn[0].getContentAsWideString();
        if (!isValidMExceptionIdentifier(identifier)) {
            Error(_W("First input argument must be a valid message identifier."));
        }
        message = argIn[1].getContentAsWideString();
    }
    if (message != L"") {
        Error(message, identifier);
    }
    return retval;
}
//=============================================================================
