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
#include "Exception.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "IsErrorStruct.hpp"
#include "MException.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadName.hpp"
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
            if (argIn[0].isClassType() && argIn[0].getClassType() == "message") {
                std::wstring messageId = argIn[0].getField("Identifier").getContentAsWideString();
                FunctionDefPtr funcDef = nullptr;
                if (!eval->lookupFunction(
                        OVERLOAD_FUNCTION_NAME("message", "getString"), funcDef)) {
                    raiseError2(L"nelson:runtime:functionNotFound", L"getString");
                }
                ArrayOfVector inputs;
                inputs << argIn[0];
                try {
                    ArrayOfVector ouputs = funcDef->evaluateFunction(eval, inputs, 1);
                    message = ouputs[0].getContentAsWideString();
                } catch (const Exception&) {
                    raiseError2(L"nelson:runtime:incorrectHoleCount", messageId);
                }
                Error(message, messageId);
            }
            Exception e;
            if (IsErrorStruct(argIn[0], e)) {
                eval->setLastErrorException(e);
                throw e;
            }
            raiseError2(L"nelson:validators:mustBeType", 1, NLS_STRING_ARRAY_STR);
        }
    } else {
        // nargin == 2
        identifier = argIn[0].getContentAsWideString();
        if (!isValidMExceptionIdentifier(identifier)) {
            raiseError(L"Nelson:error_manager:ERROR_FIRST_INPUT_ARGUMENT_MUST_BE_A_VALID_MESSAGE_"
                       L"IDENTIFIER",
                ERROR_FIRST_INPUT_ARGUMENT_MUST_BE_A_VALID_MESSAGE_IDENTIFIER);
        }
        message = argIn[1].getContentAsWideString();
    }
    if (message != L"") {
        if (identifier.empty()) {
            Error(message, L"");
        } else {
            Error(message, identifier);
        }
    }
    return retval;
}
//=============================================================================
