//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "evalinBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::evalinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    SCOPE_LEVEL scope = SCOPE_LEVEL::LOCAL_SCOPE;
    std::wstring command;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        std::wstring scopeName = argIn[0].asWideString();
        if (scopeName == L"caller" || scopeName == L"base" || scopeName == L"local") {
            if (scopeName == L"caller") {
                scope = SCOPE_LEVEL::CALLER_SCOPE;
            }
            if (scopeName == L"base") {
                scope = SCOPE_LEVEL::BASE_SCOPE;
            }
            if (scopeName == L"local") {
                scope = SCOPE_LEVEL::LOCAL_SCOPE;
            }
        } else {
            Error(L"Nelson:core:invalidOption", L"%s expected.",
                L"Argument #1 : 'base', 'local' or 'caller'");
        }
    } else {
        Error(L"Nelson:types:wrongArgumentTypeExpected",
            L"Wrong type for argument #%d: %s expected.", 1, L"string");
    }
    if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
        command = argIn[1].asWideString();
    } else {
        Error(L"Nelson:types:wrongArgumentTypeExpected",
            L"Wrong type for argument #%d: %s expected.", 2, L"string");
    }
    return EvaluateInCommand(eval, nLhs, scope, command);
}
//=============================================================================
