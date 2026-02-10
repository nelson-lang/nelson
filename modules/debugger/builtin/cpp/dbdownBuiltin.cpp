//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbdownBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbdownBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!eval) {
        raiseError(L"Nelson:debugger:ERROR_EVALUATOR_IS_NULL", ERROR_EVALUATOR_IS_NULL);
    }

    if (!eval->isBreakpointActive()) {
        raiseError(L"Nelson:debugger:ERROR_NO_ACTIVE_BREAKPOINT", ERROR_NO_ACTIVE_BREAKPOINT);
    }

    int N = 1;
    if (argIn.size() == 1) {
        N = argIn[0].getContentAsInteger32Scalar();
        if (N < 1) {
            raiseError(L"Nelson:debugger:ERROR_N_MUST_BE_POSITIVE_INTEGER",
                ERROR_N_MUST_BE_POSITIVE_INTEGER);
        }
    }

    if (eval->dbDown(N)) {
        // Get the current scope information after moving down
        Scope* currentScope = eval->getContext()->getCurrentScope();
        if (currentScope) {
            std::wstring scopeName = utf8_to_wstring(currentScope->getName());
            std::wstring filename = currentScope->getFilename();
            std::wstring message = L"In workspace belonging to " + scopeName;
            if (!filename.empty()) {
                // Extract just the filename from full path
                size_t lastSlash = filename.find_last_of(L"/\\");
                if (lastSlash != std::wstring::npos) {
                    filename = filename.substr(lastSlash + 1);
                }
            }
            eval->getInterface()->outputMessage(message + L"\n");
        }
    }

    return {};
}
//=============================================================================
