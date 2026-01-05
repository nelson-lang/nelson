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
        Error(_W("Evaluator is null."), _W("dbdown:InvalidEvaluator"));
    }

    if (!eval->isBreakpointActive()) {
        Error(_W("No active breakpoint."));
    }

    int N = 1;
    if (argIn.size() == 1) {
        N = argIn[0].getContentAsInteger32Scalar();
        if (N < 1) {
            Error(_W("N must be a positive integer."), _W("dbdown:InvalidInput"));
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
