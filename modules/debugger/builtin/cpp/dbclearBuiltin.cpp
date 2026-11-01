//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbclearBuiltin.hpp"
#include "BreakpointTarget.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ErrorID.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::DebuggerGateway;
//=============================================================================
static void
clearInFile(
    Evaluator* eval, const std::wstring& nameOrFile, const wstringVector& tokens, size_t index);
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbclearBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dbclear [all] [in file [at location]], dbclear if error, dbclear if caught error
    nargoutcheck(nLhs, 0, 0);
    wstringVector tokens = breakpointCommandTokens(argIn);
    if (tokens.empty() || (tokens.size() == 1 && tokens[0] == L"all")) {
        eval->clearBreakpoints();
        return {};
    }
    if (tokens[0] == L"if") {
        std::wstring condition;
        for (size_t k = 1; k < tokens.size(); ++k) {
            if (!condition.empty()) {
                condition += L" ";
            }
            condition += tokens[k];
        }
        if (condition == L"error") {
            eval->setStopOnError(false);
            return {};
        }
        if (condition == L"caught error") {
            eval->setStopOnCaughtError(false);
            return {};
        }
        Error(L"Nelson:dbclear:invalidArgument", L"Invalid dbclear syntax.");
    }
    if (tokens[0] == L"all") {
        if (tokens.size() < 3 || tokens[1] != L"in") {
            Error(L"Nelson:dbclear:invalidArgument", L"Invalid dbclear syntax.");
        }
        clearInFile(eval, tokens[2], tokens, 3);
        return {};
    }
    if (tokens[0] != L"in" || tokens.size() < 2) {
        Error(L"Nelson:dbclear:invalidArgument", L"Invalid dbclear syntax.");
    }
    clearInFile(eval, tokens[1], tokens, 2);
    return {};
}
//=============================================================================
void
clearInFile(
    Evaluator* eval, const std::wstring& nameOrFile, const wstringVector& tokens, size_t index)
{
    BreakpointTarget target;
    std::wstring errorMessage;
    if (!resolveBreakpointTarget(eval, nameOrFile, target, errorMessage)) {
        RaiseError(errorMessage, L"Nelson:dbclear:functionNotFound");
    }
    if (index >= tokens.size()) {
        // error breakpoints are kept; nothing is reported when no breakpoint matches
        eval->removeAllBreakpoints(target.filename);
        return;
    }
    if (tokens[index] != L"at" || index + 1 >= tokens.size()) {
        Error(L"Nelson:dbclear:invalidArgument", L"Invalid dbclear syntax.");
    }
    size_t position = 0;
    if (!parseBreakpointLineToken(tokens[index + 1], position)) {
        Error(L"Nelson:dbclear:invalidArgument", L"Invalid dbclear syntax.");
    }
    if (index + 2 < tokens.size()) {
        Error(L"Nelson:dbclear:invalidArgument", L"Invalid dbclear syntax.");
    }
    size_t adjustedLine = position;
    std::wstring adjustError;
    if (eval->adjustBreakpointLine(target.adjustName, position, adjustedLine, adjustError)) {
        eval->removeBreakpoint(target.filename, adjustedLine);
    }
    // the requested line itself, when the adjustment moved elsewhere
    eval->removeBreakpoint(target.filename, position);
}
//=============================================================================
