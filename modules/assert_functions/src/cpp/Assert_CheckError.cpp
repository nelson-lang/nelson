//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Assert_CheckError.hpp"
#include "Error.hpp"
#include "EvaluateCommand.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
Assert_CheckError(Evaluator* eval, const std::wstring& command, const std::wstring& expectedmsg,
    std::wstring& msg)
{
    bool bEval = false;
    std::wstring computedmsg;
    try {
        bEval = EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        bEval = false;
        computedmsg = e.message();
    }
    bool bRes = false;
    if (bEval == false) {
        if (computedmsg == expectedmsg) {
            bRes = true;
            msg.clear();
        } else {
            bRes = false;
            msg = formatI18nMessage(
                resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedErrorMessage",
                    L"Assertion failed: expected error message = \"%s\" computed error message = "
                    L"\"%s\""),
                expectedmsg, computedmsg);
        }
    } else {
        msg = Nelson::resolveMessageTemplate(
            L"Nelson:assert_functions:errorProducedWhileEvaluating",
            L"No error was produced while evaluating command.");
    }
    return bRes;
}
//=============================================================================
bool
Assert_CheckError(Evaluator* eval, const std::wstring& command, const std::wstring& expectedmsg,
    const std::wstring& expectedid, std::wstring& msg)
{
    bool bEval = false;
    std::wstring computedmsg;
    std::wstring computedid;
    try {
        bEval = EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        bEval = false;
        computedmsg = e.message();
        computedid = e.identifier();
    }
    bool bRes = false;
    if (bEval == false) {
        if (computedmsg == expectedmsg && computedid == expectedid) {
            bRes = true;
            msg.clear();
        } else {
            bRes = false;
            if (computedmsg != expectedmsg) {
                msg = formatI18nMessage(
                    resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedErrorMessage",
                        L"Assertion failed: expected error message = \"%s\" computed error message "
                        L"= \"%s\""),
                    expectedmsg, computedmsg);
                return bRes;
            }
            if (computedid != expectedid) {
                msg = formatI18nMessage(
                    resolveMessageTemplate(
                        L"Nelson:assert_functions:assertionFailedErrorIdentifier",
                        L"Assertion failed: expected error identifier = \"%s\" computed error "
                        L"identifier = \"%s\""),
                    expectedid, computedid);
            }
        }
    } else {
        msg = Nelson::resolveMessageTemplate(
            L"Nelson:assert_functions:errorProducedWhileEvaluating",
            L"No error was produced while evaluating command.");
    }
    return bRes;
}
//=============================================================================
bool
Assert_CheckErrorContains(Evaluator* eval, const std::wstring& command,
    const std::wstring& expectedSubstring, std::wstring& msg)
{
    bool bEval = false;
    std::wstring computedmsg;
    try {
        bEval = EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        bEval = false;
        computedmsg = e.message();
    }
    if (bEval) {
        msg = Nelson::resolveMessageTemplate(
            L"Nelson:assert_functions:errorProducedWhileEvaluating",
            L"No error was produced while evaluating command.");
        return false;
    }
    if (computedmsg.find(expectedSubstring) != std::wstring::npos) {
        msg.clear();
        return true;
    }
    msg = formatI18nMessage(
        resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedErrorMessage2",
            L"Assertion failed: expected error message to contain = \"%s\" computed error message "
            L"= \"%s\""),
        expectedSubstring, computedmsg);
    return false;
}
//=============================================================================
bool
Assert_CheckErrorContains(Evaluator* eval, const std::wstring& command,
    const std::wstring& expectedSubstring, const std::wstring& expectedid, std::wstring& msg)
{
    bool bEval = false;
    std::wstring computedmsg;
    std::wstring computedid;
    try {
        bEval = EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        bEval = false;
        computedmsg = e.message();
        computedid = e.identifier();
    }
    if (bEval) {
        msg = Nelson::resolveMessageTemplate(
            L"Nelson:assert_functions:errorProducedWhileEvaluating",
            L"No error was produced while evaluating command.");
        return false;
    }
    if (computedmsg.find(expectedSubstring) != std::wstring::npos && computedid == expectedid) {
        msg.clear();
        return true;
    }
    if (computedmsg.find(expectedSubstring) == std::wstring::npos) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedErrorMessage2",
                L"Assertion failed: expected error message to contain = \"%s\" computed error "
                L"message = \"%s\""),
            expectedSubstring, computedmsg);
        return false;
    }
    msg = formatI18nMessage(
        resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedErrorIdentifier",
            L"Assertion failed: expected error identifier = \"%s\" computed error identifier = "
            L"\"%s\""),
        expectedid, computedid);
    return false;
}
//=============================================================================
bool
Assert_CheckWarning(
    Evaluator* eval, const std::wstring& command, const std::wstring& expected, std::wstring& msg)
{
    eval->resetLastWarningException();
    try {
        EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        msg = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:assert_functions:unexpectedErrorProducedWhile",
                L"Unexpected error was produced while evaluating command: %s"),
            e.message());
        return false;
    }
    NelsonException warning = eval->getLastWarningException();
    std::wstring computedmsg = warning.message();
    std::wstring computedid = warning.identifier();
    if (computedmsg.empty() && computedid.empty()) {
        msg = Nelson::resolveMessageTemplate(
            L"Nelson:assert_functions:warningProducedWhileEvaluating",
            L"No warning was produced while evaluating command.");
        return false;
    }
    if (computedmsg == expected || computedid == expected) {
        msg.clear();
        return true;
    }
    msg = formatI18nMessage(
        resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedWarningMessage2",
            L"Assertion failed: expected warning message or identifier = \"%s\" computed warning "
            L"message = \"%s\" computed warning identifier = \"%s\""),
        expected, computedmsg, computedid);
    return false;
}
//=============================================================================
bool
Assert_CheckWarning(Evaluator* eval, const std::wstring& command, const std::wstring& expectedmsg,
    const std::wstring& expectedid, std::wstring& msg)
{
    eval->resetLastWarningException();
    try {
        EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        msg = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:assert_functions:unexpectedErrorProducedWhile",
                L"Unexpected error was produced while evaluating command: %s"),
            e.message());
        return false;
    }
    NelsonException warning = eval->getLastWarningException();
    std::wstring computedmsg = warning.message();
    std::wstring computedid = warning.identifier();
    if (computedmsg.empty() && computedid.empty()) {
        msg = Nelson::resolveMessageTemplate(
            L"Nelson:assert_functions:warningProducedWhileEvaluating",
            L"No warning was produced while evaluating command.");
        return false;
    }
    if (computedmsg == expectedmsg && computedid == expectedid) {
        msg.clear();
        return true;
    }
    if (computedmsg != expectedmsg) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedWarningMessage",
                L"Assertion failed: expected warning message = \"%s\" computed warning message = "
                L"\"%s\""),
            expectedmsg, computedmsg);
        return false;
    }
    msg = formatI18nMessage(
        resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedWarningIdentifier",
            L"Assertion failed: expected warning identifier = \"%s\" computed warning identifier = "
            L"\"%s\""),
        expectedid, computedid);
    return false;
}
//=============================================================================
bool
Assert_WarningFree(Evaluator* eval, const std::wstring& command, std::wstring& msg)
{
    eval->resetLastWarningException();
    try {
        EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        msg = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:assert_functions:unexpectedErrorProducedWhile",
                L"Unexpected error was produced while evaluating command: %s"),
            e.message());
        return false;
    }
    NelsonException warning = eval->getLastWarningException();
    std::wstring computedmsg = warning.message();
    std::wstring computedid = warning.identifier();
    if (computedmsg.empty() && computedid.empty()) {
        msg.clear();
        return true;
    }
    msg = formatI18nMessage(
        Nelson::resolveMessageTemplate(L"Nelson:assert_functions:unexpectedWarningProducedWhile",
            L"Unexpected warning was produced while evaluating command: \"%s\" identifier = "
            L"\"%s\""),
        computedmsg, computedid);
    return false;
}
//=============================================================================
bool
Assert_NoError(Evaluator* eval, const std::wstring& command, std::wstring& msg)
{
    try {
        EvaluateCommand(eval, command, true);
    } catch (NelsonException& e) {
        msg = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:assert_functions:unexpectedErrorProducedWhile",
                L"Unexpected error was produced while evaluating command: %s"),
            e.message());
        return false;
    }
    msg.clear();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
