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
#include "Exception.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
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
    } catch (Exception& e) {
        bEval = false;
        computedmsg = e.getMessage();
    }
    bool bRes = false;
    if (bEval == false) {
        if (computedmsg == expectedmsg) {
            bRes = true;
            msg.clear();
        } else {
            bRes = false;
            msg = formatErrorMessage(_E("nelson:assert:assertionFailedMessageExpectedComputed"),
                expectedmsg, computedmsg);
        }
    } else {
        raiseError(L"Nelson:assert_functions:ERROR_NO_ERROR_PRODUCED_WHILE_EVALUATING_COMMAND",
            ERROR_NO_ERROR_PRODUCED_WHILE_EVALUATING_COMMAND);
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
    } catch (Exception& e) {
        bEval = false;
        computedmsg = e.getMessage();
        computedid = e.getIdentifier();
    }
    bool bRes = false;
    if (bEval == false) {
        if (computedmsg == expectedmsg && computedid == expectedid) {
            bRes = true;
            msg.clear();
        } else {
            bRes = false;

            if (computedmsg != expectedmsg) {
                msg = formatErrorMessage(_E("nelson:assert:assertionFailedMessageExpectedComputed"),
                    expectedmsg, computedmsg);
                return bRes;
            }
            if (computedid != expectedid) {
                msg = formatErrorMessage(_E("nelson:assert:assertionFailedValueExpectedComputed"),
                    expectedid, computedid);
            }
        }
    } else {
        raiseError(L"Nelson:assert_functions:ERROR_NO_ERROR_PRODUCED_WHILE_EVALUATING_COMMAND",
            ERROR_NO_ERROR_PRODUCED_WHILE_EVALUATING_COMMAND);
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
