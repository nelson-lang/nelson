//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "history_managerBuiltin.hpp"
#include "Error.hpp"
#include "HistoryManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HistoryManagerGateway::history_managerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    if (argIn.empty()) {
        if (eval->HistoryManager) {
            retval << ArrayOf::characterArrayConstructor(L"on");
        } else {
            retval << ArrayOf::characterArrayConstructor(L"off");
        }
    } else if (argIn.size() == 1) {
        if (argIn[0].isCharacterArray()) {
            bool bOldMode = false;
            std::wstring arg = argIn[0].getContentAsWideString();
            if (eval->HistoryManager) {
                bOldMode = true;
            } else {
                bOldMode = false;
            }
            if (arg.compare(L"on") == 0) {
                if (eval->HistoryManager == nullptr) {
                    auto* ptrHistoryManager = new HistoryManager();
                    eval->HistoryManager = (void*)ptrHistoryManager;
                }
            } else if (arg.compare(L"off") == 0) {
                if (eval->HistoryManager) {
                    auto* ptrHistoryManager = static_cast<HistoryManager*>(eval->HistoryManager);
                    delete ptrHistoryManager;
                }
                eval->HistoryManager = nullptr;
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
            if (bOldMode) {
                retval << ArrayOf::characterArrayConstructor(L"on");
            } else {
                retval << ArrayOf::characterArrayConstructor(L"off");
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
