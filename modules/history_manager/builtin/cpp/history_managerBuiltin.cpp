//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "history_managerBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HistoryManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HistoryManagerGateway::history_managerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    auto* ptrHistoryManager
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (argIn.empty()) {
        if (ptrHistoryManager) {
            retval << ArrayOf::characterArray(L"on");
        } else {
            retval << ArrayOf::characterArray(L"off");
        }
    } else if (argIn.size() == 1) {
        if (argIn[0].isCharacterArray()) {
            bool bOldMode = false;
            std::wstring arg = argIn[0].asWideString();
            if (ptrHistoryManager) {
                bOldMode = true;
            } else {
                bOldMode = false;
            }
            if (arg.compare(L"on") == 0) {
                if (ptrHistoryManager == nullptr) {
                    auto* ptrHistoryManager = new HistoryManager();
                    NelsonConfiguration::getInstance()->setHistoryManager((void*)ptrHistoryManager);
                }
            } else if (arg.compare(L"off") == 0) {
                if (ptrHistoryManager) {
                    delete ptrHistoryManager;
                }
                NelsonConfiguration::getInstance()->setHistoryManager(nullptr);
            } else {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (1));
            }
            if (bOldMode) {
                retval << ArrayOf::characterArray(L"on");
            } else {
                retval << ArrayOf::characterArray(L"off");
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    return retval;
}
//=============================================================================
