//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HistoryManager.hpp"
#include "NelsonGateway.hpp"
#include "historyBuiltin.hpp"
#include "history_managerBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"history_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "history_manager", (ptrBuiltin)Nelson::HistoryManagerGateway::history_managerBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "history", (ptrBuiltin)Nelson::HistoryManagerGateway::historyBuiltin, 0, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
static bool
initializeHistoryManagerModule(Nelson::Evaluator* eval)
{
    if (eval->HistoryManager == nullptr) {
        auto* ptrHistoryManager = new HistoryManager();
        eval->HistoryManager = (void*)ptrHistoryManager;
    }
    return true;
}
//=============================================================================
static bool
finishHistoryManagerModule(Nelson::Evaluator* eval)
{
    if (eval->HistoryManager) {
        auto* ptrHistoryManager = static_cast<HistoryManager*>(eval->HistoryManager);
        delete ptrHistoryManager;
    }
    eval->HistoryManager = nullptr;
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeHistoryManagerModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishHistoryManagerModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
