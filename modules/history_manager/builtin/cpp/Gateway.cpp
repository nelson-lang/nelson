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
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"history_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "history_manager", (ptrBuiltin)Nelson::HistoryManagerGateway::history_managerBuiltin, 1, 1,
        CPP_BUILTIN },
    { "history", (ptrBuiltin)Nelson::HistoryManagerGateway::historyBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
static bool
initializeHistoryManagerModule(Nelson::Evaluator* eval)
{
    auto* ptrHistoryManager
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (ptrHistoryManager == nullptr) {
        auto* ptrHistoryManager = new HistoryManager();
        NelsonConfiguration::getInstance()->setHistoryManager((void*)ptrHistoryManager);
    }
    return true;
}
//=============================================================================
static bool
finishHistoryManagerModule(Nelson::Evaluator* eval)
{
    auto* ptrHistoryManager
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (ptrHistoryManager) {
        delete ptrHistoryManager;
    }
    NelsonConfiguration::getInstance()->setHistoryManager(nullptr);
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
