//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
