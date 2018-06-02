//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    { "history_manager", Nelson::HistoryManagerGateway::history_managerBuiltin, 1, 1 },
    { "history", Nelson::HistoryManagerGateway::historyBuiltin, 0, 0 },
};
//=============================================================================
static bool
initializeHistoryManagerModule(Nelson::Evaluator* eval)
{
    if (eval->HistoryManager == nullptr) {
        HistoryManager* ptrHistoryManager = new HistoryManager();
        eval->HistoryManager = (void*)ptrHistoryManager;
    }
    return true;
}
//=============================================================================
static bool
finishHistoryManagerModule(Nelson::Evaluator* eval)
{
    if (eval->HistoryManager) {
        HistoryManager* ptrHistoryManager = (HistoryManager*)eval->HistoryManager;
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
