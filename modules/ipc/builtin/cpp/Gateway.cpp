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
#include "NelsonGateway.hpp"
#include "getpidBuiltin.hpp"
#include "ipcBuiltin.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "RemoveIpcOldFiles.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"ipc";
//=============================================================================
static const nlsGateway gateway[]
    = { { "getpid", (void*)Nelson::IpcGateway::getpidBuiltin, 1, 0, CPP_BUILTIN },
          { "ipc", (void*)Nelson::IpcGateway::ipcBuiltin, -1, 3, CPP_BUILTIN_WITH_EVALUATOR } };
//=============================================================================
static bool
initializeIpcModule(Nelson::Evaluator* eval)
{
    int latestPid = getLatestPidInSharedMemory();
    if (latestPid == 0) {
        RemoveIpcOldFiles();
    }
    int currentPID = getCurrentPID();
    auto mode = (NELSON_ENGINE_MODE)eval->getNelsonEngineMode();
    registerPidInSharedMemory(currentPID, mode);
    createNelsonInterprocessReceiver(currentPID, eval->haveEventsLoop());
    return true;
}
//=============================================================================
static bool
finishIpcModule(Nelson::Evaluator* eval)
{
    int currentPID = getCurrentPID();
    auto mode = (NELSON_ENGINE_MODE)eval->getNelsonEngineMode();

    removeNelsonInterprocessReceiver(currentPID, eval->haveEventsLoop());
    unregisterPidInSharedMemory(currentPID);
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeIpcModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishIpcModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
