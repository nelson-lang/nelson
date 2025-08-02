//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
static const nlsGateway gateway[] = { { "getpid", (ptrBuiltin)Nelson::IpcGateway::getpidBuiltin, 1,
                                          0, CPP_BUILTIN },
    { "ipc", (ptrBuiltin)Nelson::IpcGateway::ipcBuiltin, -1, 3, CPP_BUILTIN_WITH_EVALUATOR } };
//=============================================================================
static bool
initializeIpcModule(Nelson::Evaluator* eval)
{
    int latestPid = getLatestPidInSharedMemory();
    if (latestPid == 0) {
        RemoveIpcOldFiles();
    }
    int currentPID = getCurrentPID();
    auto mode = (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode();
    registerPidInSharedMemory(currentPID, mode);
    return createNelsonInterprocessReceiver(currentPID, eval->haveEventsLoop());
}
//=============================================================================
static bool
finishIpcModule(Nelson::Evaluator* eval)
{
    int currentPID = getCurrentPID();
    auto mode = (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode();

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
