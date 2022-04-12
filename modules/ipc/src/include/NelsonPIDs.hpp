//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include "nlsIpc_exports.h"
#include "NelSon_engine_mode.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSIPC_IMPEXP
std::vector<int>
getNelsonPIDs();
//=============================================================================
NLSIPC_IMPEXP
std::vector<NELSON_ENGINE_MODE>
getNelsonPIDModes();
//=============================================================================
NLSIPC_IMPEXP
bool
registerPidInSharedMemory(int pid, NELSON_ENGINE_MODE _mode);
//=============================================================================
NLSIPC_IMPEXP
bool
unregisterPidInSharedMemory(int pid);
//=============================================================================
NLSIPC_IMPEXP
int
getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE _mode);
//=============================================================================
NLSIPC_IMPEXP
int
getLatestPidInSharedMemory();
//=============================================================================
NLSIPC_IMPEXP
bool
isPIDRunning(int pID);
//=============================================================================
NLSIPC_IMPEXP
int
getCurrentPID();
//=============================================================================
}
//=============================================================================
