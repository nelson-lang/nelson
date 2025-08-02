//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
//=============================================================================
#include "nlsIpc_exports.h"
#include "ArrayOf.hpp"
// #include "DataInterProcessToExchange.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSIPC_IMPEXP
bool
createNelsonInterprocessReceiver(int pid, bool withEventsLoop);
//=============================================================================
NLSIPC_IMPEXP
bool
removeNelsonInterprocessReceiver(int pid, bool withEventsLoop);
//=============================================================================
NLSIPC_IMPEXP
bool
evalCommandToNelsonInterprocessReceiver(int pidDestination, const std::wstring& command,
    bool withEventsLoop, std::wstring& result, std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP bool
sendVariableToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& var,
    const std::wstring& name, const std::wstring& scope, bool withEventsLoop,
    std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
bool
sendMinimizeToNelsonInterprocessReceiver(
    int pidDestination, bool minimize, bool withEventsLoop, std::wstring& errorMessage);
// //=============================================================================
NLSIPC_IMPEXP
bool
isMinimizedFromNelsonInterprocessReceiver(
    int pidDestination, bool withEventsLoop, std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
ArrayOf
getVariableFromNelsonInterprocessReceiver(int pidDestination, const std::wstring& name,
    const std::wstring& scope, bool withEventsLoop, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
