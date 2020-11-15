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
#pragma once
//=============================================================================
#include <string>
//=============================================================================
#include "nlsIpc_exports.h"
#include "ArrayOf.hpp"
#include "DataInterProcessToExchange.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define NELSON_COMMAND_INTERPROCESS "NELSON_COMMAND_INTERPROCESS"
//=============================================================================
NLSIPC_IMPEXP
bool
createNelsonInterprocessReceiver(int pid, bool withEventsLoop);
//=============================================================================
NLSIPC_IMPEXP
void
waitMessageQueueUntilReady(bool withEventsLoop);
//=============================================================================
NLSIPC_IMPEXP
bool
removeNelsonInterprocessReceiver(int pid, bool withEventsLoop);
//=============================================================================
NLSIPC_IMPEXP
bool
sendCommandToNelsonInterprocessReceiver(int pidDestination, const std::wstring& command,
    bool withEventsLoop, std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
bool
sendVariableToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& var,
    const std::wstring& name, const std::wstring& scope, bool withEventsLoop,
    std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
bool
sendMinimizeToNelsonInterprocessReceiver(
    int pidDestination, bool minimize, bool withEventsLoop, std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
bool
isMinimizedFromNelsonInterprocessReceiver(
    int pidDestination, bool withEventsLoop, std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
bool
isVariableFromNelsonInterprocessReceiver(int pidDestination, const std::wstring& name,
    const std::wstring& scope, bool withEventsLoop, std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
bool
isReadyFromNelsonInterprocessReceiver(int pidDestination);
//=============================================================================
NLSIPC_IMPEXP
ArrayOf
getVariableFromNelsonInterprocessReceiver(int pidDestination, const std::wstring& name,
    const std::wstring& scope, bool withEventsLoop, std::wstring& errorMessage);
//=============================================================================
NLSIPC_IMPEXP
bool
sendCommandFileExtensionToNelsonInterprocessReceiver(int pidDestination,
    NELSON_INTERPROCESS_COMMAND commandType, const std::vector<std::wstring>& filenames);
//=============================================================================
}
//=============================================================================
