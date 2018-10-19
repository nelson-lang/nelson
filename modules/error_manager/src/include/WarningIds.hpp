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
#pragma once
//=============================================================================
#include "nlsError_manager_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    DISABLED,
    ENABLED,
    AS_ERROR,
    NOT_FOUND
} WARNING_STATE;
//=============================================================================
typedef struct
{
    std::vector<std::wstring> IDs;
    std::vector<WARNING_STATE> states;
} WARNING_IDS_STATES;
//=============================================================================
NLSERROR_MANAGER_IMPEXP WARNING_STATE
warningCheckState(const std::wstring& id);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
initializeDefaultWarningIdsList();
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
clearWarningIdsList();
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
disableWarning(const std::wstring& id);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
enableWarning(const std::wstring& id);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
setWarningId(const std::wstring& id, WARNING_STATE state, bool withClear = true);
//=============================================================================
NLSERROR_MANAGER_IMPEXP WARNING_IDS_STATES
getAllWarningState();
//=============================================================================
} // namespace Nelson
  //=============================================================================
