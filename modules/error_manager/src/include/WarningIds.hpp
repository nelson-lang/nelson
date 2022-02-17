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
#include <vector>
#include "nlsError_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr auto WARNING_COLON_ARRAY_AS_SCALAR = L"Nelson:colon:array-as-scalar";
constexpr auto WARNING_MATIO_TYPE_NOT_SUPPORTED = L"Nelson:matio:type-not-supported";
constexpr auto WARNING_RANK_DEFICIENT_MATRIX = L"Nelson:rankDeficientMatrix";
constexpr auto WARNING_NEARLY_SINGULAR_MATRIX = L"Nelson:nearlySingularMatrix";
constexpr auto WARNING_IMAGINARY_PART_IGNORED = L"Nelson:imaginaryPartIgnored";
constexpr auto WARNING_NOT_FULLY_SERIALIZED = L"Nelson:notFullySerialized";
//=============================================================================
enum WARNING_STATE
{
    DISABLED,
    ENABLED,
    AS_ERROR,
    NOT_FOUND
};
//=============================================================================
struct WARNING_IDS_STATES
{
    std::vector<std::wstring> IDs;
    std::vector<WARNING_STATE> states;
};
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
