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
