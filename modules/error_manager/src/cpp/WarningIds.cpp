//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WarningIds.hpp"
#include <map>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::map<std::wstring, WARNING_STATE> warningsMap;
//=============================================================================
WARNING_STATE
warningCheckState(const std::wstring& id)
{
    WARNING_STATE state = WARNING_STATE::NOT_FOUND;
    std::map<std::wstring, WARNING_STATE>::iterator iter;
    if (id.empty()) {
        iter = warningsMap.find(L"all");
        if (iter != warningsMap.end()) {
            state = iter->second;
        }
    } else if (warningsMap.count(id) > 0) {
        iter = warningsMap.find(id);
        if (iter != warningsMap.end()) {
            state = iter->second;
        }
    }
    return state;
}
//=============================================================================
void
initializeDefaultWarningIdsList()
{
    setWarningId(L"all", WARNING_STATE::ENABLED, false);
    setWarningId(WARNING_COLON_ARRAY_AS_SCALAR, WARNING_STATE::ENABLED, false);
    setWarningId(WARNING_MATIO_TYPE_NOT_SUPPORTED, WARNING_STATE::ENABLED, false);
    setWarningId(WARNING_RANK_DEFICIENT_MATRIX, WARNING_STATE::ENABLED, false);
    setWarningId(WARNING_NEARLY_SINGULAR_MATRIX, WARNING_STATE::ENABLED, false);
    setWarningId(WARNING_IMAGINARY_PART_IGNORED, WARNING_STATE::ENABLED, false);
}
//=============================================================================
void
clearWarningIdsList()
{
    warningsMap.clear();
}
//=============================================================================
void
disableWarning(const std::wstring& id)
{
    setWarningId(id, WARNING_STATE::DISABLED, true);
}
//=============================================================================
void
enableWarning(const std::wstring& id)
{
    setWarningId(id, WARNING_STATE::ENABLED, true);
}
//=============================================================================
void
setWarningId(const std::wstring& id, WARNING_STATE state, bool withClear)
{
    std::wstring _id = id;
    if (withClear) {
        clearWarningIdsList();
    }
    if (id.empty() || id == L"all") {
        _id = L"all";
    }
    auto iter = warningsMap.find(_id);
    if (iter != warningsMap.end()) {
        std::swap(warningsMap[_id], iter->second);
    } else {
        warningsMap.emplace(id, state);
    }
}
//=============================================================================
WARNING_IDS_STATES
getAllWarningState()
{
    WARNING_IDS_STATES list;
    list.IDs.clear();
    list.states.clear();
    for (const auto& p : warningsMap) {
        list.IDs.push_back(p.first);
        list.states.push_back(p.second);
    }
    return list;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
