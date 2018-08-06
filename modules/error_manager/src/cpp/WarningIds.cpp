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
#include "WarningIds.hpp"
#include <map>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::map<std::wstring, WARNING_STATE> warningsMap;
//=============================================================================
WARNING_STATE
warningCheckState(const std::wstring &id)
{
    if (id == L"") {
        std::map<std::wstring, WARNING_STATE>::iterator iter = warningsMap.find(L"all");
        return iter->second;
    }
    if (warningsMap.count(id) > 0) {
        std::map<std::wstring, WARNING_STATE>::iterator iter = warningsMap.find(id);
        return iter->second;
    }
    return WARNING_STATE::NOT_FOUND;
}
//=============================================================================
void
initializeDefaultWarningIdsList()
{
    setWarningId(L"all", WARNING_STATE::ENABLED);
    setWarningId(L"Nelson:colon:array-as-scalar", WARNING_STATE::ENABLED);
}
//=============================================================================
void
clearWarningIdsList()
{
    warningsMap.clear();
}
//=============================================================================
void
disableWarning(const std::wstring &id)
{
    if (id == L"") {
        warningsMap.emplace(L"all", WARNING_STATE::DISABLED);
	} else {
        warningsMap.emplace(id, WARNING_STATE::DISABLED);
	}
}
//=============================================================================
void
enableWarning(const std::wstring &id)
{
    if (id == L"") {
        warningsMap.emplace(L"all", WARNING_STATE::ENABLED);
	} else {
        warningsMap.emplace(id, WARNING_STATE::ENABLED);
	}
}
//=============================================================================
void
setWarningId(const std::wstring &id, WARNING_STATE state)
{
    if (id == L"") {
        warningsMap.emplace(L"all", state);
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
