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
#include <unordered_map>
#include <algorithm>
#include "GOWindowManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::unordered_map<uint64, GOWindow*> GOWindowMap;
//=============================================================================
static uint64 lastID = 0;
//=============================================================================
void
initializeGOWindowManager()
{
    lastID = 0;
}
//=============================================================================
void
finishGOWindowManager()
{
    lastID = 0;
}
//=============================================================================
uint64
getAvailableGOWindowID()
{
    for (size_t k = 1; k < GOWindowMap.size(); ++k) {
        if (GOWindowMap.count(k) == (size_t)0) {
            return (uint64)k;
        }
    }
    return (uint64)GOWindowMap.size() + (uint64)1;
}
//=============================================================================
bool
addGOWindow(uint64 id, GOWindow* goFig)
{
    if (GOWindowMap.count(id) > 0) {
        return false;
    }
    GOWindowMap.emplace(id, goFig);
    lastID = id;
    return true;
}
//=============================================================================
bool
removeGOWindow(uint64 id)
{
    GOWindowMap.erase(id);
    return true;
}
//=============================================================================
GOWindow*
getGOWindow(uint64 id)
{
    GOWindow* res = nullptr;
    if (GOWindowMap.count(id) > 0) {
        res = GOWindowMap[id];
    }
    return res;
}
//=============================================================================
uint64
getCurrentGOWindowID()
{
    return lastID;
}
//=============================================================================
std::vector<GOWindow*>
getGOWindows()
{
    std::vector<GOWindow*> res;
    for (auto w : GOWindowMap) {
        res.push_back(w.second);
    }
    std::reverse(res.begin(), res.end());
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
