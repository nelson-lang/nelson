//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fifo_map.hpp>
#include <algorithm>
#include "GOWindowManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
nlohmann::fifo_map<uint64, GOWindow*> GOWindowMap;
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
    bool res = false;
    if (GOWindowMap.find(id) != GOWindowMap.end()) {
        GOWindowMap.erase(id);
        res = true;
        lastID = findCurrentGOWindowID();
    }
    return res;
}
//=============================================================================
uint64
findCurrentGOWindowID()
{
    uint64 currentGOWindowId = 0;
    std::vector<GOWindow*> goWins = getGOWindows();
    size_t nbWindows = goWins.size();
    if (nbWindows != 0u) {
        if (nbWindows - 1 > 0) {
            GOWindow* res = getGOWindow(nbWindows - 1);
            if (res != nullptr) {
                currentGOWindowId = res->ID();
            }
        }
    }
    return currentGOWindowId;
}
//=============================================================================
GOWindow*
getGOWindow(uint64 id, bool selectIt)
{
    GOWindow* res = nullptr;
    if (GOWindowMap.count(id) > 0) {
        res = GOWindowMap[id];
        if (selectIt) {
            lastID = id;
        }
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
