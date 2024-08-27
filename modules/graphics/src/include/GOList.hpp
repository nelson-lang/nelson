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
#include <map>
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class GOList
{
    //=============================================================================
    using value_type = T;
    std::map<int64, T, std::less<>> graphicsobjects;
    int64 max_graphicsobject { 0 };
    //=============================================================================
public:
    //=============================================================================
    GOList() = default;
    //=============================================================================
    ~GOList() = default;
    //=============================================================================
    int64
    assignGO(T val)
    {
        int64 next = 0;
        bool freeGOFound = false;
        while ((next < maxGO()) && !freeGOFound) {
            freeGOFound = (graphicsobjects.count(next) == 0);
            if (!freeGOFound) {
                next++;
            }
        }
        graphicsobjects[next] = val;
        if (next >= maxGO()) {
            max_graphicsobject = next + 1;
        }
        return next + 1;
    }
    //=============================================================================
    void
    deleteGO(int64 handle)
    {
        int64 index = handle - 1;
        if (graphicsobjects.count(index) > 0) {
            graphicsobjects.erase(index);
            if (index + 1 == maxGO()) {
                // Adjust max_graphicsobject only if the deleted one was the highest
                while (
                    max_graphicsobject > 0 && graphicsobjects.count(max_graphicsobject - 1) == 0) {
                    max_graphicsobject--;
                }
            }
        }
    }
    //=============================================================================
    int64
    maxGO() const
    {
        return max_graphicsobject;
    }
    //=============================================================================
    T
    findGO(int64 handle, bool throwError = true) const
    {
        int64 index = handle - 1;
        auto it = graphicsobjects.find(index);
        if (it == graphicsobjects.end()) {
            if (throwError) {
                Error(_W("Invalid Graphics Object."));
            }
            return nullptr;
        }
        return it->second;
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
