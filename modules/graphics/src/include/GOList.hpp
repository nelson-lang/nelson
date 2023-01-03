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
    GOList() { }
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
            max_graphicsobject++;
        }
        return next + 1;
    }
    //=============================================================================
    void
    deleteGO(int64 handle)
    {
        if ((handle - 1) == maxGO()) {
            max_graphicsobject--;
        }
        graphicsobjects.erase(handle - 1);
    }
    //=============================================================================
    int64
    maxGO()
    {
        return max_graphicsobject;
    }
    //=============================================================================
    T
    findGO(int64 handle, bool throwError = true)
    {
        if (graphicsobjects.count(handle - 1) == 0) {
            if (throwError) {
                Error(_W("Invalid Graphics Object."));
            } else {
                return nullptr;
            }
        }
        return graphicsobjects[handle - 1];
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
