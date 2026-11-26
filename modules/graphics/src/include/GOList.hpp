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
#include <map>
#include "Error.hpp"
#include "ErrorID.hpp"
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
    // Handles are never reused: a deleted object's handle stays invalid, so a stale
    // handle cannot reach an object created later.
    int64
    assignGO(T val)
    {
        int64 next = max_graphicsobject;
        graphicsobjects[next] = val;
        max_graphicsobject = next + 1;
        return next + 1;
    }
    //=============================================================================
    void
    deleteGO(int64 handle)
    {
        graphicsobjects.erase(handle - 1);
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
                Error(L"Nelson:graphics:invalidGraphicsObject", L"Valid graphics object expected.");
            }
            return nullptr;
        }
        return it->second;
    }
    //=============================================================================
    int64
    findHandle(T val) const
    {
        for (const auto& it : graphicsobjects) {
            if (it.second == val) {
                return it.first + 1;
            }
        }
        return -1;
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
