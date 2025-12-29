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
#include <unordered_map>
#include <list>
#include <vector>
#include <shared_mutex>
#include <mutex>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class KEY_TYPE, class T> class GenericTable
{
private:
    mutable std::shared_mutex m_mutex;
    using value_type = T;
    using ListType = std::list<std::pair<KEY_TYPE, value_type>>;
    using ListIterator = typename ListType::iterator;

    ListType m_list;
    std::unordered_map<KEY_TYPE, ListIterator> m_map;
    //=============================================================================
public:
    GenericTable() = default;
    ~GenericTable() = default;
    //=============================================================================
    value_type*
    findSymbol(const KEY_TYPE& key)
    {
        std::shared_lock lock { m_mutex };
        auto it = m_map.find(key);
        if (it != m_map.end()) {
            return &(it->second->second);
        }
        return nullptr;
    }
    //=============================================================================
    void
    deleteSymbol(const KEY_TYPE& key)
    {
        std::unique_lock lock { m_mutex };
        auto it = m_map.find(key);
        if (it != m_map.end()) {
            m_list.erase(it->second);
            m_map.erase(it);
        }
    }
    //=============================================================================
    void
    insertSymbol(const KEY_TYPE& key, const value_type& val)
    {
        std::unique_lock lock { m_mutex };
        auto it = m_map.find(key);
        if (it != m_map.end()) {
            it->second->second = val;
        } else {
            m_list.emplace_back(key, val);
            m_map[key] = std::prev(m_list.end());
        }
    }
    //=============================================================================
    std::vector<KEY_TYPE>
    getAllSymbols()
    {
        std::shared_lock lock { m_mutex };
        std::vector<KEY_TYPE> retlist;
        retlist.reserve(m_list.size());
        for (const auto& [key, val] : m_list) {
            retlist.push_back(key);
        }
        return retlist;
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
