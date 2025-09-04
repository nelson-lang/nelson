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
#include <cstring>
#include <utility>
#include <vector>
#include <functional>
#include <mutex>
#include "Types.hpp"
//=============================================================================
#define SYMTAB 8192
//=============================================================================
namespace Nelson {
//=============================================================================
template <class KEY_TYPE, class T> class GenericTable
{
private:
    std::mutex m_mutex;
    //=============================================================================
    using value_type = T;
    //=============================================================================
    struct Entry
    {
        KEY_TYPE key;
        value_type val;
        Entry* next;
        Entry(KEY_TYPE k, value_type v, Entry* n) : key(std::move(k)), val(v), next(n) { }
    };
    //=============================================================================
    Entry* hashTable[SYMTAB];
    //=============================================================================
    size_t
    hashKey(const KEY_TYPE& key)
    {
        std::hash<KEY_TYPE> hash_fn;
        return hash_fn(key);
    }
    //=============================================================================
public:
    GenericTable() { memset(hashTable, 0, sizeof(Entry*) * SYMTAB); }
    //=============================================================================
    ~GenericTable()
    {
        for (auto& i : hashTable) {
            Entry* ptr = i;
            while (ptr) {
                Entry* nxt = ptr->next;
                delete ptr;
                ptr = nxt;
            }
        }
    }
    //=============================================================================
    value_type*
    findSymbol(const KEY_TYPE& key)
    {
        std::scoped_lock<std::mutex> lock { m_mutex };
        size_t i = hashKey(key) % SYMTAB; // Hash
        Entry* ptr = hashTable[i];
        while (ptr) {
            if (ptr->key == key) {
                return (&ptr->val);
            }
            ptr = ptr->next;
        }
        return nullptr;
    }
    //=============================================================================
    void
    deleteSymbol(const KEY_TYPE& key)
    {
        std::scoped_lock<std::mutex> lock { m_mutex };
        size_t i = hashKey(key) % SYMTAB; // Hash
        Entry* ptr = hashTable[i];
        if (!ptr) {
            return;
        }
        // Check for the first element in the table matching
        // the key.
        if (ptr->key == key) {
            hashTable[i] = ptr->next;
            delete ptr;
            ptr = nullptr;
            return;
        }
        // No - its not, set a next pointer
        Entry* nxt = ptr->next;
        while (nxt != nullptr) {
            if (nxt->key == key) {
                ptr->next = nxt->next;
                delete nxt;
                nxt = nullptr;
                return;
            }
            nxt = nxt->next;
            ptr = ptr->next;
        }
    }
    //=============================================================================
    void
    insertSymbol(const KEY_TYPE& key, const value_type& val)
    {
        std::scoped_lock<std::mutex> lock { m_mutex };
        size_t i = hashKey(key) % SYMTAB;
        Entry* ptr = hashTable[i];
        if (!ptr) {
            hashTable[i] = new Entry(key, val, nullptr);
            return;
        }
        while (ptr) {
            if (ptr->key == key) {
                ptr->val = val;
                return;
            }
            ptr = ptr->next;
        }
        hashTable[i] = new Entry(key, val, hashTable[i]);
    }
    //=============================================================================
    std::vector<KEY_TYPE>
    getAllSymbols()
    {
        std::scoped_lock<std::mutex> lock { m_mutex };
        std::vector<KEY_TYPE> retlist;
        for (auto& i : hashTable) {
            if (i != nullptr) {
                Entry* ptr;
                ptr = i;
                while (ptr != nullptr) {
                    retlist.push_back(ptr->key);
                    ptr = ptr->next;
                }
            }
        }
        return retlist;
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
