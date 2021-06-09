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
#pragma once
//=============================================================================
#include <cstring>
#include <utility>
#include <vector>
#include <functional>
#include "Types.hpp"
//=============================================================================
using key_type = std::string;
//=============================================================================
#define SYMTAB 8192
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class GenericTable
{
private:
    //=============================================================================
    using value_type = T;
    //=============================================================================
    struct Entry
    {
        key_type key;
        value_type val;
        Entry* next;
        Entry(key_type k, value_type v, Entry* n) : key(std::move(k)), val(v), next(n) {}
    };
    //=============================================================================
    Entry* hashTable[SYMTAB];
    //=============================================================================
    size_t
    hashKey(const key_type& key)
    {
        std::hash<std::string> hash_fn;
        return hash_fn(key);
    }
    //=============================================================================
public:
    GenericTable() { memset(hashTable, 0, sizeof(Entry*) * SYMTAB); }
    //=============================================================================
    ~GenericTable()
    {
        for (auto& i : hashTable) {
            if (i != nullptr) {
                Entry* ptr;
                Entry* nxt;
                ptr = i;
                while (ptr) {
                    nxt = ptr;
                    ptr = ptr->next;
                    delete nxt;
                }
            }
        }
    }
    //=============================================================================
    value_type*
    findSymbol(const key_type& key)
    {
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
    deleteSymbol(const key_type& key)
    {
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
    insertSymbol(const key_type& key, const value_type& val)
    {
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
    stringVector
    getAllSymbols()
    {
        stringVector retlist;
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
