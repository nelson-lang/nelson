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
#include "ArrayOf.hpp"
#include "HandleGenericObject.hpp"
#include <cstdint>
#include <memory>
#include <vector>
//=============================================================================
// Handle category for the native dictionary store. No dot in the name: a dotted
// category is probed as a package class on every builtin call.
#define NLS_HANDLE_DICTIONARY_STORE_CATEGORY_STR "__dictionary_store__"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
// Native backing store for the value-class 'dictionary'. The map is an
// immutable persistent treap keyed by the 64-bit keyHash; every mutation
// returns a NEW store that shares untouched subtrees with the old one. Because
// an older version is never changed in place, a copied dictionary (d2 = d) that
// shares the handle keeps its own contents when either side is later written,
// which is exactly the value-copy semantics the language requires -- with no
// reference-count bookkeeping.
//=============================================================================
struct DictEntry
{
    ArrayOf key;
    ArrayOf value;
    uint64_t seq; // insertion order, so keys()/values() iterate as inserted
};
//=============================================================================
struct TreapNode
{
    uint64_t hkey; // keyHash of the key(s) in this node
    std::vector<DictEntry> bucket; // one entry normally, more only on a hash collision
    uint32_t priority; // treap balance priority
    std::shared_ptr<const TreapNode> left;
    std::shared_ptr<const TreapNode> right;

    TreapNode(uint64_t h, std::vector<DictEntry>&& b, uint32_t p,
        std::shared_ptr<const TreapNode> l, std::shared_ptr<const TreapNode> r)
        : hkey(h), bucket(std::move(b)), priority(p), left(std::move(l)), right(std::move(r))
    {
    }
};
//=============================================================================
using DictNodePtr = std::shared_ptr<const TreapNode>;
//=============================================================================
class DictionaryStoreObject : public HandleGenericObject
{
public:
    DictionaryStoreObject();
    DictionaryStoreObject(DictNodePtr root, size_t count, uint64_t nextSeq);

    size_t
    count() const
    {
        return _count;
    }

    // Look up the value stored under 'key'. Returns false when absent.
    bool
    lookup(Evaluator* eval, const ArrayOf& key, ArrayOf& value) const;

    bool
    isKey(Evaluator* eval, const ArrayOf& key) const;

    // Insert/overwrite every (keys[i], values[i]) pair, last value winning, and
    // return the resulting store. 'this' is left unchanged.
    DictionaryStoreObject*
    withSet(Evaluator* eval, const std::vector<ArrayOf>& keys,
        const std::vector<ArrayOf>& values) const;

    // Remove every present key and return the resulting store ('this' unchanged).
    DictionaryStoreObject*
    withRemoved(Evaluator* eval, const std::vector<ArrayOf>& keys) const;

    // Stored keys and values in insertion order.
    std::vector<ArrayOf>
    keysInOrder() const;
    std::vector<ArrayOf>
    valuesInOrder() const;

private:
    DictNodePtr _root;
    size_t _count;
    uint64_t _nextSeq;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
