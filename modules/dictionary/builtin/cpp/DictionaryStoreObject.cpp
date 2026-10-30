//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DictionaryStoreObject.hpp"
#include "KeyHash.hpp"
#include "Evaluator.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include <algorithm>
#include <cmath>
#include <cstring>
#include <random>
//=============================================================================
namespace Nelson {
//=============================================================================
static uint32_t
nextPriority()
{
    // Random treap priorities keep the tree balanced in expectation; a fixed
    // seed makes the shape reproducible, which does not affect correctness.
    static std::mt19937 rng(0x9e3779b9u);
    return rng();
}
//=============================================================================
static size_t
scalarByteSize(NelsonType c)
{
    switch (c) {
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
        return 1;
    case NLS_INT16:
    case NLS_UINT16:
        return 2;
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_SINGLE:
        return 4;
    case NLS_INT64:
    case NLS_UINT64:
    case NLS_DOUBLE:
        return 8;
    case NLS_CHAR:
        return sizeof(charType);
    default:
        return 0;
    }
}
//=============================================================================
// Exact equality using isequaln (matches the keyMatch semantics: same class
// and equal values, NaN equal to NaN). Used only for exotic key types or on a
// genuine 64-bit hash collision.
static bool
isEqualFallback(Evaluator* eval, const ArrayOf& a, const ArrayOf& b)
{
    if (eval != nullptr) {
        FunctionDefPtr funcDef = nullptr;
        if (eval->getContext()->lookupFunction("isequaln", funcDef) && funcDef != nullptr) {
            ArrayOfVector args;
            args << a;
            args << b;
            ArrayOfVector res = funcDef->evaluateFunction(eval, args, 1);
            return !res.empty() && res[0].isLogical() && res[0].isScalar()
                && (res[0].asLogicalScalar() != 0u);
        }
    }
    return KeyHash(eval, a) == KeyHash(eval, b);
}
//=============================================================================
// Whether two already keyType-coerced keys are the same key. Fast native paths
// for the scalar numeric, char and string key types (the hot paths); anything
// else defers to isequaln.
static bool
keyEquals(Evaluator* eval, const ArrayOf& a, const ArrayOf& b)
{
    const NelsonType ca = a.dataClass();
    if (ca != b.dataClass()) {
        return false;
    }
    if (a.isSparse() || b.isSparse() || a.isComplex() || b.isComplex()) {
        return isEqualFallback(eval, a, b);
    }
    switch (ca) {
    case NLS_DOUBLE:
        if (a.isScalar() && b.isScalar()) {
            const double x = a.asDoubleScalar();
            const double y = b.asDoubleScalar();
            return x == y || (std::isnan(x) && std::isnan(y));
        }
        break;
    case NLS_SINGLE:
        if (a.isScalar() && b.isScalar()) {
            const single x = a.asSingleScalar();
            const single y = b.asSingleScalar();
            return x == y || (std::isnan(x) && std::isnan(y));
        }
        break;
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
        if (a.isScalar() && b.isScalar()) {
            return std::memcmp(a.dataPointer(), b.dataPointer(), scalarByteSize(ca)) == 0;
        }
        break;
    case NLS_CHAR:
        if (!a.dimensions().equals(b.dimensions())) {
            return false;
        }
        return std::memcmp(a.dataPointer(), b.dataPointer(),
                   (size_t)a.elementCount() * sizeof(charType))
            == 0;
    case NLS_STRING_ARRAY: {
        if (!a.dimensions().equals(b.dimensions())) {
            return false;
        }
        const ArrayOf* ea = static_cast<const ArrayOf*>(a.dataPointer());
        const ArrayOf* eb = static_cast<const ArrayOf*>(b.dataPointer());
        if (ea == nullptr || eb == nullptr) {
            return a.elementCount() == b.elementCount();
        }
        const size_t n = (size_t)a.elementCount();
        for (size_t i = 0; i < n; ++i) {
            if (!keyEquals(eval, ea[i], eb[i])) {
                return false;
            }
        }
        return true;
    }
    default:
        break;
    }
    return isEqualFallback(eval, a, b);
}
//=============================================================================
static DictNodePtr
makeNode(uint64_t h, std::vector<DictEntry> bucket, uint32_t priority, DictNodePtr left,
    DictNodePtr right)
{
    return std::make_shared<const TreapNode>(
        h, std::move(bucket), priority, std::move(left), std::move(right));
}
//=============================================================================
static DictNodePtr
rotateRight(const DictNodePtr& t)
{
    const DictNodePtr& l = t->left;
    DictNodePtr newRight = makeNode(t->hkey, t->bucket, t->priority, l->right, t->right);
    return makeNode(l->hkey, l->bucket, l->priority, l->left, newRight);
}
//=============================================================================
static DictNodePtr
rotateLeft(const DictNodePtr& t)
{
    const DictNodePtr& r = t->right;
    DictNodePtr newLeft = makeNode(t->hkey, t->bucket, t->priority, t->left, r->left);
    return makeNode(r->hkey, r->bucket, r->priority, newLeft, r->right);
}
//=============================================================================
static DictNodePtr
setRec(const DictNodePtr& t, uint64_t h, const ArrayOf& key, const ArrayOf& value, Evaluator* eval,
    size_t& count, uint64_t& nextSeq)
{
    if (!t) {
        std::vector<DictEntry> bucket;
        bucket.push_back(DictEntry { key, value, nextSeq++ });
        ++count;
        return makeNode(h, std::move(bucket), nextPriority(), nullptr, nullptr);
    }
    if (h == t->hkey) {
        std::vector<DictEntry> bucket = t->bucket;
        bool found = false;
        for (auto& e : bucket) {
            if (keyEquals(eval, e.key, key)) {
                e.value = value;
                found = true;
                break;
            }
        }
        if (!found) {
            bucket.push_back(DictEntry { key, value, nextSeq++ });
            ++count;
        }
        return makeNode(h, std::move(bucket), t->priority, t->left, t->right);
    }
    if (h < t->hkey) {
        DictNodePtr nl = setRec(t->left, h, key, value, eval, count, nextSeq);
        DictNodePtr nt = makeNode(t->hkey, t->bucket, t->priority, nl, t->right);
        if (nl && nl->priority > nt->priority) {
            return rotateRight(nt);
        }
        return nt;
    }
    DictNodePtr nr = setRec(t->right, h, key, value, eval, count, nextSeq);
    DictNodePtr nt = makeNode(t->hkey, t->bucket, t->priority, t->left, nr);
    if (nr && nr->priority > nt->priority) {
        return rotateLeft(nt);
    }
    return nt;
}
//=============================================================================
static DictNodePtr
mergeNodes(const DictNodePtr& l, const DictNodePtr& r)
{
    if (!l) {
        return r;
    }
    if (!r) {
        return l;
    }
    if (l->priority > r->priority) {
        return makeNode(l->hkey, l->bucket, l->priority, l->left, mergeNodes(l->right, r));
    }
    return makeNode(r->hkey, r->bucket, r->priority, mergeNodes(l, r->left), r->right);
}
//=============================================================================
static DictNodePtr
removeRec(const DictNodePtr& t, uint64_t h, const ArrayOf& key, Evaluator* eval, size_t& count)
{
    if (!t) {
        return nullptr;
    }
    if (h < t->hkey) {
        DictNodePtr nl = removeRec(t->left, h, key, eval, count);
        return makeNode(t->hkey, t->bucket, t->priority, nl, t->right);
    }
    if (h > t->hkey) {
        DictNodePtr nr = removeRec(t->right, h, key, eval, count);
        return makeNode(t->hkey, t->bucket, t->priority, t->left, nr);
    }
    std::vector<DictEntry> bucket = t->bucket;
    for (size_t i = 0; i < bucket.size(); ++i) {
        if (keyEquals(eval, bucket[i].key, key)) {
            bucket.erase(bucket.begin() + i);
            --count;
            break;
        }
    }
    if (!bucket.empty()) {
        return makeNode(t->hkey, std::move(bucket), t->priority, t->left, t->right);
    }
    return mergeNodes(t->left, t->right);
}
//=============================================================================
static const TreapNode*
findNode(const TreapNode* t, uint64_t h)
{
    while (t != nullptr) {
        if (h == t->hkey) {
            return t;
        }
        t = (h < t->hkey) ? t->left.get() : t->right.get();
    }
    return nullptr;
}
//=============================================================================
static void
collectInOrder(const TreapNode* t, std::vector<const DictEntry*>& out)
{
    if (t == nullptr) {
        return;
    }
    collectInOrder(t->left.get(), out);
    for (const auto& e : t->bucket) {
        out.push_back(&e);
    }
    collectInOrder(t->right.get(), out);
}
//=============================================================================
DictionaryStoreObject::DictionaryStoreObject()
    : HandleGenericObject(NLS_HANDLE_DICTIONARY_STORE_CATEGORY_STR, this, false)
    , _root(nullptr)
    , _count(0)
    , _nextSeq(0)
{
}
//=============================================================================
DictionaryStoreObject::DictionaryStoreObject(DictNodePtr root, size_t count, uint64_t nextSeq)
    : HandleGenericObject(NLS_HANDLE_DICTIONARY_STORE_CATEGORY_STR, this, false)
    , _root(std::move(root))
    , _count(count)
    , _nextSeq(nextSeq)
{
}
//=============================================================================
bool
DictionaryStoreObject::lookup(Evaluator* eval, const ArrayOf& key, ArrayOf& value) const
{
    const uint64_t h = (uint64_t)KeyHash(eval, key);
    const TreapNode* node = findNode(_root.get(), h);
    if (node == nullptr) {
        return false;
    }
    for (const auto& e : node->bucket) {
        if (keyEquals(eval, e.key, key)) {
            value = e.value;
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
DictionaryStoreObject::isKey(Evaluator* eval, const ArrayOf& key) const
{
    ArrayOf ignored;
    return lookup(eval, key, ignored);
}
//=============================================================================
DictionaryStoreObject*
DictionaryStoreObject::withSet(
    Evaluator* eval, const std::vector<ArrayOf>& keys, const std::vector<ArrayOf>& values) const
{
    DictNodePtr root = _root;
    size_t count = _count;
    uint64_t nextSeq = _nextSeq;
    for (size_t k = 0; k < keys.size(); ++k) {
        const uint64_t h = (uint64_t)KeyHash(eval, keys[k]);
        root = setRec(root, h, keys[k], values[k], eval, count, nextSeq);
    }
    return new DictionaryStoreObject(root, count, nextSeq);
}
//=============================================================================
DictionaryStoreObject*
DictionaryStoreObject::withRemoved(Evaluator* eval, const std::vector<ArrayOf>& keys) const
{
    DictNodePtr root = _root;
    size_t count = _count;
    for (size_t k = 0; k < keys.size(); ++k) {
        const uint64_t h = (uint64_t)KeyHash(eval, keys[k]);
        root = removeRec(root, h, keys[k], eval, count);
    }
    return new DictionaryStoreObject(root, count, _nextSeq);
}
//=============================================================================
std::vector<ArrayOf>
DictionaryStoreObject::keysInOrder() const
{
    std::vector<const DictEntry*> entries;
    entries.reserve(_count);
    collectInOrder(_root.get(), entries);
    std::sort(entries.begin(), entries.end(),
        [](const DictEntry* a, const DictEntry* b) { return a->seq < b->seq; });
    std::vector<ArrayOf> out;
    out.reserve(entries.size());
    for (const DictEntry* e : entries) {
        out.push_back(e->key);
    }
    return out;
}
//=============================================================================
std::vector<ArrayOf>
DictionaryStoreObject::valuesInOrder() const
{
    std::vector<const DictEntry*> entries;
    entries.reserve(_count);
    collectInOrder(_root.get(), entries);
    std::sort(entries.begin(), entries.end(),
        [](const DictEntry* a, const DictEntry* b) { return a->seq < b->seq; });
    std::vector<ArrayOf> out;
    out.reserve(entries.size());
    for (const DictEntry* e : entries) {
        out.push_back(e->value);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
