//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include "AbstractSyntaxTree.hpp"
#include <mutex>
#include <shared_mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
// Thread-safe storage for AST nodes
class ASTThreadSafeStorage
{
    //=============================================================================
private:
    AbstractSyntaxTreePtrVector storage;
    mutable std::shared_mutex mutex;
    //=============================================================================
public:
    //=============================================================================
    void
    clear()
    {
        std::unique_lock<std::shared_mutex> lock(mutex);
        storage.clear();
    }
    //=============================================================================
    void
    add(AbstractSyntaxTreePtr ptr)
    {
        std::unique_lock<std::shared_mutex> lock(mutex);
        storage.push_back(ptr);
    }
    //=============================================================================
    void
    deleteAll()
    {
        std::unique_lock<std::shared_mutex> lock(mutex);
        for (auto& a : storage) {
            if (a != nullptr) {
                delete a;
                a = nullptr;
            }
        }
        storage.clear();
    }
    //=============================================================================
    AbstractSyntaxTreePtrVector
    getCopy() const
    {
        std::shared_lock<std::shared_mutex> lock(mutex);
        return storage;
    }
    //=============================================================================
};
//=============================================================================
// Global storage instance with static lifetime
static ASTThreadSafeStorage g_astStorage;
//=============================================================================
void
AbstractSyntaxTree::clearReferences()
{
    g_astStorage.clear();
}
//=============================================================================
void
AbstractSyntaxTree::deleteReferences()
{
    g_astStorage.deleteAll();
}
//=============================================================================
void
AbstractSyntaxTree::deleteReferences(AbstractSyntaxTreePtrVector& astAsVector)
{
    // This method doesn't need to be protected by mutex as it operates on a local copy
    for (auto& a : astAsVector) {
        if (a != nullptr) {
            delete a;
            a = nullptr;
        }
    }
    astAsVector.clear();
}
//=============================================================================
AbstractSyntaxTreePtrVector
AbstractSyntaxTree::getReferences()
{
    return g_astStorage.getCopy();
}
//=============================================================================

AbstractSyntaxTreePtr
AbstractSyntaxTree::createNode(NODE_TYPE ntype, const std::string& name, int context)
{
    AbstractSyntaxTreePtr p;
    try {
        p = new AbstractSyntaxTree(ntype, name, context);
        g_astStorage.add(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
AbstractSyntaxTreePtr
AbstractSyntaxTree::createNode(NODE_TYPE ntype, int token, int context)
{
    AbstractSyntaxTreePtr p;
    try {
        p = new AbstractSyntaxTree(ntype, token, context);
        g_astStorage.add(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
AbstractSyntaxTreePtr
AbstractSyntaxTree::createNode(
    OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr rt, int context)
{
    AbstractSyntaxTreePtr p;
    try {
        p = new AbstractSyntaxTree(op, lt, rt, context);
        g_astStorage.add(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
AbstractSyntaxTreePtr
AbstractSyntaxTree::createNode(OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr md,
    AbstractSyntaxTreePtr rt, int context)
{
    AbstractSyntaxTreePtr p;
    try {
        p = new AbstractSyntaxTree(op, lt, md, rt, context);
        g_astStorage.add(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
AbstractSyntaxTreePtr
AbstractSyntaxTree::createNode(OP_TYPE op, AbstractSyntaxTreePtr arg, int context)
{
    AbstractSyntaxTreePtr p;
    try {
        p = new AbstractSyntaxTree(op, arg, context);
        g_astStorage.add(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
AbstractSyntaxTree::AbstractSyntaxTree()
{
    m_context = 0;
    type = non_terminal;
    text.clear();
    tokenNumber = 0;
    down = nullptr;
    right = nullptr;
    opNum = OP_NULL;
}
//=============================================================================
AbstractSyntaxTree::AbstractSyntaxTree(NODE_TYPE ntype, const std::string& name, int context)
    : type(ntype), text(name), m_context(context)
{
    tokenNumber = 0;
    down = nullptr;
    right = nullptr;
    opNum = OP_NULL;
}
//=============================================================================
AbstractSyntaxTree::AbstractSyntaxTree(NODE_TYPE ntype, int token, int context)
{
    type = ntype;
    tokenNumber = token;
    down = nullptr;
    right = nullptr;
    text.clear();
    opNum = OP_NULL;
    m_context = context;
}
//=============================================================================
AbstractSyntaxTree::AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr arg, int context)
{
    type = non_terminal;
    text.clear();
    tokenNumber = 0;
    down = arg;
    right = nullptr;
    opNum = op;
    m_context = context;
}
//=============================================================================
AbstractSyntaxTree::AbstractSyntaxTree(
    OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr rt, int context)
{
    type = non_terminal;
    text.clear();
    opNum = op;
    lt->right = rt;
    tokenNumber = 0;
    down = lt;
    right = nullptr;
    m_context = context;
}
//=============================================================================
AbstractSyntaxTree::AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr lt,
    AbstractSyntaxTreePtr md, AbstractSyntaxTreePtr rt, int context)
{
    type = non_terminal;
    text.clear();
    opNum = op;
    lt->right = md;
    md->right = rt;
    tokenNumber = 0;
    down = lt;
    right = nullptr;
    m_context = context;
}
//=============================================================================
AbstractSyntaxTree::~AbstractSyntaxTree()
{
    m_context = 0;
    type = non_terminal;
    text.clear();
    tokenNumber = 0;
    down = nullptr;
    right = nullptr;
    opNum = OP_NULL;
}
//=============================================================================
int
AbstractSyntaxTree::getContext()
{
    return m_context;
}
//=============================================================================
bool
AbstractSyntaxTree::match(OP_TYPE test)
{
    return (test == opNum);
}
//=============================================================================
stringVector
AbstractSyntaxTree::toStringList()
{
    stringVector res;
    if (!text.empty()) {
        res.push_back(text);
    }
    if (down != nullptr) {
        AbstractSyntaxTreePtr cp = down;
        while (cp != nullptr) {
            if (!cp->text.empty()) {
                res.push_back(cp->text);
            }
            cp = cp->right;
        }
    }
    return res;
}
//=============================================================================
void
AbstractSyntaxTree::addChild(AbstractSyntaxTreePtr arg)
{
    if (down == nullptr) {
        down = arg;
        arg->right = nullptr;
    } else {
        AbstractSyntaxTreePtr cp = down;
        while (cp->right != nullptr) {
            cp = cp->right;
        }
        cp->right = arg;
        arg->right = nullptr;
    }
}
//=============================================================================
void
AbstractSyntaxTree::addPeer(AbstractSyntaxTreePtr arg)
{
    if (right == nullptr) {
        right = arg;
        arg->right = nullptr;
    } else {
        AbstractSyntaxTreePtr cp = right;
        while (cp->right != nullptr) {
            cp = cp->right;
        }
        cp->right = arg;
        arg->right = nullptr;
    }
}
//=============================================================================
int
AbstractSyntaxTree::peerCount()
{
    int count = 0;
    AbstractSyntaxTreePtr t = down;
    while (t != nullptr) {
        count++;
        t = t->right;
    }
    return count;
}
//=============================================================================
int
AbstractSyntaxTree::childCount()
{
    int count = 0;
    AbstractSyntaxTreePtr t = down;
    while (t != nullptr) {
        count++;
        t = t->down;
    }
    return count;
}
//=============================================================================
bool
AbstractSyntaxTree::isEmpty()
{
    return ((type == null_node) || (type == non_terminal && opNum == OP_NULL));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
