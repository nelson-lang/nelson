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
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include "AbstractSyntaxTree.hpp"
//=============================================================================
namespace Nelson {
AbstractSyntaxTreePtrVector AbstractSyntaxTree::astUsedAsVector;
//=============================================================================
void
AbstractSyntaxTree::clearReferences()
{
    astUsedAsVector.clear();
}
//=============================================================================
void
AbstractSyntaxTree::deleteReferences()
{
    deleteReferences(astUsedAsVector);
}
//=============================================================================
void
AbstractSyntaxTree::deleteReferences(AbstractSyntaxTreePtrVector& astAsVector)
{
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
    return astUsedAsVector;
}
//=============================================================================

AbstractSyntaxTreePtr
AbstractSyntaxTree::createNode(NODE_TYPE ntype, const char* name, int context)
{
    AbstractSyntaxTreePtr p;
    try {
        p = new AbstractSyntaxTree(ntype, name, context);
        astUsedAsVector.push_back(p);
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
        astUsedAsVector.push_back(p);
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
        astUsedAsVector.push_back(p);
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
        astUsedAsVector.push_back(p);
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
        astUsedAsVector.push_back(p);
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
AbstractSyntaxTree::AbstractSyntaxTree(NODE_TYPE ntype, const char* name, int context)
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
