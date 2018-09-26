//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#include "AST.hpp"
#include "AstManager.hpp"
#include "Keywords.hpp"
#include "Serialize.hpp"
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

namespace Nelson {

AST::AST(void)
{
    m_context = 0;
    type = non_terminal;
    text.clear();
    tokenNumber = 0;
    down = nullptr;
    right = nullptr;
    opNum = OP_NULL;
}

AST::AST(NODE_TYPE ntype, const char* name, int context)
{
    type = ntype;
    text = std::string(name);
    tokenNumber = 0;
    down = nullptr;
    right = nullptr;
    opNum = OP_NULL;
    m_context = context;
}

AST::AST(NODE_TYPE ntype, int token, int context)
{
    type = ntype;
    tokenNumber = token;
    down = nullptr;
    right = nullptr;
    text.clear();
    opNum = OP_NULL;
    m_context = context;
}

AST::AST(OP_TYPE op, AST* arg, int context)
{
    type = non_terminal;
    text.clear();
    tokenNumber = 0;
    down = arg;
    right = nullptr;
    opNum = op;
    m_context = context;
}

AST::AST(OP_TYPE op, AST* lt, AST* rt, int context)
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

AST::AST(OP_TYPE op, AST* lt, AST* md, AST* rt, int context)
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

AST::~AST() { text.clear(); }

int
AST::context()
{
    return m_context;
}

bool
AST::match(OP_TYPE test)
{
    return (test == opNum);
}

stringVector
AST::toStringList()
{
    stringVector res;
    if (text.size() > 0) {
        res.push_back(text);
    }
    if (down != nullptr) {
        AST* cp = down;
        while (cp != nullptr) {
            if (cp->text.size() > 0) {
                res.push_back(cp->text);
            }
            cp = cp->right;
        }
    }
    return res;
}

void
AST::addChild(AST* arg)
{
    if (down == nullptr) {
        down = arg;
        arg->right = nullptr;
    } else {
        AST* cp;
        cp = down;
        while (cp->right != nullptr) {
            cp = cp->right;
        }
        cp->right = arg;
        arg->right = nullptr;
    }
}

void
AST::addPeer(AST* arg)
{
    if (right == nullptr) {
        right = arg;
        arg->right = nullptr;
    } else {
        AST* cp;
        cp = right;
        while (cp->right != nullptr) {
            cp = cp->right;
        }
        cp->right = arg;
        arg->right = nullptr;
    }
}

int
AST::peerCount()
{
    AST* t;
    int count;
    count = 0;
    t = down;
    while (t != nullptr) {
        count++;
        t = t->right;
    }
    return count;
}

int
AST::childCount()
{
    AST* t;
    int count;
    count = 0;
    t = down;
    while (t != nullptr) {
        count++;
        t = t->down;
    }
    return count;
}

bool
AST::isEmpty()
{
    return ((type == null_node) || (type == non_terminal && opNum == OP_NULL));
}

int tabLevel = 0;

void
outTabs()
{
    for (int i = 0; i < tabLevel; i++) {
        printf("   ");
    }
}

#define cnum(op, msg)                                                                              \
    case op:                                                                                       \
        printf(msg);                                                                               \
        break;
void
printAST(ASTPtr t)
{
    if (t == NULL) {
        return;
    }
    if (t->isEmpty()) {
        return;
    }
    outTabs();
    if (t->type == reserved_node) {
        printf("<%d,%s>\r\n", t->tokenNumber, keyWord[t->tokenNumber].word);
    } else if (t->type == non_terminal) {
        switch (t->opNum) {
            cnum(OP_BLOCK, "block");
            cnum(OP_CASEBLOCK, "caseblock");
            cnum(OP_RHS, "rhs");
            cnum(OP_CSTAT, "cond. statement");
            cnum(OP_ELSEIFBLOCK, "elseifblock");
            cnum(OP_ASSIGN, "assign");
            cnum(OP_MULTICALL, "multicall");
            cnum(OP_COLON, ":");
            cnum(OP_PLUS, "+");
            cnum(OP_SUBTRACT, "-");
            cnum(OP_TIMES, "*");
            cnum(OP_RDIV, "/");
            cnum(OP_LDIV, "\\");
            cnum(OP_OR, "|");
            cnum(OP_AND, "&");
            cnum(OP_LT, "<");
            cnum(OP_LEQ, "<=");
            cnum(OP_GT, ">");
            cnum(OP_GEQ, ">=");
            cnum(OP_EQ, "==");
            cnum(OP_NEQ, "!=");
            cnum(OP_DOT_TIMES, ".*");
            cnum(OP_DOT_RDIV, "./");
            cnum(OP_DOT_LDIV, ".\\");
            cnum(OP_NEG, "neg");
            cnum(OP_POS, "pos");
            cnum(OP_NOT, "~");
            cnum(OP_POWER, "^");
            cnum(OP_DOT_POWER, ".^");
            cnum(OP_TRANSPOSE, "'");
            cnum(OP_DOT_TRANSPOSE, ".'");
            cnum(OP_EMPTY, "empty []");
            cnum(OP_EMPTY_CELL, "empty {}");
            cnum(OP_PARENS, "()");
            cnum(OP_BRACES, "{}");
            cnum(OP_BRACKETS, "[]");
            cnum(OP_DOT, ".");
            cnum(OP_ALL, "all");
            cnum(OP_INDEX_LIST, "index list");
            cnum(OP_ROW_DEF, "row def");
            cnum(OP_SEMICOLON, ";");
            cnum(OP_NULL, "null");
            cnum(OP_RSTATEMENT, "end stat.");
            cnum(OP_QSTATEMENT, "end quiet stat.");
            cnum(OP_SCALL, "special call");
            cnum(OP_KEYWORD, "keyword");
            cnum(OP_DOTDYN, ".()");
        }
        printf("\r\n");
    } else if (t->type == id_node) {
        printf("ident: %s\r\n", t->text.c_str());
    } else if (t->type == const_int_node) {
        printf("int: %s\r\n", t->text.c_str());
    } else if (t->type == const_uint64_node) {
        printf("int: %s\r\n", t->text.c_str());
    } else if (t->type == const_double_node) {
        printf("double: %s\r\n", t->text.c_str());
    } else if (t->type == const_float_node) {
        printf("single: %s\r\n", t->text.c_str());
    } else if (t->type == const_dcomplex_node) {
        printf("double: %si\r\n", t->text.c_str());
    } else if (t->type == const_complex_node) {
        printf("single: %si\r\n", t->text.c_str());
    } else if (t->type == const_character_array_node) {
        printf("string: '%s'\r\n", t->text.c_str());
    } else if (t->type == const_string_node) {
        printf("string: '%s'\r\n", t->text.c_str());
    } else if (t->type == null_node) {
        // NOTHING TO DO
    } else {
        printf("context: %s\r\n", t->text.c_str());
    }
    tabLevel++;
    printAST(t->down);
    tabLevel--;
    printAST(t->right);
}

void
FreezeAST(ASTPtr t, Serialize* s)
{
    if (t == nullptr) {
        s->putByte(0);
        return;
    }
    s->putByte(1);
    s->putByte(t->type);
    s->putInt(t->tokenNumber);
    s->putByte(t->opNum);
    s->putString(t->text);
    FreezeAST(t->down, s);
    FreezeAST(t->right, s);
}

ASTPtr
ThawAST(Serialize* s)
{
    char flag;
    flag = s->getByte();
    if (!flag) {
        return nullptr;
    }
    ASTPtr t;
    try {
        t = new AST();
    } catch (const std::bad_alloc&) {
        t = nullptr;
    }
    if (t) {
        t->type = (NODE_TYPE)s->getByte();
        t->tokenNumber = s->getInt();
        t->opNum = (OP_TYPE)s->getByte();
        t->text = s->getString();
        t->down = ThawAST(s);
        t->right = ThawAST(s);
    }
    return t;
}
}
