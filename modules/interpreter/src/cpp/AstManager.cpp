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
#include <algorithm>
#include "AstManager.hpp"
#include "NelSonParser.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<ASTPtr> AstUsedVector;
//=============================================================================
void
resetAstBackupPosition()
{
    AstUsedVector.clear();
}
//=============================================================================
std::vector<ASTPtr>
getAstUsed()
{
    return AstUsedVector;
}
//=============================================================================
bool
deleteAst(ASTPtr pt, std::vector<ASTPtr> v)
{
    bool bFind = false;
    std::vector<ASTPtr>::iterator it = std::find(v.begin(), v.end(), pt);
    if (it != v.end()) {
        delete pt;
        v.erase(it);
        bFind = true;
    }
    return bFind;
}
//=============================================================================
void
deleteAstVector(std::vector<ASTPtr> v)
{
    for (size_t k = 0; k < v.size(); ++k) {
        if (v[k]) {
            delete v[k];
            v[k] = nullptr;
        }
    }
    v.clear();
}
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(void)
{
    ASTPtr p;
    try {
        p = new AST();
        AstUsedVector.push_back(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(NODE_TYPE ntype, const char* name, int context)
{
    ASTPtr p;
    try {
        p = new AST(ntype, name, context);
        AstUsedVector.push_back(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(NODE_TYPE ntype, int token, int context)
{
    ASTPtr p;
    try {
        p = new AST(ntype, token, context);
        AstUsedVector.push_back(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(OP_TYPE op, ASTPtr lt, ASTPtr rt, int context)
{
    ASTPtr p;
    try {
        p = new AST(op, lt, rt, context);
        AstUsedVector.push_back(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(OP_TYPE op, ASTPtr lt, ASTPtr md, ASTPtr rt, int context)
{
    ASTPtr p;
    try {
        p = new AST(op, lt, md, rt, context);
        AstUsedVector.push_back(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(OP_TYPE op, ASTPtr arg, int context)
{
    ASTPtr p;
    try {
        p = new AST(op, arg, context);
        AstUsedVector.push_back(p);
    } catch (const std::bad_alloc&) {
        p = nullptr;
    }
    return p;
}
//=============================================================================
}
//=============================================================================
