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
#include <vector>
#include "nlsInterpreter_exports.h"
#include "AST.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree();
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(NODE_TYPE ntype, const char* name, int context);
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(NODE_TYPE ntype, int token, int context);
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(OP_TYPE op, ASTPtr lt, ASTPtr rt, int context);
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(OP_TYPE op, ASTPtr lt, ASTPtr md, ASTPtr rt, int context);
//=============================================================================
ASTPtr
allocateAbstractSyntaxTree(OP_TYPE op, ASTPtr arg, int context);
//=============================================================================
NLSINTERPRETER_IMPEXP void
resetAstBackupPosition();
//=============================================================================
NLSINTERPRETER_IMPEXP std::vector<ASTPtr>
getAstUsed();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
deleteAst(ASTPtr pt, std::vector<ASTPtr> v);
//=============================================================================
NLSINTERPRETER_IMPEXP void
deleteAstVector(std::vector<ASTPtr>& v);
//=============================================================================

} // namespace Nelson
//=============================================================================
