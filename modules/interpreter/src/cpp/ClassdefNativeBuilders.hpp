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
#include <string>
#include <vector>
#include "AbstractSyntaxTree.hpp"
//=============================================================================
// Small reusable AST builders shared by the classdef native-emission code. They
// assemble the exact node shapes the parser produces and the bytecode compiler
// expects, so classdef functions can be built directly instead of synthesizing
// source and re-parsing it. Definitions live in ClassdefNativeBuilders.cpp; the
// per-function builders that consume them live in ClassdefNativeBuilders*.cpp.
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
AbstractSyntaxTreePtr
astIdentifier(const std::string& name, int context);
AbstractSyntaxTreePtr
astCharacterArray(const std::string& value, int context);
AbstractSyntaxTreePtr
astDoubleLiteral(const std::string& text, int context);
AbstractSyntaxTreePtr
astCall(
    const std::string& function, const std::vector<AbstractSyntaxTreePtr>& arguments, int context);
AbstractSyntaxTreePtr
astReferenceExpression(AbstractSyntaxTreePtr reference, int context);
AbstractSyntaxTreePtr
astUnaryNot(AbstractSyntaxTreePtr operand, int context);
AbstractSyntaxTreePtr
astBinaryOperation(OP_TYPE op, AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs, int context);
AbstractSyntaxTreePtr
astDotReference(const std::string& object, const std::string& field, int context);
AbstractSyntaxTreePtr
astAssignment(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs, int context);
AbstractSyntaxTreePtr
astQuietStatement(AbstractSyntaxTreePtr statement, int context);
AbstractSyntaxTreePtr
astStatementBlock(const std::vector<AbstractSyntaxTreePtr>& statements, int context);
AbstractSyntaxTreePtr
astIfElse(AbstractSyntaxTreePtr condition, AbstractSyntaxTreePtr thenBlock,
    AbstractSyntaxTreePtr elseBlock, int context);
AbstractSyntaxTreePtr
astForLoop(const std::string& variableName, AbstractSyntaxTreePtr rangeExpression,
    AbstractSyntaxTreePtr bodyBlock, int context);
AbstractSyntaxTreePtr
astBraceIndex(const std::string& base, AbstractSyntaxTreePtr indexExpression, int context);
// Parse a single user-written expression to an AST node, appending the parse's
// nodes to `owned`; returns nullptr when the text is not a single-statement
// expression. Parses user input; does not synthesize code.
AbstractSyntaxTreePtr
parseExpressionInto(const std::string& expressionText, AbstractSyntaxTreePtrVector& owned);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
