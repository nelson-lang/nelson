//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefNativeBuilders.hpp"
#include "AbstractSyntaxTree.hpp"
#include "ParserInterface.hpp"
#include "LexerContext.hpp"
#include "Keywords.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
AbstractSyntaxTreePtr
astIdentifier(const std::string& name, int context)
{
    return AbstractSyntaxTree::createNode(id_node, name, context);
}
//=============================================================================
AbstractSyntaxTreePtr
astCharacterArray(const std::string& value, int context)
{
    return AbstractSyntaxTree::createNode(const_character_array_node, value, context);
}
//=============================================================================
AbstractSyntaxTreePtr
astDoubleLiteral(const std::string& text, int context)
{
    return AbstractSyntaxTree::createNode(const_double_node, text, context);
}
//=============================================================================
AbstractSyntaxTreePtr
astCall(
    const std::string& function, const std::vector<AbstractSyntaxTreePtr>& arguments, int context)
{
    AbstractSyntaxTreePtr base = astIdentifier(function, context);
    AbstractSyntaxTreePtr parentheses;
    if (arguments.empty()) {
        parentheses = AbstractSyntaxTree::createNode(OP_PARENS, nullptr, context);
    } else {
        for (size_t k = 1; k < arguments.size(); ++k) {
            arguments[0]->addPeer(arguments[k]);
        }
        parentheses = AbstractSyntaxTree::createNode(OP_PARENS, arguments[0], context);
    }
    base->addChild(parentheses);
    return base;
}
//=============================================================================
// A symbRefList used as an expression (a variable read or a call) is wrapped
// in OP_RHS by the grammar; assignment targets use the bare symbRefList.
AbstractSyntaxTreePtr
astReferenceExpression(AbstractSyntaxTreePtr reference, int context)
{
    return AbstractSyntaxTree::createNode(OP_RHS, reference, context);
}
//=============================================================================
AbstractSyntaxTreePtr
astUnaryNot(AbstractSyntaxTreePtr operand, int context)
{
    return AbstractSyntaxTree::createNode(OP_NOT, operand, context);
}
//=============================================================================
AbstractSyntaxTreePtr
astBinaryOperation(OP_TYPE op, AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs, int context)
{
    return AbstractSyntaxTree::createNode(op, lhs, rhs, context);
}
//=============================================================================
// if (condition) thenBlock [else elseBlock] end. Matches the shape compileIf
// expects: the IF reserved node's first child is an OP_CSTAT whose down is the
// condition and whose down->right is the body block; the OP_CSTAT's right peer
// is the (optional) else OP_BLOCK. elseBlock may itself hold a nested if to
// express an elseif chain without OP_ELSEIFBLOCK.
AbstractSyntaxTreePtr
astIfElse(AbstractSyntaxTreePtr condition, AbstractSyntaxTreePtr thenBlock,
    AbstractSyntaxTreePtr elseBlock, int context)
{
    AbstractSyntaxTreePtr ifNode
        = AbstractSyntaxTree::createNode(reserved_node, static_cast<int>(NLS_KEYWORD_IF), context);
    ifNode->addChild(AbstractSyntaxTree::createNode(OP_CSTAT, condition, thenBlock, context));
    if (elseBlock != nullptr) {
        ifNode->addChild(elseBlock);
    }
    return ifNode;
}
//=============================================================================
// for variableName = rangeExpression; bodyBlock; end. Matches compileFor: the
// FOR reserved node's first child is the loop-variable id_node whose down is
// the range and whose right peer is the body block.
AbstractSyntaxTreePtr
astForLoop(const std::string& variableName, AbstractSyntaxTreePtr rangeExpression,
    AbstractSyntaxTreePtr bodyBlock, int context)
{
    AbstractSyntaxTreePtr forNode
        = AbstractSyntaxTree::createNode(reserved_node, static_cast<int>(NLS_KEYWORD_FOR), context);
    AbstractSyntaxTreePtr index = astIdentifier(variableName, context);
    index->addChild(rangeExpression);
    forNode->addChild(index);
    forNode->addChild(bodyBlock);
    return forNode;
}
//=============================================================================
// base{indexExpression}: a symbRefList (id + OP_BRACES child). Wrap in
// astReferenceExpression to use it as a value expression.
AbstractSyntaxTreePtr
astBraceIndex(const std::string& base, AbstractSyntaxTreePtr indexExpression, int context)
{
    AbstractSyntaxTreePtr node = astIdentifier(base, context);
    node->addChild(AbstractSyntaxTree::createNode(OP_BRACES, indexExpression, context));
    return node;
}
//=============================================================================
AbstractSyntaxTreePtr
astDotReference(const std::string& object, const std::string& field, int context)
{
    AbstractSyntaxTreePtr base = astIdentifier(object, context);
    base->addChild(AbstractSyntaxTree::createNode(OP_DOT, astIdentifier(field, context), context));
    return base;
}
//=============================================================================
AbstractSyntaxTreePtr
astAssignment(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs, int context)
{
    return AbstractSyntaxTree::createNode(OP_ASSIGN, lhs, rhs, context);
}
//=============================================================================
AbstractSyntaxTreePtr
astQuietStatement(AbstractSyntaxTreePtr statement, int context)
{
    AbstractSyntaxTreePtr node = AbstractSyntaxTree::createNode(OP_QSTATEMENT, nullptr, context);
    node->down = statement;
    return node;
}
//=============================================================================
AbstractSyntaxTreePtr
astStatementBlock(const std::vector<AbstractSyntaxTreePtr>& statements, int context)
{
    AbstractSyntaxTreePtr block = AbstractSyntaxTree::createNode(OP_BLOCK, statements[0], context);
    for (size_t k = 1; k < statements.size(); ++k) {
        block->addChild(statements[k]);
    }
    return block;
}
//=============================================================================
AbstractSyntaxTreePtr
parseExpressionInto(const std::string& expressionText, AbstractSyntaxTreePtrVector& owned)
{
    AbstractSyntaxTree::clearReferences();
    LexerContext lexerContext;
    ParserContext parsed = parseStringResult(lexerContext, expressionText);
    AbstractSyntaxTreePtr expression = nullptr;
    if (parsed.state() == ScriptBlock) {
        AbstractSyntaxTreePtr block = getParsedScriptBlock();
        // OP_BLOCK -> OP_(R|Q)STATEMENT -> expression, exactly one statement.
        if (block != nullptr && block->opNum == OP_BLOCK && block->down != nullptr
            && block->down->right == nullptr && block->down->down != nullptr
            && block->down->down->opNum != OP_ASSIGN) {
            expression = block->down->down;
        }
    }
    AbstractSyntaxTreePtrVector nodes = AbstractSyntaxTree::getReferences();
    owned.insert(owned.end(), nodes.begin(), nodes.end());
    AbstractSyntaxTree::clearReferences();
    return expression;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
