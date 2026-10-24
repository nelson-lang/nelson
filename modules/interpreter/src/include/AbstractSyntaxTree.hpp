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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include <vector>
#include "nlsInterpreter_exports.h"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum NODE_TYPE
{
    const_double_node = 0,
    const_int_node,
    const_int8_node,
    const_uint8_node,
    const_int16_node,
    const_uint16_node,
    const_int32_node,
    const_uint32_node,
    const_int64_node,
    const_uint64_node,
    const_float_node,
    const_character_array_node,
    const_string_node,
    const_dcomplex_node,
    const_complex_node,
    reserved_node,
    non_terminal,
    id_node,
    null_node,
};
//=============================================================================
enum OP_TYPE
{
    OP_BLOCK,
    OP_CASEBLOCK,
    OP_RHS,
    OP_CSTAT,
    OP_ELSEIFBLOCK,
    OP_ASSIGN,
    OP_MULTICALL,
    OP_COLON,
    OP_PLUS,
    OP_SUBTRACT,
    OP_TIMES,
    OP_RDIV,
    OP_LDIV,
    OP_OR,
    OP_AND,
    OP_SOR,
    OP_SAND,
    OP_LT,
    OP_LEQ,
    OP_GT,
    OP_GEQ,
    OP_EQ,
    OP_NEQ,
    OP_DOT_TIMES,
    OP_DOT_RDIV,
    OP_DOT_LDIV,
    OP_UMINUS,
    OP_UPLUS,
    OP_NOT,
    OP_MPOWER,
    OP_POWER,
    OP_TRANSPOSE,
    OP_DOT_TRANSPOSE,
    OP_EMPTY,
    OP_EMPTY_CELL,
    OP_PARENS,
    OP_BRACES,
    OP_BRACKETS,
    OP_DOT,
    OP_ALL,
    OP_SEMICOLON,
    OP_NULL,
    OP_RSTATEMENT,
    OP_QSTATEMENT,
    OP_SCALL,
    OP_KEYWORD,
    OP_DOTDYN,
    OP_FUNCTION_HANDLE_ANONYMOUS,
    OP_FUNCTION_HANDLE_NAMED,
    OP_POSTFIX,
    OP_SUPERCLASS_CALL, // obj@Super(args) / method@Super(args)
    OP_ARGUMENTS // arguments ... end validation block (opaque source captured by the lexer)

};
//=============================================================================
class AbstractSyntaxTree;
using AbstractSyntaxTreePtr = AbstractSyntaxTree*;
using AbstractSyntaxTreePtrVector = std::vector<AbstractSyntaxTreePtr>;
//=============================================================================
/** AST node with a "down" (first child) and a "right" (next peer) pointer. */
class NLSINTERPRETER_IMPEXP AbstractSyntaxTree
{
public:
    //=============================================================================
    NODE_TYPE type;
    std::string text;
    int tokenNumber;
    int m_context;
    AbstractSyntaxTreePtr down;
    AbstractSyntaxTreePtr right;
    OP_TYPE opNum;
    //=============================================================================
    static AbstractSyntaxTreePtrVector astUsedAsVector;
    //=============================================================================
    static AbstractSyntaxTreePtr
    createNode(NODE_TYPE ntype, const std::string& name, int context);
    //=============================================================================
    static AbstractSyntaxTreePtr
    createNode(NODE_TYPE ntype, int token, int context);
    //=============================================================================
    static AbstractSyntaxTreePtr
    createNode(OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr rt, int context);
    //=============================================================================
    static AbstractSyntaxTreePtr
    createNode(OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr md,
        AbstractSyntaxTreePtr rt, int context);
    //=============================================================================
    static AbstractSyntaxTreePtr
    createNode(OP_TYPE op, AbstractSyntaxTreePtr arg, int context);
    //=============================================================================
    static void
    clearReferences();
    //=============================================================================
    static void
    deleteReferences();
    //=============================================================================
    static void
    deleteReferences(AbstractSyntaxTreePtrVector& astAsVector);
    //=============================================================================
    static AbstractSyntaxTreePtrVector
    getReferences();
    //=============================================================================
    int
    getContext();
    bool
    match(OP_TYPE test);
    /** Add arg as a child. With no child yet (down == nullptr) it becomes the
     * first child, otherwise it is added as a peer of the last child.
     */
    void
    addChild(AbstractSyntaxTreePtr arg);
    /** Add arg as a peer. With no peer yet (right == nullptr) it becomes the
     * first peer, otherwise it is added after the last peer.
     */
    void
    addPeer(AbstractSyntaxTreePtr arg);
    /** Number of direct children (children of children are not counted). */
    int
    childCount();
    /** Number of peers on the right (children of peers are not counted). */
    int
    peerCount();
    stringVector
    toStringList();

    std::string
    toString(bool firstLevel);

    bool
    isEmpty();

    ~AbstractSyntaxTree();

private:
    AbstractSyntaxTree();
    AbstractSyntaxTree(NODE_TYPE ntype, const std::string& name, int context);
    AbstractSyntaxTree(NODE_TYPE ntype, int token, int context);
    AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr rt, int context);
    AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr md,
        AbstractSyntaxTreePtr rt, int context);
    AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr arg, int context);

    static AbstractSyntaxTreePtrVector pAstVector;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
