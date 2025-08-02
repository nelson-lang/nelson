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
    OP_BLOCK, // block
    OP_CASEBLOCK, // caseblock
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
    OP_FUNCTION_HANDLE_NAMED

};
//=============================================================================
class AbstractSyntaxTree;
using AbstractSyntaxTreePtr = AbstractSyntaxTree*;
using AbstractSyntaxTreePtrVector = std::vector<AbstractSyntaxTreePtr>;
//=============================================================================
/** The abstract syntax tree class
 * This class represents an abstract syntax tree class (AST).  The AST node has
 * both a "down" and "right" child.
 */
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
    /** Context string
     * Returns the context string for this node.
     */
    int
    getContext();
    /** Test for a match
     * Returns true if textual content of this node matches the supplied argument
     * (and is not NULL).
     */
    bool
    match(OP_TYPE test);
    /** Add the given tree as a child to the current node
     * Adds the argument tree as a child to the current node.  If the current node
     * has no "children" (i.e., down = nullptr) then the supplied tree is placed as
     * a child.  If there are children, then the node is added as a peer to the last
     * such child.
     */
    void
    addChild(AbstractSyntaxTreePtr arg);
    /** Add the given tree as a peer to the current node
     * Adds the argument tree as a peer to the current node.  If the current node
     * has no "peers" (i.e., right = nullptr) then the supplied tree is placed as a
     * peer. If there are peers, then the node is added as a peer to the last such
     * peer.
     */
    void
    addPeer(AbstractSyntaxTreePtr arg);
    /** Count children
     * Returns the number of children to the current tree.  This is only the number of
     * children that are immediately below the current node (i.e., children of children
     * are not counted).
     */
    int
    childCount();
    /** Count peers
     * Returns the number of peers to the current tree.  This is only the number of
     * peers that are adjacent to the current node (i.e., children of peers are
     * not counted).
     */
    int
    peerCount();
    /** Convert this into a string list.
     * Builds a list of strings from the current node and its children.
     */
    stringVector
    toStringList();

    std::string
    toString(bool firstLevel);

    /** Test for an empty tree
     * Returns true if the current node is unallocated or is not a reserved node
     * with an empty text string.
     */
    bool
    isEmpty();

    /** Destructor
     */
    ~AbstractSyntaxTree();

private:
    /** Default constructor
     * Creates an empty AST node.  All pointers are initialized to NULL,
     * the type is set to non_terminal.
     */
    AbstractSyntaxTree();
    /** Text constructor
     * Creates a node of the specified type with the text field set to a copy
     * of the name argument.
     */
    AbstractSyntaxTree(NODE_TYPE ntype, const std::string& name, int context);
    /** Token constructor
     * Creates a node of the specified type with the tokenNumber field set to
     * the token argument.  This constructor is useful for nodes that are represented
     * by a single, numeric token (as opposed to a string).
     */
    AbstractSyntaxTree(NODE_TYPE ntype, int token, int context);
    /** Nonterminal constructor with two arguments
     * Creates a non-terminal node with the text set to a copy of the name argument
     * with the given left and right AST nodes.
     */
    AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr rt, int context);
    /** Nonterminal constructor with three arguments
     * Creates a non-terminal node with the text set to a copy of the name argument
     * with the given three AST nodes.
     */
    AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr lt, AbstractSyntaxTreePtr md,
        AbstractSyntaxTreePtr rt, int context);
    /** Nonterminal constructor with a single argument
     * Creates a non-terminal node with the text set to a copy of the name argument
     * with the given AST node.
     */
    AbstractSyntaxTree(OP_TYPE op, AbstractSyntaxTreePtr arg, int context);

    static AbstractSyntaxTreePtrVector pAstVector;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
