//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BytecodeIO.hpp"
#include <iterator>
//=============================================================================
namespace Nelson::BytecodeIO {
//=============================================================================
namespace {
    enum class Operand
    {
        Zero,
        Value,
        Local,
        Name,
        Array,
        Text,
        Jump,
        Loop,
        Span,
        Program,
        Scoped,
        Rejected
    };
    //=============================================================================
    struct Rule
    {
        OpCode op;
        Operand a;
        Operand b;
        Operand c;
    };
    //=============================================================================
    // One explicit rule per opcode. Adding an opcode requires updating the
    // persisted representation audit, including reserved VM-only instructions.
    constexpr Rule rules[] = {
        { OpCode::LOAD_CONST, Operand::Array, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_TRUE, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_FALSE, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_EMPTY, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_EMPTY_CELL, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_LOCAL, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_LOCAL_DOT, Operand::Local, Operand::Text, Operand::Zero },
        { OpCode::LOAD_PARENS_LOCAL, Operand::Local, Operand::Local, Operand::Value },
        { OpCode::LOAD_PARENS_LOCAL_OFFSET, Operand::Local, Operand::Local, Operand::Array },
        { OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL, Operand::Local, Operand::Local, Operand::Local },
        { OpCode::LOAD_BRACES_LOCAL, Operand::Local, Operand::Local, Operand::Value },
        { OpCode::LOAD_LOCAL_COUNT_RANGE, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::STORE_LOCAL, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_GLOBAL, Operand::Name, Operand::Value, Operand::Zero },
        { OpCode::LOAD_GLOBAL_COUNT_RANGE, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::STORE_GLOBAL, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_ANS, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::STORE_ANS, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::DELETE_LOCAL, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::CLEAR_LOCAL, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::POP, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::DUP, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::SWAP, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::SET_NAME, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::OP_PLUS, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_MINUS, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_MTIMES, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_TIMES, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_MRDIV, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_RDIV, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_MLDIV, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_LDIV, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_MPOWER, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_POWER, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_COLON_UNIT, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_COLON, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_UPLUS, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_UMINUS, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_NOT, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_TRANSPOSE, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_DOT_TRANSPOSE, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_LT, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_LEQ, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_GT, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_GEQ, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_EQ, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_NEQ, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_AND, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_OR, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::OP_SAND, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::OP_SOR, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::SUBIDX_PARENS, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::SUBIDX_BRACES, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::SUBIDX_DOT, Operand::Value, Operand::Text, Operand::Value },
        { OpCode::SUBIDX_DOTDYN, Operand::Zero, Operand::Zero, Operand::Value },
        { OpCode::ASGN_PARENS, Operand::Local, Operand::Value, Operand::Zero },
        { OpCode::ASGN_PARENS_CONST, Operand::Local, Operand::Array, Operand::Array },
        { OpCode::ASGN_STRING_SCALAR_CONST, Operand::Local, Operand::Value, Operand::Array },
        { OpCode::ASGN_BRACES, Operand::Local, Operand::Value, Operand::Zero },
        { OpCode::ASGN_BRACES_LOCAL, Operand::Local, Operand::Local, Operand::Local },
        { OpCode::ASGN_BRACES_LOCAL_COL, Operand::Local, Operand::Local, Operand::Local },
        { OpCode::ASGN_BRACES_CONST_COL, Operand::Local, Operand::Local, Operand::Array },
        { OpCode::ASGN_DOT, Operand::Local, Operand::Text, Operand::Zero },
        { OpCode::ASGN_DOT_LOCAL, Operand::Local, Operand::Text, Operand::Local },
        { OpCode::ASGN_DOT_PLUS_CONST, Operand::Local, Operand::Text, Operand::Array },
        { OpCode::ASGN_DOT_DOT_PLUS_SMALL, Operand::Local, Operand::Text, Operand::Text },
        { OpCode::ASGN_DOT_DOT_LOCAL, Operand::Local, Operand::Text, Operand::Text },
        { OpCode::ASGN_DOTDYN, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::ASGN_PARENS_DOT, Operand::Local, Operand::Value, Operand::Text },
        { OpCode::ASGN_PARENS_VALUE, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::ASGN_BRACES_VALUE, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::ASGN_DOT_CHAIN_VALUE, Operand::Text, Operand::Value, Operand::Zero },
        { OpCode::ASGN_BRACES_MULTI_VALUE, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::ASGN_DOT_VALUE, Operand::Zero, Operand::Text, Operand::Zero },
        { OpCode::ASGN_DOTDYN_VALUE, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::ASGN_DOT_MULTI_VALUE, Operand::Text, Operand::Zero, Operand::Value },
        { OpCode::ASGN_DOTDYN_MULTI_VALUE, Operand::Zero, Operand::Zero, Operand::Value },
        { OpCode::LOAD_FIELD_TARGET_COUNT, Operand::Scoped, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_CONTENT_TARGET_COUNT, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::ASSIGN, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::ASSIGN_PRINT, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::ASSIGN_PARENS_LOCAL, Operand::Local, Operand::Local, Operand::Local },
        { OpCode::ASSIGN_PLUS_STACK, Operand::Local, Operand::Zero, Operand::Zero },
        { OpCode::ASSIGN_PLUS_LOCAL, Operand::Local, Operand::Local, Operand::Zero },
        { OpCode::ASSIGN_PLUS_CONST, Operand::Local, Operand::Array, Operand::Zero },
        { OpCode::ASSIGN_PLUS_RDIV_LOCAL, Operand::Local, Operand::Local, Operand::Array },
        { OpCode::ASSIGN_PLUS_LOCAL_RDIV_LOCAL, Operand::Local, Operand::Local, Operand::Local },
        { OpCode::ASSIGN_PLUS_PARENS_LOCAL, Operand::Local, Operand::Local, Operand::Local },
        { OpCode::ASSIGN_PLUS_CELL_LOCAL, Operand::Local, Operand::Local, Operand::Local },
        { OpCode::JUMP, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::JUMP_IF_FALSE, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::JUMP_IF_TRUE, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::JUMP_IF_FALSE_SCALAR, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::JUMP_IF_TRUE_SCALAR, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::JUMP_IF_NARGIN_CMP, Operand::Jump, Operand::Value, Operand::Zero },
        { OpCode::JUMP_IF_ARG_PRED, Operand::Jump, Operand::Local, Operand::Value },
        { OpCode::CHECK_SCALAR_AND, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::CHECK_SCALAR_OR, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::CASE_MATCH, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::FOR_INIT, Operand::Loop, Operand::Scoped, Operand::Jump },
        { OpCode::FOR_STEP, Operand::Loop, Operand::Scoped, Operand::Jump },
        { OpCode::FOR_INIT_DBL, Operand::Loop, Operand::Scoped, Operand::Jump },
        { OpCode::FOR_STEP_DBL, Operand::Loop, Operand::Scoped, Operand::Jump },
        { OpCode::FOR_INIT_RANGE, Operand::Loop, Operand::Scoped, Operand::Jump },
        { OpCode::FOR_STEP_RANGE, Operand::Loop, Operand::Scoped, Operand::Jump },
        { OpCode::WHILE_CHECK, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::BREAK, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::CONTINUE, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::CALL_BUILTIN, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::CALL_MACRO, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::CALL_CLASSDEF_MEMBER, Operand::Text, Operand::Value, Operand::Value },
        { OpCode::CALL_NAMED, Operand::Name, Operand::Value, Operand::Value },
        { OpCode::CALL_NAMED_DYNAMIC, Operand::Name, Operand::Value, Operand::Value },
        { OpCode::SUPER_CALL, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::CALL_HANDLE, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::CALL_HANDLE_DYNAMIC, Operand::Zero, Operand::Value, Operand::Value },
        { OpCode::EXPAND_COMMA_LIST, Operand::Value, Operand::Zero, Operand::Zero },
        { OpCode::EXPAND_COMMA_LIST_DYNAMIC, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::MULTI_ASSIGN, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::BUILD_MATRIX, Operand::Value, Operand::Zero, Operand::Zero },
        { OpCode::BUILD_CELL, Operand::Value, Operand::Value, Operand::Zero },
        { OpCode::BUILD_CELL_FIELD_LIST, Operand::Zero, Operand::Text, Operand::Zero },
        { OpCode::BUILD_ROW, Operand::Value, Operand::Zero, Operand::Zero },
        { OpCode::PUSH_END_CTX, Operand::Value, Operand::Value, Operand::Value },
        { OpCode::PUSH_END_CTX_LOCAL, Operand::Local, Operand::Value, Operand::Value },
        { OpCode::POP_END_CTX, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_END, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::DISPLAY, Operand::Local, Operand::Name, Operand::Zero },
        { OpCode::DISPLAY_STACK, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::DISPLAY_ANS, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::MAKE_FH_NAMED, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::MAKE_FH_ANONYMOUS, Operand::Text, Operand::Text, Operand::Text },
        { OpCode::DECLARE_GLOBAL, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::DECLARE_PERSISTENT, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::DECLARE_IMPORT, Operand::Name, Operand::Zero, Operand::Zero },
        { OpCode::RETURN, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::ABORT, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::QUIT, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::TRY_BEGIN, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::TRY_END, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::CATCH_BEGIN, Operand::Scoped, Operand::Zero, Operand::Zero },
        { OpCode::DEBUG_STMT, Operand::Span, Operand::Zero, Operand::Zero },
        { OpCode::PROF_BEGIN, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::PROF_END, Operand::Rejected, Operand::Zero, Operand::Zero },
        { OpCode::CHECK_INTERRUPT, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::NOP, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::SCALAR_EXPR, Operand::Program, Operand::Zero, Operand::Zero },
        { OpCode::CHAIN_BEGIN, Operand::Local, Operand::Value, Operand::Zero },
        { OpCode::CHAIN_END, Operand::Zero, Operand::Zero, Operand::Zero },
        { OpCode::LOAD_GLOBAL_OR_JUMP, Operand::Jump, Operand::Name, Operand::Value },
        { OpCode::ENSURE_DOTTED_FUNCTION, Operand::Name, Operand::Name, Operand::Zero },
        { OpCode::KEYBOARD, Operand::Span, Operand::Zero, Operand::Zero },
        { OpCode::JUMP_IF_NOT_TABLE, Operand::Jump, Operand::Zero, Operand::Zero },
        { OpCode::ASGN_DOTDYN_CHAIN_VALUE, Operand::Zero, Operand::Value, Operand::Zero },
        { OpCode::SUBIDX_DOTDYN_CHAIN, Operand::Zero, Operand::Value, Operand::Zero },
    };
    //=============================================================================
    static_assert(std::size(rules) == static_cast<size_t>(OpCode::OPCODE_COUNT));
    void
    validateOperand(
        const BytecodeChunk& chunk, const Instruction& ins, uint16_t value, Operand kind)
    {
        switch (kind) {
        case Operand::Zero:
            require(value == 0, "Invalid unused bytecode operand.");
            break;
        case Operand::Value:
            break;
        case Operand::Local:
            require(value < chunk.nLocals, "Invalid bytecode local slot.");
            break;
        case Operand::Name:
            require(value < chunk.names.size(), "Invalid bytecode name index.");
            break;
        case Operand::Array:
            require(value < chunk.constants.size()
                    && std::holds_alternative<ArrayOf>(chunk.constants.get(value)),
                "Invalid bytecode array constant.");
            break;
        case Operand::Text:
            require(value < chunk.constants.size()
                    && std::holds_alternative<std::string>(chunk.constants.get(value)),
                "Invalid bytecode text constant.");
            break;
        case Operand::Jump:
            require(value <= chunk.code.size(), "Invalid bytecode jump target.");
            break;
        case Operand::Loop:
            require(value < chunk.nLoopSlots, "Invalid bytecode loop slot.");
            break;
        case Operand::Span:
            require(value < chunk.spans.size(), "Invalid bytecode source span.");
            break;
        case Operand::Program:
            require(value < chunk.scalarExprPrograms.size(), "Invalid bytecode scalar program.");
            break;
        case Operand::Scoped:
            validateOperand(chunk, ins, value,
                (ins.flags & INST_FLAG_GLOBAL) != 0 ? Operand::Name : Operand::Local);
            break;
        case Operand::Rejected:
            throw std::runtime_error("Opcode cannot be loaded from a bytecode artifact.");
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
validateInstruction(const BytecodeChunk& chunk, size_t pc, const Instruction& ins)
{
    require(ins.op < OpCode::OPCODE_COUNT, "Invalid bytecode opcode.");
    const auto& rule = rules[static_cast<size_t>(ins.op)];
    require(rule.op == ins.op);
    validateOperand(chunk, ins, ins.A, rule.a);
    validateOperand(chunk, ins, ins.B, rule.b);
    validateOperand(chunk, ins, ins.C, rule.c);
    switch (ins.op) {
    case OpCode::JUMP_IF_NARGIN_CMP:
        require(ins.flags <= NARGIN_CMP_NE);
        break;
    case OpCode::JUMP_IF_ARG_PRED: {
        const auto predicate = ins.flags & ARG_PRED_ID_MASK;
        require(predicate <= ARG_PRED_ISSCALAR);
        const auto window = (ins.flags >> ARG_PRED_WINDOW_SHIFT) & ARG_PRED_WINDOW_MASK;
        require(pc + 1 + window <= chunk.code.size());
        if (predicate == ARG_PRED_ISA) {
            validateOperand(chunk, ins, ins.C, Operand::Text);
        }
        break;
    }
    case OpCode::LOAD_GLOBAL:
        require(ins.B == 0 || ins.B - 1 < chunk.nLocals);
        break;
    case OpCode::LOAD_GLOBAL_OR_JUMP:
        if ((ins.flags & INST_FLAG_PREFER_DOTTED) != 0) {
            validateOperand(chunk, ins, ins.C, Operand::Name);
        }
        break;
    case OpCode::ASGN_DOT_DOT_LOCAL:
        require(ins.flags != 0 && ins.flags - 1 < chunk.nLocals);
        break;
    case OpCode::ASGN_DOT_DOT_PLUS_SMALL:
    case OpCode::ASGN_BRACES_LOCAL_COL:
    case OpCode::ASGN_BRACES_CONST_COL:
        require(ins.flags != 0);
        break;
    case OpCode::CHAIN_BEGIN:
        require(ins.B <= 2);
        break;
    default:
        break;
    }
}
//=============================================================================
} // namespace Nelson::BytecodeIO
//=============================================================================
