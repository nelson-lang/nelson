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
#include <cstdint>
#include <string>
#include "ArrayOf.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class OpCode : uint8_t
{
    LOAD_CONST,
    LOAD_TRUE,
    LOAD_FALSE,
    LOAD_EMPTY,
    LOAD_EMPTY_CELL,
    LOAD_LOCAL,
    LOAD_LOCAL_COUNT_RANGE,
    STORE_LOCAL,
    LOAD_GLOBAL,
    LOAD_GLOBAL_COUNT_RANGE,
    STORE_GLOBAL,
    LOAD_ANS,
    STORE_ANS,
    DELETE_LOCAL,
    CLEAR_LOCAL,
    POP,
    DUP,
    SWAP,
    SET_NAME,
    OP_PLUS,
    OP_MINUS,
    OP_MTIMES,
    OP_TIMES,
    OP_MRDIV,
    OP_RDIV,
    OP_MLDIV,
    OP_LDIV,
    OP_MPOWER,
    OP_POWER,
    OP_COLON_UNIT,
    OP_COLON,
    OP_UPLUS,
    OP_UMINUS,
    OP_NOT,
    OP_TRANSPOSE,
    OP_DOT_TRANSPOSE,
    OP_LT,
    OP_LEQ,
    OP_GT,
    OP_GEQ,
    OP_EQ,
    OP_NEQ,
    OP_AND,
    OP_OR,
    OP_SAND,
    OP_SOR,
    SUBIDX_PARENS,
    SUBIDX_BRACES,
    SUBIDX_DOT,
    SUBIDX_DOTDYN,
    ASGN_PARENS,
    ASGN_BRACES,
    ASGN_DOT,
    ASGN_DOTDYN,
    ASGN_PARENS_VALUE,
    ASGN_BRACES_VALUE,
    ASGN_BRACES_MULTI_VALUE,
    ASGN_DOT_VALUE,
    ASGN_DOTDYN_VALUE,
    ASSIGN,
    ASSIGN_PRINT,
    JUMP,
    JUMP_IF_FALSE,
    JUMP_IF_TRUE,
    JUMP_IF_FALSE_SCALAR,
    JUMP_IF_TRUE_SCALAR,
    CHECK_SCALAR_AND,
    CHECK_SCALAR_OR,
    CASE_MATCH,
    FOR_INIT,
    FOR_STEP,
    FOR_INIT_DBL,
    FOR_STEP_DBL,
    FOR_INIT_RANGE,
    FOR_STEP_RANGE,
    WHILE_CHECK,
    BREAK,
    CONTINUE,
    CALL_BUILTIN,
    CALL_MACRO,
    CALL_CLASSDEF_MEMBER,
    CALL_NAMED,
    CALL_NAMED_DYNAMIC,
    CALL_HANDLE,
    CALL_HANDLE_DYNAMIC,
    MULTI_ASSIGN,
    BUILD_MATRIX,
    BUILD_CELL,
    BUILD_CELL_FIELD_LIST,
    BUILD_ROW,
    PUSH_END_CTX,
    POP_END_CTX,
    LOAD_END,
    DISPLAY,
    DISPLAY_STACK,
    DISPLAY_ANS,
    MAKE_FH_NAMED,
    MAKE_FH_ANONYMOUS,
    DECLARE_GLOBAL,
    DECLARE_PERSISTENT,
    RETURN,
    ABORT,
    QUIT,
    TRY_BEGIN,
    TRY_END,
    CATCH_BEGIN,
    DEBUG_STMT,
    PROF_BEGIN,
    PROF_END,
    CHECK_INTERRUPT,
    NOP,
    OPCODE_COUNT
};
//=============================================================================
struct Instruction
{
    OpCode op;
    uint8_t flags;
    uint16_t A;
    uint16_t B;
    uint16_t C;
};
static_assert(sizeof(Instruction) == 8, "Instruction must be 8 bytes");
//=============================================================================
constexpr uint8_t INST_FLAG_PRINT = 0x01;
constexpr uint8_t INST_FLAG_QUIET = 0x02;
constexpr uint8_t INST_FLAG_ECHO = 0x04;
constexpr uint8_t INST_FLAG_GLOBAL = 0x08;
constexpr uint8_t INST_FLAG_RANGE_HAS_STEP = 0x10;
constexpr uint8_t INST_FLAG_LIST = 0x20;
constexpr uint8_t INST_FLAG_INDEX_ONLY = 0x40;
constexpr uint8_t INST_FLAG_ALLOW_UNDEFINED = 0x80;
//=============================================================================
struct SourceSpan
{
    uint32_t instrIndex = 0;
    uint32_t line = 1;
    uint16_t col = 0;
    uint16_t endLine = 0;
};
//=============================================================================
struct GenericLoopState
{
    ArrayOf indexSet;
    indexType current = 0;
    indexType count = 0;
    bool isRowVector = true;
};
//=============================================================================
struct DoubleLoopState
{
    ArrayOf indexSet;
    const double* dataPtr = nullptr;
    indexType current = 0;
    indexType count = 0;
};
//=============================================================================
struct RangeLoopState
{
    double start = 0.;
    double step = 1.;
    double stop = 0.;
    double current = 0.;
};
//=============================================================================
const char*
opCodeName(OpCode op);
//=============================================================================
} // namespace Nelson
//=============================================================================
