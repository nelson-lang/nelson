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
    LOAD_LOCAL_DOT,
    LOAD_PARENS_LOCAL,
    LOAD_PARENS_LOCAL_OFFSET,
    LOAD_PARENS_LOCAL_MINUS_LOCAL,
    LOAD_BRACES_LOCAL,
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
    ASGN_PARENS_CONST,
    ASGN_STRING_SCALAR_CONST,
    ASGN_BRACES,
    ASGN_BRACES_LOCAL,
    ASGN_BRACES_LOCAL_COL,
    ASGN_BRACES_CONST_COL,
    ASGN_DOT,
    ASGN_DOT_LOCAL,
    ASGN_DOT_PLUS_CONST,
    ASGN_DOT_DOT_PLUS_SMALL,
    ASGN_DOT_DOT_LOCAL,
    ASGN_DOTDYN,
    ASGN_PARENS_DOT,
    ASGN_PARENS_VALUE,
    ASGN_BRACES_VALUE,
    ASGN_DOT_CHAIN_VALUE,
    ASGN_BRACES_MULTI_VALUE,
    ASGN_DOT_VALUE,
    ASGN_DOTDYN_VALUE,
    // Distribute N stacked values across the fields of an N-element struct
    // array target, e.g. `[s.field] = deal(1, 2)`. A = field name constant,
    // C = value count (0 = use numel(base) computed at runtime).
    ASGN_DOT_MULTI_VALUE,
    // Same as ASGN_DOT_MULTI_VALUE for a dynamic field name `[s.(expr)]`; the
    // evaluated field-name expression sits on top of the stack above the base.
    ASGN_DOTDYN_MULTI_VALUE,
    // Output-count spec for a `[s.field]` struct-field assignment target: pushes
    // `1:max(1, numel(s))` so an undefined or empty target still requests a
    // single output and is grown into a 1x1 struct. INST_FLAG_GLOBAL selects a
    // by-name (A = name index) lookup; otherwise A = local slot.
    LOAD_FIELD_TARGET_COUNT,
    // Output-count spec for a multi-subscript brace assignment target such as
    // `[c{1:2,1}]`: pops the target base and B evaluated subscript values from
    // the stack, resolves any colon against the base, and pushes a range whose
    // element count is the product of the per-subscript element counts (the
    // number of assignment targets). B = number of subscripts.
    LOAD_CONTENT_TARGET_COUNT,
    ASSIGN,
    ASSIGN_PRINT,
    ASSIGN_PARENS_LOCAL,
    ASSIGN_PLUS_STACK,
    ASSIGN_PLUS_LOCAL,
    ASSIGN_PLUS_CONST,
    ASSIGN_PLUS_RDIV_LOCAL,
    ASSIGN_PLUS_LOCAL_RDIV_LOCAL,
    ASSIGN_PLUS_PARENS_LOCAL,
    ASSIGN_PLUS_CELL_LOCAL,
    JUMP,
    JUMP_IF_FALSE,
    JUMP_IF_TRUE,
    JUMP_IF_FALSE_SCALAR,
    JUMP_IF_TRUE_SCALAR,
    // Fused `if nargin CMP K` jump: flags = comparison id (see
    // NARGIN_CMP_*), A = else target, B = K. Jumps when the comparison is
    // false. Keeps "nargin" out of the chunk names so the chunk stays
    // eligible for the scope-free call fast paths.
    JUMP_IF_NARGIN_CMP,
    // Fused argument-predicate jump (machine-generated `if ~isa(x, 'cls')`
    // and `if ~isscalar(x)` checks): flags bits 0-1 = predicate id
    // (ARG_PRED_*), bit 2 = jump when the predicate is TRUE, bits 3-5 =
    // length of the generic fallback window that follows; A = else target,
    // B = tested local slot, C = class-name constant (isa only). When the
    // predicate name is shadowed at runtime the VM falls through to the
    // window, which re-evaluates the original condition.
    JUMP_IF_ARG_PRED,
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
    SUPER_CALL,
    CALL_HANDLE,
    CALL_HANDLE_DYNAMIC,
    EXPAND_COMMA_LIST,
    // Expand a comma-list source to a runtime-computed number of values.
    // Pops an output-count spec, then the comma-list value below it, and
    // pushes exactly that many values (erroring if the source yields fewer).
    // Used by `[x{1:N}] = c{:}` where the target count is only known at runtime.
    EXPAND_COMMA_LIST_DYNAMIC,
    MULTI_ASSIGN,
    BUILD_MATRIX,
    BUILD_CELL,
    BUILD_CELL_FIELD_LIST,
    BUILD_ROW,
    PUSH_END_CTX,
    PUSH_END_CTX_LOCAL,
    POP_END_CTX,
    LOAD_END,
    DISPLAY,
    DISPLAY_STACK,
    DISPLAY_ANS,
    MAKE_FH_NAMED,
    MAKE_FH_ANONYMOUS,
    DECLARE_GLOBAL,
    DECLARE_PERSISTENT,
    DECLARE_IMPORT,
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
    // Fused straight-line scalar arithmetic; replaces the first instruction of
    // a matched window, the remaining original instructions are kept as the
    // fallback path (see BytecodeChunk::scalarExprPrograms).
    SCALAR_EXPR,
    // Whole-chain indexing: CHAIN_BEGIN(A=headSlot) activates accumulation when
    // the head is a subsref/Redefines* classdef object; per-level opcodes then
    // record instead of apply; CHAIN_END dispatches the full chain once.
    CHAIN_BEGIN,
    CHAIN_END,
    // Guarded head load for a possible package-qualified call pkg.func(args):
    // loads the head identifier exactly like LOAD_GLOBAL (B = name index,
    // same flags) and falls through into the subindex chain; only when the
    // name resolves to neither a variable nor a zero-argument function does it
    // jump to A (the package-call fallback) instead of raising
    // "Undefined variable". With INST_FLAG_PREFER_DOTTED set, C is the name
    // index of the full dotted call target: when the head is not a variable
    // and that dotted name resolves to a function (classdef constructor or
    // static method, package function), the jump is taken before the head is
    // evaluated as a function, so `pkg.Class.method(args)` keeps its priority
    // over `headFunction().Class.method(args)`.
    LOAD_GLOBAL_OR_JUMP,
    // First instruction of the package-call fallback: raises
    // "Undefined variable: <head>" (B = head name index) unless the dotted
    // name (A = name index) resolves to a builtin or a path-indexed package
    // function, then falls through to the CALL_NAMED that performs the call.
    ENSURE_DOTTED_FUNCTION,
    // `keyboard` statement (A = span index): enters the interactive debug
    // prompt (K>>) unconditionally, returning on dbcont/dbstep, stopping on
    // dbquit.
    KEYBOARD,
    // Guard of `T.name(indices) = value` and `T.name{indices} = value`: falls
    // through when the value on top of the stack (left in place) is a table,
    // timetable or eventtable reached through its overloaded subsasgn, which
    // then receives both subscripts (ASGN_DOT_CHAIN_VALUE, without the value
    // of T.name: a name that is not a variable yet cannot be read), and jumps
    // to A (the per-level assignment) otherwise.
    JUMP_IF_NOT_TABLE,
    // `T.(expr)(indices) = value` and `T.(expr){indices} = value` on a target
    // accepted by JUMP_IF_NOT_TABLE: the field name or variable number sits
    // above the value, the B indices above it; INST_FLAG_INDEX_ONLY selects
    // braces. The subsasgn of T receives both subscripts.
    ASGN_DOTDYN_CHAIN_VALUE,
    // `T.(expr)(indices)` read on a value accepted by JUMP_IF_NOT_TABLE: the
    // field name or variable number sits above T, the B indices above it. A
    // table variable is indexed directly, otherwise the subsref of T receives
    // both subscripts (timetable row times).
    SUBIDX_DOTDYN_CHAIN,
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
//=============================================================================
static_assert(sizeof(Instruction) == 8, "Instruction must be 8 bytes");
//=============================================================================
// Opcodes safe to execute through the direct leaf-macro call fast path
// (canDirectLeafMacroBytecodeCall in BytecodeVM.cpp); compileInto caches the
// whole-chunk result in BytecodeChunk::directLeafOpsAllowed.
inline bool
isDirectLeafMacroOpcode(OpCode op)
{
    switch (op) {
    case OpCode::LOAD_CONST:
    case OpCode::LOAD_TRUE:
    case OpCode::LOAD_FALSE:
    case OpCode::LOAD_EMPTY:
    case OpCode::LOAD_EMPTY_CELL:
    case OpCode::LOAD_LOCAL:
    case OpCode::LOAD_PARENS_LOCAL:
    case OpCode::LOAD_PARENS_LOCAL_OFFSET:
    case OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL:
    case OpCode::STORE_LOCAL:
    case OpCode::POP:
    case OpCode::DUP:
    case OpCode::SWAP:
    case OpCode::SET_NAME:
    case OpCode::OP_PLUS:
    case OpCode::OP_MINUS:
    case OpCode::OP_MTIMES:
    case OpCode::OP_TIMES:
    case OpCode::OP_MRDIV:
    case OpCode::OP_RDIV:
    case OpCode::OP_MLDIV:
    case OpCode::OP_LDIV:
    case OpCode::OP_MPOWER:
    case OpCode::OP_POWER:
    case OpCode::OP_UPLUS:
    case OpCode::OP_UMINUS:
    case OpCode::OP_NOT:
    case OpCode::OP_TRANSPOSE:
    case OpCode::OP_DOT_TRANSPOSE:
    case OpCode::OP_LT:
    case OpCode::OP_LEQ:
    case OpCode::OP_GT:
    case OpCode::OP_GEQ:
    case OpCode::OP_EQ:
    case OpCode::OP_NEQ:
    case OpCode::OP_AND:
    case OpCode::OP_OR:
    case OpCode::ASSIGN:
    case OpCode::RETURN:
    case OpCode::DEBUG_STMT:
    case OpCode::CHECK_INTERRUPT:
    case OpCode::NOP:
    // The fused fast path only touches double-scalar locals; its fallback
    // window is made of opcodes already in this list.
    case OpCode::SCALAR_EXPR:
        return true;
    default:
        return false;
    }
}
//=============================================================================
constexpr uint8_t INST_FLAG_PRINT = 0x01;
constexpr uint8_t INST_FLAG_QUIET = 0x02;
constexpr uint8_t INST_FLAG_ECHO = 0x04;
constexpr uint8_t INST_FLAG_GLOBAL = 0x08;
constexpr uint8_t INST_FLAG_RANGE_HAS_STEP = 0x10;
constexpr uint8_t INST_FLAG_LIST = 0x20;
constexpr uint8_t INST_FLAG_INDEX_ONLY = 0x40;
constexpr uint8_t INST_FLAG_ALLOW_UNDEFINED = 0x80;
constexpr uint8_t INST_FLAG_VALUE_IS_STRING = 0x01;
// LOAD_GLOBAL_OR_JUMP only: C holds the dotted call target name index, tried
// as a function before the head is evaluated as a zero-argument function.
constexpr uint8_t INST_FLAG_PREFER_DOTTED = 0x40;
// SUPER_CALL only: set for the constructor form obj@Super(...), clear for the
// method form method@Super(...).
constexpr uint8_t INST_FLAG_SUPER_CONSTRUCTOR = 0x01;
// ASGN_DOT_CHAIN_VALUE only: the value of the field is not on the stack.
constexpr uint8_t INST_FLAG_NO_PARENT_VALUE = 0x01;
//=============================================================================
// JUMP_IF_NARGIN_CMP comparison ids (whole flags byte)
constexpr uint8_t NARGIN_CMP_LT = 0;
constexpr uint8_t NARGIN_CMP_LE = 1;
constexpr uint8_t NARGIN_CMP_GT = 2;
constexpr uint8_t NARGIN_CMP_GE = 3;
constexpr uint8_t NARGIN_CMP_EQ = 4;
constexpr uint8_t NARGIN_CMP_NE = 5;
//=============================================================================
// JUMP_IF_ARG_PRED flags encoding
constexpr uint8_t ARG_PRED_ISA = 0;
constexpr uint8_t ARG_PRED_ISSCALAR = 1;
constexpr uint8_t ARG_PRED_ID_MASK = 0x03;
constexpr uint8_t ARG_PRED_JUMP_ON_TRUE = 0x04;
constexpr uint8_t ARG_PRED_WINDOW_SHIFT = 3;
constexpr uint8_t ARG_PRED_WINDOW_MASK = 0x07;
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
