//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <sstream>
#include <stdexcept>
#include "BytecodeChunk.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
const char*
opCodeName(OpCode op)
{
    switch (op) {
    case OpCode::LOAD_CONST:
        return "LOAD_CONST";
    case OpCode::LOAD_TRUE:
        return "LOAD_TRUE";
    case OpCode::LOAD_FALSE:
        return "LOAD_FALSE";
    case OpCode::LOAD_EMPTY:
        return "LOAD_EMPTY";
    case OpCode::LOAD_EMPTY_CELL:
        return "LOAD_EMPTY_CELL";
    case OpCode::LOAD_LOCAL:
        return "LOAD_LOCAL";
    case OpCode::LOAD_LOCAL_COUNT_RANGE:
        return "LOAD_LOCAL_COUNT_RANGE";
    case OpCode::STORE_LOCAL:
        return "STORE_LOCAL";
    case OpCode::LOAD_GLOBAL:
        return "LOAD_GLOBAL";
    case OpCode::LOAD_GLOBAL_COUNT_RANGE:
        return "LOAD_GLOBAL_COUNT_RANGE";
    case OpCode::STORE_GLOBAL:
        return "STORE_GLOBAL";
    case OpCode::LOAD_ANS:
        return "LOAD_ANS";
    case OpCode::STORE_ANS:
        return "STORE_ANS";
    case OpCode::DELETE_LOCAL:
        return "DELETE_LOCAL";
    case OpCode::CLEAR_LOCAL:
        return "CLEAR_LOCAL";
    case OpCode::POP:
        return "POP";
    case OpCode::DUP:
        return "DUP";
    case OpCode::SWAP:
        return "SWAP";
    case OpCode::SET_NAME:
        return "SET_NAME";
    case OpCode::OP_PLUS:
        return "OP_PLUS";
    case OpCode::OP_MINUS:
        return "OP_MINUS";
    case OpCode::OP_MTIMES:
        return "OP_MTIMES";
    case OpCode::OP_TIMES:
        return "OP_TIMES";
    case OpCode::OP_MRDIV:
        return "OP_MRDIV";
    case OpCode::OP_RDIV:
        return "OP_RDIV";
    case OpCode::OP_MLDIV:
        return "OP_MLDIV";
    case OpCode::OP_LDIV:
        return "OP_LDIV";
    case OpCode::OP_MPOWER:
        return "OP_MPOWER";
    case OpCode::OP_POWER:
        return "OP_POWER";
    case OpCode::OP_COLON_UNIT:
        return "OP_COLON_UNIT";
    case OpCode::OP_COLON:
        return "OP_COLON";
    case OpCode::OP_UPLUS:
        return "OP_UPLUS";
    case OpCode::OP_UMINUS:
        return "OP_UMINUS";
    case OpCode::OP_NOT:
        return "OP_NOT";
    case OpCode::OP_TRANSPOSE:
        return "OP_TRANSPOSE";
    case OpCode::OP_DOT_TRANSPOSE:
        return "OP_DOT_TRANSPOSE";
    case OpCode::OP_LT:
        return "OP_LT";
    case OpCode::OP_LEQ:
        return "OP_LEQ";
    case OpCode::OP_GT:
        return "OP_GT";
    case OpCode::OP_GEQ:
        return "OP_GEQ";
    case OpCode::OP_EQ:
        return "OP_EQ";
    case OpCode::OP_NEQ:
        return "OP_NEQ";
    case OpCode::OP_AND:
        return "OP_AND";
    case OpCode::OP_OR:
        return "OP_OR";
    case OpCode::OP_SAND:
        return "OP_SAND";
    case OpCode::OP_SOR:
        return "OP_SOR";
    case OpCode::SUBIDX_PARENS:
        return "SUBIDX_PARENS";
    case OpCode::SUBIDX_BRACES:
        return "SUBIDX_BRACES";
    case OpCode::SUBIDX_DOT:
        return "SUBIDX_DOT";
    case OpCode::SUBIDX_DOTDYN:
        return "SUBIDX_DOTDYN";
    case OpCode::ASGN_PARENS:
        return "ASGN_PARENS";
    case OpCode::ASGN_BRACES:
        return "ASGN_BRACES";
    case OpCode::ASGN_DOT:
        return "ASGN_DOT";
    case OpCode::ASGN_DOTDYN:
        return "ASGN_DOTDYN";
    case OpCode::ASGN_PARENS_VALUE:
        return "ASGN_PARENS_VALUE";
    case OpCode::ASGN_BRACES_VALUE:
        return "ASGN_BRACES_VALUE";
    case OpCode::ASGN_BRACES_MULTI_VALUE:
        return "ASGN_BRACES_MULTI_VALUE";
    case OpCode::ASGN_DOT_VALUE:
        return "ASGN_DOT_VALUE";
    case OpCode::ASGN_DOTDYN_VALUE:
        return "ASGN_DOTDYN_VALUE";
    case OpCode::ASSIGN:
        return "ASSIGN";
    case OpCode::ASSIGN_PRINT:
        return "ASSIGN_PRINT";
    case OpCode::JUMP:
        return "JUMP";
    case OpCode::JUMP_IF_FALSE:
        return "JUMP_IF_FALSE";
    case OpCode::JUMP_IF_TRUE:
        return "JUMP_IF_TRUE";
    case OpCode::JUMP_IF_FALSE_SCALAR:
        return "JUMP_IF_FALSE_SCALAR";
    case OpCode::JUMP_IF_TRUE_SCALAR:
        return "JUMP_IF_TRUE_SCALAR";
    case OpCode::CHECK_SCALAR_AND:
        return "CHECK_SCALAR_AND";
    case OpCode::CHECK_SCALAR_OR:
        return "CHECK_SCALAR_OR";
    case OpCode::CASE_MATCH:
        return "CASE_MATCH";
    case OpCode::FOR_INIT:
        return "FOR_INIT";
    case OpCode::FOR_STEP:
        return "FOR_STEP";
    case OpCode::FOR_INIT_DBL:
        return "FOR_INIT_DBL";
    case OpCode::FOR_STEP_DBL:
        return "FOR_STEP_DBL";
    case OpCode::FOR_INIT_RANGE:
        return "FOR_INIT_RANGE";
    case OpCode::FOR_STEP_RANGE:
        return "FOR_STEP_RANGE";
    case OpCode::WHILE_CHECK:
        return "WHILE_CHECK";
    case OpCode::BREAK:
        return "BREAK";
    case OpCode::CONTINUE:
        return "CONTINUE";
    case OpCode::CALL_BUILTIN:
        return "CALL_BUILTIN";
    case OpCode::CALL_MACRO:
        return "CALL_MACRO";
    case OpCode::CALL_CLASSDEF_MEMBER:
        return "CALL_CLASSDEF_MEMBER";
    case OpCode::CALL_NAMED:
        return "CALL_NAMED";
    case OpCode::CALL_NAMED_DYNAMIC:
        return "CALL_NAMED_DYNAMIC";
    case OpCode::CALL_HANDLE:
        return "CALL_HANDLE";
    case OpCode::CALL_HANDLE_DYNAMIC:
        return "CALL_HANDLE_DYNAMIC";
    case OpCode::MULTI_ASSIGN:
        return "MULTI_ASSIGN";
    case OpCode::BUILD_MATRIX:
        return "BUILD_MATRIX";
    case OpCode::BUILD_CELL:
        return "BUILD_CELL";
    case OpCode::BUILD_CELL_FIELD_LIST:
        return "BUILD_CELL_FIELD_LIST";
    case OpCode::BUILD_ROW:
        return "BUILD_ROW";
    case OpCode::PUSH_END_CTX:
        return "PUSH_END_CTX";
    case OpCode::POP_END_CTX:
        return "POP_END_CTX";
    case OpCode::LOAD_END:
        return "LOAD_END";
    case OpCode::DISPLAY:
        return "DISPLAY";
    case OpCode::DISPLAY_STACK:
        return "DISPLAY_STACK";
    case OpCode::DISPLAY_ANS:
        return "DISPLAY_ANS";
    case OpCode::MAKE_FH_NAMED:
        return "MAKE_FH_NAMED";
    case OpCode::MAKE_FH_ANONYMOUS:
        return "MAKE_FH_ANONYMOUS";
    case OpCode::DECLARE_GLOBAL:
        return "DECLARE_GLOBAL";
    case OpCode::DECLARE_PERSISTENT:
        return "DECLARE_PERSISTENT";
    case OpCode::RETURN:
        return "RETURN";
    case OpCode::ABORT:
        return "ABORT";
    case OpCode::QUIT:
        return "QUIT";
    case OpCode::TRY_BEGIN:
        return "TRY_BEGIN";
    case OpCode::TRY_END:
        return "TRY_END";
    case OpCode::CATCH_BEGIN:
        return "CATCH_BEGIN";
    case OpCode::DEBUG_STMT:
        return "DEBUG_STMT";
    case OpCode::PROF_BEGIN:
        return "PROF_BEGIN";
    case OpCode::PROF_END:
        return "PROF_END";
    case OpCode::CHECK_INTERRUPT:
        return "CHECK_INTERRUPT";
    case OpCode::NOP:
        return "NOP";
    case OpCode::OPCODE_COUNT:
        return "OPCODE_COUNT";
    }
    return "UNKNOWN";
}
//=============================================================================
uint32_t
BytecodeChunk::emit(OpCode op, uint8_t flags, uint16_t A, uint16_t B, uint16_t C)
{
    if (op == OpCode::MAKE_FH_NAMED) {
        mayCreateNamedFunctionHandle = true;
    }
    code.push_back(Instruction { op, flags, A, B, C });
    return static_cast<uint32_t>(code.size() - 1);
}
//=============================================================================
void
BytecodeChunk::patchJump(uint32_t instrIdx, uint16_t target)
{
    if (instrIdx >= code.size()) {
        throw std::out_of_range("BytecodeChunk::patchJump instruction index out of range");
    }
    code[instrIdx].A = target;
}
//=============================================================================
void
BytecodeChunk::recordSpan(uint32_t instrIdx, uint32_t line, uint16_t col, uint16_t endLine)
{
    spans.push_back(SourceSpan { instrIdx, line == 0 ? 1U : line, col, endLine });
}
//=============================================================================
const SourceSpan*
BytecodeChunk::spanForPC(uint32_t pc) const
{
    if (spans.empty()) {
        return nullptr;
    }
    auto it = std::upper_bound(spans.begin(), spans.end(), pc,
        [](uint32_t value, const SourceSpan& span) { return value < span.instrIndex; });
    if (it == spans.begin()) {
        return &spans.front();
    }
    --it;
    return &*it;
}
//=============================================================================
std::string
BytecodeChunk::disassemble() const
{
    std::ostringstream stream;
    stream << "BytecodeChunk " << functionName << " instructions=" << code.size()
           << " locals=" << nLocals << " loops=" << nLoopSlots << " maxStack=" << maxStack << "\n";
    for (size_t k = 0; k < code.size(); ++k) {
        const Instruction& ins = code[k];
        stream << k << " " << opCodeName(ins.op) << " flags=" << static_cast<int>(ins.flags)
               << " A=" << ins.A << " B=" << ins.B << " C=" << ins.C;
        if (const SourceSpan* span = spanForPC(static_cast<uint32_t>(k))) {
            stream << " line=" << span->line << " col=" << span->col;
        }
        stream << "\n";
    }
    return stream.str();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
