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
#include <algorithm>
//=============================================================================
namespace Nelson::BytecodeIO {
//=============================================================================
namespace {
    bool
    isBranch(OpCode op)
    {
        return op == OpCode::WHILE_CHECK || op == OpCode::JUMP_IF_FALSE
            || op == OpCode::JUMP_IF_TRUE || op == OpCode::JUMP_IF_FALSE_SCALAR
            || op == OpCode::JUMP_IF_TRUE_SCALAR;
    }
    //=============================================================================
    void
    validateScalarProgram(const BytecodeChunk& chunk, size_t pc, const ScalarExprProgram& program)
    {
        require(program.original.flags == 0
            && (program.original.op == OpCode::LOAD_LOCAL
                || program.original.op == OpCode::LOAD_CONST
                || program.original.op == OpCode::LOAD_PARENS_LOCAL
                || program.original.op == OpCode::LOAD_PARENS_LOCAL_OFFSET
                || program.original.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL));
        validateInstruction(chunk, pc, program.original);
        require(program.windowLength >= 2 && program.windowLength <= chunk.code.size() - pc);
        require(!program.ops.empty() && program.ops.size() <= 48);
        require(program.resultKind == ScalarExprResultKind::Store
            || program.resultKind == ScalarExprResultKind::Branch
            || program.resultKind == ScalarExprResultKind::Push
            || program.resultKind == ScalarExprResultKind::IndexedStore);
        if (program.resultKind == ScalarExprResultKind::Store) {
            require(program.storeSlot < chunk.nLocals);
        } else if (program.resultKind == ScalarExprResultKind::IndexedStore) {
            require(program.storeSlot < chunk.nLocals && program.indexSlot < chunk.nLocals);
        } else if (program.resultKind == ScalarExprResultKind::Branch) {
            require(isBranch(program.branchOp) && program.branchTarget <= chunk.code.size());
        }
        size_t depth = 0;
        for (size_t i = 0; i < program.ops.size(); ++i) {
            const auto& op = program.ops[i];
            require(op.kind <= ScalarExprOpKind::CmpNe);
            switch (op.kind) {
            case ScalarExprOpKind::LoadLocal:
                require(op.slot < chunk.nLocals);
                ++depth;
                break;
            case ScalarExprOpKind::LoadIndexedLocal:
                require(op.slot < chunk.nLocals && op.slot2 < chunk.nLocals);
                ++depth;
                break;
            case ScalarExprOpKind::LoadIndexedLocalOffset:
                require(op.slot < chunk.nLocals && op.slot2 < chunk.nLocals);
                ++depth;
                break;
            case ScalarExprOpKind::LoadIndexedLocalMinusLocal:
                require(op.slot < chunk.nLocals && op.slot2 < chunk.nLocals
                    && op.slot3 < chunk.nLocals);
                ++depth;
                break;
            case ScalarExprOpKind::LoadConst:
                ++depth;
                break;
            case ScalarExprOpKind::Neg:
                require(depth >= 1);
                break;
            default:
                require(depth >= 2);
                --depth;
                if (op.kind >= ScalarExprOpKind::CmpLt) {
                    require(i + 1 == program.ops.size()
                        && program.resultKind == ScalarExprResultKind::Branch);
                }
                break;
            }
            require(depth <= 15, "Bytecode scalar stack limit exceeded.");
        }
        require(depth == 1);
    }
    //=============================================================================
} // namespace
//=============================================================================
void
validateChunk(const BytecodeChunk& chunk)
{
    require(chunk.names.size() <= 65536 && chunk.constants.size() <= 65536);
    require(
        chunk.localNames.size() == chunk.nLocals && chunk.localAssigned.size() == chunk.nLocals);
    require(chunk.argNames.size() <= chunk.nLocals);
    require(std::all_of(chunk.localAssigned.begin(), chunk.localAssigned.end(),
        [](uint8_t value) { return value <= 1; }));
    for (auto slot : chunk.retSlots) {
        require(slot < chunk.nLocals);
    }
    require(chunk.retSlots.size() <= chunk.retNames.size());
    uint32_t previousPC = 0;
    for (const auto& span : chunk.spans) {
        require(span.instrIndex < chunk.code.size() && span.instrIndex >= previousPC);
        previousPC = span.instrIndex;
    }
    for (size_t pc = 0; pc < chunk.code.size(); ++pc) {
        const auto& instruction = chunk.code[pc];
        validateInstruction(chunk, pc, instruction);
        if (instruction.op == OpCode::SCALAR_EXPR) {
            require(instruction.A < chunk.scalarExprPrograms.size());
            validateScalarProgram(chunk, pc, chunk.scalarExprPrograms[instruction.A]);
        }
    }
}
//=============================================================================
} // namespace Nelson::BytecodeIO
//=============================================================================
