//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "BytecodeScalarFusion.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
tryReadScalarDoubleConstant(const BytecodeChunk& chunk, uint16_t index, double& value);
static bool
scalarExprComparisonKind(OpCode op, ScalarExprOpKind& kind);
static bool
isScalarExprBranchTerminator(OpCode op);
static bool
isUsefulScalarWindow(const std::vector<ScalarExprMicroOp>& ops, ScalarExprResultKind kind,
    uint16_t slot, size_t arithmetic, size_t loads, size_t length)
{
    if (arithmetic > 0 && loads >= 2 && length >= 4) {
        return true;
    }
    return kind == ScalarExprResultKind::Store && length == 2 && ops.size() == 1
        && ops[0].kind == ScalarExprOpKind::LoadLocal && ops[0].slot == slot;
}
//=============================================================================
void
fuseScalarExpressions(BytecodeChunk& chunk)
{
    constexpr size_t maxStackDepth = 15;
    constexpr size_t maxProgramOps = 48;
    const size_t codeSize = chunk.code.size();
    size_t pc = 0;
    while (pc < codeSize) {
        const Instruction& first = chunk.code[pc];
        // Only these can start a window: on a refusal the VM replays the
        // window's first instruction itself, and it knows how to replay
        // exactly these (see the SCALAR_EXPR fallback in BytecodeVM.cpp).
        const bool startsWindow = first.flags == 0
            && (first.op == OpCode::LOAD_LOCAL || first.op == OpCode::LOAD_CONST
                || first.op == OpCode::LOAD_PARENS_LOCAL
                || first.op == OpCode::LOAD_PARENS_LOCAL_OFFSET
                || first.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL);
        if (!startsWindow) {
            ++pc;
            continue;
        }
        std::vector<ScalarExprMicroOp> ops;
        size_t depth = 0;
        size_t maxDepthSeen = 0;
        size_t loadCount = 0;
        size_t arithCount = 0;
        bool matched = false;
        bool aborted = false;
        uint16_t storeSlot = 0;
        uint16_t indexSlot = 0;
        ScalarExprResultKind resultKind = ScalarExprResultKind::Store;
        OpCode branchOp = OpCode::NOP;
        uint16_t branchTarget = 0;
        size_t q = pc;
        for (; q < codeSize; ++q) {
            const Instruction& ins = chunk.code[q];
            if (ins.flags != 0) {
                break;
            }
            if (ins.op == OpCode::LOAD_LOCAL && depth == 1 && q + 1 < codeSize) {
                const Instruction& assign = chunk.code[q + 1];
                if (assign.flags == 0 && assign.op == OpCode::ASGN_PARENS && assign.B == 1) {
                    storeSlot = assign.A;
                    indexSlot = ins.A;
                    resultKind = ScalarExprResultKind::IndexedStore;
                    matched = true;
                    ++q;
                    break;
                }
            }
            if (ins.op == OpCode::LOAD_LOCAL) {
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::LoadLocal;
                op.slot = ins.A;
                ops.push_back(op);
                ++depth;
                ++loadCount;
            } else if (ins.op == OpCode::LOAD_PARENS_LOCAL) {
                // Single-subscript read of a local: fused only when the
                // instruction asks for one value (C > 1 is a comma list).
                if (ins.C > 1) {
                    break;
                }
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::LoadIndexedLocal;
                op.slot = ins.A;
                op.slot2 = ins.B;
                ops.push_back(op);
                ++depth;
                ++loadCount;
            } else if (ins.op == OpCode::LOAD_PARENS_LOCAL_OFFSET) {
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::LoadIndexedLocalOffset;
                op.slot = ins.A;
                op.slot2 = ins.B;
                if (!tryReadScalarDoubleConstant(chunk, ins.C, op.imm)) {
                    break;
                }
                ops.push_back(op);
                ++depth;
                ++loadCount;
            } else if (ins.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL) {
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::LoadIndexedLocalMinusLocal;
                op.slot = ins.A;
                op.slot2 = ins.B;
                op.slot3 = ins.C;
                ops.push_back(op);
                ++depth;
                ++loadCount;
            } else if (ins.op == OpCode::LOAD_CONST) {
                double value = 0.;
                if (!tryReadScalarDoubleConstant(chunk, ins.A, value)) {
                    break;
                }
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::LoadConst;
                op.imm = value;
                ops.push_back(op);
                ++depth;
                ++loadCount;
            } else if (ins.op == OpCode::OP_PLUS || ins.op == OpCode::OP_MINUS
                || ins.op == OpCode::OP_TIMES || ins.op == OpCode::OP_MTIMES
                || ins.op == OpCode::OP_RDIV || ins.op == OpCode::OP_MRDIV) {
                if (depth < 2) {
                    break;
                }
                ScalarExprMicroOp op;
                switch (ins.op) {
                case OpCode::OP_PLUS:
                    op.kind = ScalarExprOpKind::Add;
                    break;
                case OpCode::OP_MINUS:
                    op.kind = ScalarExprOpKind::Sub;
                    break;
                case OpCode::OP_TIMES:
                case OpCode::OP_MTIMES:
                    op.kind = ScalarExprOpKind::Mul;
                    break;
                default:
                    op.kind = ScalarExprOpKind::Div;
                    break;
                }
                ops.push_back(op);
                --depth;
                ++arithCount;
            } else if (ScalarExprOpKind comparisonKind;
                scalarExprComparisonKind(ins.op, comparisonKind)) {
                // A comparison is only fusable as the last value-producing
                // op of a window ending on a conditional branch: its result
                // is a branch decision, never a value (a materialised
                // comparison would have to be logical, not double).
                if (depth < 2 || q + 1 >= codeSize) {
                    break;
                }
                const Instruction& branch = chunk.code[q + 1];
                if (branch.flags != 0 || !isScalarExprBranchTerminator(branch.op)) {
                    break;
                }
                ScalarExprMicroOp op;
                op.kind = comparisonKind;
                ops.push_back(op);
                --depth;
                ++arithCount;
                if (depth != 1) {
                    break;
                }
                resultKind = ScalarExprResultKind::Branch;
                branchOp = branch.op;
                branchTarget = branch.A;
                matched = true;
                ++q;
                break;
            } else if (ins.op == OpCode::OP_UMINUS) {
                if (depth < 1) {
                    break;
                }
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::Neg;
                ops.push_back(op);
                ++arithCount;
            } else if (ins.op == OpCode::OP_UPLUS) {
                if (depth < 1) {
                    break;
                }
            } else if (ins.op == OpCode::STORE_LOCAL || ins.op == OpCode::ASSIGN) {
                if (depth != 1) {
                    break;
                }
                storeSlot = ins.A;
                matched = true;
                break;
            } else if (ins.op == OpCode::ASSIGN_PLUS_STACK) {
                // locals[A] = <second on stack> + <top of stack>
                if (depth != 2) {
                    break;
                }
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::Add;
                ops.push_back(op);
                ++arithCount;
                storeSlot = ins.A;
                matched = true;
                break;
            } else {
                break;
            }
            maxDepthSeen = std::max(maxDepthSeen, depth);
            if (maxDepthSeen > maxStackDepth || ops.size() > maxProgramOps) {
                aborted = true;
                break;
            }
        }
        if (!matched && !aborted && depth == 1 && arithCount > 0 && loadCount >= 2) {
            resultKind = ScalarExprResultKind::Push;
            matched = true;
        }
        const size_t windowLength
            = matched ? (resultKind == ScalarExprResultKind::Push ? q - pc : q - pc + 1) : 0;
        if (!matched || aborted
            || !isUsefulScalarWindow(
                ops, resultKind, storeSlot, arithCount, loadCount, windowLength)
            || chunk.scalarExprPrograms.size() >= 0xFFFF) {
            ++pc;
            continue;
        }
        ScalarExprProgram program;
        program.ops = std::move(ops);
        program.original = chunk.code[pc];
        program.windowLength = static_cast<uint32_t>(windowLength);
        program.storeSlot = storeSlot;
        program.indexSlot = indexSlot;
        program.resultKind = resultKind;
        program.branchOp = branchOp;
        program.branchTarget = branchTarget;
        const auto programIndex = static_cast<uint16_t>(chunk.scalarExprPrograms.size());
        chunk.scalarExprPrograms.push_back(std::move(program));
        Instruction fused {};
        fused.op = OpCode::SCALAR_EXPR;
        fused.flags = 0;
        fused.A = programIndex;
        fused.B = 0;
        fused.C = 0;
        chunk.code[pc] = fused;
        pc = q + 1;
    }
}
//=============================================================================
static bool
tryReadScalarDoubleConstant(const BytecodeChunk& chunk, uint16_t index, double& value)
{
    if (index >= chunk.constants.size()) {
        return false;
    }
    const ConstantValue& constant = chunk.constants.get(index);
    const ArrayOf* array = std::get_if<ArrayOf>(&constant);
    if (array == nullptr || !array->isDoubleType(true) || array->elementCount() != 1) {
        return false;
    }
    value = *static_cast<const double*>(array->dataPointer());
    return true;
}
//=============================================================================
static bool
scalarExprComparisonKind(OpCode op, ScalarExprOpKind& kind)
{
    switch (op) {
    case OpCode::OP_LT:
        kind = ScalarExprOpKind::CmpLt;
        return true;
    case OpCode::OP_LEQ:
        kind = ScalarExprOpKind::CmpLe;
        return true;
    case OpCode::OP_GT:
        kind = ScalarExprOpKind::CmpGt;
        return true;
    case OpCode::OP_GEQ:
        kind = ScalarExprOpKind::CmpGe;
        return true;
    case OpCode::OP_EQ:
        kind = ScalarExprOpKind::CmpEq;
        return true;
    case OpCode::OP_NEQ:
        kind = ScalarExprOpKind::CmpNe;
        return true;
    default:
        return false;
    }
}
//=============================================================================
// Conditional jumps that consume the value on top of the stack. Kept in
// sync with applyImmediateBranch in BytecodeVM.cpp.
static bool
isScalarExprBranchTerminator(OpCode op)
{
    switch (op) {
    case OpCode::WHILE_CHECK:
    case OpCode::JUMP_IF_FALSE:
    case OpCode::JUMP_IF_FALSE_SCALAR:
    case OpCode::JUMP_IF_TRUE:
    case OpCode::JUMP_IF_TRUE_SCALAR:
        return true;
    default:
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
