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
#include <vector>
#include "BytecodeScalarFusion.hpp"
#include "ScalarBuiltinFastPath.hpp"
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
    // An indexed store always pays off: the typed write skips the generic
    // subset-assignment opcode even when the value is a plain local or constant.
    if (kind == ScalarExprResultKind::IndexedStore
        || kind == ScalarExprResultKind::IndexedStoreComputed
        || kind == ScalarExprResultKind::IndexedStore2DComputed) {
        return true;
    }
    // a scalar builtin call skips the argument naming, the call dispatch and the boxing,
    // a power the generic operator
    if (std::any_of(ops.begin(), ops.end(), [](const ScalarExprMicroOp& op) {
            return op.kind == ScalarExprOpKind::CallBuiltin1
                || op.kind == ScalarExprOpKind::CallBuiltin2 || op.kind == ScalarExprOpKind::Pow;
        })) {
        return true;
    }
    if (arithmetic > 0 && loads >= 2 && length >= 4) {
        return true;
    }
    // s = s + v(k) or s + m(i, j): an indexed read makes a three-instruction window worth it
    const bool readsIndexed = std::any_of(ops.begin(), ops.end(), [](const ScalarExprMicroOp& op) {
        return op.kind == ScalarExprOpKind::LoadIndexedLocal
            || op.kind == ScalarExprOpKind::LoadIndexed2DLocal;
    });
    // s = s + p.f or s + c{k} too
    const bool readsElement
        = readsIndexed || std::any_of(ops.begin(), ops.end(), [](const ScalarExprMicroOp& op) {
              return op.kind == ScalarExprOpKind::LoadLocalField
                  || op.kind == ScalarExprOpKind::LoadCellContent;
          });
    if (arithmetic > 0 && loads >= 2 && length == 3 && readsElement) {
        return true;
    }
    // x = v(k) or x = m(i, j): the typed read skips the boxed element and its stack trip
    if (kind == ScalarExprResultKind::Store && length == 2 && ops.size() == 1 && readsIndexed) {
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
                || first.op == OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL
                || first.op == OpCode::LOAD_PARENS2_LOCAL || first.op == OpCode::PUSH_END_CTX_LOCAL
                || first.op == OpCode::LOAD_LOCAL_DOT || first.op == OpCode::LOAD_BRACES_LOCAL);
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
        // longest prefix leaving exactly one value, used when the whole window fails
        bool hasPrefix = false;
        size_t prefixEnd = 0;
        size_t prefixOps = 0;
        size_t prefixArith = 0;
        size_t prefixLoads = 0;
        uint16_t storeSlot = 0;
        uint16_t indexSlot = 0;
        // local whose `end` context is open inside the window, -1 when none
        int endContextSlot = -1;
        uint16_t endPosition = 0;
        uint16_t endCount = 0;
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
            } else if (ins.op == OpCode::LOAD_LOCAL_DOT || ins.op == OpCode::LOAD_BRACES_LOCAL) {
                // p.f and c{k} in value context (a list context sets a flag)
                ScalarExprMicroOp op;
                op.kind = ins.op == OpCode::LOAD_LOCAL_DOT ? ScalarExprOpKind::LoadLocalField
                                                           : ScalarExprOpKind::LoadCellContent;
                op.slot = ins.A;
                op.slot2 = ins.B;
                ops.push_back(op);
                ++depth;
                ++loadCount;
            } else if (ins.op == OpCode::LOAD_PARENS2_LOCAL) {
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::LoadIndexed2DLocal;
                op.slot = ins.A;
                op.slot2 = ins.B;
                op.slot3 = ins.C;
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
                || ins.op == OpCode::OP_RDIV || ins.op == OpCode::OP_MRDIV
                || ins.op == OpCode::OP_POWER || ins.op == OpCode::OP_MPOWER) {
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
                case OpCode::OP_POWER:
                case OpCode::OP_MPOWER:
                    op.kind = ScalarExprOpKind::Pow;
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
            } else if (ins.op == OpCode::SET_NAME) {
                // names a call argument for inputname: nothing to do on registers
                if (depth < 1) {
                    break;
                }
            } else if (ins.op == OpCode::CALL_NAMED) {
                // abs(x), max(x, y), ...: one output, one or two double inputs; the VM
                // checks at run time that the name still resolves to the builtin
                if (ins.C != 1 || (ins.B != 1 && ins.B != 2) || depth < ins.B
                    || ins.A >= chunk.names.size()) {
                    break;
                }
                const ScalarBuiltinFastPath fastPath
                    = scalarBuiltinFastPathForName(chunk.names[ins.A], ins.B);
                if (!isScalarBuiltinFastPathCandidate(fastPath)) {
                    break;
                }
                ScalarExprMicroOp op;
                op.kind
                    = ins.B == 1 ? ScalarExprOpKind::CallBuiltin1 : ScalarExprOpKind::CallBuiltin2;
                op.slot = ins.A;
                op.slot2 = static_cast<uint16_t>(fastPath.kind);
                ops.push_back(op);
                depth -= static_cast<size_t>(ins.B - 1);
                ++arithCount;
            } else if (ins.op == OpCode::PUSH_END_CTX_LOCAL) {
                // `end` of a local subscript (position B of C) becomes that extent; one
                // context at a time, closed by its POP_END_CTX inside the window.
                if (endContextSlot >= 0 || ins.C == 0 || ins.B >= ins.C) {
                    break;
                }
                endContextSlot = ins.A;
                endPosition = ins.B;
                endCount = ins.C;
            } else if (ins.op == OpCode::LOAD_END) {
                if (endContextSlot < 0) {
                    break;
                }
                ScalarExprMicroOp op;
                op.kind = ScalarExprOpKind::LoadEndLocal;
                op.slot = static_cast<uint16_t>(endContextSlot);
                op.slot2 = endPosition;
                op.slot3 = endCount;
                ops.push_back(op);
                ++depth;
                ++loadCount;
            } else if (ins.op == OpCode::POP_END_CTX) {
                if (endContextSlot < 0) {
                    break;
                }
                endContextSlot = -1;
            } else if (ins.op == OpCode::ASGN_PARENS) {
                // the value then one or two subscripts computed on the stack
                if (ins.B == 1 && depth == 2) {
                    resultKind = ScalarExprResultKind::IndexedStoreComputed;
                } else if (ins.B == 2 && depth == 3) {
                    resultKind = ScalarExprResultKind::IndexedStore2DComputed;
                } else {
                    break;
                }
                storeSlot = ins.A;
                matched = true;
                break;
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
            // not after SET_NAME: the name it gives the value must still be applied
            if (depth == 1 && arithCount > 0 && loadCount >= 2 && endContextSlot < 0
                && ins.op != OpCode::SET_NAME) {
                hasPrefix = true;
                prefixEnd = q;
                prefixOps = ops.size();
                prefixArith = arithCount;
                prefixLoads = loadCount;
            }
        }
        if (endContextSlot >= 0) {
            // an end context left open would be skipped with the window while its
            // POP_END_CTX still runs
            aborted = true;
        }
        if (!matched && hasPrefix) {
            // e.g. a*b + c inside f(a*b + c, d) or [a*b + c, d]: the value is pushed and
            // the rest of the original window runs as usual
            resultKind = ScalarExprResultKind::Push;
            matched = true;
            aborted = false;
            q = prefixEnd + 1;
            ops.resize(prefixOps);
            arithCount = prefixArith;
            loadCount = prefixLoads;
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
        program.callsBuiltins = scalarProgramCallsBuiltins(program);
        const auto programIndex = static_cast<uint16_t>(chunk.scalarExprPrograms.size());
        chunk.scalarExprPrograms.push_back(std::move(program));
        Instruction fused {};
        fused.op = OpCode::SCALAR_EXPR;
        fused.flags = 0;
        fused.A = programIndex;
        fused.B = 0;
        fused.C = 0;
        chunk.code[pc] = fused;
        // a pushed value is consumed by the instruction at q, which may start a window
        pc = resultKind == ScalarExprResultKind::Push ? q : q + 1;
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
