//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include <utility>
#include <vector>
#include "ArrayOf.hpp"
#include "ArrayOfVector.hpp"
#include "ErrorID.hpp"
#include "BytecodeVMInternal.hpp"
#include "Context.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
inline ArrayOf
popStackValue(std::vector<ArrayOf>& stack)
{
    if (stack.empty()) {
        Error(L"Nelson:bytecode:stackUnderflow", L"BytecodeVM: stack underflow.");
    }
    ArrayOf value = stack.back();
    stack.pop_back();
    return value;
}
//=============================================================================
inline ArrayOfVector
popStackValues(std::vector<ArrayOf>& stack, uint16_t count)
{
    if (count > stack.size()) {
        Error(L"Nelson:bytecode:stackUnderflow", L"BytecodeVM: stack underflow.");
    }
    ArrayOfVector values(count);
    const size_t first = stack.size() - count;
    // Append existing values in argument order without constructing empty slots.
    for (size_t index = first; index < stack.size(); ++index) {
        values.push_back(std::move(stack[index]));
    }
    stack.resize(first);
    return values;
}
//=============================================================================
inline bool
trySkipNumericSelfAssignment(VMCallFrame& frame, const BytecodeChunk& chunk, Context* context,
    const ScalarExprProgram& program)
{
    if (program.resultKind != ScalarExprResultKind::Store || program.windowLength != 2
        || program.original.op != OpCode::LOAD_LOCAL || program.original.flags != 0
        || program.original.A != program.storeSlot || program.ops.size() != 1
        || program.ops[0].kind != ScalarExprOpKind::LoadLocal
        || program.ops[0].slot != program.storeSlot) {
        return false;
    }
    const size_t slot = program.storeSlot;
    if (slot >= frame.locals.size() || !isRuntimeAssignedLocalSlot(frame, slot)) {
        return false;
    }
    const auto& value = frame.locals[slot];
    if (!value.isNumeric() && value.dataClass() != NLS_LOGICAL) {
        return false;
    }
    if (slot < chunk.localNames.size() && !chunk.localNames[slot].empty()
        && context->isLockedVariable(chunk.localNames[slot])) {
        return false;
    }
    // An assigned numeric value needs no stack or ownership change for x = x.
    return true;
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
