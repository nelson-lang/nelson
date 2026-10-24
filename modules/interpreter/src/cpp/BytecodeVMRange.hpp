//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include <array>
#include <cmath>
#include "BytecodeVMInternal.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
void
markRuntimeAssigned(VMCallFrame& frame, size_t slot);
void
setLocalDouble(std::vector<ArrayOf>& locals, uint16_t slot, double value);
void
pollInterrupt(Evaluator* eval);
bool
advanceRangeLoop(VMCallFrame& frame, const BytecodeChunk& chunk, Context* context,
    const Instruction& instruction);
enum class ScalarRangeKind : uint8_t
{
    Boundary,
    Constant,
    Move,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate
};
//=============================================================================
struct ScalarRangeOperation
{
    ScalarRangeKind kind = ScalarRangeKind::Move;
    uint8_t target = 0;
    uint8_t left = 0;
    uint8_t right = 0;
    uint32_t pc = 0;
    double value = 0.;
};
//=============================================================================
struct ScalarRangeLocal
{
    uint16_t slot = 0;
    uint8_t reg = 0;
    bool input = false;
    bool written = false;
};
//=============================================================================
struct ScalarRangeProgram
{
    bool enabled = false;
    uint32_t begin = 0;
    uint32_t stepPC = 0;
    uint16_t loopSlot = 0;
    uint8_t induction = 0;
    uint8_t registerCount = 0;
    std::vector<ScalarRangeOperation> operations;
    std::vector<ScalarRangeLocal> locals;
};
//=============================================================================
// Per-execution plans: bytecode chunks and serialized artifacts stay unchanged.
struct ScalarRangeLoopCache
{
    std::vector<ScalarRangeProgram> programs;
};
//=============================================================================
inline bool
worthPlanningScalarRange(const RangeLoopState& state)
{
    // Avoid plan and callback allocations for short ranges.
    return std::isfinite(state.current) && std::isfinite(state.stop) && std::isfinite(state.step)
        && state.step != 0. && (state.stop - state.current) / state.step >= 31.;
}
//=============================================================================
bool
quietScalarRangeBoundary(Evaluator* eval, uint32_t& boundaryCounter);
void
beginScalarRangeLoop(ScalarRangeLoopCache& cache, Evaluator* eval, VMCallFrame& frame,
    const BytecodeChunk& chunk, Context* context, const Instruction& instruction, uint32_t& pc,
    uint32_t& backedgeCounter, const std::function<bool(const Instruction&)>& boundary);
void
continueScalarRangeLoop(ScalarRangeProgram& program, Evaluator* eval, VMCallFrame& frame,
    const BytecodeChunk& chunk, Context* context, uint32_t& pc, uint32_t& backedgeCounter,
    const std::function<bool(const Instruction&)>& boundary);
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
