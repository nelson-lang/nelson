//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "BytecodeVMRange.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include "Context.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "MainThreadTaskQueue.hpp"
#include "NelsonConfiguration.hpp"
#include "Profiler.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson::bcvm_detail {
void
pollInterrupt(Evaluator* eval)
{
    uint64 id = eval->getID();
    NelsonConfiguration* config = NelsonConfiguration::getInstance();
    if (!config->getInterruptPending(id)) {
        return;
    }
    if (id == 0) {
        config->setInterruptPending(false, id);
        CallbackQueue::getInstance()->clear();
        EventQueue::getInstance()->clear();
        eval->setState(NLS_STATE_ABORT);
        Error(L"Nelson:interpreter:interrupted", L"Interrupt (ctrl-c) encountered.");
    }
    Error(L"parallel:fevalqueue:ExecutionCancelled", L"Execution of the future was cancelled.");
}
//=============================================================================
namespace {
    constexpr size_t maxRegisters = 64;
    struct Operand
    {
        uint8_t reg;
        bool temporary;
    };
    //=============================================================================
    int
    allocateRegister(ScalarRangeProgram& program)
    {
        return program.registerCount < maxRegisters ? program.registerCount++ : -1;
    }
    //=============================================================================
    int
    localRegister(ScalarRangeProgram& program, uint16_t slot, bool input, bool written)
    {
        auto found = std::find_if(program.locals.begin(), program.locals.end(),
            [slot](const ScalarRangeLocal& value) { return value.slot == slot; });
        if (found == program.locals.end()) {
            const int reg = allocateRegister(program);
            if (reg < 0) {
                return -1;
            }
            program.locals.push_back({ slot, static_cast<uint8_t>(reg), input, written });
            return reg;
        }
        found->input |= input && !found->written;
        found->written |= written;
        return found->reg;
    }
    //=============================================================================
    int
    appendConstant(ScalarRangeProgram& program, const BytecodeChunk& chunk, uint16_t index)
    {
        if (index >= chunk.constants.size()) {
            return -1;
        }
        const auto* value = std::get_if<ArrayOf>(&chunk.constants.get(index));
        if (!value || !value->isDoubleType(true) || value->elementCount() != 1
            || value->dataPointer() == nullptr) {
            return -1;
        }
        const int reg = allocateRegister(program);
        if (reg >= 0) {
            ScalarRangeOperation operation;
            operation.kind = ScalarRangeKind::Constant;
            operation.target = static_cast<uint8_t>(reg);
            operation.value = *static_cast<const double*>(value->dataPointer());
            program.operations.push_back(operation);
        }
        return reg;
    }
    //=============================================================================
    bool
    appendArithmetic(ScalarRangeProgram& program, std::vector<Operand>& stack, OpCode code)
    {
        ScalarRangeKind kind;
        switch (code) {
        case OpCode::OP_PLUS:
            kind = ScalarRangeKind::Add;
            break;
        case OpCode::OP_MINUS:
            kind = ScalarRangeKind::Subtract;
            break;
        case OpCode::OP_TIMES:
        case OpCode::OP_MTIMES:
            kind = ScalarRangeKind::Multiply;
            break;
        case OpCode::OP_RDIV:
        case OpCode::OP_MRDIV:
            kind = ScalarRangeKind::Divide;
            break;
        case OpCode::OP_UMINUS:
            kind = ScalarRangeKind::Negate;
            break;
        case OpCode::OP_UPLUS:
            return !stack.empty();
        default:
            return false;
        }
        const size_t count = kind == ScalarRangeKind::Negate ? 1 : 2;
        if (stack.size() < count) {
            return false;
        }
        const int target = allocateRegister(program);
        if (target < 0) {
            return false;
        }
        ScalarRangeOperation operation;
        operation.kind = kind;
        operation.target = static_cast<uint8_t>(target);
        operation.right = stack.back().reg;
        if (count == 2) {
            stack.pop_back();
        }
        operation.left = stack.back().reg;
        stack.back() = { operation.target, true };
        program.operations.push_back(operation);
        return true;
    }
    //=============================================================================
    bool
    appendInstruction(ScalarRangeProgram& program, const BytecodeChunk& chunk,
        Instruction instruction, uint32_t pc, std::vector<Operand>& stack)
    {
        if (instruction.op == OpCode::SCALAR_EXPR) {
            if (instruction.A >= chunk.scalarExprPrograms.size()) {
                return false;
            }
            // Fusion retains the rest of the original window at its original addresses.
            instruction = chunk.scalarExprPrograms[instruction.A].original;
        }
        if (instruction.flags != 0 || stack.size() > 15) {
            return false;
        }
        switch (instruction.op) {
        case OpCode::NOP:
            return true;
        case OpCode::DEBUG_STMT: {
            if (!stack.empty()) {
                return false;
            }
            ScalarRangeOperation operation;
            operation.kind = ScalarRangeKind::Boundary;
            operation.pc = pc;
            program.operations.push_back(operation);
            return true;
        }
        case OpCode::LOAD_LOCAL: {
            const int reg = localRegister(program, instruction.A, true, false);
            if (reg < 0) {
                return false;
            }
            stack.push_back({ static_cast<uint8_t>(reg), false });
            return true;
        }
        case OpCode::LOAD_CONST: {
            const int reg = appendConstant(program, chunk, instruction.A);
            if (reg < 0) {
                return false;
            }
            stack.push_back({ static_cast<uint8_t>(reg), true });
            return true;
        }
        case OpCode::ASSIGN_PLUS_LOCAL:
        case OpCode::ASSIGN_PLUS_CONST: {
            if (!stack.empty()) {
                return false;
            }
            const int left = localRegister(program, instruction.A, true, false);
            const int right = instruction.op == OpCode::ASSIGN_PLUS_LOCAL
                ? localRegister(program, instruction.B, true, false)
                : appendConstant(program, chunk, instruction.B);
            if (left < 0 || right < 0) {
                return false;
            }
            localRegister(program, instruction.A, false, true);
            ScalarRangeOperation operation;
            operation.kind = ScalarRangeKind::Add;
            operation.target = static_cast<uint8_t>(left);
            operation.left = static_cast<uint8_t>(left);
            operation.right = static_cast<uint8_t>(right);
            program.operations.push_back(operation);
            return true;
        }
        case OpCode::STORE_LOCAL:
        case OpCode::ASSIGN: {
            if (stack.size() != 1) {
                return false;
            }
            const int target = localRegister(program, instruction.A, false, true);
            if (target < 0) {
                return false;
            }
            const Operand source = stack.back();
            stack.pop_back();
            if (source.temporary && !program.operations.empty()
                && program.operations.back().target == source.reg) {
                // A temporary has one consumer; local registers can have aliases in later loads.
                program.operations.back().target = static_cast<uint8_t>(target);
            } else {
                ScalarRangeOperation operation;
                operation.kind = ScalarRangeKind::Move;
                operation.target = static_cast<uint8_t>(target);
                operation.left = source.reg;
                program.operations.push_back(operation);
            }
            return true;
        }
        default:
            return appendArithmetic(program, stack, instruction.op);
        }
    }
    //=============================================================================
    bool
    buildProgram(ScalarRangeProgram& program, const BytecodeChunk& chunk, const Instruction& init,
        uint32_t begin)
    {
        if ((init.flags & INST_FLAG_GLOBAL) != 0 || init.C <= begin || init.C > chunk.code.size()
            || init.C - begin > 128) {
            return false;
        }
        const uint32_t stepPC = init.C - 1;
        const Instruction& step = chunk.code[stepPC];
        if (step.op != OpCode::FOR_STEP_RANGE || step.flags != 0 || step.A != init.A
            || step.B != init.B || step.C != begin) {
            return false;
        }
        program.begin = begin;
        program.stepPC = stepPC;
        program.loopSlot = init.A;
        const int induction = localRegister(program, init.B, true, false);
        if (induction < 0) {
            return false;
        }
        program.induction = static_cast<uint8_t>(induction);
        std::vector<Operand> stack;
        for (uint32_t pc = begin; pc < stepPC; ++pc) {
            if (!appendInstruction(program, chunk, chunk.code[pc], pc, stack)) {
                return false;
            }
        }
        return stack.empty()
            && std::any_of(program.operations.begin(), program.operations.end(),
                [](const ScalarRangeOperation& operation) {
                    return operation.kind == ScalarRangeKind::Boundary;
                });
    }
    //=============================================================================
    bool
    canRun(Evaluator* eval)
    {
        return !Profiler::getInstance()->isOn() && !eval->isBreakpointActive() && !eval->stepMode
            && !eval->hasEnabledBreakpoints() && !PendingHandleFinalizers::hasPending()
            && !NelsonConfiguration::getInstance()->hasOpenFigureWindows();
    }
    //=============================================================================
    bool
    loadRegisters(const ScalarRangeProgram& program, VMCallFrame& frame, const BytecodeChunk& chunk,
        Context* context, std::array<double, maxRegisters>& values)
    {
        for (const auto& local : program.locals) {
            if (local.slot >= frame.locals.size()) {
                return false;
            }
            const bool assigned = isRuntimeAssignedLocalSlot(frame, local.slot);
            if (local.input && !assigned) {
                return false;
            }
            if (assigned) {
                const ArrayOf& value = frame.locals[local.slot];
                if (!value.isDoubleType(true) || value.elementCount() != 1 || !value.name().empty()
                    || !value.fieldNames().empty() || value.dataPointer() == nullptr) {
                    return false;
                }
                values[local.reg] = *static_cast<const double*>(value.dataPointer());
            }
            if (local.written && local.slot < chunk.localNames.size()
                && context->isLockedVariable(chunk.localNames[local.slot])) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    void
    publishRegisters(const ScalarRangeProgram& program, VMCallFrame& frame,
        const std::array<double, maxRegisters>& values,
        const std::array<bool, maxRegisters>& changed)
    {
        // Do not assign destinations whose first store has not run yet.
        for (const auto& local : program.locals) {
            if (changed[local.reg]) {
                frame.locals[local.slot] = ArrayOf::doubleScalar(values[local.reg]);
                markRuntimeAssigned(frame, local.slot);
            }
        }
    }
    //=============================================================================
    bool
    executeOperations(const ScalarRangeProgram& program, const BytecodeChunk& chunk,
        std::array<double, maxRegisters>& values, std::array<bool, maxRegisters>& changed,
        uint32_t& pc, const std::function<bool(const Instruction&)>& boundary)
    {
        for (const auto& operation : program.operations) {
            double& target = values[operation.target];
            switch (operation.kind) {
            case ScalarRangeKind::Boundary:
                pc = operation.pc;
                if (!boundary(chunk.code[pc])) {
                    return false;
                }
                continue;
            case ScalarRangeKind::Constant:
                target = operation.value;
                break;
            case ScalarRangeKind::Move:
                target = values[operation.left];
                break;
            case ScalarRangeKind::Add:
                target = values[operation.left] + values[operation.right];
                break;
            case ScalarRangeKind::Subtract:
                target = values[operation.left] - values[operation.right];
                break;
            case ScalarRangeKind::Multiply:
                target = values[operation.left] * values[operation.right];
                break;
            case ScalarRangeKind::Divide:
                target = values[operation.left] / values[operation.right];
                break;
            case ScalarRangeKind::Negate:
                target = -values[operation.left];
                break;
            }
            changed[operation.target] = true;
        }
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
quietScalarRangeBoundary(Evaluator* eval, uint32_t& counter)
{
    // Leave the original DEBUG_STMT to service queues and GUI events at the same cadence.
    if (((counter + 1) & 63U) == 0 || NelsonConfiguration::getInstance()->hasOpenFigureWindows()
        || !eval->commandQueue.isEmpty() || !MainThreadTaskQueue::getInstance()->isEmpty()
        || PendingHandleFinalizers::hasPending()) {
        return false;
    }
    ++counter;
    return true;
}
//=============================================================================
void
beginScalarRangeLoop(ScalarRangeLoopCache& cache, Evaluator* eval, VMCallFrame& frame,
    const BytecodeChunk& chunk, Context* context, const Instruction& instruction, uint32_t& pc,
    uint32_t& backedgeCounter, const std::function<bool(const Instruction&)>& boundary)
{
    if (!canRun(eval)) {
        return;
    }
    if (cache.programs.size() <= instruction.A) {
        cache.programs.resize(instruction.A + 1);
    }
    ScalarRangeProgram& program = cache.programs[instruction.A];
    program = ScalarRangeProgram();
    if (!buildProgram(program, chunk, instruction, pc)) {
        return;
    }
    program.enabled = true;
    continueScalarRangeLoop(program, eval, frame, chunk, context, pc, backedgeCounter, boundary);
}
//=============================================================================
void
continueScalarRangeLoop(ScalarRangeProgram& program, Evaluator* eval, VMCallFrame& frame,
    const BytecodeChunk& chunk, Context* context, uint32_t& pc, uint32_t& backedgeCounter,
    const std::function<bool(const Instruction&)>& boundary)
{
    std::array<double, maxRegisters> values {};
    std::array<bool, maxRegisters> changed {};
    RangeLoopState& state = frame.rangeLoops[program.loopSlot];
    if (pc != program.begin || !canRun(eval) || !std::isfinite(state.current)
        || !std::isfinite(state.stop) || !std::isfinite(state.step) || !frame.valueStack.empty()
        || !loadRegisters(program, frame, chunk, context, values)) {
        program.enabled = false;
        return;
    }
    try {
        for (;;) {
            if (!executeOperations(program, chunk, values, changed, pc, boundary)) {
                break;
            }
            pc = program.stepPC + 1;
            state.current += state.step;
            const bool more
                = state.step > 0 ? state.current <= state.stop : state.current >= state.stop;
            if (!more) {
                break;
            }
            values[program.induction] = state.current;
            changed[program.induction] = true;
            if ((++backedgeCounter & 31U) == 0) {
                pollInterrupt(eval);
            }
        }
    } catch (...) {
        // Interrupts must expose the same completed assignments as ordinary execution.
        publishRegisters(program, frame, values, changed);
        throw;
    }
    publishRegisters(program, frame, values, changed);
}
//=============================================================================
bool
advanceRangeLoop(VMCallFrame& frame, const BytecodeChunk& chunk, Context* context,
    const Instruction& instruction)
{
    const bool isGlobal = (instruction.flags & INST_FLAG_GLOBAL) != 0;
    if (instruction.A >= frame.rangeLoops.size()
        || (!isGlobal && instruction.B >= frame.locals.size())
        || (isGlobal && instruction.B >= chunk.names.size())) {
        Error(L"Nelson:bytecode:rangeForLoopOperandOutOfRange",
            L"BytecodeVM: range for-loop operand out of range.");
    }
    RangeLoopState& state = frame.rangeLoops[instruction.A];
    state.current += state.step;
    const bool more = state.step > 0.0 ? state.current <= state.stop : state.current >= state.stop;
    if (more) {
        if (isGlobal) {
            context->insertVariable(
                chunk.names[instruction.B], ArrayOf::doubleScalar(state.current));
        } else {
            setLocalDouble(frame.locals, instruction.B, state.current);
            markRuntimeAssigned(frame, instruction.B);
        }
    }
    return more;
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
