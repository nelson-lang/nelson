//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Errors raised by the bytecode VM when a variable read finds no value or an
// index read fails, or a brace assignment targets a function handle. Cold:
// only reached on the error path.
//=============================================================================
#include <algorithm>
#include "BytecodeVMInternal.hpp"
#include "Context.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "NelsonException.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
static bool
isMissingInputArgument(Context* context, const BytecodeChunk& chunk, const std::string& name);
//=============================================================================
static bool
readTablePropertiesIndexTarget(Evaluator* eval, const BytecodeChunk& chunk,
    const VMCallFrame& frame, size_t failingPc, ArrayOf& table, std::string& property);
//=============================================================================
static bool
singleValueExpressionStart(const BytecodeChunk& chunk, size_t last, size_t& start);
//=============================================================================
static bool
readPropertiesHead(Evaluator* eval, const BytecodeChunk& chunk, const VMCallFrame& frame,
    size_t head, ArrayOf& table);
//=============================================================================
static bool
readTextConstant(const BytecodeChunk& chunk, uint16_t index, std::string& text);
//=============================================================================
static bool
indexExtent(const ArrayOf& index, double& extent);
//=============================================================================
void
raiseUndefinedVariable(Context* context, const BytecodeChunk& chunk, const std::string& name)
{
    if (isMissingInputArgument(context, chunk, name)) {
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    }
    Error("Nelson:UndefinedVariable", "Undefined variable: %s", name);
}
//=============================================================================
void
checkBracesAssignTarget(const ArrayOf& base)
{
    if (base.isFunctionHandle()) {
        Error(L"Nelson:cellAssToNonCell",
            L"Unable to perform assignment because brace indexing is not supported for "
            L"variables of this type.");
    }
}
//=============================================================================
void
raiseTablePropertiesIndexError(Evaluator* eval, const BytecodeChunk& chunk,
    const VMCallFrame& frame, size_t failingPc, const ArrayOfVector& indices)
{
    ArrayOf table;
    std::string property;
    if (readTablePropertiesIndexTarget(eval, chunk, frame, failingPc, table, property)) {
        raiseTablePropertiesIndexError(eval, table, property, indices);
    }
}
//=============================================================================
void
raiseTablePropertiesIndexError(Evaluator* eval, const ArrayOf& table, const std::string& property,
    const ArrayOfVector& indices)
{
    // Inside the methods of the class, T.Properties is the stored struct.
    double extent = 0;
    if (!table.isClassType() || !table.isScalar()
        || !(table.isTable() || table.isTimetable() || table.classType() == "eventtable")
        || !eval->withOverload || eval->isClassdefMethodContextOf(table.classType())
        || indices.size() != 1 || !indexExtent(indices[0], extent)) {
        return;
    }
    // the dimension <name> follows: 1 rows, 2 variables, 3 the dimensions
    int dimension = 0;
    if (property == "RowNames" || property == "RowTimes") {
        dimension = 1;
    } else if (property.rfind("Variable", 0) == 0) {
        dimension = 2;
    } else if (property == "DimensionNames") {
        dimension = 3;
    } else {
        return;
    }
    double bound = 2;
    if (dimension != 3) {
        ArrayOfVector sizes;
        try {
            if (!eval->bytecodeInvokeObjectMethodIfExists(table, "size", {}, 1, sizes)
                || sizes.empty()) {
                return;
            }
            std::vector<double> dims = sizes[0].asDoubleVector();
            if (dims.size() < 2) {
                return;
            }
            bound = dims[dimension - 1];
        } catch (const NelsonException&) {
            return;
        }
    }
    if (extent <= bound) {
        return;
    }
    if (dimension == 1) {
        Error("Nelson:table:rowIndexOutOfRange", "Row index exceeds table dimensions.");
    }
    if (dimension == 2) {
        Error("Nelson:table:varIndexOutOfRange", "Variable index exceeds table dimensions.");
    }
    Error("Nelson:table:dimIndexOutOfRange", "Dimension index exceeds number of table dimensions.");
}
//=============================================================================
// A formal input parameter of the running function that the caller did not
// pass (its position is beyond nargin); varargin is always defined.
static bool
isMissingInputArgument(Context* context, const BytecodeChunk& chunk, const std::string& name)
{
    if (context == nullptr || context->getCurrentScope() == nullptr || name == "varargin") {
        return false;
    }
    const size_t nargin = static_cast<size_t>(std::max(context->getCurrentScope()->getNargIn(), 0));
    for (size_t k = nargin; k < chunk.argNames.size(); ++k) {
        if (chunk.argNames[k] == name) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// The read at failingPc as compiled for T.Properties.<name>{index} and
// T.Properties.<name>(index): the read of T.Properties (LOAD_LOCAL_DOT, or a
// load of T and its SUBIDX_DOT), then either the SUBIDX_DOT of <name> and the
// index block (PUSH_END_CTX ... POP_END_CTX) read by failingPc, or the index
// block read with <name> by failingPc.
static bool
readTablePropertiesIndexTarget(Evaluator* eval, const BytecodeChunk& chunk,
    const VMCallFrame& frame, size_t failingPc, ArrayOf& table, std::string& property)
{
    if (failingPc < 3 || failingPc >= chunk.code.size()) {
        return false;
    }
    // pc becomes the first instruction of the index argument
    size_t pc = failingPc - 1;
    if (chunk.code[pc].op == OpCode::POP_END_CTX) {
        int depth = 0;
        do {
            OpCode op = chunk.code[pc].op;
            if (op == OpCode::POP_END_CTX) {
                ++depth;
            } else if (op == OpCode::PUSH_END_CTX || op == OpCode::PUSH_END_CTX_LOCAL) {
                --depth;
            }
        } while (depth > 0 && pc-- > 0);
        if (depth != 0) {
            return false;
        }
    } else if (!singleValueExpressionStart(chunk, failingPc - 1, pc)) {
        // an argument without `end` has no context around it
        return false;
    }
    const Instruction& failing = chunk.code[failingPc];
    const bool nameWithIndex = failing.op == OpCode::SUBIDX_DOT;
    if (pc < (nameWithIndex ? 1 : 2)) {
        return false;
    }
    const Instruction& dot = nameWithIndex ? failing : chunk.code[pc - 1];
    size_t head = nameWithIndex ? pc - 1 : pc - 2;
    return dot.op == OpCode::SUBIDX_DOT && dot.A == (nameWithIndex ? 1 : 0)
        && dot.C == (nameWithIndex ? 1 : 0) && readTextConstant(chunk, dot.B, property)
        && readPropertiesHead(eval, chunk, frame, head, table);
}
//=============================================================================
// First instruction of the one value expression ending at chunk.code[last], walking back
// over the loads and arithmetic a plain index argument compiles to; false for anything else.
static bool
singleValueExpressionStart(const BytecodeChunk& chunk, size_t last, size_t& start)
{
    int needed = 1;
    size_t pc = last + 1;
    while (pc > 0) {
        --pc;
        Instruction ins = chunk.code[pc];
        if (ins.op == OpCode::SCALAR_EXPR) {
            // a fused window keeps its other instructions in place
            if (ins.A >= chunk.scalarExprPrograms.size()) {
                return false;
            }
            ins = chunk.scalarExprPrograms[ins.A].original;
        }
        int produced = 1;
        int consumed = 0;
        switch (ins.op) {
        case OpCode::LOAD_CONST:
        case OpCode::LOAD_TRUE:
        case OpCode::LOAD_FALSE:
        case OpCode::LOAD_EMPTY:
        case OpCode::LOAD_LOCAL:
        case OpCode::LOAD_GLOBAL:
        case OpCode::LOAD_LOCAL_DOT:
        case OpCode::LOAD_PARENS_LOCAL:
        case OpCode::LOAD_PARENS_LOCAL_OFFSET:
        case OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL:
        case OpCode::LOAD_PARENS2_LOCAL:
            break;
        case OpCode::OP_PLUS:
        case OpCode::OP_MINUS:
        case OpCode::OP_TIMES:
        case OpCode::OP_MTIMES:
        case OpCode::OP_RDIV:
        case OpCode::OP_MRDIV:
        case OpCode::OP_COLON_UNIT:
            consumed = 2;
            break;
        case OpCode::OP_COLON:
            consumed = 3;
            break;
        case OpCode::OP_UMINUS:
        case OpCode::OP_UPLUS:
            consumed = 1;
            break;
        case OpCode::BUILD_ROW:
        case OpCode::BUILD_MATRIX:
            consumed = ins.A;
            break;
        default:
            return false;
        }
        needed += consumed - produced;
        if (needed == 0) {
            start = pc;
            return true;
        }
    }
    return false;
}
//=============================================================================
// The read of T.Properties ending at chunk.code[head], T being a variable.
static bool
readPropertiesHead(Evaluator* eval, const BytecodeChunk& chunk, const VMCallFrame& frame,
    size_t head, ArrayOf& table)
{
    std::string field;
    const Instruction& read = chunk.code[head];
    uint16_t slot = read.A;
    if (read.op == OpCode::SUBIDX_DOT && read.A == 0 && read.C == 0 && head > 0) {
        const Instruction& load = chunk.code[head - 1];
        if (load.op == OpCode::LOAD_GLOBAL && (load.flags & INST_FLAG_GLOBAL) == 0) {
            return readTextConstant(chunk, read.B, field) && field == "Properties"
                && load.A < chunk.names.size()
                && eval->getContext()->lookupVariable(chunk.names[load.A], table);
        }
        if (load.op != OpCode::LOAD_LOCAL) {
            return false;
        }
        slot = load.A;
    } else if (read.op != OpCode::LOAD_LOCAL_DOT) {
        return false;
    }
    if (!readTextConstant(chunk, read.B, field) || field != "Properties") {
        return false;
    }
    if (isRuntimeAssignedLocalSlot(frame, slot)) {
        table = frame.locals[slot];
        return true;
    }
    return slot < chunk.localNames.size()
        && eval->getContext()->lookupVariable(chunk.localNames[slot], table);
}
//=============================================================================
static bool
readTextConstant(const BytecodeChunk& chunk, uint16_t index, std::string& text)
{
    if (index >= chunk.constants.size()) {
        return false;
    }
    const auto* value = std::get_if<std::string>(&chunk.constants.get(index));
    if (value == nullptr) {
        return false;
    }
    text = *value;
    return true;
}
//=============================================================================
// The largest position an index reaches: its length for a logical mask.
static bool
indexExtent(const ArrayOf& index, double& extent)
{
    if (index.isLogical()) {
        extent = static_cast<double>(index.elementCount());
        return true;
    }
    if (!index.isNumeric() || index.isComplex() || index.isSparse() || index.isEmpty()) {
        return false;
    }
    std::vector<double> values = index.asDoubleVector();
    extent = *std::max_element(values.begin(), values.end());
    return true;
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
