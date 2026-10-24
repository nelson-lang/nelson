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
    void
    writeProgram(BinaryWriter& writer, const ScalarExprProgram& program)
    {
        writer.writePod(program.original);
        writer.writePod(program.windowLength);
        writer.writePod(program.storeSlot);
        writer.writePod(program.indexSlot);
        writer.writePod(program.resultKind);
        writer.writePod(program.branchOp);
        writer.writePod(program.branchTarget);
        writer.writeU64(program.ops.size());
        for (const auto& op : program.ops) {
            writer.writePod(op.kind);
            writer.writePod(op.slot);
            writer.writePod(op.slot2);
            writer.writePod(op.slot3);
            writer.writePod(op.imm);
        }
    }
    //=============================================================================
    ScalarExprProgram
    readProgram(Reader& reader)
    {
        ScalarExprProgram program;
        program.original = read<Instruction>(reader);
        program.windowLength = read<uint32_t>(reader);
        program.storeSlot = read<uint16_t>(reader);
        program.indexSlot = read<uint16_t>(reader);
        program.resultKind = read<ScalarExprResultKind>(reader);
        program.branchOp = read<OpCode>(reader);
        program.branchTarget = read<uint16_t>(reader);
        const auto count = read<uint64_t>(reader);
        require(count <= 48);
        reader.charge(static_cast<size_t>(count) * sizeof(ScalarExprMicroOp));
        program.ops.reserve(static_cast<size_t>(count));
        for (uint64_t i = 0; i < count; ++i) {
            ScalarExprMicroOp op;
            op.kind = read<ScalarExprOpKind>(reader);
            op.slot = read<uint16_t>(reader);
            op.slot2 = read<uint16_t>(reader);
            op.slot3 = read<uint16_t>(reader);
            op.imm = read<double>(reader);
            program.ops.push_back(op);
        }
        return program;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
writeChunk(BinaryWriter& writer, const BytecodeChunk& chunk)
{
    writer.writeString(chunk.functionName);
    writer.writeString(chunk.classdefAccessClassName);
    writer.writeU8(chunk.isScript);
    writer.writeU8(chunk.isAnonymous);
    writer.writePod(chunk.nLocals);
    writer.writePod(chunk.nLoopSlots);
    writer.writePod(chunk.maxStack);
    writer.writeStringVector(chunk.names);
    writer.writeStringVector(chunk.argNames);
    writer.writeStringVector(chunk.retNames);
    writer.writeStringVector(chunk.capturedNames);
    writer.writeStringVector(chunk.localNames);
    writer.writeVector(chunk.localAssigned);
    writer.writeVector(chunk.retSlots);
    writer.writeVector(chunk.code);
    writer.writeVector(chunk.spans);
    writeConstants(writer, chunk.constants);
    writer.writeU64(chunk.scalarExprPrograms.size());
    for (const auto& program : chunk.scalarExprPrograms) {
        writeProgram(writer, program);
    }
}
//=============================================================================
std::unique_ptr<BytecodeChunk>
readChunk(Reader& reader)
{
    auto chunk = std::make_unique<BytecodeChunk>();
    chunk->functionName = readString(reader);
    chunk->classdefAccessClassName = readString(reader);
    chunk->isScript = readBool(reader);
    chunk->isAnonymous = readBool(reader);
    chunk->nLocals = read<uint16_t>(reader);
    chunk->nLoopSlots = read<uint16_t>(reader);
    chunk->maxStack = read<uint16_t>(reader);
    chunk->names = readStrings(reader);
    chunk->argNames = readStrings(reader);
    chunk->retNames = readStrings(reader);
    chunk->capturedNames = readStrings(reader);
    chunk->localNames = readStrings(reader);
    chunk->localAssigned = readVector<uint8_t>(reader);
    chunk->retSlots = readVector<uint16_t>(reader);
    chunk->code = readVector<Instruction>(reader, 1024 * 1024);
    chunk->spans = readVector<SourceSpan>(reader);
    chunk->constants = readConstants(reader);
    const auto count = read<uint64_t>(reader);
    require(count <= chunk->code.size());
    reader.charge(static_cast<size_t>(count) * sizeof(ScalarExprProgram));
    for (uint64_t i = 0; i < count; ++i) {
        chunk->scalarExprPrograms.push_back(readProgram(reader));
    }
    validateChunk(*chunk);
    chunk->directLeafOpsAllowed = std::all_of(chunk->code.begin(), chunk->code.end(),
        [](const Instruction& ins) { return isDirectLeafMacroOpcode(ins.op); });
    const auto usesName = [&](const char* name) {
        return std::find(chunk->names.begin(), chunk->names.end(), name) != chunk->names.end()
            || std::find(chunk->localNames.begin(), chunk->localNames.end(), name)
            != chunk->localNames.end();
    };
    chunk->usesNarginNargout = usesName("nargin") || usesName("nargout");
    for (const auto& ins : chunk->code) {
        chunk->hasDeclareImport |= ins.op == OpCode::DECLARE_IMPORT;
        chunk->mayCreateNamedFunctionHandle |= ins.op == OpCode::MAKE_FH_NAMED;
    }
    return chunk;
}
//=============================================================================
} // namespace Nelson::BytecodeIO
//=============================================================================
