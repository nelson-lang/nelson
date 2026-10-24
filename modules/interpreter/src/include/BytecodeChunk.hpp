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
#include <atomic>
#include <string>
#include <vector>
#include "Bytecode.hpp"
#include "ConstantPool.hpp"
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class FunctionDef;
//=============================================================================
enum class ScalarExprOpKind : uint8_t
{
    LoadLocal,
    LoadConst,
    // locals[slot](locals[slot2]) on a real double array with a scalar
    // in-bounds positive integer index; anything else refuses the window.
    LoadIndexedLocal,
    // locals[slot](locals[slot2] + imm), same restrictions as LoadIndexedLocal.
    LoadIndexedLocalOffset,
    // locals[slot](locals[slot2] - locals[slot3]), same restrictions.
    LoadIndexedLocalMinusLocal,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    // Comparisons only ever appear as the final operation of a window whose
    // terminator is a conditional branch, so their result is consumed as a
    // branch decision and never materialised as a value (which would have to
    // be logical, not double).
    CmpLt,
    CmpLe,
    CmpGt,
    CmpGe,
    CmpEq,
    CmpNe
};
//=============================================================================
enum class ScalarExprResultKind : uint8_t
{
    Store,
    Branch,
    Push,
    IndexedStore
};
//=============================================================================
struct ScalarExprMicroOp
{
    ScalarExprOpKind kind = ScalarExprOpKind::LoadConst;
    uint16_t slot = 0;
    // LoadIndexedLocal only: slot holding the index value.
    uint16_t slot2 = 0;
    // LoadIndexedLocalMinusLocal only: slot holding the offset value.
    uint16_t slot3 = 0;
    double imm = 0.;
};
//=============================================================================
struct ScalarExprProgram
{
    std::vector<ScalarExprMicroOp> ops;
    // Original first instruction of the fused window: executed verbatim when
    // the typed fast path bails out, then the untouched remaining window
    // instructions run.
    Instruction original {};
    // Number of instructions in the window including the first one; on fast
    // path success the VM skips the whole window.
    uint32_t windowLength = 0;
    uint16_t storeSlot = 0;
    // resultKind == IndexedStore: local slot holding the linear index.
    uint16_t indexSlot = 0;
    ScalarExprResultKind resultKind = ScalarExprResultKind::Store;
    // resultKind == Branch: the window ends on a conditional jump instead of a
    // store. Both are captured at fuse time; code never moves afterwards, so
    // the target stays valid.
    OpCode branchOp = OpCode::NOP;
    uint16_t branchTarget = 0;
};
//=============================================================================
class NLSINTERPRETER_IMPEXP BytecodeChunk
{
public:
    // Unique identity: a recompiled chunk may reuse the same address, so
    // pooled per-chunk caches must key on (pointer, chunkId).
    static inline std::atomic<uint64_t> nextChunkId { 1 };
    uint64_t chunkId = nextChunkId.fetch_add(1, std::memory_order_relaxed);

    std::wstring sourcePath;
    std::string functionName;
    std::string classdefAccessClassName;
    FunctionDef* selfFunction = nullptr;
    // True when selfFunction is a nested MacroFunctionDef: avoids a
    // dynamic_cast on every executeFunction exit.
    bool selfIsNestedMacro = false;
    bool isScript = false;
    bool mayCreateNamedFunctionHandle = false;
    // Set by emit() when a DECLARE_IMPORT is emitted: lets per-call paths skip
    // rescanning the whole chunk (applyImportDeclarations, direct-call checks).
    bool hasDeclareImport = false;
    // Computed once at the end of compileInto; conservative defaults keep any
    // chunk that bypassed compileInto off the direct-call fast paths.
    bool usesNarginNargout = true;
    bool directLeafOpsAllowed = false;
    // memoized "no scope-dependent opcodes" scan for anonymous-function calls;
    // recomputed lazily after each (re)compile since the chunk is replaced
    bool anonymousScopeFree = false;
    bool anonymousScopeFreeComputed = false;

    std::vector<Instruction> code;
    ConstantPool constants;
    std::vector<std::string> names;
    std::vector<ScalarExprProgram> scalarExprPrograms;

    uint16_t nLocals = 0;
    uint16_t nLoopSlots = 0;
    uint16_t maxStack = 0;

    stringVector argNames;
    stringVector retNames;
    stringVector capturedNames;
    stringVector localNames;
    std::vector<uint8_t> localAssigned;
    std::vector<uint16_t> retSlots;

    std::vector<SourceSpan> spans;

    // Cached transitive caller-context requirements; valid only while the
    // matching generation equals FunctionsInMemory::generation() (function
    // tables unchanged since the walk resolved this chunk's callees).
    bool callerContextSyncCacheValid = false;
    bool callerContextSyncRequired = true;
    uint64_t callerContextSyncCacheGeneration = 0;
    bool callerContextRefreshCacheValid = false;
    bool callerContextRefreshRequired = true;
    uint64_t callerContextRefreshCacheGeneration = 0;
    // Body of an anonymous function: its caller (evaluateFunction) reads the
    // captured variables back from the scope after the frame ends, so the
    // frame must leave that scope intact.
    bool isAnonymous = false;

    uint32_t
    emit(OpCode op, uint8_t flags = 0, uint16_t A = 0, uint16_t B = 0, uint16_t C = 0);

    void
    patchJump(uint32_t instrIdx, uint16_t target);

    void
    recordSpan(uint32_t instrIdx, uint32_t line, uint16_t col = 0, uint16_t endLine = 0);

    const SourceSpan*
    spanForPC(uint32_t pc) const;

    std::string
    disassemble() const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
