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
#include <vector>
#include "ArrayOf.hpp"
#include "Bytecode.hpp"
#include "BytecodeChunk.hpp"
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct ExceptionFrame
{
    uint32_t savedPC = 0;
    uint32_t catchPC = 0;
};
//=============================================================================
struct EndContext
{
    ArrayOf base;
    indexType index = 0;
    size_t count = 0;
};
//=============================================================================
// Whole-chain indexing accumulation. Activated by CHAIN_BEGIN only when the
// chain head is a subsref-defining or Redefines{Paren,Brace,Dot} classdef
// object; the per-level indexing opcodes then record (subtype, subsindex)
// instead of applying, and CHAIN_END dispatches the whole chain once so the
// hook / subsref sees the full operation list (numel>1). Inactive for every
// other value (one predicted-false check).
struct ChainCollectState
{
    bool active = false;
    ArrayOf base;
    stringVector subtypes;
    ArrayOfVector subsindices;
};
//=============================================================================
struct NLSINTERPRETER_IMPEXP VMCallFrame
{
    const BytecodeChunk* chunk = nullptr;
    uint32_t startPC = 0;
    std::vector<ArrayOf> locals;
    std::vector<uint8_t> assignedLocals;
    std::vector<ArrayOf> valueStack;
    std::vector<GenericLoopState> loopStates;
    std::vector<DoubleLoopState> dblLoops;
    std::vector<RangeLoopState> rangeLoops;
    std::vector<ExceptionFrame> exceptionFrames;
    std::vector<EndContext> endContexts;
    ChainCollectState chainCollect;
    uint64 profTic = 0;
    std::vector<ArrayOf> captured;
    int nLhs = 0;
    // Number of inputs the caller actually passed; -1 when unknown (script
    // frames). Lets fused nargin checks run without a context scope.
    int nArgIn = -1;
    bool skipDebugStatements = false;
    // False when the caller skipped context->pushScope (scope-free fast call).
    // If a runtime-resolved callee then needs the caller context, runChunk
    // pushes a scope on demand and records it in lazyScopePushed so
    // executeFunction pops it on every exit path.
    bool hasContextScope = true;
    bool lazyScopePushed = false;
    // Set when this frame's assigned locals have been snapshot into the context
    // scope for a caller-context-touching call (evalin/assignin/inputname and,
    // conservatively, any evaluator builtin). The snapshot co-owns each local,
    // so an in-place indexed assignment to one would otherwise COW-copy the
    // whole array. In-place store opcodes drop the stale snapshot first, which
    // keeps such assignments O(1) instead of O(n) per call. Only set on frames
    // that actually synced, so scope-free and never-syncing frames pay nothing.
    bool localsSyncedToContext = false;
    // Per-chunk lookup caches, pooled with the frame buffer so they survive
    // across calls of the same chunk instead of being reallocated per call.
    std::vector<int> namedLocalSlotCache;
    std::vector<FunctionDef*> namedScalarBuiltinCache;
    std::vector<uint8_t> namedScalarBuiltinCacheState;
    std::vector<FunctionDef*> namedFunctionCache;
    std::vector<uint8_t> namedFunctionCacheState;
    std::vector<double> scalarDoubleConstantCache;
    std::vector<uint8_t> scalarDoubleConstantCacheState;
    uint64 namedBuiltinCacheGeneration = 0;

    void
    init(const BytecodeChunk& ch, int requestedLhs);
    void
    clearLocals();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
