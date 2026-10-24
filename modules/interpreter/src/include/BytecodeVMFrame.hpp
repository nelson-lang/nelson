//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <exception>
#include <memory>
#include <unordered_map>
#include <vector>
#include "ClassdefHandleFinalization.hpp"
#include "Context.hpp"
#include "Evaluator.hpp"
#include "VMCallFrame.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
// Reuse a chunk's frame capacities and lookup caches between calls.
struct FrameBuffer
{
    uint64_t chunkId = 0;
    bool inUse = false;
    VMCallFrame frame;
};
//=============================================================================
inline FrameBuffer*
acquireFrameBuffer(const BytecodeChunk& chunk)
{
    static thread_local std::unordered_map<const BytecodeChunk*,
        std::vector<std::unique_ptr<FrameBuffer>>>
        buffersByChunk;
    std::vector<std::unique_ptr<FrameBuffer>>& buffers = buffersByChunk[&chunk];
    for (std::unique_ptr<FrameBuffer>& buffer : buffers) {
        if (!buffer->inUse) {
            if (buffer->chunkId != chunk.chunkId) {
                // Same address but a recompiled chunk: pooled caches are stale.
                buffer->chunkId = chunk.chunkId;
                buffer->frame.namedLocalSlotCache.clear();
                buffer->frame.namedScalarBuiltinCache.clear();
                buffer->frame.namedScalarBuiltinCacheState.clear();
                buffer->frame.namedFunctionCache.clear();
                buffer->frame.namedFunctionCacheState.clear();
                buffer->frame.scalarDoubleConstantCache.clear();
                buffer->frame.scalarDoubleConstantCacheState.clear();
            }
            buffer->inUse = true;
            return buffer.get();
        }
    }
    buffers.emplace_back(std::make_unique<FrameBuffer>());
    FrameBuffer* buffer = buffers.back().get();
    buffer->chunkId = chunk.chunkId;
    buffer->inUse = true;
    return buffer;
}
//=============================================================================
inline void
releaseFrameBuffer(FrameBuffer* buffer)
{
    VMCallFrame& frame = buffer->frame;
    frame.clearLocals();
    frame.assignedLocals.clear();
    frame.valueStack.clear();
    frame.loopStates.clear();
    frame.dblLoops.clear();
    frame.rangeLoops.clear();
    frame.exceptionFrames.clear();
    frame.endContexts.clear();
    buffer->inUse = false;
}
//=============================================================================
// Own the frame before initialization or argument binding, and keep ownership
// through output validation. Finalizers must not replace an unwinding error.
class FrameBufferGuard
{
public:
    FrameBufferGuard(Evaluator* evaluator, const BytecodeChunk& chunk)
        : eval(evaluator), buffer(acquireFrameBuffer(chunk))
    {
    }
    ~FrameBufferGuard()
    {
        if (buffer != nullptr) {
            try {
                release();
            } catch (...) {
            }
        }
    }
    FrameBufferGuard(const FrameBufferGuard&) = delete;
    FrameBufferGuard&
    operator=(const FrameBufferGuard&)
        = delete;

    VMCallFrame&
    frame()
    {
        return buffer->frame;
    }

    void
    restoreLazyScope()
    {
        if (buffer->frame.lazyScopePushed && eval->getContext() != nullptr) {
            buffer->frame.hasContextScope = buffer->frame.lazyScopePushed = false;
            eval->getContext()->popScope();
        }
    }

    void
    release()
    {
        std::exception_ptr scopeError;
        try {
            restoreLazyScope();
        } catch (...) {
            scopeError = std::current_exception();
        }
        releaseFrameBuffer(buffer);
        buffer = nullptr;
        try {
            FinalizePendingHandles(eval);
        } catch (...) {
            if (!scopeError) {
                throw;
            }
        }
        if (scopeError) {
            std::rethrow_exception(scopeError);
        }
    }

private:
    Evaluator* eval;
    FrameBuffer* buffer;
};
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
