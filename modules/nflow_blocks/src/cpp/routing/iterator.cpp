//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Helper blocks for the For / While Iterator subsystems (see
// docs/iterator_subsystems.md). The iterative execution itself lives in the
// subsystem handler (ctrlKind 7 = forIterator, 8 = whileIterator); these two
// blocks are the body-side interface to it:
//
//   iteratorNumber    - a source that outputs the current 1-based iteration
//                       index, read from SimCtx::iterationIndex, which the
//                       subsystem handler sets on the inner context before each
//                       pass over the body.
//   iteratorCondition - a scalar pass-through whose input is the While loop's
//                       continue predicate. The subsystem handler reads the
//                       block's output after each iteration to decide whether to
//                       loop again; the pass-through lets the same signal also
//                       drive a scope/probe. Inert inside a For iterator.
//
// Native only: iterator subsystems have no code-generation lowering.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "routing_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleIteratorNumber(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::OUTPUT) {
            return false;
        }
        // 0 outside an iterator body (the default context), 1..N inside one.
        setOutput(ctx, b.nid, (double)ctx.iterationIndex);
        return false;
    }
    //=============================================================================
    bool
    handleIteratorCondition(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        // Read the condition through the normal signal path and stash it in this
        // block's state, which the subsystem handler reads back after each iteration.
        // Mirror it on the output so a probe can observe it. An unconnected condition
        // holds the loop.
        const double cond = hasInput(ctx, b.nid, 0) ? getInput(ctx, b.nid, 0, 1.0) : 1.0;
        ctx.blockState[b.nid].output = cond;
        setOutput(ctx, b.nid, cond);
        return false;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
