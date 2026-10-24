//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VMCallFrame.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    const ArrayOf&
    emptyFrameValue()
    {
        // Copies detach on mutation; each interpreter thread owns its seed.
        static thread_local const ArrayOf value = ArrayOf::empty();
        return value;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
VMCallFrame::init(const BytecodeChunk& ch, int requestedLhs)
{
    chunk = &ch;
    startPC = 0;
    nLhs = requestedLhs;
    locals.assign(ch.nLocals, emptyFrameValue());
    assignedLocals.assign(ch.nLocals, 0);
    valueStack.clear();
    valueStack.reserve(ch.maxStack == 0 ? 8 : ch.maxStack);
    if (ch.nLoopSlots != 0 || !loopStates.empty() || !dblLoops.empty() || !rangeLoops.empty()) {
        loopStates.assign(ch.nLoopSlots, GenericLoopState());
        dblLoops.assign(ch.nLoopSlots, DoubleLoopState());
        rangeLoops.assign(ch.nLoopSlots, RangeLoopState());
    }
    exceptionFrames.clear();
    endContexts.clear();
    profTic = 0;
    nArgIn = -1;
    skipDebugStatements = false;
    hasContextScope = true;
    lazyScopePushed = false;
    localsSyncedToContext = false;
}
//=============================================================================
void
VMCallFrame::clearLocals()
{
    const ArrayOf& empty = emptyFrameValue();
    for (ArrayOf& local : locals) {
        local = empty;
    }
    locals.clear();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
