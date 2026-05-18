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
void
VMCallFrame::init(const BytecodeChunk& ch, int requestedLhs)
{
    chunk = &ch;
    startPC = 0;
    nLhs = requestedLhs;
    locals.assign(ch.nLocals, ArrayOf::emptyConstructor());
    assignedLocals.assign(ch.nLocals, 0);
    valueStack.clear();
    valueStack.reserve(ch.maxStack == 0 ? 8 : ch.maxStack);
    loopStates.assign(ch.nLoopSlots, GenericLoopState());
    dblLoops.assign(ch.nLoopSlots, DoubleLoopState());
    rangeLoops.assign(ch.nLoopSlots, RangeLoopState());
    exceptionFrames.clear();
    endContexts.clear();
    profTic = 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
