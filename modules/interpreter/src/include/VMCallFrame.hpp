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
    uint64 profTic = 0;
    std::vector<ArrayOf> captured;
    int nLhs = 0;

    void
    init(const BytecodeChunk& ch, int requestedLhs);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
