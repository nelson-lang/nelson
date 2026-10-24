//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// atan2: four-quadrant arctangent, out = atan2(y, x) with y on input port 0
// and x on input port 1. Element-wise with scalar expansion.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "math_blocks.hpp"
#include <cmath>
//=============================================================================
bool
Nelson::NFlow::handleAtan2(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0) || !hasInput(ctx, b.nid, 1)) {
        return false;
    }
    SigView y = getInputSig(ctx, b.nid, 0);
    SigView x = getInputSig(ctx, b.nid, 1);
    return emitElementwise(ctx, b.nid, [&](int i) { return std::atan2(sigAt(y, i), sigAt(x, i)); });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCAtan2()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = atan2({in0}, {in1});";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustAtan2()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = libm::atan2({in0}, {in1});";
    return t;
}
//=============================================================================
