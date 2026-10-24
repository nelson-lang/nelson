//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <algorithm>
#include "source_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleClock(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    setOutput(ctx, b.nid, ctx.t);
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCClock()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = t;";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustClock()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = t;";
    return t;
}
//=============================================================================
