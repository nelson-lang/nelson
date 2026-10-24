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
Nelson::NFlow::handleRamp(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double slope = bd.paramDouble(nflow::kSlope, 0.0);
    double start = bd.paramDouble(nflow::kStart, 0.0);
    setOutput(ctx, b.nid, (ctx.t >= start) ? slope * (ctx.t - start) : 0.0);
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCRamp()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = (t >= {param:start:0.0} ? (t - {param:start:0.0}) * {param:slope:0.0} : "
             "0.0);";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustRamp()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = if t >= {param:start:0.0} { (t - {param:start:0.0}) * {param:slope:0.0} } "
             "else { 0.0_f64 };";
    return t;
}
//=============================================================================
