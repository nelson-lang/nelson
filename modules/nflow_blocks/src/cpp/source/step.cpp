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
Nelson::NFlow::handleStep(SimCtx& ctx, const Block& b, Phase phase)
{
    nflow::BlockDescriptor bd(b, ctx.variables);
    double stepTime = bd.paramDouble(nflow::kStepTime, 0.0);
    if (phase == Phase::OUTPUT) {
        setOutput(ctx, b.nid, (ctx.t >= stepTime) ? 1.0 : 0.0);
        return false;
    }
    if (phase == Phase::ZERO_CROSSING) {
        // Surface for the step discontinuity: a variable-step solver locates
        // t = stepTime exactly and cold-restarts there instead of stepping over
        // the jump. Inactive on the fixed-step path (no g vector).
        double* g = blockG(ctx, b.nid);
        if (g) {
            g[0] = ctx.t - stepTime;
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCStep()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = (t >= {param:Time:0.0} ? 1.0 : 0.0);";
    // Zero-crossing seam: one surface (t - Time), as in the simulator, so the
    // variable-step solvers stop exactly at the discontinuity.
    t.emitRk4 = [](const BlockCodegenArgs& a) {
        if (a.rk4Op != Rk4Crossing) {
            return;
        }
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.line(a.rk4Arr + "[" + std::to_string(a.crossingOffset) + "] = t - "
            + a.fmt(bd.paramDouble("Time", 0.0)) + ";");
    };
    t.crossingCount = [](const BlockCodegenArgs&) -> int { return 1; };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustStep()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = if t >= {param:Time:0.0} { 1.0_f64 } else { 0.0_f64 };";
    t.emitRk4 = [](const BlockCodegenArgs& a) {
        if (a.rk4Op != Rk4Crossing) {
            return;
        }
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.line(a.rk4Arr + "[" + std::to_string(a.crossingOffset) + "] = t - "
            + a.fmt(bd.paramDouble("Time", 0.0)) + ";");
    };
    t.crossingCount = [](const BlockCodegenArgs&) -> int { return 1; };
    return t;
}
//=============================================================================
