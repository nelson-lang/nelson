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
#include "nonlinear_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleDeadZone(SimCtx& ctx, const Block& b, Phase phase)
{
    nflow::BlockDescriptor bd(b, ctx.variables);
    double lo = bd.paramDouble(nflow::kLowerValue, -1.0);
    double hi = bd.paramDouble(nflow::kUpperValue, 1.0);
    if (phase == Phase::ZERO_CROSSING) {
        // Surfaces where the dead-band edges engage: u - lower, u - upper, two
        // per signal element.
        double* g = blockG(ctx, b.nid);
        if (g) {
            SigView u = getInputSig(ctx, b.nid, 0);
            const int w = std::max(1, outputWidth(ctx, b.nid, 0));
            for (int i = 0; i < w; ++i) {
                const double ui = sigAt(u, i);
                g[2 * i] = ui - lo;
                g[2 * i + 1] = ui - hi;
            }
        }
        return false;
    }
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    SigView uv = getInputSig(ctx, b.nid, 0);
    return emitElementwise(ctx, b.nid, [&](int i) {
        double u = sigAt(uv, i);
        if (u < lo) {
            return u - lo;
        }
        if (u > hi) {
            return u - hi;
        }
        return 0.0;
    });
}
//=============================================================================
namespace {
// Zero-crossing seam: the dead-band edges u - lower and u - upper
// (sim ZERO_CROSSING parity, two surfaces).
void
deadZoneCrossing(const Nelson::NFlow::BlockCodegenArgs& a)
{
    if (a.rk4Op != Nelson::NFlow::Rk4Crossing) {
        return;
    }
    nflow::BlockDescriptor bd(*a.block, *a.variables);
    const double lo = bd.paramDouble(nflow::kLowerValue, -1.0);
    const double hi = bd.paramDouble(nflow::kUpperValue, 1.0);
    a.line(a.rk4Arr + "[" + std::to_string(a.crossingOffset) + "] = (" + a.in[0] + ") - "
        + a.fmt(lo) + ";");
    a.line(a.rk4Arr + "[" + std::to_string(a.crossingOffset + 1) + "] = (" + a.in[0] + ") - "
        + a.fmt(hi) + ";");
}
//=============================================================================
int
deadZoneCrossingCount(const Nelson::NFlow::BlockCodegenArgs&)
{
    return 2;
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCDeadZone()
{
    BlockCodegenTemplate t;
    t.step = "{ double u = {in0};\n"
             "  out_{id} = (u < {param:LowerValue:-1.0}) ? u - {param:LowerValue:-1.0} : ((u > "
             "{param:UpperValue:1.0}) ? u - {param:UpperValue:1.0} : 0.0); }";
    t.emitRk4 = deadZoneCrossing;
    t.crossingCount = deadZoneCrossingCount;
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustDeadZone()
{
    BlockCodegenTemplate t;
    t.step
        = "out_{id} = if {in0} < {param:LowerValue:-1.0} { {in0} - {param:LowerValue:-1.0} } else "
          "if {in0} > {param:UpperValue:1.0} { {in0} - {param:UpperValue:1.0} } else { 0.0_f64 };";
    t.emitRk4 = deadZoneCrossing;
    t.crossingCount = deadZoneCrossingCount;
    return t;
}
//=============================================================================
