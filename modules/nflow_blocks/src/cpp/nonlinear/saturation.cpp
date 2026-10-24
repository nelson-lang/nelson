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
Nelson::NFlow::handleSaturation(SimCtx& ctx, const Block& b, Phase phase)
{
    nflow::BlockDescriptor bd(b, ctx.variables);
    double mn = bd.paramDouble(nflow::kLowerLimit, -std::numeric_limits<double>::infinity());
    double mx = bd.paramDouble(nflow::kUpperLimit, std::numeric_limits<double>::infinity());
    if (phase == Phase::ZERO_CROSSING) {
        // Surfaces where the clamp engages/disengages: u - lower, u - upper,
        // two per signal element. A variable-step solver stops at these kinks.
        // Infinite limits never cross (constant, non-zero surface).
        double* g = blockG(ctx, b.nid);
        if (g) {
            SigView u = getInputSig(ctx, b.nid, 0);
            const int w = std::max(1, outputWidth(ctx, b.nid, 0));
            for (int i = 0; i < w; ++i) {
                const double ui = sigAt(u, i);
                g[2 * i] = std::isfinite(mn) ? (ui - mn) : 1.0;
                g[2 * i + 1] = std::isfinite(mx) ? (ui - mx) : 1.0;
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
    SigView u = getInputSig(ctx, b.nid, 0);
    return emitElementwise(ctx, b.nid, [&](int i) { return clampVal(sigAt(u, i), mn, mx); });
}
//=============================================================================
namespace {
// Zero-crossing seam: the clamp kinks u - lower and u - upper (an infinite
// limit never crosses, constant 1.0).
void
saturationCrossing(const Nelson::NFlow::BlockCodegenArgs& a)
{
    if (a.rk4Op != Nelson::NFlow::Rk4Crossing) {
        return;
    }
    nflow::BlockDescriptor bd(*a.block, *a.variables);
    const double mn = bd.paramDouble(nflow::kLowerLimit, -std::numeric_limits<double>::infinity());
    const double mx = bd.paramDouble(nflow::kUpperLimit, std::numeric_limits<double>::infinity());
    const std::string co = std::to_string(a.crossingOffset);
    const std::string co1 = std::to_string(a.crossingOffset + 1);
    a.line(a.rk4Arr + "[" + co + "] = "
        + (std::isfinite(mn) ? ("(" + a.in[0] + ") - " + a.fmt(mn)) : std::string("1.0")) + ";");
    a.line(a.rk4Arr + "[" + co1 + "] = "
        + (std::isfinite(mx) ? ("(" + a.in[0] + ") - " + a.fmt(mx)) : std::string("1.0")) + ";");
}
//=============================================================================
int
saturationCrossingCount(const Nelson::NFlow::BlockCodegenArgs& a)
{
    nflow::BlockDescriptor bd(*a.block, *a.variables);
    const double mn = bd.paramDouble(nflow::kLowerLimit, -std::numeric_limits<double>::infinity());
    const double mx = bd.paramDouble(nflow::kUpperLimit, std::numeric_limits<double>::infinity());
    return (std::isfinite(mn) || std::isfinite(mx)) ? 2 : 0;
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCSaturation()
{
    BlockCodegenTemplate t;
    // Absent limits default to a wide-open clamp (the interpreter's -inf/+inf,
    // the Rust backend's +/-1e308), not 0.0/0.0.
    t.step = "out_{id} = fmin(fmax({in0}, {param:LowerLimit:-1e308}), {param:UpperLimit:1e308});";
    t.emitRk4 = saturationCrossing;
    t.crossingCount = saturationCrossingCount;
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustSaturation()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = libm::fmin(libm::fmax({in0}, {param:LowerLimit:-1e308}), "
             "{param:UpperLimit:1e308});";
    t.emitRk4 = saturationCrossing;
    t.crossingCount = saturationCrossingCount;
    return t;
}
//=============================================================================
