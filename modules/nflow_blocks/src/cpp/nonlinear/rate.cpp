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
Nelson::NFlow::handleRate(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        if (w > 1) {
            st.vec.assign(w, 0.0);
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // Direct feedthrough, as the emitted C/Rust step: the output at t is the
        // current input slewed from the state committed at the end of the
        // previous step. Pure: UPDATE commits the state, never this, so an RK
        // stage or an algebraic-loop sweep may re-enter it safely.
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double rise = std::max(0.0, bd.paramDouble(nflow::kRise, 0.0));
        const double fall = std::max(0.0, bd.paramDouble(nflow::kFall, 0.0));
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) {
            const double prev = (w <= 1) ? st.scalar : ((i < (int)st.vec.size()) ? st.vec[i] : 0.0);
            return std::min(prev + rise * ctx.dt, std::max(prev - fall * ctx.dt, sigAt(u, i)));
        });
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double rise = std::max(0.0, bd.paramDouble(nflow::kRise, 0.0));
        double fall = std::max(0.0, bd.paramDouble(nflow::kFall, 0.0));
        if (w <= 1) {
            double inp = getInput(ctx, b.nid, 0, 0.0);
            double maxRise = st.scalar + rise * ctx.dt;
            double maxFall = st.scalar - fall * ctx.dt;
            st.scalar = std::min(maxRise, std::max(maxFall, inp));
        } else {
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            for (int i = 0; i < w; ++i) {
                double maxRise = st.vec[i] + rise * ctx.dt;
                double maxFall = st.vec[i] - fall * ctx.dt;
                st.vec[i] = std::min(maxRise, std::max(maxFall, sigAt(u, i)));
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCRate()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) { a.addState("rate_" + a.id, "", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double rise = std::fmax(0.0, bd.paramDouble(nflow::kRise, 0.0));
        double fall = std::fmax(0.0, bd.paramDouble(nflow::kFall, 0.0));
        a.line("{ double maxRise = s->rate_" + a.id + " + " + a.fmt(rise) + " * dt;");
        a.line("  double maxFall = s->rate_" + a.id + " - " + a.fmt(fall) + " * dt;");
        a.line("  double v = " + a.in[0] + ";");
        a.line("  if (v > maxRise) v = maxRise;");
        a.line("  if (v < maxFall) v = maxFall;");
        a.line("  s->rate_" + a.id + " = v;");
        a.line("  out_" + a.id + " = v; }");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustRate()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) { a.addState("rate_" + a.id, "", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double rise = std::fmax(0.0, bd.paramDouble(nflow::kRise, 0.0));
        double fall = std::fmax(0.0, bd.paramDouble(nflow::kFall, 0.0));
        a.line("{");
        a.line("    let max_rise = s.rate_" + a.id + " + " + a.fmt(rise) + " * dt;");
        a.line("    let max_fall = s.rate_" + a.id + " - " + a.fmt(fall) + " * dt;");
        a.line("    let mut v = " + a.in[0] + ";");
        a.line("    if v > max_rise { v = max_rise; }");
        a.line("    if v < max_fall { v = max_fall; }");
        a.line("    s.rate_" + a.id + " = v;");
        a.line("    out_" + a.id + " = v;");
        a.line("}");
    };
    return t;
}
//=============================================================================
