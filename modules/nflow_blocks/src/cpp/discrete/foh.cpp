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
#include "discrete_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleFoh(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    // st.vec = current sample, st.vec2 = previous sample (per element); timing
    // (dLastTime/dNextTime) is shared. st.outLatch holds the interpolated out.
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        st.scalar2 = 0.0;
        st.dLastTime = 0.0;
        st.dNextTime = 0.0;
        st.output = 0.0;
        if (w > 1) {
            st.vec.assign(w, 0.0);
            st.vec2.assign(w, 0.0);
            st.outLatch.assign(w, 0.0);
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // At a hit the hold takes the sample THERE, so the output at that
        // instant is the current input; between hits it rides the slope built
        // from the last two samples. Pure: UPDATE owns the state.
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double ts = std::max(0.001, bd.paramDouble(nflow::kTs, ctx.dt));
        const bool hit = (ctx.t + 1e-6 >= st.dNextTime);
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) {
            if (hit) {
                return sigAt(u, i);
            }
            const double last = (w <= 1) ? st.scalar : ((i < (int)st.vec.size()) ? st.vec[i] : 0.0);
            const double prev
                = (w <= 1) ? st.scalar2 : ((i < (int)st.vec2.size()) ? st.vec2[i] : 0.0);
            return last + ((last - prev) / ts) * (ctx.t - st.dLastTime);
        });
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double ts = std::max(0.001, bd.paramDouble(nflow::kTs, ctx.dt));
        if (w <= 1) {
            double inp = getInput(ctx, b.nid, 0, 0.0);
            if (ctx.t + 1e-6 >= st.dNextTime) {
                st.scalar2 = st.scalar;
                st.scalar = inp;
                st.dLastTime = ctx.t;
                st.dNextTime = ctx.t + ts;
            }
            double slope = (st.scalar - st.scalar2) / ts;
            st.output = st.scalar + slope * (ctx.t - st.dLastTime);
        } else {
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            if ((int)st.vec2.size() != w) {
                st.vec2.assign(w, 0.0);
            }
            if ((int)st.outLatch.size() != w) {
                st.outLatch.assign(w, 0.0);
            }
            const bool sample = (ctx.t + 1e-6 >= st.dNextTime);
            if (sample) {
                for (int i = 0; i < w; ++i) {
                    st.vec2[i] = st.vec[i];
                    st.vec[i] = sigAt(u, i);
                }
                st.dLastTime = ctx.t;
                st.dNextTime = ctx.t + ts;
            }
            for (int i = 0; i < w; ++i) {
                double slope = (st.vec[i] - st.vec2[i]) / ts;
                st.outLatch[i] = st.vec[i] + slope * (ctx.t - st.dLastTime);
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCFoh()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("foh_prev_" + a.id, "", "");
        a.addState("foh_last_" + a.id, "", "");
        a.addState("foh_last_t_" + a.id, "", "");
        a.addState("foh_next_" + a.id, "", "");
        a.addState("foh_out_" + a.id, "", "");
    };
    // No emitOutput: the hold takes its sample AT the hit instant, so the sample
    // has to be taken before the output is formed.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        a.line("if (t + 1e-6 >= s->foh_next_" + a.id + ") {");
        a.line("  s->foh_prev_" + a.id + " = s->foh_last_" + a.id + ";");
        a.line("  s->foh_last_" + a.id + " = " + a.in[0] + ";");
        a.line("  s->foh_last_t_" + a.id + " = t;");
        a.line("  s->foh_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("{ double slope = (s->foh_last_" + a.id + " - s->foh_prev_" + a.id + ") / "
            + a.fmt(ts) + ";");
        a.line("  s->foh_out_" + a.id + " = s->foh_last_" + a.id + " + slope * (t - s->foh_last_t_"
            + a.id + "); }");
        a.line("out_" + a.id + " = s->foh_out_" + a.id + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustFoh()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("foh_prev_" + a.id, "", "");
        a.addState("foh_last_" + a.id, "", "");
        a.addState("foh_last_t_" + a.id, "", "");
        a.addState("foh_next_" + a.id, "", "");
        a.addState("foh_out_" + a.id, "", "");
    };
    // Same reordering as the C template: sample, then form the output.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        a.line("if t + 1e-6 >= s.foh_next_" + a.id + " {");
        a.line("    s.foh_prev_" + a.id + " = s.foh_last_" + a.id + ";");
        a.line("    s.foh_last_" + a.id + " = " + a.in[0] + ";");
        a.line("    s.foh_last_t_" + a.id + " = t;");
        a.line("    s.foh_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("{");
        a.line("    let slope = (s.foh_last_" + a.id + " - s.foh_prev_" + a.id + ") / " + a.fmt(ts)
            + ";");
        a.line("    s.foh_out_" + a.id + " = s.foh_last_" + a.id + " + slope * (t - s.foh_last_t_"
            + a.id + ");");
        a.line("}");
        a.line("out_" + a.id + " = s.foh_out_" + a.id + ";");
    };
    return t;
}
//=============================================================================
