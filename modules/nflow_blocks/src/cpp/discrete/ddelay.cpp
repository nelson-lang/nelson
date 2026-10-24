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
Nelson::NFlow::handleDdelay(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    // Per-element FIFO ring: st.queue is a flat [width * steps] buffer,
    // element e occupying [e*steps, e*steps+steps); delayIndex is the shared
    // head. st.outLatch holds the per-element delayed output.
    if (phase == Phase::INIT) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        int steps = std::max(1, (int)std::round(bd.paramDouble(nflow::kSteps, 1.0)));
        double ts_v = std::max(0.001, bd.paramDouble(nflow::kTs, 0.1));
        const int w = std::max(1, outputWidth(ctx, b.nid, 0));
        st.queue.assign((size_t)steps * w, 0.0);
        st.delayIndex = 0;
        st.outLatch.assign(w, 0.0);
        st.dTs = ts_v;
        st.dNextTime = 0.0;
        st.dLastOut = 0.0;
        st.output = 0.0;
        return false;
    }
    if (phase == Phase::OUTPUT) {
        const int w = std::max(1, outputWidth(ctx, b.nid, 0));
        if (w <= 1) {
            setOutput(ctx, b.nid, st.outLatch.empty() ? st.output : st.outLatch[0]);
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            for (int e = 0; e < w; ++e) {
                y[e] = (e < (int)st.outLatch.size()) ? st.outLatch[e] : 0.0;
            }
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double ts_v = std::max(0.001, bd.paramDouble(nflow::kTs, st.dTs));
        st.dTs = ts_v;
        int steps = std::max(1, (int)std::round(bd.paramDouble(nflow::kSteps, 1.0)));
        const int w = std::max(1, outputWidth(ctx, b.nid, 0));
        if (ctx.t + 1e-6 >= st.dNextTime) {
            // Keep the buffer sized to steps*width (Steps may be tunable).
            if ((int)st.queue.size() != steps * w) {
                st.queue.assign((size_t)steps * w, 0.0);
                st.delayIndex = 0;
            }
            if ((int)st.outLatch.size() != w) {
                st.outLatch.assign(w, 0.0);
            }
            SigView u = getInputSig(ctx, b.nid, 0);
            const int head = st.delayIndex % steps;
            const int oldest = (head + 1) % steps;
            for (int e = 0; e < w; ++e) {
                double* ring = st.queue.data() + (size_t)e * steps;
                ring[head] = sigAt(u, e);
                st.outLatch[e] = ring[oldest];
            }
            st.delayIndex = oldest;
            st.dLastOut = st.outLatch.empty() ? 0.0 : st.outLatch[0];
            st.dNextTime = ctx.t + ts_v;
        }
        st.output = st.outLatch.empty() ? st.dLastOut : st.outLatch[0];
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCDdelay()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        int steps = std::max(1, static_cast<int>(std::round(bd.paramDouble(nflow::kSteps, 1.0))));
        if (steps < 1) {
            steps = 1;
        }
        a.declState("double ddelay_buf_" + a.id + "[" + std::to_string(steps) + "];");
        a.addState("ddelay_next_" + a.id, "0.0", "");
        a.addState("ddelay_last_" + a.id, "0.0", "");
        a.addInit("  for (int i = 0; i < " + std::to_string(steps) + "; i++) s->ddelay_buf_" + a.id
            + "[i] = 0.0;");
    };
    t.emitOutput = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = s->ddelay_last_" + a.id + ";");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        int steps = std::max(1, static_cast<int>(std::round(bd.paramDouble(nflow::kSteps, 0.0))));
        if (steps < 1) {
            steps = 1;
        }
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = 0.1;
        }
        a.line("out_" + a.id + " = s->ddelay_last_" + a.id + ";");
        a.line("if (t + 1e-6 >= s->ddelay_next_" + a.id + ") {");
        a.line("  for (int i = 0; i < " + std::to_string(steps - 1) + "; i++) s->ddelay_buf_" + a.id
            + "[i] = s->ddelay_buf_" + a.id + "[i + 1];");
        a.line(
            "  s->ddelay_buf_" + a.id + "[" + std::to_string(steps - 1) + "] = " + a.in[0] + ";");
        a.line("  s->ddelay_last_" + a.id + " = s->ddelay_buf_" + a.id + "[0];");
        a.line("  s->ddelay_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustDdelay()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        int steps = std::max(1, static_cast<int>(std::round(bd.paramDouble(nflow::kSteps, 1.0))));
        if (steps < 1) {
            steps = 1;
        }
        a.declState("    pub ddelay_buf_" + a.id + ": [f64; " + std::to_string(steps) + "],");
        a.declState("    pub ddelay_next_" + a.id + ": f64,");
        a.declState("    pub ddelay_last_" + a.id + ": f64,");
        a.addInit("    s.ddelay_buf_" + a.id + " = [" + a.fmt(0.0) + "; " + std::to_string(steps)
            + "]; ");
        a.addInit("    s.ddelay_next_" + a.id + " = 0.0_f64;");
        a.addInit("    s.ddelay_last_" + a.id + " = 0.0_f64;");
    };
    t.emitOutput = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = s.ddelay_last_" + a.id + ";");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        int steps = std::max(1, static_cast<int>(std::round(bd.paramDouble(nflow::kSteps, 0.0))));
        if (steps < 1) {
            steps = 1;
        }
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = 0.1;
        }
        a.line("out_" + a.id + " = s.ddelay_last_" + a.id + ";");
        a.line("if (t + 1e-6 >= s.ddelay_next_" + a.id + ") {");
        a.line("    for i in 0.." + std::to_string(steps > 0 ? steps - 1 : 0) + " { s.ddelay_buf_"
            + a.id + "[i] = s.ddelay_buf_" + a.id + "[i + 1]; }");
        a.line("    s.ddelay_buf_" + a.id + "[" + std::to_string(std::max(1, steps) - 1)
            + "] = " + a.in[0] + ";");
        a.line("    s.ddelay_last_" + a.id + " = s.ddelay_buf_" + a.id + "[0];");
        a.line("    s.ddelay_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
    };
    return t;
}
//=============================================================================
