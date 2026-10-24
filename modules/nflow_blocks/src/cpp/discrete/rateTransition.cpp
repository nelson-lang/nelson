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
#include "NFlowCodegenLang.hpp"
#include <cmath>
#include <algorithm>
#include "discrete_blocks.hpp"
//=============================================================================
// Rate transition (minimal multi-rate primitive): a zero-order-hold sample and
// hold that resamples its input at its own OutPortSampleTime, holding the last
// sample between ticks. This lets one block run slower than the base step
// without a full sample-time scheduler. OutPortSampleTime <= base step (or the
// default -1 = inherit) means "every step"; a larger value Ts holds the output
// for Ts / base-step steps. st.scalar carries the next sample time.
//
// The tick answers the input it samples in the SAME step. Sampling in UPDATE and
// answering from OUTPUT would put a whole output period between the two.
//=============================================================================
bool
Nelson::NFlow::handleRateTransition(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        if (w <= 1) {
            st.output = bd.paramDouble(nflow::kInitial, 0.0);
        } else {
            std::vector<double> iniList = bd.paramList(nflow::kInitial);
            st.vec.assign(w, 0.0);
            for (int i = 0; i < w; ++i) {
                st.vec[i] = iniList.empty()
                    ? 0.0
                    : (i < (int)iniList.size() ? iniList[i] : iniList.back());
            }
        }
        st.scalar = 0.0;
        return false;
    }
    // Is the step now running a sample tick? Read the same way from both
    // phases, which see the same t, so the sample and the schedule agree
    // without a flag between them.
    auto isTick = [&]() -> bool {
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double ts = bd.paramDouble("OutPortSampleTime", -1.0);
        const double base = (ctx.dt > 0.0) ? ctx.dt : 1e-9;
        if (ts <= base * (1.0 + 1e-9)) {
            return true; // inherit / base rate: every step
        }
        return (ctx.t + base * 1e-6 >= st.scalar);
    };
    if (phase == Phase::ALGEBRAIC) {
        if (isTick()) {
            if (w <= 1) {
                st.output = getInput(ctx, b.nid, 0, 0.0);
            } else {
                SigView u = getInputSig(ctx, b.nid, 0);
                if ((int)st.vec.size() != w) {
                    st.vec.assign(w, 0.0);
                }
                for (int i = 0; i < w; ++i) {
                    st.vec[i] = sigAt(u, i);
                }
            }
        }
        if (w <= 1) {
            setOutput(ctx, b.nid, st.output);
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            for (int i = 0; i < w; ++i) {
                y[i] = (i < (int)st.vec.size()) ? st.vec[i] : 0.0;
            }
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double ts = bd.paramDouble("OutPortSampleTime", -1.0);
        const double base = (ctx.dt > 0.0) ? ctx.dt : 1e-9;
        if (!isTick()) {
            return false; // hold between sample ticks
        }
        if (ts > base) {
            if (st.scalar <= 0.0) {
                st.scalar = ctx.t;
            }
            while (st.scalar <= ctx.t + base * 1e-6) {
                st.scalar += ts;
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
// Single language-parameterized emitter (CodegenLang), the zoh shape: a held
// sample with time-based self-scheduling on OutPortSampleTime. The tick samples
// FIRST and the output answers with what it just sampled, as in the simulator.
// OutPortSampleTime <= the base step (or the default -1 = inherit) samples every
// step.
static Nelson::NFlow::BlockCodegenTemplate
makeRateTransitionCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    t.emitState = [L](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState("rt_last_" + a.id, a.fmt(bd.paramDouble(nflow::kInitial, 0.0)), "");
        a.addState("rt_next_" + a.id, "", "");
    };
    t.emitOutput = [L](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = " + L.sref("rt_last_" + a.id) + ";");
    };
    t.emitStep = [L](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double ts = bd.paramDouble("OutPortSampleTime", -1.0);
        const std::string last = L.sref("rt_last_" + a.id);
        if (ts <= a.dt * (1.0 + 1e-9)) {
            // Inherit / base rate: a plain sample and hold, answering the
            // sample it has just taken.
            a.line(last + " = " + a.in[0] + ";");
            a.line("out_" + a.id + " = " + last + ";");
            return;
        }
        const std::string next = L.sref("rt_next_" + a.id);
        if (L.rust) {
            a.line("if t + 1e-6 >= " + next + " {");
            a.line("    " + last + " = " + a.in[0] + ";");
            a.line("    " + next + " = t + " + a.fmt(ts) + ";");
            a.line("}");
        } else {
            a.line("if (t + 1e-6 >= " + next + ") {");
            a.line("  " + last + " = " + a.in[0] + ";");
            a.line("  " + next + " = t + " + a.fmt(ts) + ";");
            a.line("}");
        }
        a.line("out_" + a.id + " = " + last + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCRateTransition()
{
    return makeRateTransitionCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustRateTransition()
{
    return makeRateTransitionCodegen({ true });
}
//=============================================================================
