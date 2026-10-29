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
// risingEdge (Detect Rising Edge): output 1.0 on the step where the input rises
// to a strictly positive value while its previous value was nonpositive, else
// 0.0. Stateful; InitialCondition seeds the value before the first step.
//=============================================================================
bool
Nelson::NFlow::handleRisingEdge(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double ic = bd.paramDouble(nflow::kInitial, 0.0);
        if (w <= 1) {
            st.scalar = ic;
            st.output = 0.0;
        } else {
            st.vec.assign(w, ic);
            st.outLatch.assign(w, 0.0);
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // Direct feedthrough: the output reads the CURRENT input, so it belongs
        // in the topologically ordered ALGEBRAIC phase (the declaration-ordered
        // OUTPUT phase could sample a not-yet-computed input). A sideEffectFree
        // sub-step replays the last held value. The output is recomputed every
        // base step even under a slower SampleTime; only st.scalar / st.vec
        // advance on the sample hit, in UPDATE.
        if (!ctx.sideEffectFree) {
            if (w <= 1) {
                const double inp = getInput(ctx, b.nid, 0, 0.0);
                st.output = (st.scalar <= 0.0 && inp > 0.0) ? 1.0 : 0.0;
            } else {
                SigView u = getInputSig(ctx, b.nid, 0);
                if ((int)st.outLatch.size() != w) {
                    st.outLatch.assign(w, 0.0);
                }
                for (int i = 0; i < w; ++i) {
                    const double prev = (i < (int)st.vec.size()) ? st.vec[i] : 0.0;
                    st.outLatch[i] = (prev <= 0.0 && sigAt(u, i) > 0.0) ? 1.0 : 0.0;
                }
            }
        }
        if (w <= 1) {
            setOutput(ctx, b.nid, st.output);
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            for (int i = 0; i < w; ++i) {
                y[i] = (i < (int)st.outLatch.size()) ? st.outLatch[i] : 0.0;
            }
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        if (w <= 1) {
            st.scalar = getInput(ctx, b.nid, 0, 0.0);
        } else {
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            for (int i = 0; i < w; ++i) {
                st.vec[i] = sigAt(u, i);
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCRisingEdge()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState(
            "re_prev_" + a.id, nflow::formatNumber(bd.paramDouble(nflow::kInitial, 0.0)), "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = (s->re_prev_" + a.id + " <= 0.0 && " + a.in[0]
            + " > 0.0) ? 1.0 : 0.0;");
        const int rate = codegenSampleRate(asJson(a.params), a.dt);
        const std::string upd = "s->re_prev_" + a.id + " = " + a.in[0] + ";";
        if (rate > 1) {
            a.line("if (__nflow_step % " + std::to_string(rate) + " == 0) " + upd);
        } else {
            a.line(upd);
        }
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustRisingEdge()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState("re_prev_" + a.id, a.fmt(bd.paramDouble(nflow::kInitial, 0.0)), "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = if s.re_prev_" + a.id + " <= 0.0 && " + a.in[0]
            + " > 0.0 { 1.0_f64 } else { 0.0_f64 };");
        const int rate = codegenSampleRate(asJson(a.params), a.dt);
        const std::string upd = "s.re_prev_" + a.id + " = " + a.in[0] + ";";
        if (rate > 1) {
            a.line("if __nflow_step % " + std::to_string(rate) + " == 0 { " + upd + " }");
        } else {
            a.line(upd);
        }
    };
    return t;
}
//=============================================================================
