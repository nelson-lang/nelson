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
Nelson::NFlow::handleDifference(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        if (w <= 1) {
            st.scalar = bd.paramDouble(nflow::kICPrevInput, 0.0);
            st.output = 0.0;
        } else {
            std::vector<double> iniList = bd.paramList(nflow::kICPrevInput);
            st.vec.assign(w, 0.0);
            st.outLatch.assign(w, 0.0);
            for (int i = 0; i < w; ++i) {
                st.vec[i] = iniList.empty()
                    ? 0.0
                    : (i < (int)iniList.size() ? iniList[i] : iniList.back());
            }
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // The output is direct-feedthrough (it reads the CURRENT input), so it is
        // produced in the topologically-ordered ALGEBRAIC phase rather than the
        // declaration-ordered OUTPUT phase: the source feeding this block is then
        // guaranteed to have run first (an OUTPUT-phase difference could be
        // scheduled ahead of its source and read a not-yet-computed input).
        // st.scalar / st.vec = previous SAMPLE's input; st.output / st.outLatch =
        // the held backward difference replayed between sample points.
        //
        // Recompute (u_k - u_{k-1}) from the current input at every committed
        // sample (fixed-step loop, and the solver loop's recordAt / located-event
        // passes where ctx.sideEffectFree == false). A pure rhs / zero-crossing
        // sub-step (sideEffectFree == true) instead replays the value held at the
        // last sample, so a continuous consumer sees a stable zero-order hold.
        // Under a slower SampleTime the output is still recomputed every base step
        // (setAlgebraicOutputEveryStep exempts it from the sub-rate hold); only
        // the stored previous input advances on the sample hit, in UPDATE.
        const PortSig* ps = portSigOf(ctx, b.nid, 0);
        const SigType ty = ps ? ps->type : SigType::Double;
        const bool typed = (ty != SigType::Double);
        const bool sat = typed ? blockSaturates(ctx, b.nid) : true;
        if (!ctx.sideEffectFree) {
            if (w <= 1) {
                double inp = getInput(ctx, b.nid, 0, 0.0);
                double v = inp - st.scalar;
                st.output = typed ? quantizeToType(v, ty, sat) : v;
            } else {
                SigView u = getInputSig(ctx, b.nid, 0);
                if ((int)st.outLatch.size() != w) {
                    st.outLatch.assign(w, 0.0);
                }
                for (int i = 0; i < w; ++i) {
                    double v = sigAt(u, i) - ((i < (int)st.vec.size()) ? st.vec[i] : 0.0);
                    st.outLatch[i] = typed ? quantizeToType(v, ty, sat) : v;
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
Nelson::NFlow::getCodeGenCDifference()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState(
            "diff_prev_" + a.id, nflow::formatNumber(bd.paramDouble(nflow::kICPrevInput, 0.0)), "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = " + a.in[0] + " - s->diff_prev_" + a.id + ";");
        // Multi-rate: advance the stored previous input only on the sample hit.
        const int rate = codegenSampleRate(a.params, a.dt);
        const std::string upd = "s->diff_prev_" + a.id + " = " + a.in[0] + ";";
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
Nelson::NFlow::getCodeGenRustDifference()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState("diff_prev_" + a.id, a.fmt(bd.paramDouble(nflow::kICPrevInput, 0.0)), "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = " + a.in[0] + " - s.diff_prev_" + a.id + ";");
        // Multi-rate: advance the stored previous input only on the sample hit.
        const int rate = codegenSampleRate(a.params, a.dt);
        const std::string upd = "s.diff_prev_" + a.id + " = " + a.in[0] + ";";
        if (rate > 1) {
            a.line("if __nflow_step % " + std::to_string(rate) + " == 0 { " + upd + " }");
        } else {
            a.line(upd);
        }
    };
    return t;
}
//=============================================================================
