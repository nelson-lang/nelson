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
#include "continuous_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleDerivative(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    // st.scalar / st.vec = previous SAMPLE's input; st.output / st.outLatch =
    // the held backward difference replayed between sample points.
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        st.output = 0.0;
        st.derivPrimed = false;
        if (w > 1) {
            st.vec.assign(w, 0.0);
            st.outLatch.assign(w, 0.0);
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // Feedthrough (reads the CURRENT input), so it runs in the topologically
        // ordered ALGEBRAIC phase: the source feeding this block has then run
        // first (an OUTPUT-phase derivative could be scheduled ahead of its source
        // and read a stale input).
        // The backward difference (u_k - u_{k-1}) / dt is recomputed at every
        // committed sample point, so the recorded sample is the current step's
        // derivative. Committed passes are the fixed-step loop and the solver
        // loop's recordAt / located-event passes (ctx.sideEffectFree == false). A
        // pure rhs / zero-crossing evaluation (sideEffectFree == true) replays the
        // held value instead: a continuous consumer then sees a zero-order hold,
        // not an intra-step slope that collapses to zero at the step boundary.
        if (!ctx.sideEffectFree) {
            const double invDt = 1.0 / std::max(ctx.dt, 1e-6);
            // The first committed sample has nothing to difference against: the
            // derivative is zero there and the previous input is seeded with the
            // current one. A previous input of 0 fired a u(0)/dt spike for any
            // input not starting at 0 (a constant 5 gave 500 at t = 0).
            const bool prime = !st.derivPrimed;
            st.derivPrimed = true;
            if (w <= 1) {
                double inp = getInput(ctx, b.nid, 0, 0.0);
                if (prime) {
                    st.scalar = inp;
                }
                st.output = (inp - st.scalar) * invDt;
            } else {
                SigView u = getInputSig(ctx, b.nid, 0);
                if ((int)st.vec.size() != w) {
                    st.vec.assign(w, 0.0);
                }
                if ((int)st.outLatch.size() != w) {
                    st.outLatch.assign(w, 0.0);
                }
                for (int i = 0; i < w; ++i) {
                    if (prime) {
                        st.vec[i] = sigAt(u, i);
                    }
                    st.outLatch[i] = (sigAt(u, i) - st.vec[i]) * invDt;
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
        // Advance the stored previous-sample input for the next backward
        // difference. Latched at the sample point so the solver loop's sub-step
        // OUTPUT replays a stable held derivative.
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
// The emitted step mirrors the simulator's committed pass: backward difference
// from the current input, then advance the stored previous input. Codegen emits
// blocks in topological order, so the source is available. Generated code only
// runs the committed step, so the solver's zero-order-hold branch needs no
// generated state.
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCDerivative()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("der_prev_" + a.id, "", "");
        a.addState("der_primed_" + a.id, "0.0", "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        // Seed the previous input on the first step: nothing to difference
        // against, so the derivative is zero (a previous input of 0 fired a
        // u(0)/dt spike).
        a.line("if (s->der_primed_" + a.id + " == 0.0) {");
        a.line("  s->der_prev_" + a.id + " = " + a.in[0] + ";");
        a.line("  s->der_primed_" + a.id + " = 1.0;");
        a.line("}");
        a.line("out_" + a.id + " = (" + a.in[0] + " - s->der_prev_" + a.id + ") / fmax(dt, 1e-6);");
        a.line("s->der_prev_" + a.id + " = " + a.in[0] + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustDerivative()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("der_prev_" + a.id, "", "");
        a.addState("der_primed_" + a.id, "0.0", "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        a.line("if s.der_primed_" + a.id + " == 0.0 {");
        a.line("    s.der_prev_" + a.id + " = " + a.in[0] + ";");
        a.line("    s.der_primed_" + a.id + " = 1.0;");
        a.line("}");
        a.line("out_" + a.id + " = (" + a.in[0] + " - s.der_prev_" + a.id
            + ") / libm::fmax(dt, 1e-6_f64);");
        a.line("s.der_prev_" + a.id + " = " + a.in[0] + ";");
    };
    return t;
}
//=============================================================================
