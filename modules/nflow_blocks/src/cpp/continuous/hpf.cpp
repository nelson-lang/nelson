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
#include "NFlowCodegenPolynomial.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "NFlowCodegenLang.hpp"
#include <cmath>
#include <algorithm>
#include "continuous_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleHpf(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        st.output = 0.0;
        if (w > 1) {
            st.vec.assign(w, 0.0); // state x
            st.vec2.assign(w, 0.0); // latched high-pass output
        }
        return false;
    }
    if (phase == Phase::OUTPUT) {
        // Variable-step minor step: y = u - x (feedthrough) from the scattered
        // global state; else the latched value (fixed-step path, unchanged).
        double* xs = blockX(ctx, b.nid);
        if (w <= 1) {
            if (xs) {
                setOutput(ctx, b.nid, getInput(ctx, b.nid, 0, 0.0) - xs[0]);
            } else {
                setOutput(ctx, b.nid, st.output);
            }
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            if (xs) {
                SigView u = getInputSig(ctx, b.nid, 0);
                for (int i = 0; i < w; ++i) {
                    y[i] = sigAt(u, i) - xs[i];
                }
            } else {
                for (int i = 0; i < w; ++i) {
                    y[i] = (i < (int)st.vec2.size()) ? st.vec2[i] : 0.0;
                }
            }
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double fc = std::max(0.0, bd.paramDouble(nflow::kCutoff, 0.0));
        double wc = 2.0 * M_PI * fc;
        if (w <= 1) {
            double inp = getInput(ctx, b.nid, 0, 0.0);
            double next = st.scalar + ctx.dt * wc * (inp - st.scalar);
            st.scalar = next;
            st.output = inp - next;
        } else {
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            if ((int)st.vec2.size() != w) {
                st.vec2.assign(w, 0.0);
            }
            for (int i = 0; i < w; ++i) {
                double inp = sigAt(u, i);
                st.vec[i] += ctx.dt * wc * (inp - st.vec[i]);
                st.vec2[i] = inp - st.vec[i];
            }
        }
        return false;
    }
    if (phase == Phase::DERIVATIVE) {
        // Continuous state derivative: x' = wc*(u - x) (same state as lpf; the
        // high-pass output y = u - x is formed in the OUTPUT phase).
        double* xdot = blockXdot(ctx, b.nid);
        if (xdot) {
            nflow::BlockDescriptor bd(b, ctx.variables);
            double fc = std::max(0.0, bd.paramDouble(nflow::kCutoff, 0.0));
            double wc = 2.0 * M_PI * fc;
            double* xs = blockX(ctx, b.nid);
            if (w <= 1) {
                double x = xs ? xs[0] : st.scalar;
                xdot[0] = wc * (getInput(ctx, b.nid, 0, 0.0) - x);
            } else {
                SigView u = getInputSig(ctx, b.nid, 0);
                for (int i = 0; i < w; ++i) {
                    double x = xs ? xs[i] : (i < (int)st.vec.size() ? st.vec[i] : 0.0);
                    xdot[i] = wc * (sigAt(u, i) - x);
                }
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
// One emitter for C and Rust (CodegenLang); they differ only in state-field access.
static Nelson::NFlow::BlockCodegenTemplate
makeHpfCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    // Forward-Euler on the internal state x' = wc*(u - x); output y = u_prev - x
    // (matching the interpreter's latched high-pass output). No Tustin.
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("hpf_x_" + a.id, "", "");
        a.addState("hpf_uprev_" + a.id, "", "");
    };
    t.emitStep = [L](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double wc = 2.0 * M_PI * std::fmax(0.0, bd.paramDouble(nflow::kCutoff, 0.0));
        const std::string x = L.sref("hpf_x_" + a.id);
        const std::string uprev = L.sref("hpf_uprev_" + a.id);
        a.line("out_" + a.id + " = " + uprev + " - " + x + ";");
        a.line(x + " = " + x + " + dt * " + a.fmt(wc) + " * (" + a.in[0] + " - " + x + ");");
        a.line(uprev + " = " + a.in[0] + ";");
    };
    // Variable-step codegen: the high-pass is a single continuous state
    // x' = wc*(u - x) with a direct-feedthrough output y = u - x. The hpf_uprev
    // field is only the fixed-step Euler one-sample latch and is unused here.
    t.continuousWidth = [](const BlockCodegenArgs&) -> int { return 1; };
    t.emitRk4 = [L](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double wc = 2.0 * M_PI * std::fmax(0.0, bd.paramDouble(nflow::kCutoff, 0.0));
        const std::string off = std::to_string(a.stateOffset);
        const std::string x = L.sref("hpf_x_" + a.id);
        switch (a.rk4Op) {
        case Rk4Gather:
            a.line(a.rk4Arr + "[" + off + "] = " + x + ";");
            break;
        case Rk4Scatter:
            a.line(x + " = " + a.rk4Arr + "[" + off + "];");
            break;
        case Rk4Output:
            a.line("out_" + a.id + " = " + a.in[0] + " - " + x + ";");
            break;
        case Rk4Deriv:
            a.line(a.rk4Arr + "[" + off + "] = " + a.fmt(wc) + " * (" + a.in[0] + " - " + x + ");");
            break;
        default:
            break;
        }
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCHpf()
{
    return makeHpfCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustHpf()
{
    return makeHpfCodegen({ true });
}
//=============================================================================
