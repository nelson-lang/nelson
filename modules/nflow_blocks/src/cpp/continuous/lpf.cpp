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
Nelson::NFlow::handleLpf(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        st.output = 0.0;
        if (w > 1) {
            st.vec.assign(w, 0.0);
        }
        return false;
    }
    if (phase == Phase::OUTPUT) {
        // Variable-step minor step: y = x (the low-pass state) read from the
        // scattered global state; else the latched value (fixed-step path).
        double* xs = blockX(ctx, b.nid);
        if (w <= 1) {
            setOutput(ctx, b.nid, xs ? xs[0] : st.output);
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            for (int i = 0; i < w; ++i) {
                y[i] = xs ? xs[i] : (i < (int)st.vec.size() ? st.vec[i] : 0.0);
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
            st.output = next;
        } else {
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            for (int i = 0; i < w; ++i) {
                st.vec[i] += ctx.dt * wc * (sigAt(u, i) - st.vec[i]);
            }
        }
        return false;
    }
    if (phase == Phase::DERIVATIVE) {
        // Continuous state derivative: x' = wc*(u - x).
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
makeLpfCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    // Forward-Euler on x' = wc*(u - x), matching the interpreter.
    // Output-then-update reproduces its one-sample-latched output timing.
    t.emitState = [](const BlockCodegenStateArgs& a) { a.addState("lpf_x_" + a.id, "", ""); };
    t.emitStep = [L](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double wc = 2.0 * M_PI * std::fmax(0.0, bd.paramDouble(nflow::kCutoff, 0.0));
        const std::string x = L.sref("lpf_x_" + a.id);
        a.line("out_" + a.id + " = " + x + ";");
        a.line(x + " = " + x + " + dt * " + a.fmt(wc) + " * (" + a.in[0] + " - " + x + ");");
    };
    // Variable-step codegen: one continuous state x' = wc*(u - x), output y = x,
    // part of the shared RK4 rhs.
    t.continuousWidth = [](const BlockCodegenArgs&) -> int { return 1; };
    t.emitRk4 = [L](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double wc = 2.0 * M_PI * std::fmax(0.0, bd.paramDouble(nflow::kCutoff, 0.0));
        const std::string off = std::to_string(a.stateOffset);
        const std::string x = L.sref("lpf_x_" + a.id);
        switch (a.rk4Op) {
        case Rk4Gather:
            a.line(a.rk4Arr + "[" + off + "] = " + x + ";");
            break;
        case Rk4Scatter:
            a.line(x + " = " + a.rk4Arr + "[" + off + "];");
            break;
        case Rk4Output:
            a.line("out_" + a.id + " = " + x + ";");
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
Nelson::NFlow::getCodeGenCLpf()
{
    return makeLpfCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustLpf()
{
    return makeLpfCodegen({ true });
}
//=============================================================================
