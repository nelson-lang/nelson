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
#include "StateSpaceMatrices.hpp"
#include <cmath>
#include <algorithm>
#include "discrete_blocks.hpp"
//=============================================================================
// MIMO form: x(k+1) = A x(k) + B u(k), y(k) = C x(k) + D u(k). The state lives
// in st.vec (nx entries) and the emitted output in st.outLatch (ny), advanced on
// the block's own sample hits like every other discrete block here.
static bool
dstateSpaceMimo(
    Nelson::NFlow::SimCtx& ctx, const Nelson::NFlow::Block& b, Nelson::NFlow::Phase phase)
{
    using namespace Nelson::NFlow;
    auto& st = getState(ctx, b.nid);
    const int nu = (numInputs(ctx, b.nid) > 0) ? std::max(1, b.inWidth(0)) : 1;
    const SsMatrices m = ssBuild(b, ctx.variables, nu);
    if (phase == Phase::INIT) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        st.dTs = std::max(0.001, bd.paramDouble(nflow::kTs, 0.1));
        st.dNextTime = 0.0;
        st.vec = ssInitialStateOf(bd, m.nx);
        st.outLatch.assign((size_t)m.ny, 0.0);
        st.output = 0.0;
        return false;
    }
    if ((int)st.vec.size() != m.nx) {
        st.vec.assign((size_t)m.nx, 0.0);
    }
    if ((int)st.outLatch.size() != m.ny) {
        st.outLatch.assign((size_t)m.ny, 0.0);
    }
    if (phase == Phase::ALGEBRAIC) {
        // y(k) = C x(k) + D u(k): the state as it stands and the input of THIS
        // step, like the scalar form.
        SigView u = getInputSig(ctx, b.nid, 0);
        double* y = outputSlice(ctx, b.nid, 0);
        const int wy = std::min(m.ny, std::max(1, outputWidth(ctx, b.nid, 0)));
        std::vector<double> tmp((size_t)m.ny, 0.0);
        ssEmit(m, st.vec.data(), u, tmp.data());
        for (int i = 0; i < wy; ++i) {
            y[i] = tmp[(size_t)i];
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        if (ctx.t + 1e-9 >= st.dNextTime) {
            SigView u = getInputSig(ctx, b.nid, 0);
            ssEmit(m, st.vec.data(), u, st.outLatch.data());
            std::vector<double> next((size_t)m.nx, 0.0);
            ssAdvance(m, st.vec.data(), u, next.data());
            st.vec = next;
            st.dNextTime = ctx.t + st.dTs;
            st.output = st.outLatch.empty() ? 0.0 : st.outLatch[0];
        }
        return false;
    }
    return false;
}
//=============================================================================
bool
Nelson::NFlow::handleDstateSpace(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    // Two forms share this block. Scalar A/B/C/D is applied INDEPENDENTLY per signal
    // element, so a width-3 input carries three unrelated first-order states. A
    // matrix anywhere makes it a MIMO state space (A is nx x nx, B nx x nu, C ny x nx,
    // D ny x nu), read exactly as the continuous stateSpace reads it.
    if (!ssIsScalarForm(b, ctx.variables)) {
        return dstateSpaceMimo(ctx, b, phase);
    }
    // Element-wise (scalar A/B/C/D applied independently per element): st.vec
    // is the per-element state, st.outLatch the per-element output.
    const int w = std::max(1, outputWidth(ctx, b.nid, 0));
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        nflow::BlockDescriptor bd(b, ctx.variables);
        st.dTs = std::max(0.001, bd.paramDouble(nflow::kTs, 0.1));
        st.dNextTime = 0.0;
        st.dLastOut = 0.0;
        st.output = 0.0;
        st.vec.assign(w, 0.0);
        st.outLatch.assign(w, 0.0);
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // y(k) = C x(k) + D u(k): the state as it stands and the input of THIS step.
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double C_ = bd.paramDouble(nflow::kC, 0.0);
        const double D_ = bd.paramDouble(nflow::kD, 0.0);
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int e) {
            const double x = (e < (int)st.vec.size()) ? st.vec[e] : 0.0;
            return C_ * x + D_ * sigAt(u, e);
        });
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double A_ = bd.paramDouble(nflow::kA, 0.0);
        double B_ = bd.paramDouble(nflow::kB, 0.0);
        double C_ = bd.paramDouble(nflow::kC, 0.0);
        double D_ = bd.paramDouble(nflow::kD, 0.0);
        double ts_v = std::max(0.001, bd.paramDouble(nflow::kTs, st.dTs));
        st.dTs = ts_v;
        if (ctx.t + 1e-9 >= st.dNextTime) {
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            if ((int)st.outLatch.size() != w) {
                st.outLatch.assign(w, 0.0);
            }
            SigView u = getInputSig(ctx, b.nid, 0);
            for (int e = 0; e < w; ++e) {
                // Emit first (ALGEBRAIC read x(k)), advance after: x(k+1) = A x(k) + B u(k).
                st.outLatch[e] = C_ * st.vec[e] + D_ * sigAt(u, e);
                st.vec[e] = A_ * st.vec[e] + B_ * sigAt(u, e);
            }
            st.dNextTime = ctx.t + ts_v;
            st.output = st.outLatch.empty() ? 0.0 : st.outLatch[0];
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCDstateSpace()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("dss_x_" + a.id, "", "");
        a.addState("dss_next_" + a.id, "", "");
        a.addState("dss_last_" + a.id, "", "");
    };
    // No emitOutput: y(k) = C x(k) + D u(k) reads the CURRENT input, so it is
    // formed from the state as it stands and the state advances afterwards.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double A = bd.paramDouble(nflow::kA, 0.0);
        double B = bd.paramDouble(nflow::kB, 0.0);
        double C = bd.paramDouble(nflow::kC, 0.0);
        double D = bd.paramDouble(nflow::kD, 0.0);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        a.line("if (t + 1e-6 >= s->dss_next_" + a.id + ") {");
        a.line("  s->dss_last_" + a.id + " = " + a.fmt(C) + " * s->dss_x_" + a.id + " + " + a.fmt(D)
            + " * " + a.in[0] + ";");
        a.line("  s->dss_x_" + a.id + " = " + a.fmt(A) + " * s->dss_x_" + a.id + " + " + a.fmt(B)
            + " * " + a.in[0] + ";");
        a.line("  s->dss_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("out_" + a.id + " = s->dss_last_" + a.id + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustDstateSpace()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("dss_x_" + a.id, "", "");
        a.addState("dss_next_" + a.id, "", "");
        a.addState("dss_last_" + a.id, "", "");
    };
    // Same ordering as the C template: emit from x(k), then advance.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double A = bd.paramDouble(nflow::kA, 0.0);
        double B = bd.paramDouble(nflow::kB, 0.0);
        double C = bd.paramDouble(nflow::kC, 0.0);
        double D = bd.paramDouble(nflow::kD, 0.0);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        a.line("if t + 1e-6 >= s.dss_next_" + a.id + " {");
        a.line("    s.dss_last_" + a.id + " = " + a.fmt(C) + " * s.dss_x_" + a.id + " + " + a.fmt(D)
            + " * " + a.in[0] + ";");
        a.line("    s.dss_x_" + a.id + " = " + a.fmt(A) + " * s.dss_x_" + a.id + " + " + a.fmt(B)
            + " * " + a.in[0] + ";");
        a.line("    s.dss_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("out_" + a.id + " = s.dss_last_" + a.id + ";");
    };
    return t;
}
//=============================================================================
