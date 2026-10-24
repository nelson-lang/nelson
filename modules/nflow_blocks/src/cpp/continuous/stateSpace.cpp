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
#include <string>
#include <vector>
#include "continuous_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Row-major state-space matrices: A is nx x nx, B is nx x nu, C is ny x nx,
    // D is ny x nu. A scalar param (SISO) yields nx = nu = ny = 1.
    struct SsMatrices
    {
        int nx = 1, nu = 1, ny = 1;
        std::vector<double> A, B, C, D; // row-major
    };
    //=============================================================================
    int
    stateSpaceStateCount(const Block& b, const ValMap& vars)
    {
        nflow::BlockDescriptor bd(b, vars);
        std::vector<double> a = bd.paramList(nflow::kA);
        if (a.empty()) {
            return 1;
        }
        const int nx = (int)std::llround(std::sqrt((double)a.size()));
        return std::max(1, nx);
    }
    //=============================================================================
    // Output width for dimension propagation: ny = rows(C) = len(C) / nx.
    int
    stateSpaceOutputWidth(const Block& b, const ValMap& vars)
    {
        nflow::BlockDescriptor bd(b, vars);
        const int nx = stateSpaceStateCount(b, vars);
        std::vector<double> c = bd.paramList(nflow::kC);
        if (c.empty() || nx <= 0) {
            return 1;
        }
        return std::max(1, (int)c.size() / nx);
    }
    //=============================================================================
    // Initial continuous state: a scalar broadcasts over the nx states, a list
    // gives them one by one, an absent parameter means all zeros.
    static std::vector<double>
    ssInitialState(const nflow::BlockDescriptor& bd, int nx)
    {
        std::vector<double> out((size_t)std::max(0, nx), 0.0);
        const std::vector<double> x0 = bd.paramList(nflow::kInitial);
        if (x0.empty()) {
            return out;
        }
        for (int i = 0; i < nx; ++i) {
            out[(size_t)i] = (i < (int)x0.size()) ? x0[(size_t)i] : x0.back();
        }
        return out;
    }
    //=============================================================================
    static SsMatrices
    buildSs(const Block& b, const ValMap& vars, int nu)
    {
        nflow::BlockDescriptor bd(b, vars);
        SsMatrices m;
        m.A = bd.paramList(nflow::kA);
        m.B = bd.paramList(nflow::kB);
        m.C = bd.paramList(nflow::kC);
        m.D = bd.paramList(nflow::kD);
        if (m.A.empty()) {
            m.A = { 0.0 };
        }
        m.nx = std::max(1, (int)std::llround(std::sqrt((double)m.A.size())));
        m.nu = std::max(1, nu);
        m.ny = m.C.empty() ? 1 : std::max(1, (int)m.C.size() / m.nx);
        // zero padding keeps the index math in range
        m.A.resize((size_t)m.nx * m.nx, 0.0);
        m.B.resize((size_t)m.nx * m.nu, 0.0);
        m.C.resize((size_t)m.ny * m.nx, 0.0);
        m.D.resize((size_t)m.ny * m.nu, 0.0);
        return m;
    }
    //=============================================================================
    static void
    ssOutput(const SsMatrices& m, const double* x, const SigView& u, double* y)
    {
        for (int i = 0; i < m.ny; ++i) {
            double acc = 0.0;
            for (int j = 0; j < m.nx; ++j) {
                acc += m.C[(size_t)i * m.nx + j] * x[j];
            }
            for (int j = 0; j < m.nu; ++j) {
                acc += m.D[(size_t)i * m.nu + j] * sigAt(u, j);
            }
            y[i] = acc;
        }
    }
    //=============================================================================
    // u comes from a raw latch (nullptr = zeros). Fixed-step OUTPUT runs before
    // the algebraic wave recomputes this step's sources, so the D feedthrough
    // reads the PREVIOUS step's inputs latched by UPDATE.
    static void
    ssOutputU(const SsMatrices& m, const double* x, const double* u, double* y)
    {
        for (int i = 0; i < m.ny; ++i) {
            double acc = 0.0;
            for (int j = 0; j < m.nx; ++j) {
                acc += m.C[(size_t)i * m.nx + j] * x[j];
            }
            for (int j = 0; j < m.nu; ++j) {
                acc += m.D[(size_t)i * m.nu + j] * (u ? u[j] : 0.0);
            }
            y[i] = acc;
        }
    }
    //=============================================================================
    static void
    ssDeriv(const SsMatrices& m, const double* x, const SigView& u, double* xdot)
    {
        for (int i = 0; i < m.nx; ++i) {
            double acc = 0.0;
            for (int j = 0; j < m.nx; ++j) {
                acc += m.A[(size_t)i * m.nx + j] * x[j];
            }
            for (int j = 0; j < m.nu; ++j) {
                acc += m.B[(size_t)i * m.nu + j] * sigAt(u, j);
            }
            xdot[i] = acc;
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
bool
Nelson::NFlow::handleStateSpace(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int nu = numInputs(ctx, b.nid) > 0 ? b.inWidth(0) : 1;
    SsMatrices m = buildSs(b, ctx.variables, nu);

    if (phase == Phase::INIT) {
        // x in [0, nx), then the input latch in [nx, nx+nu); the continuous
        // seeding reads only the first nx entries.
        st.vec.assign((size_t)m.nx + m.nu, 0.0);
        const std::vector<double> x0
            = ssInitialState(nflow::BlockDescriptor(b, ctx.variables), m.nx);
        for (int i = 0; i < m.nx && i < (int)x0.size(); ++i) {
            st.vec[(size_t)i] = x0[(size_t)i];
        }
        st.scalar = 0.0;
        st.output = 0.0;
        return false;
    }
    if (phase == Phase::OUTPUT) {
        SigView u = getInputSig(ctx, b.nid, 0);
        double* xs = blockX(ctx, b.nid);
        std::vector<double> y(m.ny, 0.0);
        if (xs) {
            ssOutput(m, xs, u, y.data());
        } else if ((int)st.vec.size() >= m.nx) {
            // Fixed-step: D reads the previous step's latched inputs, because
            // the live input slots still hold this step's reset values.
            const double* ulat
                = ((int)st.vec.size() >= m.nx + m.nu) ? (st.vec.data() + m.nx) : nullptr;
            ssOutputU(m, st.vec.data(), ulat, y.data());
        }
        double* out = outputSlice(ctx, b.nid, 0);
        // A hand-written diagram declaring extra output ports must not overrun
        // the port-0 buffer.
        const int wlim = std::min(m.ny, std::max(1, b.outWidth(0)));
        for (int i = 0; i < wlim; ++i) {
            out[i] = y[i];
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // Direct feedthrough (D != 0): re-emit y with the CURRENT inputs, since
        // OUTPUT ran before this step's sources settled. A D != 0 loop becomes a
        // real algebraic cycle for the Gauss-Seidel pass. No-op when D is zero.
        bool anyD = false;
        for (double v : m.D) {
            if (v != 0.0) {
                anyD = true;
                break;
            }
        }
        if (!anyD) {
            return false;
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        double* xs = blockX(ctx, b.nid);
        const double* x
            = xs != nullptr ? xs : (((int)st.vec.size() >= m.nx) ? st.vec.data() : nullptr);
        if (x == nullptr) {
            return false;
        }
        std::vector<double> y(m.ny, 0.0);
        ssOutput(m, x, u, y.data());
        double* out = outputSlice(ctx, b.nid, 0);
        const int wlim = std::min(m.ny, std::max(1, b.outWidth(0)));
        bool changed = false;
        for (int i = 0; i < wlim; ++i) {
            if (out[i] != y[i]) {
                out[i] = y[i];
                changed = true;
            }
        }
        return changed;
    }
    if (phase == Phase::UPDATE) {
        // Euler step, then latch u for the next step's OUTPUT feedthrough.
        if ((int)st.vec.size() < m.nx + m.nu) {
            st.vec.resize((size_t)m.nx + m.nu, 0.0);
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        std::vector<double> xdot(m.nx, 0.0);
        ssDeriv(m, st.vec.data(), u, xdot.data());
        for (int i = 0; i < m.nx; ++i) {
            st.vec[i] += ctx.dt * xdot[i];
        }
        for (int j = 0; j < m.nu; ++j) {
            st.vec[(size_t)m.nx + j] = sigAt(u, j);
        }
        return false;
    }
    if (phase == Phase::DERIVATIVE) {
        double* xdot = blockXdot(ctx, b.nid);
        if (xdot) {
            double* xs = blockX(ctx, b.nid);
            const double* x = xs ? xs : st.vec.data();
            SigView u = getInputSig(ctx, b.nid, 0);
            ssDeriv(m, x, u, xdot);
        }
        return false;
    }
    return false;
}
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        // Matrices for the code generator (row-major, zero-padded). nu is the
        // number of wired scalar input ports (the vector expansion turns an
        // nu-wide wire into nu scalar ports), ny the rows of C. A MIMO system
        // emits one output variable per row (out_<id>, out_<id>_p1, ...).
        struct SsCg
        {
            int nx = 1, nu = 1, ny = 1;
            std::vector<double> A, B, C, D;
        };
        //=============================================================================
        static SsCg
        buildSsCg(const BlockCodegenArgs& a)
        {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            SsCg s;
            s.A = bd.paramList(nflow::kA);
            s.B = bd.paramList(nflow::kB);
            s.C = bd.paramList(nflow::kC);
            s.D = bd.paramList(nflow::kD);
            s.nx = s.A.empty() ? 1 : (int)std::llround(std::sqrt((double)s.A.size()));
            if (s.nx < 1) {
                s.nx = 1;
            }
            // A declared but unwired trailing port still counts; its expression
            // falls back to 0.0.
            int declIn = 0;
            if (a.block && a.block->contains(nflow::kInputs)
                && (*a.block)[nflow::kInputs].is_number()) {
                declIn = (*a.block)[nflow::kInputs].get<int>();
            }
            s.nu = std::max(1, std::max(declIn, (int)a.inputIds.size()));
            s.ny = s.C.empty() ? 1 : std::max(1, (int)s.C.size() / s.nx);
            s.A.resize((size_t)s.nx * s.nx, 0.0);
            s.B.resize((size_t)s.nx * s.nu, 0.0);
            s.C.resize((size_t)s.ny * s.nx, 0.0);
            s.D.resize((size_t)s.ny * s.nu, 0.0);
            return s;
        }
        //=============================================================================
        // SISO uses a single state field; nx>1 uses one field per state.
        static std::string
        ssName(const std::string& id, int i, int nx, const std::string& pfx)
        {
            return nx == 1 ? (pfx + "ss_x_" + id) : (pfx + "ss_x_" + id + "_" + std::to_string(i));
        }
        //=============================================================================
        // Output variable of row r: out_<id>, then out_<id>_p<r>.
        static std::string
        ssOutName(const std::string& id, int r)
        {
            return r > 0 ? ("out_" + id + "_p" + std::to_string(r)) : ("out_" + id);
        }
        //=============================================================================
        // "0.0" for a declared but unwired trailing port.
        static std::string
        ssUin(const BlockCodegenArgs& a, int k)
        {
            return ((size_t)k < a.in.size()) ? a.in[(size_t)k] : std::string("0.0");
        }
        //=============================================================================
        static std::string
        ssUlatch(const std::string& id, int k, const std::string& pfx)
        {
            return pfx + "ss_u_" + id + "_" + std::to_string(k);
        }
        //=============================================================================
        // With a nonzero D the runtime re-emits the output during ALGEBRAIC with
        // the CURRENT inputs, so the generated output must read them too.
        static bool
        ssAnyD(const SsCg& s)
        {
            for (double v : s.D) {
                if (v != 0.0) {
                    return true;
                }
            }
            return false;
        }
        //=============================================================================
        // `u` supplies the per-column input expression (current or latched).
        static std::string
        ssOutputExpr(const BlockCodegenArgs& a, const SsCg& s, int r, const std::string& pfx,
            const std::function<std::string(int)>& u)
        {
            std::string e;
            for (int j = 0; j < s.nx; ++j) {
                e += (j ? " + " : "") + a.fmt(s.C[(size_t)r * s.nx + j]) + " * "
                    + ssName(a.id, j, s.nx, pfx);
            }
            for (int k = 0; k < s.nu; ++k) {
                e += " + " + a.fmt(s.D[(size_t)r * s.nu + k]) + " * " + u(k);
            }
            return e;
        }
        //=============================================================================
        static std::string
        ssDerivExpr(const BlockCodegenArgs& a, const SsCg& s, int i, const std::string& pfx)
        {
            std::string e;
            for (int j = 0; j < s.nx; ++j) {
                e += (j ? " + " : "") + a.fmt(s.A[(size_t)i * s.nx + j]) + " * "
                    + ssName(a.id, j, s.nx, pfx);
            }
            for (int k = 0; k < s.nu; ++k) {
                e += " + " + a.fmt(s.B[(size_t)i * s.nu + k]) + " * " + ssUin(a, k);
            }
            return e;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCStateSpace()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        std::vector<double> A = bd.paramList(nflow::kA);
        int nx = A.empty() ? 1 : (int)std::llround(std::sqrt((double)A.size()));
        // Same initial state as INIT, so the generated program starts where the
        // simulation starts.
        const std::vector<double> x0 = ssInitialState(bd, nx);
        if (nx <= 1) {
            a.addState("ss_x_" + a.id, x0.empty() ? "" : a.fmt(x0[0]), "");
        } else {
            for (int i = 0; i < nx; ++i) {
                a.addState("ss_x_" + a.id + "_" + std::to_string(i),
                    (i < (int)x0.size()) ? a.fmt(x0[(size_t)i]) : "", "");
            }
        }
        // Fixed-step outputs read the LATCHED previous-step inputs, because the
        // runtime emits OUTPUT before the algebraic wave recomputes sources.
        const int nu = std::max(1,
            (a.block->contains(nflow::kInputs) && (*a.block)[nflow::kInputs].is_number())
                ? (*a.block)[nflow::kInputs].get<int>()
                : 1);
        // Latch fields only when D is all zero: a live D reads the current
        // input expressions instead.
        std::vector<double> Dv = bd.paramList(nflow::kD);
        bool anyDv = false;
        for (double v : Dv) {
            if (v != 0.0) {
                anyDv = true;
                break;
            }
        }
        for (int k = 0; !anyDv && k < nu; ++k) {
            a.addState("ss_u_" + a.id + "_" + std::to_string(k), "", "");
        }
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        SsCg s = buildSsCg(a);
        // Forward Euler: outputs at the PRE-step state, then advance with the
        // current inputs. The D term reads the CURRENT inputs when D != 0 and the
        // latched previous-step inputs otherwise.
        const bool liveD = ssAnyD(s);
        auto uOut = [&](int k) { return liveD ? ssUin(a, k) : ssUlatch(a.id, k, "s->"); };
        for (int r = 0; r < s.ny; ++r) {
            a.line(ssOutName(a.id, r) + " = " + ssOutputExpr(a, s, r, "s->", uOut) + ";");
        }
        for (int i = 0; i < s.nx; ++i) {
            a.line("double __ssd_" + a.id + "_" + std::to_string(i) + " = "
                + ssDerivExpr(a, s, i, "s->") + ";");
        }
        for (int i = 0; i < s.nx; ++i) {
            a.line(ssName(a.id, i, s.nx, "s->") + " += dt * __ssd_" + a.id + "_" + std::to_string(i)
                + ";");
        }
        if (!liveD) {
            for (int k = 0; k < s.nu; ++k) {
                a.line(ssUlatch(a.id, k, "s->") + " = " + ssUin(a, k) + ";");
            }
        }
    };
    t.continuousWidth = [](const BlockCodegenArgs& a) -> int { return buildSsCg(a).nx; };
    t.emitRk4 = [](const BlockCodegenArgs& a) {
        SsCg s = buildSsCg(a);
        switch (a.rk4Op) {
        case Rk4Gather:
            for (int i = 0; i < s.nx; ++i) {
                a.line(a.rk4Arr + "[" + std::to_string(a.stateOffset + i)
                    + "] = " + ssName(a.id, i, s.nx, "s->") + ";");
            }
            break;
        case Rk4Scatter:
            for (int i = 0; i < s.nx; ++i) {
                a.line(ssName(a.id, i, s.nx, "s->") + " = " + a.rk4Arr + "["
                    + std::to_string(a.stateOffset + i) + "];");
            }
            break;
        case Rk4Output:
            // D != 0: read the live stage inputs (per-stage ALGEBRAIC re-run).
            // D == 0: the latch fields are never written on the RK4 path and
            // stay 0.0, which is harmless since the D term is zero.
            {
                const bool liveD = ssAnyD(s);
                for (int r = 0; r < s.ny; ++r) {
                    a.line(ssOutName(a.id, r) + " = " + ssOutputExpr(a, s, r, "s->", [&](int k) {
                        return liveD ? ssUin(a, k) : ssUlatch(a.id, k, "s->");
                    }) + ";");
                }
            }
            break;
        case Rk4Deriv:
            for (int i = 0; i < s.nx; ++i) {
                a.line(a.rk4Arr + "[" + std::to_string(a.stateOffset + i)
                    + "] = " + ssDerivExpr(a, s, i, "s->") + ";");
            }
            break;
        default:
            break;
        }
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustStateSpace()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        std::vector<double> A = bd.paramList(nflow::kA);
        int nx = A.empty() ? 1 : (int)std::llround(std::sqrt((double)A.size()));
        // Same initial state as INIT, so the generated program starts where the
        // simulation starts.
        const std::vector<double> x0 = ssInitialState(bd, nx);
        if (nx <= 1) {
            a.addState("ss_x_" + a.id, x0.empty() ? "" : a.fmt(x0[0]), "");
        } else {
            for (int i = 0; i < nx; ++i) {
                a.addState("ss_x_" + a.id + "_" + std::to_string(i),
                    (i < (int)x0.size()) ? a.fmt(x0[(size_t)i]) : "", "");
            }
        }
        const int nu = std::max(1,
            (a.block->contains(nflow::kInputs) && (*a.block)[nflow::kInputs].is_number())
                ? (*a.block)[nflow::kInputs].get<int>()
                : 1);
        // Latch fields only when D is all zero: a live D reads the current
        // input expressions instead.
        std::vector<double> Dv = bd.paramList(nflow::kD);
        bool anyDv = false;
        for (double v : Dv) {
            if (v != 0.0) {
                anyDv = true;
                break;
            }
        }
        for (int k = 0; !anyDv && k < nu; ++k) {
            a.addState("ss_u_" + a.id + "_" + std::to_string(k), "", "");
        }
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        SsCg s = buildSsCg(a);
        const bool liveD = ssAnyD(s);
        auto uOut = [&](int k) { return liveD ? ssUin(a, k) : ssUlatch(a.id, k, "s."); };
        for (int r = 0; r < s.ny; ++r) {
            a.line(ssOutName(a.id, r) + " = " + ssOutputExpr(a, s, r, "s.", uOut) + ";");
        }
        for (int i = 0; i < s.nx; ++i) {
            a.line("let __ssd_" + a.id + "_" + std::to_string(i) + " = "
                + ssDerivExpr(a, s, i, "s.") + ";");
        }
        for (int i = 0; i < s.nx; ++i) {
            a.line(ssName(a.id, i, s.nx, "s.") + " += dt * __ssd_" + a.id + "_" + std::to_string(i)
                + ";");
        }
        if (!liveD) {
            for (int k = 0; k < s.nu; ++k) {
                a.line(ssUlatch(a.id, k, "s.") + " = " + ssUin(a, k) + ";");
            }
        }
    };
    t.continuousWidth = [](const BlockCodegenArgs& a) -> int { return buildSsCg(a).nx; };
    t.emitRk4 = [](const BlockCodegenArgs& a) {
        SsCg s = buildSsCg(a);
        switch (a.rk4Op) {
        case Rk4Gather:
            for (int i = 0; i < s.nx; ++i) {
                a.line(a.rk4Arr + "[" + std::to_string(a.stateOffset + i)
                    + "] = " + ssName(a.id, i, s.nx, "s.") + ";");
            }
            break;
        case Rk4Scatter:
            for (int i = 0; i < s.nx; ++i) {
                a.line(ssName(a.id, i, s.nx, "s.") + " = " + a.rk4Arr + "["
                    + std::to_string(a.stateOffset + i) + "];");
            }
            break;
        case Rk4Output:
            // Same as the C emitter.
            {
                const bool liveD = ssAnyD(s);
                for (int r = 0; r < s.ny; ++r) {
                    a.line(ssOutName(a.id, r) + " = " + ssOutputExpr(a, s, r, "s.", [&](int k) {
                        return liveD ? ssUin(a, k) : ssUlatch(a.id, k, "s.");
                    }) + ";");
                }
            }
            break;
        case Rk4Deriv:
            for (int i = 0; i < s.nx; ++i) {
                a.line(a.rk4Arr + "[" + std::to_string(a.stateOffset + i)
                    + "] = " + ssDerivExpr(a, s, i, "s.") + ";");
            }
            break;
        default:
            break;
        }
    };
    return t;
}
//=============================================================================
