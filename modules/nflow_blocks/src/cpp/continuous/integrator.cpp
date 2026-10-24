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
#include "EdgeDetect.hpp"
#include <string>
#include <cmath>
#include <algorithm>
#include "continuous_blocks.hpp"
//=============================================================================
static inline double
holdAtLimit(double x, double u, double mn, double mx);
//=============================================================================
bool
Nelson::NFlow::handleIntegrator(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    nflow::BlockDescriptor bd(b, ctx.variables);
    const int w = outputWidth(ctx, b.nid, 0);
    const double mn = bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity());
    const double mx = bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity());
    // The extra ports follow the signal: reset first (when there is one), then
    // the initial condition. 'level' holds the state at the initial condition
    // while the line is non-zero; the edge kinds reset once, on the edge, under
    // the rule in EdgeDetect.hpp.
    const std::string extReset = bd.paramStr("ExternalReset", "none");
    const int resetEdge = edgeTypeOf(extReset);
    const bool levelReset = (extReset == "level");
    const bool hasReset = (resetEdge >= 0) || levelReset;
    const bool externalIC = (bd.paramStr("InitialConditionSource", "internal") == "external");
    const int resetPort = hasReset ? 1 : -1;
    const int icPort = externalIC ? (hasReset ? 2 : 1) : -1;
    // The wired signal when the initial condition is external, the parameter
    // otherwise (scalar broadcast or per element).
    auto initialOf = [&](int i) -> double {
        if (icPort >= 0 && hasInput(ctx, b.nid, icPort)) {
            SigView v = getInputSig(ctx, b.nid, icPort);
            return sigAt(v, i);
        }
        if (w <= 1) {
            return bd.paramDouble(nflow::kInitial, 0.0);
        }
        const std::vector<double> list = bd.paramList(nflow::kInitial);
        if (list.empty()) {
            return 0.0;
        }
        return (i < (int)list.size()) ? list[i] : list.back();
    };
    const double resetLine = (hasReset && hasInput(ctx, b.nid, resetPort))
        ? getInput(ctx, b.nid, resetPort, 0.0)
        : 0.0;
    const bool holding = levelReset && (resetLine != 0.0);
    const bool resetNow = holding
        || (resetEdge >= 0 && st.resetPrimed && edgeFired(st.resetPrev, resetLine, resetEdge));
    if (phase == Phase::INIT) {
        st.resetPrev = 0.0;
        st.resetPrimed = false;
        if (w <= 1) {
            double ini = bd.paramDouble(nflow::kInitial, 0.0);
            st.scalar = clampVal(ini, mn, mx);
        } else {
            // Initial is a scalar (broadcast) or a list of the signal width.
            std::vector<double> iniList = bd.paramList(nflow::kInitial);
            st.vec.assign(w, 0.0);
            for (int i = 0; i < w; ++i) {
                double ini = iniList.empty()
                    ? 0.0
                    : (i < (int)iniList.size() ? iniList[i] : iniList.back());
                st.vec[i] = clampVal(ini, mn, mx);
            }
        }
        return false;
    }
    if (phase == Phase::OUTPUT) {
        // A variable-step solver supplies the scattered global state; otherwise
        // the block's latched state is used. The output is clamped to [min, max]
        // in case the solver's raw state drifts slightly past a bound.
        // A reset in effect this step reports the initial condition. It follows
        // the decision ALGEBRAIC stamped for this step, never a fresh one: the
        // first OUTPUT pass can see a stale reset line. The solver re-runs OUTPUT
        // after the algebraic solve, so this cannot live in ALGEBRAIC alone.
        if (hasReset && !ctx.sideEffectFree && st.resetStamp == ctx.stepIndex && st.resetActive) {
            if (w <= 1) {
                setOutput(ctx, b.nid, clampVal(initialOf(0), mn, mx));
            } else {
                double* yr = outputSlice(ctx, b.nid, 0);
                for (int i = 0; i < w; ++i) {
                    yr[i] = clampVal(initialOf(i), mn, mx);
                }
            }
            return false;
        }
        double* xs = blockX(ctx, b.nid);
        if (w <= 1) {
            setOutput(ctx, b.nid, clampVal(xs ? xs[0] : st.scalar, mn, mx));
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            for (int i = 0; i < w; ++i) {
                y[i] = clampVal(xs ? xs[i] : (i < (int)st.vec.size() ? st.vec[i] : 0.0), mn, mx);
            }
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // The reset line is settled here: decide, stamp the decision for this
        // step, and let OUTPUT and UPDATE follow it.
        if (!hasReset || ctx.sideEffectFree) {
            return false;
        }
        st.resetActive = resetNow;
        st.resetStamp = ctx.stepIndex;
        if (!resetNow) {
            return false;
        }
        if (w <= 1) {
            setOutput(ctx, b.nid, clampVal(initialOf(0), mn, mx));
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            for (int i = 0; i < w; ++i) {
                y[i] = clampVal(initialOf(i), mn, mx);
            }
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        // Under a solver the state lives in the global vector; this pass then
        // runs only for a reset integrator, to put the initial condition back.
        // The global slice must be written too, or the reset is discarded on the
        // next step.
        double* xs = blockX(ctx, b.nid);
        const bool solverOwnsState = (xs != nullptr);
        // Follows the ALGEBRAIC decision so the state and the output agree.
        const bool doReset = (st.resetStamp == ctx.stepIndex) ? st.resetActive : resetNow;
        // After an edge reset the fixed-step path (this pass produces the NEXT
        // sample) integrates the step from the initial condition. A held level
        // stays put. Under a solver the step is integrated by the solver, so
        // nothing is added here.
        SigView u = getInputSig(ctx, b.nid, 0);
        for (int i = 0; i < w; ++i) {
            const double inp = sigAt(u, i);
            double next;
            if (doReset) {
                const double base = clampVal(initialOf(i), mn, mx);
                next = (solverOwnsState || holding) ? base : clampVal(base + ctx.dt * inp, mn, mx);
                if (xs) {
                    xs[i] = base;
                }
            } else if (!solverOwnsState) {
                const double cur
                    = (w <= 1) ? st.scalar : ((i < (int)st.vec.size()) ? st.vec[i] : 0.0);
                next = clampVal(cur + ctx.dt * inp, mn, mx);
            } else {
                continue; // the solver owns the state and nothing resets it
            }
            if (w <= 1) {
                st.scalar = next;
            } else {
                if ((int)st.vec.size() != w) {
                    st.vec.assign(w, 0.0);
                }
                st.vec[i] = next;
            }
        }
        if (hasReset) {
            st.resetPrev = edgeZeroSideLatch(st.resetPrev, resetLine, st.resetPrimed);
            st.resetPrimed = true;
            st.resetHeld = holding;
        }
        return false;
    }
    if (phase == Phase::DERIVATIVE) {
        // x' = u, except that a limited integrator holds its state at an engaged
        // limit: the derivative is 0 while the input pushes further out. The
        // engage/leave instants come from the ZERO_CROSSING surfaces, so the
        // solver never integrates across a saturation kink.
        double* xdot = blockXdot(ctx, b.nid);
        const double* xs = blockX(ctx, b.nid);
        if (xdot) {
            if (levelReset && st.resetHeld) {
                // Held at the initial condition while the reset line is up. The
                // level is latched at the major step, so every stage agrees.
                for (int i = 0; i < w; ++i) {
                    xdot[i] = 0.0;
                }
                return false;
            }
            if (w <= 1) {
                const double u = getInput(ctx, b.nid, 0, 0.0);
                const double x = xs ? xs[0] : st.scalar;
                xdot[0] = holdAtLimit(x, u, mn, mx);
            } else {
                SigView u = getInputSig(ctx, b.nid, 0);
                for (int i = 0; i < w; ++i) {
                    const double xi = xs ? xs[i] : (i < (int)st.vec.size() ? st.vec[i] : 0.0);
                    xdot[i] = holdAtLimit(xi, sigAt(u, i), mn, mx);
                }
            }
        }
        return false;
    }
    if (phase == Phase::ZERO_CROSSING) {
        // Two surfaces per element: x - min and x - max. An unbounded integrator
        // has no surfaces (see zeroCrossingCount), so g may be zero-length.
        if (!std::isfinite(mn) && !std::isfinite(mx)) {
            return false;
        }
        double* g = blockG(ctx, b.nid);
        const double* xs = blockX(ctx, b.nid);
        if (g) {
            for (int i = 0; i < w; ++i) {
                const double xi = xs
                    ? xs[i]
                    : (w <= 1 ? st.scalar : (i < (int)st.vec.size() ? st.vec[i] : 0.0));
                g[2 * i] = std::isfinite(mn) ? (xi - mn) : 1.0;
                g[2 * i + 1] = std::isfinite(mx) ? (xi - mx) : 1.0;
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
static inline double
holdAtLimit(double x, double u, double mn, double mx)
{
    if (x >= mx && u > 0.0) {
        return 0.0;
    }
    if (x <= mn && u < 0.0) {
        return 0.0;
    }
    return u;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCIntegrator()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double initVal = bd.paramDouble(nflow::kInitial, 0.0);
        // Unbounded by default; a [0, 0] default would clamp the state to zero.
        double minVal = bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity());
        double maxVal = bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity());
        a.addState("int_" + a.id,
            "fmin(fmax(" + nflow::formatNumber(initVal) + ", " + nflow::formatNumber(minVal) + "), "
                + nflow::formatNumber(maxVal) + ")",
            "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        // Unbounded by default; a [0, 0] default would clamp the state to zero.
        std::string minVal = nflow::formatNumber(
            bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity()));
        std::string maxVal = nflow::formatNumber(
            bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity()));
        // Output-then-update: the output is the pre-step state.
        a.line("out_" + a.id + " = s->int_" + a.id + ";");
        a.line("s->int_" + a.id + " = fmin(fmax(s->int_" + a.id + " + " + a.in[0] + " * dt, "
            + minVal + "), " + maxVal + ");");
    };
    // A bounded integrator joins too: the derivative holds at an engaged limit
    // and the state is clamped after the step. Only the crossing instant differs
    // from the simulator's zero-crossing localization, by O(dt).
    t.continuousWidth = [](const BlockCodegenArgs&) -> int { return 1; };
    t.emitRk4 = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double mn = bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity());
        const double mx = bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity());
        const bool bounded = std::isfinite(mn) || std::isfinite(mx);
        const std::string off = std::to_string(a.stateOffset);
        const std::string x = "s->int_" + a.id;
        const std::string mnS = nflow::formatNumber(mn);
        const std::string mxS = nflow::formatNumber(mx);
        switch (a.rk4Op) {
        case Rk4Gather:
            a.line(a.rk4Arr + "[" + off + "] = " + x + ";");
            break;
        case Rk4Scatter:
            a.line(x + " = " + a.rk4Arr + "[" + off + "];");
            break;
        case Rk4Output:
            if (bounded) {
                a.line("out_" + a.id + " = fmin(fmax(" + x + ", " + mnS + "), " + mxS + ");");
            } else {
                a.line("out_" + a.id + " = " + x + ";");
            }
            break;
        case Rk4Deriv: {
            const std::string u = a.in[0];
            if (bounded) {
                a.line(a.rk4Arr + "[" + off + "] = (" + x + " >= " + mxS + " && " + u + " > 0.0) "
                    + "? 0.0 : ((" + x + " <= " + mnS + " && " + u + " < 0.0) ? 0.0 : " + u + ");");
            } else {
                a.line(a.rk4Arr + "[" + off + "] = " + u + ";");
            }
            break;
        }
        case Rk4Clamp:
            if (bounded) {
                a.line(x + " = fmin(fmax(" + x + ", " + mnS + "), " + mxS + ");");
            }
            break;
        case Rk4Crossing: {
            // x - min and x - max; an infinite limit never crosses (constant 1.0).
            const std::string co = std::to_string(a.crossingOffset);
            const std::string co1 = std::to_string(a.crossingOffset + 1);
            a.line(a.rk4Arr + "[" + co + "] = "
                + (std::isfinite(mn) ? ("(" + x + ") - " + mnS) : std::string("1.0")) + ";");
            a.line(a.rk4Arr + "[" + co1 + "] = "
                + (std::isfinite(mx) ? ("(" + x + ") - " + mxS) : std::string("1.0")) + ";");
            break;
        }
        default:
            break;
        }
    };
    t.crossingCount = [](const BlockCodegenArgs& a) -> int {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double mn = bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity());
        const double mx = bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity());
        return (std::isfinite(mn) || std::isfinite(mx)) ? 2 : 0;
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustIntegrator()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double iv = bd.paramDouble(nflow::kInitial, 0.0);
        double mn = bd.paramDouble(nflow::kMin, -1e308);
        double mx = bd.paramDouble(nflow::kMax, 1e308);
        a.addState("int_" + a.id,
            "libm::fmin(libm::fmax(" + a.fmt(iv) + ", " + a.fmt(mn) + "), " + a.fmt(mx) + ")", "");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double mn = bd.paramDouble(nflow::kMin, -1e308);
        double mx = bd.paramDouble(nflow::kMax, 1e308);
        a.line("out_" + a.id + " = s.int_" + a.id + ";");
        a.line("s.int_" + a.id + " = libm::fmin(libm::fmax(s.int_" + a.id + " + dt * " + a.in[0]
            + ", " + a.fmt(mn) + "), " + a.fmt(mx) + ");");
    };
    t.continuousWidth = [](const BlockCodegenArgs&) -> int { return 1; };
    t.emitRk4 = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double mn = bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity());
        const double mx = bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity());
        const bool bounded = std::isfinite(mn) || std::isfinite(mx);
        const std::string off = std::to_string(a.stateOffset);
        const std::string x = "s.int_" + a.id;
        // Rust has no INFINITY literal: an open bound uses the 1e308 sentinel.
        const std::string mnS = a.fmt(std::isfinite(mn) ? mn : -1e308);
        const std::string mxS = a.fmt(std::isfinite(mx) ? mx : 1e308);
        switch (a.rk4Op) {
        case Rk4Gather:
            a.line(a.rk4Arr + "[" + off + "] = " + x + ";");
            break;
        case Rk4Scatter:
            a.line(x + " = " + a.rk4Arr + "[" + off + "];");
            break;
        case Rk4Output:
            if (bounded) {
                a.line("out_" + a.id + " = libm::fmin(libm::fmax(" + x + ", " + mnS + "), " + mxS
                    + ");");
            } else {
                a.line("out_" + a.id + " = " + x + ";");
            }
            break;
        case Rk4Deriv: {
            const std::string u = a.in[0];
            if (bounded) {
                a.line(a.rk4Arr + "[" + off + "] = if " + x + " >= " + mxS + " && " + u
                    + " > 0.0 { 0.0 } else if " + x + " <= " + mnS + " && " + u
                    + " < 0.0 { 0.0 } else { " + u + " };");
            } else {
                a.line(a.rk4Arr + "[" + off + "] = " + u + ";");
            }
            break;
        }
        case Rk4Clamp:
            if (bounded) {
                a.line(x + " = libm::fmin(libm::fmax(" + x + ", " + mnS + "), " + mxS + ");");
            }
            break;
        case Rk4Crossing: {
            // x - min and x - max; an infinite limit is the constant 1.0.
            const std::string co = std::to_string(a.crossingOffset);
            const std::string co1 = std::to_string(a.crossingOffset + 1);
            a.line(a.rk4Arr + "[" + co + "] = "
                + (std::isfinite(mn) ? ("(" + x + ") - " + mnS) : std::string("1.0")) + ";");
            a.line(a.rk4Arr + "[" + co1 + "] = "
                + (std::isfinite(mx) ? ("(" + x + ") - " + mxS) : std::string("1.0")) + ";");
            break;
        }
        default:
            break;
        }
    };
    t.crossingCount = [](const BlockCodegenArgs& a) -> int {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double mn = bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity());
        const double mx = bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity());
        return (std::isfinite(mn) || std::isfinite(mx)) ? 2 : 0;
    };
    return t;
}
//=============================================================================
