//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// repeatingSequenceInterpolated: a periodic, piecewise-linear source (Repeating
// Sequence Interpolated). The (TimeValues, OutValues) table defines one period
// (period = last TimeValues entry); the output linearly interpolates the table
// at t wrapped into [0, period) and repeats. No input; scalar output; stateless
// (a pure function of time). C / Rust code generation via an unrolled ternary
// chain over the baked breakpoints.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <vector>
#include <string>
#include "source_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Evaluate the periodic piecewise-linear table at absolute time t.
    static double
    rsiEval(const std::vector<double>& tv, const std::vector<double>& ov, double t)
    {
        const int n = static_cast<int>(std::min(tv.size(), ov.size()));
        if (n == 0) {
            return 0.0;
        }
        if (n == 1) {
            return ov[0];
        }
        const double period = tv[n - 1];
        double tm = t;
        if (period > 0.0) {
            tm = std::fmod(t, period);
            if (tm < 0.0) {
                tm += period;
            }
        } else {
            return ov[0];
        }
        for (int i = 0; i < n - 1; ++i) {
            if (tm <= tv[i + 1]) {
                const double w = tv[i + 1] - tv[i];
                if (w <= 0.0) {
                    return ov[i];
                }
                return ov[i] + (ov[i + 1] - ov[i]) * (tm - tv[i]) / w;
            }
        }
        return ov[n - 1];
    }
    //=============================================================================
    bool
    handleRepeatingSequenceInterpolated(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::OUTPUT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::vector<double> tv = bd.paramList("TimeValues");
        const std::vector<double> ov = bd.paramList("OutValues");
        setOutput(ctx, b.nid, rsiEval(tv, ov, ctx.t));
        return false;
    }
    //=============================================================================
    // Build the unrolled interpolation for the wrapped-time variable `tm`, with
    // `f(x)` producing the numeric literal for a value (nflow::formatNumber / Rust
    // a.fmt). `elseVal` is the fallback literal for tm past the last breakpoint.
    static std::string
    rsiChain(const std::vector<double>& tv, const std::vector<double>& ov, const std::string& tm,
        std::string (*f)(double))
    {
        const int n = static_cast<int>(std::min(tv.size(), ov.size()));
        std::string expr = f(ov[n - 1]);
        for (int i = n - 2; i >= 0; --i) {
            const double w = tv[i + 1] - tv[i];
            const double m = (w > 0.0) ? (ov[i + 1] - ov[i]) / w : 0.0;
            // ov_i + m * (tm - tv_i)
            const std::string seg
                = "(" + f(ov[i]) + " + " + f(m) + " * (" + tm + " - " + f(tv[i]) + "))";
            expr = "((" + tm + " <= " + f(tv[i + 1]) + ") ? " + seg + " : " + expr + ")";
        }
        return expr;
    }
    //=============================================================================
    static std::string
    fmtC(double v)
    {
        return nflow::formatNumber(v);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCRepeatingSequenceInterpolated()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::vector<double> tv = bd.paramList("TimeValues");
            const std::vector<double> ov = bd.paramList("OutValues");
            const int n = static_cast<int>(std::min(tv.size(), ov.size()));
            if (n == 0) {
                a.line("out_" + a.id + " = 0.0;");
                return;
            }
            if (n == 1 || tv[n - 1] <= 0.0) {
                a.line("out_" + a.id + " = " + nflow::formatNumber(ov[0]) + ";");
                return;
            }
            const std::string per = nflow::formatNumber(tv[n - 1]);
            const std::string tm = "rsi_tm_" + a.id;
            a.line("double " + tm + " = fmod(t, " + per + ");");
            a.line("if (" + tm + " < 0.0) " + tm + " += " + per + ";");
            a.line("out_" + a.id + " = " + rsiChain(tv, ov, tm, fmtC) + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustRepeatingSequenceInterpolated()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::vector<double> tv = bd.paramList("TimeValues");
            const std::vector<double> ov = bd.paramList("OutValues");
            const int n = static_cast<int>(std::min(tv.size(), ov.size()));
            auto rfmt = [&a](double v) { return a.fmt(v); };
            if (n == 0) {
                a.line("out_" + a.id + " = 0.0_f64;");
                return;
            }
            if (n == 1 || tv[n - 1] <= 0.0) {
                a.line("out_" + a.id + " = " + a.fmt(ov[0]) + ";");
                return;
            }
            const std::string per = a.fmt(tv[n - 1]);
            const std::string tm = "rsi_tm_" + a.id;
            a.line("let mut " + tm + ": f64 = t % " + per + ";");
            a.line("if " + tm + " < 0.0_f64 { " + tm + " += " + per + "; }");
            const int nn = n;
            std::string expr = a.fmt(ov[nn - 1]);
            for (int i = nn - 2; i >= 0; --i) {
                const double w = tv[i + 1] - tv[i];
                const double m = (w > 0.0) ? (ov[i + 1] - ov[i]) / w : 0.0;
                const std::string seg = "(" + a.fmt(ov[i]) + " + " + a.fmt(m) + " * (" + tm + " - "
                    + a.fmt(tv[i]) + "))";
                expr = "if " + tm + " <= " + a.fmt(tv[i + 1]) + " { " + seg + " } else { " + expr
                    + " }";
            }
            a.line("out_" + a.id + " = " + expr + ";");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
