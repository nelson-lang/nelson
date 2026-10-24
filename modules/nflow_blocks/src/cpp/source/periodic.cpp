//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Periodic waveform sources: pulse and sawTooth.
//   pulse:    offset + (mod(t-start, period) < width%*period ? amplitude : 0)
//   sawTooth: offset + amplitude * mod(t-start, period) / period
// Both feedthrough (OUTPUT phase) and codegen-eligible (plain time expressions).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include "source_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handlePulse(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::OUTPUT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double amp = bd.paramDouble(nflow::kAmp, 1.0);
        const double period = bd.paramDouble("Period", 1.0);
        const double width = bd.paramDouble("Width", 50.0); // percent of period
        const double start = bd.paramDouble("StartTime", 0.0);
        const double offset = bd.paramDouble("Offset", 0.0);
        double out = offset;
        if (ctx.t >= start && period > 0.0) {
            const double tau = std::fmod(ctx.t - start, period);
            // (width * period) / 100 keeps the boundary exact when the duty cycle lands
            // on a sample: width/100 first rounds up (40/100*1.5 exceeds 0.6) and
            // stretches the pulse by one sample. The epsilon covers the other side:
            // 'tau' loses a few ulps in (t - start) and fmod, so a sample exactly on
            // the falling edge would read as inside the pulse. It is well below any
            // usable step size.
            if (tau < (width * period) / 100.0 - 1e-12 * period) {
                out += amp;
            }
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    bool
    handleSawTooth(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::OUTPUT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double amp = bd.paramDouble(nflow::kAmp, 1.0);
        const double period = bd.paramDouble("Period", 1.0);
        const double start = bd.paramDouble("StartTime", 0.0);
        const double offset = bd.paramDouble("Offset", 0.0);
        double out = offset;
        if (ctx.t >= start && period > 0.0) {
            out += amp * (std::fmod(ctx.t - start, period) / period);
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    bool
    handleExpSine(SimCtx& ctx, const Block& b, Phase phase)
    {
        // Exponentially damped sine: offset + amp e^{-damping (t-start)}
        //                                    sin(2 pi freq (t-start) + phase).
        if (phase != Phase::OUTPUT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double amp = bd.paramDouble(nflow::kAmp, 1.0);
        const double freq = bd.paramDouble(nflow::kFreq, 1.0);
        const double damping = bd.paramDouble("Damping", 0.0);
        const double ph = bd.paramDouble(nflow::kPhase, 0.0);
        const double start = bd.paramDouble("StartTime", 0.0);
        const double offset = bd.paramDouble("Offset", 0.0);
        double out = offset;
        if (ctx.t >= start) {
            const double td = ctx.t - start;
            out += amp * std::exp(-damping * td) * std::sin(2.0 * M_PI * freq * td + ph);
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    bool
    handleTrapezoid(SimCtx& ctx, const Block& b, Phase phase)
    {
        // One period: rising ramp (Rising), plateau (Width) at amplitude,
        // falling ramp (Falling), then 0 until Period; repeats. offset added.
        if (phase != Phase::OUTPUT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double amp = bd.paramDouble(nflow::kAmp, 1.0);
        const double rising = bd.paramDouble("Rising", 0.0);
        const double width = bd.paramDouble("Width", 0.0);
        const double falling = bd.paramDouble("Falling", 0.0);
        const double period = bd.paramDouble("Period", 1.0);
        const double start = bd.paramDouble("StartTime", 0.0);
        const double offset = bd.paramDouble("Offset", 0.0);
        double out = offset;
        if (ctx.t >= start && period > 0.0) {
            const double tau = std::fmod(ctx.t - start, period);
            if (tau < rising && rising > 0.0) {
                out += amp * (tau / rising);
            } else if (tau < rising + width) {
                out += amp;
            } else if (tau < rising + width + falling && falling > 0.0) {
                out += amp * (1.0 - (tau - rising - width) / falling);
            }
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCPulse()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = {param:Offset:0.0} + (((t >= {param:StartTime:0.0}) && "
                 "(fmod(t - {param:StartTime:0.0}, {param:Period:1.0}) < "
                 "({param:Width:50.0} * {param:Period:1.0}) / 100.0 - 1e-12 * {param:Period:1.0})) "
                 "? {param:Amplitude:1.0} "
                 ": 0.0);";
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustPulse()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = {param:Offset:0.0} + if (t >= {param:StartTime:0.0}) && "
                 "(libm::fmod(t - {param:StartTime:0.0}, {param:Period:1.0}) < "
                 "({param:Width:50.0} * {param:Period:1.0}) / 100.0 - 1e-12 * {param:Period:1.0}) "
                 "{ {param:Amplitude:1.0} } "
                 "else { 0.0 };";
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCSawTooth()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = {param:Offset:0.0} + {param:Amplitude:1.0} * "
                 "(fmod(t - {param:StartTime:0.0}, {param:Period:1.0}) / {param:Period:1.0});";
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustSawTooth()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = {param:Offset:0.0} + {param:Amplitude:1.0} * "
                 "(libm::fmod(t - {param:StartTime:0.0}, {param:Period:1.0}) / "
                 "{param:Period:1.0});";
        return t;
    }
    //=============================================================================
    bool
    handleExponentials(SimCtx& ctx, const Block& b, Phase phase)
    {
        // Rising exponential toward `amplitude` over [start, start+riseTime) with
        // time constant riseTau, then a falling exponential (constant fallTau).
        if (phase != Phase::OUTPUT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double amp = bd.paramDouble(nflow::kAmp, 1.0);
        const double riseTime = bd.paramDouble("RiseTime", 0.5);
        const double riseTau = bd.paramDouble("RiseTau", 0.1);
        const double fallTau = bd.paramDouble("FallTau", 0.1);
        const double start = bd.paramDouble("StartTime", 0.0);
        const double offset = bd.paramDouble("Offset", 0.0);
        double out = offset;
        if (ctx.t >= start) {
            const double td = ctx.t - start;
            if (td < riseTime) {
                out += amp * (1.0 - std::exp(-td / riseTau));
            } else {
                const double peak = amp * (1.0 - std::exp(-riseTime / riseTau));
                out += peak * std::exp(-(td - riseTime) / fallTau);
            }
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCExpSine()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = {param:Offset:0.0} + {param:Amplitude:1.0} * "
                 "exp(-{param:Damping:0.0} * (t - {param:StartTime:0.0})) * "
                 "sin(2.0 * M_PI * {param:Frequency:1.0} * (t - {param:StartTime:0.0}) + "
                 "{param:Phase:0.0});";
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustExpSine()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = {param:Offset:0.0} + {param:Amplitude:1.0} * "
                 "libm::exp(-{param:Damping:0.0} * (t - {param:StartTime:0.0})) * "
                 "libm::sin(2.0_f64 * core::f64::consts::PI * {param:Frequency:1.0} * "
                 "(t - {param:StartTime:0.0}) + {param:Phase:0.0});";
        return t;
    }
    //=============================================================================
    // Trapezoid bakes its params at generation time so it can guard the
    // rising/falling divisions (a zero ramp collapses that segment).
    static BlockCodegenTemplate
    makeTrapezoid(bool rust)
    {
        BlockCodegenTemplate t;
        t.emitStep = [rust](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const double amp = bd.paramDouble(nflow::kAmp, 1.0);
            const double rising = bd.paramDouble("Rising", 0.0);
            const double width = bd.paramDouble("Width", 0.0);
            const double falling = bd.paramDouble("Falling", 0.0);
            const double period = bd.paramDouble("Period", 1.0);
            const double start = bd.paramDouble("StartTime", 0.0);
            const double offset = bd.paramDouble("Offset", 0.0);
            const std::string fmodFn = rust ? "libm::fmod" : "fmod";
            const std::string tau
                = "(" + fmodFn + "(t - " + a.fmt(start) + ", " + a.fmt(period) + "))";
            std::string riseExpr = (rising > 0.0)
                ? (a.fmt(amp) + " * (" + tau + " / " + a.fmt(rising) + ")")
                : a.fmt(0.0);
            std::string fallExpr = (falling > 0.0)
                ? (a.fmt(amp) + " * (1.0 - (" + tau + " - " + a.fmt(rising + width) + ") / "
                      + a.fmt(falling) + ")")
                : a.fmt(0.0);
            const double plateauEnd = rising + width;
            const double fallEnd = rising + width + falling;
            std::string body;
            if (rust) {
                body = a.fmt(offset) + " + if " + tau + " < " + a.fmt(rising) + " { " + riseExpr
                    + " } else if " + tau + " < " + a.fmt(plateauEnd) + " { " + a.fmt(amp)
                    + " } else if " + tau + " < " + a.fmt(fallEnd) + " { " + fallExpr
                    + " } else { 0.0 }";
            } else {
                body = a.fmt(offset) + " + ((" + tau + " < " + a.fmt(rising) + ") ? (" + riseExpr
                    + ") : ((" + tau + " < " + a.fmt(plateauEnd) + ") ? (" + a.fmt(amp) + ") : (("
                    + tau + " < " + a.fmt(fallEnd) + ") ? (" + fallExpr + ") : 0.0)))";
            }
            a.line("out_" + a.id + " = " + body + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCTrapezoid()
    {
        return makeTrapezoid(false);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustTrapezoid()
    {
        return makeTrapezoid(true);
    }
    //=============================================================================
    static BlockCodegenTemplate
    makeExponentials(bool rust)
    {
        BlockCodegenTemplate t;
        t.emitStep = [rust](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const double amp = bd.paramDouble(nflow::kAmp, 1.0);
            const double riseTime = bd.paramDouble("RiseTime", 0.5);
            const double riseTau = bd.paramDouble("RiseTau", 0.1);
            const double fallTau = bd.paramDouble("FallTau", 0.1);
            const double start = bd.paramDouble("StartTime", 0.0);
            const double offset = bd.paramDouble("Offset", 0.0);
            const std::string expFn = rust ? "libm::exp" : "exp";
            const std::string td = "(t - " + a.fmt(start) + ")";
            const double peak = amp * (1.0 - std::exp(-riseTime / riseTau));
            const std::string rise
                = a.fmt(amp) + " * (1.0 - " + expFn + "(-" + td + " / " + a.fmt(riseTau) + "))";
            const std::string fall = a.fmt(peak) + " * " + expFn + "(-(" + td + " - "
                + a.fmt(riseTime) + ") / " + a.fmt(fallTau) + ")";
            std::string body;
            if (rust) {
                body = a.fmt(offset) + " + if " + td + " < " + a.fmt(riseTime) + " { " + rise
                    + " } else { " + fall + " }";
            } else {
                body = a.fmt(offset) + " + ((" + td + " < " + a.fmt(riseTime) + ") ? (" + rise
                    + ") : (" + fall + "))";
            }
            a.line("out_" + a.id + " = " + body + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCExponentials()
    {
        return makeExponentials(false);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustExponentials()
    {
        return makeExponentials(true);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
