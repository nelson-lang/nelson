//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// signalGenerator: a configurable periodic source (Signal Generator). Waveform
// is "sine", "square" or "sawtooth"; Amplitude scales it and Frequency is in Hz.
// sine = A*sin(2*pi*f*t); square starts LOW, i.e. -A*sign(sin(2*pi*f*t));
// sawtooth ramps linearly DOWN from +A to -A over each period. No input;
// scalar output; stateless (a pure function of time). C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <string>
#include "source_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Waveform codes: 0 sine, 1 square, 2 sawtooth.
    static int
    sigGenWave(const nflow::BlockDescriptor& bd)
    {
        const std::string w = bd.paramStr("Waveform", "sine");
        if (w == "square") {
            return 1;
        }
        if (w == "sawtooth") {
            return 2;
        }
        return 0;
    }
    //=============================================================================
    bool
    handleSignalGenerator(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::OUTPUT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double amp = bd.paramDouble("Amplitude", 1.0);
        const double freq = bd.paramDouble("Frequency", 1.0);
        const int wave = sigGenWave(bd);
        const double t = ctx.t;
        double v = 0.0;
        if (wave == 1) {
            // A period opens on the LOW half and turns at the half-period. Read
            // from the phase, not from sin(): sin(pi) is a small POSITIVE
            // residue in binary, so the turn landed one sample late.
            const double ph = freq * t;
            v = ((ph - std::floor(ph)) < 0.5) ? -1.0 : 1.0;
        } else if (wave == 2) {
            const double ph = freq * t;
            v = 1.0 - 2.0 * (ph - std::floor(ph));
        } else {
            v = std::sin(2.0 * M_PI * freq * t);
        }
        setOutput(ctx, b.nid, amp * v);
        return false;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCSignalGenerator()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const double amp = bd.paramDouble("Amplitude", 1.0);
            const double freq = bd.paramDouble("Frequency", 1.0);
            const int wave = sigGenWave(bd);
            const std::string A = nflow::formatNumber(amp);
            const std::string omega = nflow::formatNumber(2.0 * M_PI * freq);
            const std::string f = nflow::formatNumber(freq);
            std::string expr;
            if (wave == 1) {
                expr = A + " * (((" + f + " * t - floor(" + f + " * t)) < 0.5) ? -1.0 : 1.0)";
            } else if (wave == 2) {
                expr = A + " * (1.0 - 2.0 * (" + f + " * t - floor(" + f + " * t)))";
            } else {
                expr = A + " * sin(" + omega + " * t)";
            }
            a.line("out_" + a.id + " = " + expr + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustSignalGenerator()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const double amp = bd.paramDouble("Amplitude", 1.0);
            const double freq = bd.paramDouble("Frequency", 1.0);
            const int wave = sigGenWave(bd);
            const std::string A = a.fmt(amp);
            const std::string omega = a.fmt(2.0 * M_PI * freq);
            const std::string f = a.fmt(freq);
            std::string expr;
            if (wave == 1) {
                expr = A + " * (if (" + f + " * t - (" + f
                    + " * t).floor()) < 0.5_f64 { -1.0_f64 } else { 1.0_f64 })";
            } else if (wave == 2) {
                expr = A + " * (1.0_f64 - 2.0_f64 * (" + f + " * t - (" + f + " * t).floor()))";
            } else {
                expr = A + " * (" + omega + " * t).sin()";
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
