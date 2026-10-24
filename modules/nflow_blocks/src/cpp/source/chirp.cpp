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
#include "NFlowCodegenHelpers.hpp"
#include <cmath>
#include <algorithm>
#include "source_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleChirp(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double amp = bd.paramDouble(nflow::kAmp, 0.0);
    double f0 = bd.paramDouble(nflow::kF0, 0.0);
    double f1 = bd.paramDouble(nflow::kF1, 0.0);
    double t1 = std::max(0.001, bd.paramDouble(nflow::kT1, 1.0));
    double k = (f1 - f0) / t1;
    double ph = 2.0 * M_PI * (f0 * ctx.t + 0.5 * k * ctx.t * ctx.t);
    setOutput(ctx, b.nid, amp * std::sin(ph));
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCChirp()
{
    BlockCodegenTemplate t;
    // The sweep duration is normalized (zero-guard, floor 1 ms): native emitter.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double f0 = bd.paramDouble(nflow::kF0, 0.0);
        double f1 = bd.paramDouble(nflow::kF1, 0.0);
        // Floor the sweep duration at 1 ms, as the interpreter (handleChirp) and
        // the Rust backend do. An explicit T1 = 0 must not become 1.0 s.
        double t1 = std::fmax(0.001, bd.paramDouble(nflow::kT1, 1.0));
        a.line("{ double k = (" + a.fmt(f1) + " - " + a.fmt(f0) + ") / " + a.fmt(t1) + ";");
        a.line("  double phase = 2.0 * M_PI * (" + a.fmt(f0) + " * t + 0.5 * k * t * t);");
        a.line(
            "  out_" + a.id + " = " + a.fmt(bd.paramDouble(nflow::kAmp, 0.0)) + " * sin(phase); }");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustChirp()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double amp = bd.paramDouble(nflow::kAmp, 0.0);
        double f0 = bd.paramDouble(nflow::kF0, 0.0);
        double f1 = bd.paramDouble(nflow::kF1, 0.0);
        double t1 = std::max(0.001, bd.paramDouble(nflow::kT1, 1.0));
        double k = (f1 - f0) / t1;
        a.line("{");
        a.line("    let ph = 2.0_f64 * core::f64::consts::PI * (" + a.fmt(f0) + " * t + 0.5_f64 * "
            + a.fmt(k) + " * t * t);");
        a.line("    out_" + a.id + " = " + a.fmt(amp) + " * libm::sin(ph);");
        a.line("}");
    };
    return t;
}
//=============================================================================
