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
#include "source_blocks.hpp"
#include "../discrete/discrete_blocks.hpp" // codegenSampleRate (shared inline helper)
//=============================================================================
// counterFreeRunning: a free-running up-counter (Counter Free-Running). Starts
// at 0 and increments by 1 at every sample step, wrapping back to 0 after
// 2^NumBits - 1 (unsigned modulo-2^NumBits arithmetic). No input; the current
// count is emitted before the step's increment, so the first sample is 0.
//=============================================================================
bool
Nelson::NFlow::handleCounterFreeRunning(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        st.output = 0.0;
        return false;
    }
    if (phase == Phase::OUTPUT) {
        setOutput(ctx, b.nid, st.scalar);
        return false;
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double nbits = std::min(53.0, std::max(1.0, std::floor(bd.paramDouble("NumBits", 16.0))));
        const double m = std::pow(2.0, nbits);
        st.scalar = std::fmod(st.scalar + 1.0, m);
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCCounterFreeRunning()
{
    BlockCodegenTemplate t;
    t.emitState
        = [](const BlockCodegenStateArgs& a) { a.addState("cfr_count_" + a.id, "0.0", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double nbits = bd.paramDouble("NumBits", 16.0);
        nbits = std::min(53.0, std::max(1.0, std::floor(nbits)));
        const std::string m = nflow::formatNumber(std::pow(2.0, nbits));
        a.line("out_" + a.id + " = s->cfr_count_" + a.id + ";");
        const int rate = codegenSampleRate(a.params, a.dt);
        const std::string upd
            = "s->cfr_count_" + a.id + " = fmod(s->cfr_count_" + a.id + " + 1.0, " + m + ");";
        if (rate > 1) {
            a.line("if (__nflow_step % " + std::to_string(rate) + " == 0) " + upd);
        } else {
            a.line(upd);
        }
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustCounterFreeRunning()
{
    BlockCodegenTemplate t;
    t.emitState
        = [](const BlockCodegenStateArgs& a) { a.addState("cfr_count_" + a.id, "0.0_f64", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double nbits = bd.paramDouble("NumBits", 16.0);
        nbits = std::min(53.0, std::max(1.0, std::floor(nbits)));
        const std::string m = a.fmt(std::pow(2.0, nbits));
        a.line("out_" + a.id + " = s.cfr_count_" + a.id + ";");
        const int rate = codegenSampleRate(a.params, a.dt);
        const std::string upd
            = "s.cfr_count_" + a.id + " = (s.cfr_count_" + a.id + " + 1.0_f64) % " + m + ";";
        if (rate > 1) {
            a.line("if __nflow_step % " + std::to_string(rate) + " == 0 { " + upd + " }");
        } else {
            a.line(upd);
        }
    };
    return t;
}
//=============================================================================
