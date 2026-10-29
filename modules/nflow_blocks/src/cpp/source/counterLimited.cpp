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
// counterLimited: an up-counter that wraps at a configurable ceiling (Counter
// Limited). Starts at 0 and increments by 1 at every sample step; once it
// reaches UpperLimit it wraps back to 0 on the next step. No input; the current
// count is emitted before the step's increment, so the output sweeps the range
// 0, 1, ..., UpperLimit, 0, 1, ...
//=============================================================================
bool
Nelson::NFlow::handleCounterLimited(SimCtx& ctx, const Block& b, Phase phase)
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
        const double upper = bd.paramDouble("UpperLimit", 7.0);
        st.scalar = (st.scalar >= upper) ? 0.0 : (st.scalar + 1.0);
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCCounterLimited()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) { a.addState("cl_count_" + a.id, "0.0", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string up = nflow::formatNumber(bd.paramDouble("UpperLimit", 7.0));
        a.line("out_" + a.id + " = s->cl_count_" + a.id + ";");
        const int rate = codegenSampleRate(asJson(a.params), a.dt);
        const std::string upd = "s->cl_count_" + a.id + " = (s->cl_count_" + a.id + " >= " + up
            + ") ? 0.0 : (s->cl_count_" + a.id + " + 1.0);";
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
Nelson::NFlow::getCodeGenRustCounterLimited()
{
    BlockCodegenTemplate t;
    t.emitState
        = [](const BlockCodegenStateArgs& a) { a.addState("cl_count_" + a.id, "0.0_f64", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string up = a.fmt(bd.paramDouble("UpperLimit", 7.0));
        a.line("out_" + a.id + " = s.cl_count_" + a.id + ";");
        const int rate = codegenSampleRate(asJson(a.params), a.dt);
        const std::string upd = "s.cl_count_" + a.id + " = if s.cl_count_" + a.id + " >= " + up
            + " { 0.0_f64 } else { s.cl_count_" + a.id + " + 1.0_f64 };";
        if (rate > 1) {
            a.line("if __nflow_step % " + std::to_string(rate) + " == 0 { " + upd + " }");
        } else {
            a.line(upd);
        }
    };
    return t;
}
//=============================================================================
