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
#include "discrete_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleUnitDelay(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        if (w <= 1) {
            st.scalar = bd.paramDouble(nflow::kInitial, 0.0);
            st.output = st.scalar;
        } else {
            std::vector<double> iniList = bd.paramList(nflow::kInitial);
            st.vec.assign(w, 0.0);
            for (int i = 0; i < w; ++i) {
                st.vec[i] = iniList.empty()
                    ? 0.0
                    : (i < (int)iniList.size() ? iniList[i] : iniList.back());
            }
        }
        return false;
    }
    if (phase == Phase::OUTPUT) {
        if (w <= 1) {
            setOutput(ctx, b.nid, st.output);
        } else {
            double* y = outputSlice(ctx, b.nid, 0);
            for (int i = 0; i < w; ++i) {
                y[i] = (i < (int)st.vec.size()) ? st.vec[i] : 0.0;
            }
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        if (w <= 1) {
            st.output = getInput(ctx, b.nid, 0, 0.0);
        } else {
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            for (int i = 0; i < w; ++i) {
                st.vec[i] = sigAt(u, i);
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCUnitDelay()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState(
            "unit_delay_" + a.id, nflow::formatNumber(bd.paramDouble(nflow::kInitial, 0.0)), "");
    };
    t.emitOutput = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = s->unit_delay_" + a.id + ";");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = s->unit_delay_" + a.id + ";");
        // Multi-rate: advance the stored state only on the block's sample hit.
        // rate = round(SampleTime / base step); 1 = every step.
        const int rate = codegenSampleRate(asJson(a.params), a.dt);
        const std::string upd = "s->unit_delay_" + a.id + " = " + a.in[0] + ";";
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
Nelson::NFlow::getCodeGenRustUnitDelay()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState("unit_delay_" + a.id, a.fmt(bd.paramDouble(nflow::kInitial, 0.0)), "");
    };
    t.emitOutput = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = s.unit_delay_" + a.id + ";");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = s.unit_delay_" + a.id + ";");
        // Multi-rate: advance the stored state only on the block's sample hit.
        const int rate = codegenSampleRate(asJson(a.params), a.dt);
        const std::string upd = "s.unit_delay_" + a.id + " = " + a.in[0] + ";";
        if (rate > 1) {
            a.line("if __nflow_step % " + std::to_string(rate) + " == 0 { " + upd + " }");
        } else {
            a.line(upd);
        }
    };
    return t;
}
//=============================================================================
