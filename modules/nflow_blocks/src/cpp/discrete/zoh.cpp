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
Nelson::NFlow::handleZoh(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        st.output = 0.0;
        st.dNextTime = 0.0;
        if (w > 1) {
            st.vec.assign(w, 0.0);
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // A hold samples at t = kT and holds until the next hit, so at a hit
        // instant the output is the value taken THERE. Direct feedthrough at a
        // hit, hence ALGEBRAIC: the input may itself come from the algebraic
        // schedule. Pure: UPDATE owns the latch, so an RK stage may re-enter this.
        const bool hit = (ctx.t + 1e-6 >= st.dNextTime);
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) {
            if (hit) {
                return sigAt(u, i);
            }
            return (w <= 1) ? st.output : ((i < (int)st.vec.size()) ? st.vec[i] : 0.0);
        });
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double ts = std::max(0.001, bd.paramDouble(nflow::kTs, ctx.dt));
        if (ctx.t + 1e-6 >= st.dNextTime) {
            if (w <= 1) {
                st.scalar = getInput(ctx, b.nid, 0, 0.0);
            } else {
                SigView u = getInputSig(ctx, b.nid, 0);
                if ((int)st.vec.size() != w) {
                    st.vec.assign(w, 0.0);
                }
                for (int i = 0; i < w; ++i) {
                    st.vec[i] = sigAt(u, i);
                }
            }
            st.dNextTime = ctx.t + ts;
        }
        st.output = st.scalar;
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCZoh()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("zoh_last_" + a.id, "", "");
        a.addState("zoh_next_" + a.id, "", "");
    };
    // No emitOutput: the hold is direct feedthrough at a hit instant, so the
    // sample has to be taken BEFORE the output is read.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        a.line("if (t + 1e-6 >= s->zoh_next_" + a.id + ") {");
        a.line("  s->zoh_last_" + a.id + " = " + a.in[0] + ";");
        a.line("  s->zoh_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("out_" + a.id + " = s->zoh_last_" + a.id + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustZoh()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.addState("zoh_last_" + a.id, "", "");
        a.addState("zoh_next_" + a.id, "", "");
    };
    // Same reordering as the C template: sample first, then read the hold.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        a.line("if t + 1e-6 >= s.zoh_next_" + a.id + " {");
        a.line("    s.zoh_last_" + a.id + " = " + a.in[0] + ";");
        a.line("    s.zoh_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("out_" + a.id + " = s.zoh_last_" + a.id + ";");
    };
    return t;
}
//=============================================================================
