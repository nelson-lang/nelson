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
#include "nonlinear_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleBacklash(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int w = outputWidth(ctx, b.nid, 0);
    if (phase == Phase::INIT) {
        st.scalar = 0.0;
        st.output = 0.0;
        if (w > 1) {
            st.vec.assign(w, 0.0);
        }
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // Direct feedthrough, as the emitted C/Rust step: the play is resolved
        // against the current input, not the value latched at the end of the
        // previous step. Pure: only UPDATE commits, so RK stages and algebraic
        // sweeps may re-enter this freely.
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double width = std::max(0.0, bd.paramDouble(nflow::kBacklashWidth, 0.0));
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) {
            const double prev = (w <= 1) ? st.scalar : ((i < (int)st.vec.size()) ? st.vec[i] : 0.0);
            const double inp = sigAt(u, i);
            if (inp > prev + width / 2.0) {
                return inp - width / 2.0;
            }
            if (inp < prev - width / 2.0) {
                return inp + width / 2.0;
            }
            return prev;
        });
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double width = std::max(0.0, bd.paramDouble(nflow::kBacklashWidth, 0.0));
        auto step = [&](double inp, double prev) {
            double out = prev;
            if (inp > prev + width / 2.0) {
                out = inp - width / 2.0;
            }
            if (inp < prev - width / 2.0) {
                out = inp + width / 2.0;
            }
            return out;
        };
        if (w <= 1) {
            double out = step(getInput(ctx, b.nid, 0, 0.0), st.scalar);
            st.scalar = out;
            st.output = out;
        } else {
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.vec.size() != w) {
                st.vec.assign(w, 0.0);
            }
            for (int i = 0; i < w; ++i) {
                st.vec[i] = step(sigAt(u, i), st.vec[i]);
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCBacklash()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) { a.addState("backlash_" + a.id, "", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double width = std::fmax(0.0, bd.paramDouble(nflow::kBacklashWidth, 0.0));
        a.line("{ double v = " + a.in[0] + ";");
        a.line("  if (v > s->backlash_" + a.id + " + " + a.fmt(width) + " / 2.0) s->backlash_"
            + a.id + " = v - " + a.fmt(width) + " / 2.0;");
        a.line("  if (v < s->backlash_" + a.id + " - " + a.fmt(width) + " / 2.0) s->backlash_"
            + a.id + " = v + " + a.fmt(width) + " / 2.0;");
        a.line("  out_" + a.id + " = s->backlash_" + a.id + "; }");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustBacklash()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) { a.addState("backlash_" + a.id, "", ""); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double width = std::fmax(0.0, bd.paramDouble(nflow::kBacklashWidth, 0.0));
        a.line("{");
        a.line("    let v = " + a.in[0] + ";");
        a.line("    if v > s.backlash_" + a.id + " + " + a.fmt(width) + " / 2.0_f64 { s.backlash_"
            + a.id + " = v - " + a.fmt(width) + " / 2.0_f64; }");
        a.line("    if v < s.backlash_" + a.id + " - " + a.fmt(width) + " / 2.0_f64 { s.backlash_"
            + a.id + " = v + " + a.fmt(width) + " / 2.0_f64; }");
        a.line("    out_" + a.id + " = s.backlash_" + a.id + ";");
        a.line("}");
    };
    return t;
}
//=============================================================================
