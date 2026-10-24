//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// hitCrossing: detects when the (scalar) input reaches HitCrossingOffset in the
// configured direction and outputs 1.0 on the step where the crossing happens,
// else 0.0. Direction is "rising", "falling" or "either" (default). Stateful:
// the previous input (relative to the offset) is held so a straddle can be
// detected. The first step has nothing to straddle, so it fires only when the
// input STARTS on the offset - which is a hit, and the only one that instant
// can report. C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <string>
#include "nonlinear_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Direction codes: 0 = either, 1 = rising, 2 = falling.
    static int
    hcDirection(const nflow::BlockDescriptor& bd)
    {
        const std::string d = bd.paramStr("HitCrossingDirection", "either");
        if (d == "rising") {
            return 1;
        }
        if (d == "falling") {
            return 2;
        }
        return 0;
    }
    //=============================================================================
    static bool
    hcHit(double prev, double cur, int dir)
    {
        const bool rising = (prev < 0.0 && cur >= 0.0);
        const bool falling = (prev > 0.0 && cur <= 0.0);
        if (dir == 1) {
            return rising;
        }
        if (dir == 2) {
            return falling;
        }
        return rising || falling;
    }
    //=============================================================================
    bool
    handleHitCrossing(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);
        if (phase == Phase::INIT) {
            st.scalar = 0.0; // previous input relative to the offset
            st.scalar2 = 0.0; // primed flag (0 until the first step is seen)
            st.output = 0.0;
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const double off = bd.paramDouble("HitCrossingOffset", 0.0);
        if (phase == Phase::OUTPUT) {
            const double cur = getInput(ctx, b.nid, 0, 0.0) - off;
            double out = 0.0;
            if (st.scalar2 != 0.0) {
                out = hcHit(st.scalar, cur, hcDirection(bd)) ? 1.0 : 0.0;
            } else if (cur == 0.0) {
                // Starts on the offset: a hit, whatever direction was asked for
                // (there is no previous sample to give the crossing a sign).
                out = 1.0;
            }
            setOutput(ctx, b.nid, out);
            return false;
        }
        if (phase == Phase::UPDATE) {
            st.scalar = getInput(ctx, b.nid, 0, 0.0) - off;
            st.scalar2 = 1.0;
            return false;
        }
        return false;
    }
    //=============================================================================
    static std::string
    hcCondC(const BlockCodegenArgs& a, const std::string& c, int dir)
    {
        const std::string p = "s->hc_prev_" + a.id;
        const std::string rising = "(" + p + " < 0.0 && " + c + " >= 0.0)";
        const std::string falling = "(" + p + " > 0.0 && " + c + " <= 0.0)";
        if (dir == 1) {
            return rising;
        }
        if (dir == 2) {
            return falling;
        }
        return "(" + rising + " || " + falling + ")";
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCHitCrossing()
    {
        BlockCodegenTemplate t;
        t.emitState = [](const BlockCodegenStateArgs& a) {
            a.addState("hc_prev_" + a.id, "0.0", "");
            a.addState("hc_primed_" + a.id, "0.0", "");
        };
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::string off = a.fmt(bd.paramDouble("HitCrossingOffset", 0.0));
            const int dir = hcDirection(bd);
            const std::string c = "(" + a.in[0] + " - " + off + ")";
            // Before the first latch, a hit is the input STARTING on the offset.
            a.line("out_" + a.id + " = (s->hc_primed_" + a.id + " != 0.0) ? ((" + hcCondC(a, c, dir)
                + ") ? 1.0 : 0.0) : ((" + c + " == 0.0) ? 1.0 : 0.0);");
            a.line("s->hc_prev_" + a.id + " = " + c + ";");
            a.line("s->hc_primed_" + a.id + " = 1.0;");
        };
        return t;
    }
    //=============================================================================
    static std::string
    hcCondRust(const BlockCodegenArgs& a, const std::string& c, int dir)
    {
        const std::string p = "s.hc_prev_" + a.id;
        const std::string rising = "(" + p + " < 0.0_f64 && " + c + " >= 0.0_f64)";
        const std::string falling = "(" + p + " > 0.0_f64 && " + c + " <= 0.0_f64)";
        if (dir == 1) {
            return rising;
        }
        if (dir == 2) {
            return falling;
        }
        return "(" + rising + " || " + falling + ")";
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustHitCrossing()
    {
        BlockCodegenTemplate t;
        t.emitState = [](const BlockCodegenStateArgs& a) {
            a.addState("hc_prev_" + a.id, "0.0_f64", "");
            a.addState("hc_primed_" + a.id, "0.0_f64", "");
        };
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::string off = a.fmt(bd.paramDouble("HitCrossingOffset", 0.0));
            const int dir = hcDirection(bd);
            const std::string c = "(" + a.in[0] + " - " + off + ")";
            // Before the first latch, a hit is the input STARTING on the offset.
            a.line("out_" + a.id + " = if s.hc_primed_" + a.id + " != 0.0_f64 { if "
                + hcCondRust(a, c, dir) + " { 1.0_f64 } else { 0.0_f64 } } else if " + c
                + " == 0.0_f64 { 1.0_f64 } else { 0.0_f64 };");
            a.line("s.hc_prev_" + a.id + " = " + c + ";");
            a.line("s.hc_primed_" + a.id + " = 1.0_f64;");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
