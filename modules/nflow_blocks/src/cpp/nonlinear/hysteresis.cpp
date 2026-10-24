//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// hysteresis (relay with two thresholds, Coselica Blocks.Nonlinear.Hysteresis).
// The output latches: it switches to yHigh when the input rises past uHigh, to
// yLow when it falls past uLow, and holds otherwise. Stateful (INIT/OUTPUT/
// UPDATE, latched output), with C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include "nonlinear_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleHysteresis(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);
        if (phase == Phase::INIT) {
            nflow::BlockDescriptor bd(b, ctx.variables);
            // Initial branch: start low unless the initial input is already high.
            st.output = bd.paramDouble("yLow", 0.0);
            return false;
        }
        if (phase == Phase::ALGEBRAIC) {
            // Direct feedthrough, as the emitted C/Rust step: the relay answers
            // the current input and only holds the latched branch inside the
            // dead band. Pure: UPDATE owns the latch, so an RK stage or an
            // algebraic sweep may re-enter this.
            nflow::BlockDescriptor bd(b, ctx.variables);
            const double uHigh = bd.paramDouble("uHigh", 1.0);
            const double uLow = bd.paramDouble("uLow", -1.0);
            const double yHigh = bd.paramDouble("yHigh", 1.0);
            const double yLow = bd.paramDouble("yLow", 0.0);
            const double u = getInput(ctx, b.nid, 0, 0.0);
            double y = st.output;
            if (u >= uHigh) {
                y = yHigh;
            } else if (u <= uLow) {
                y = yLow;
            }
            setOutput(ctx, b.nid, y);
            return false;
        }
        if (phase == Phase::UPDATE) {
            nflow::BlockDescriptor bd(b, ctx.variables);
            const double uHigh = bd.paramDouble("uHigh", 1.0);
            const double uLow = bd.paramDouble("uLow", -1.0);
            const double yHigh = bd.paramDouble("yHigh", 1.0);
            const double yLow = bd.paramDouble("yLow", 0.0);
            const double u = getInput(ctx, b.nid, 0, 0.0);
            if (u >= uHigh) {
                st.output = yHigh;
            } else if (u <= uLow) {
                st.output = yLow;
            } // else: hold the latched output
            return false;
        }
        return false;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCHysteresis()
    {
        BlockCodegenTemplate t;
        t.emitState = [](const BlockCodegenStateArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            a.addState("hyst_" + a.id, a.fmt(bd.paramDouble("yLow", 0.0)), "");
        };
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::string uHigh = a.fmt(bd.paramDouble("uHigh", 1.0));
            const std::string uLow = a.fmt(bd.paramDouble("uLow", -1.0));
            const std::string yHigh = a.fmt(bd.paramDouble("yHigh", 1.0));
            const std::string yLow = a.fmt(bd.paramDouble("yLow", 0.0));
            a.line("if (" + a.in[0] + " >= " + uHigh + ") s->hyst_" + a.id + " = " + yHigh + ";");
            a.line(
                "else if (" + a.in[0] + " <= " + uLow + ") s->hyst_" + a.id + " = " + yLow + ";");
            a.line("out_" + a.id + " = s->hyst_" + a.id + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustHysteresis()
    {
        BlockCodegenTemplate t;
        t.emitState = [](const BlockCodegenStateArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            a.addState("hyst_" + a.id, a.fmt(bd.paramDouble("yLow", 0.0)), "");
        };
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::string uHigh = a.fmt(bd.paramDouble("uHigh", 1.0));
            const std::string uLow = a.fmt(bd.paramDouble("uLow", -1.0));
            const std::string yHigh = a.fmt(bd.paramDouble("yHigh", 1.0));
            const std::string yLow = a.fmt(bd.paramDouble("yLow", 0.0));
            a.line("if " + a.in[0] + " >= " + uHigh + " { s.hyst_" + a.id + " = " + yHigh + "; }");
            a.line(
                "else if " + a.in[0] + " <= " + uLow + " { s.hyst_" + a.id + " = " + yLow + "; }");
            a.line("out_" + a.id + " = s.hyst_" + a.id + ";");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
