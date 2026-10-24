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
#include "logic_blocks.hpp"
//=============================================================================
// intervalTest: output 1.0 when the input lies between LowerLimit and
// UpperLimit, else 0.0. The interval is closed on the left/right according to
// IntervalClosedLeft / IntervalClosedRight (both closed by default). Pure
// algebraic feedthrough, element-wise over the input width.
//=============================================================================
bool
Nelson::NFlow::handleIntervalTest(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const double lo = bd.paramDouble("LowerLimit", 0.0);
    const double up = bd.paramDouble("UpperLimit", 1.0);
    const bool cl = bd.paramDouble("IntervalClosedLeft", 1.0) != 0.0;
    const bool cr = bd.paramDouble("IntervalClosedRight", 1.0) != 0.0;
    SigView u = getInputSig(ctx, b.nid, 0);
    return emitElementwise(ctx, b.nid, [&](int i) -> double {
        const double x = sigAt(u, i);
        const bool okLo = cl ? (x >= lo) : (x > lo);
        const bool okUp = cr ? (x <= up) : (x < up);
        return (okLo && okUp) ? 1.0 : 0.0;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCIntervalTest()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double lo = bd.paramDouble("LowerLimit", 0.0);
        const double up = bd.paramDouble("UpperLimit", 1.0);
        const bool cl = bd.paramDouble("IntervalClosedLeft", 1.0) != 0.0;
        const bool cr = bd.paramDouble("IntervalClosedRight", 1.0) != 0.0;
        const std::string x = a.in[0];
        const std::string opL = cl ? " >= " : " > ";
        const std::string opR = cr ? " <= " : " < ";
        a.line("out_" + a.id + " = (" + x + opL + a.fmt(lo) + " && " + x + opR + a.fmt(up)
            + ") ? 1.0 : 0.0;");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustIntervalTest()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const double lo = bd.paramDouble("LowerLimit", 0.0);
        const double up = bd.paramDouble("UpperLimit", 1.0);
        const bool cl = bd.paramDouble("IntervalClosedLeft", 1.0) != 0.0;
        const bool cr = bd.paramDouble("IntervalClosedRight", 1.0) != 0.0;
        const std::string x = a.in[0];
        const std::string opL = cl ? " >= " : " > ";
        const std::string opR = cr ? " <= " : " < ";
        a.line("out_" + a.id + " = if " + x + opL + a.fmt(lo) + " && " + x + opR + a.fmt(up)
            + " { 1.0_f64 } else { 0.0_f64 };");
    };
    return t;
}
//=============================================================================
