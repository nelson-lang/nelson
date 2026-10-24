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
// intervalTestDynamic: like intervalTest but the bounds come from input signals
// instead of parameters. Port 0 = lower limit (lo), port 1 = value (u), port 2
// = upper limit (up); output is 1.0 when lo <= u <= up, else 0.0. The interval
// is closed on the left/right per IntervalClosedLeft / IntervalClosedRight (both
// closed by default). Pure algebraic feedthrough, element-wise over the value
// width; scalar bounds broadcast.
//=============================================================================
bool
Nelson::NFlow::handleIntervalTestDynamic(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0) || !hasInput(ctx, b.nid, 1) || !hasInput(ctx, b.nid, 2)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const bool cl = bd.paramDouble("IntervalClosedLeft", 1.0) != 0.0;
    const bool cr = bd.paramDouble("IntervalClosedRight", 1.0) != 0.0;
    SigView lo = getInputSig(ctx, b.nid, 0);
    SigView u = getInputSig(ctx, b.nid, 1);
    SigView up = getInputSig(ctx, b.nid, 2);
    return emitElementwise(ctx, b.nid, [&](int i) -> double {
        const double x = sigAt(u, i);
        const double l = sigAt(lo, (lo.width > 1) ? i : 0);
        const double h = sigAt(up, (up.width > 1) ? i : 0);
        const bool okLo = cl ? (x >= l) : (x > l);
        const bool okUp = cr ? (x <= h) : (x < h);
        return (okLo && okUp) ? 1.0 : 0.0;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCIntervalTestDynamic()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const bool cl = bd.paramDouble("IntervalClosedLeft", 1.0) != 0.0;
        const bool cr = bd.paramDouble("IntervalClosedRight", 1.0) != 0.0;
        const std::string lo = a.in[0];
        const std::string x = a.in[1];
        const std::string up = a.in[2];
        const std::string opL = cl ? " >= " : " > ";
        const std::string opR = cr ? " <= " : " < ";
        a.line("out_" + a.id + " = (" + x + opL + lo + " && " + x + opR + up + ") ? 1.0 : 0.0;");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustIntervalTestDynamic()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const bool cl = bd.paramDouble("IntervalClosedLeft", 1.0) != 0.0;
        const bool cr = bd.paramDouble("IntervalClosedRight", 1.0) != 0.0;
        const std::string lo = a.in[0];
        const std::string x = a.in[1];
        const std::string up = a.in[2];
        const std::string opL = cl ? " >= " : " > ";
        const std::string opR = cr ? " <= " : " < ";
        a.line("out_" + a.id + " = if " + x + opL + lo + " && " + x + opR + up
            + " { 1.0_f64 } else { 0.0_f64 };");
    };
    return t;
}
//=============================================================================
