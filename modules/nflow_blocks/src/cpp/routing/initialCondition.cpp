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
#include "routing_blocks.hpp"
//=============================================================================
// initialCondition (IC): outputs InitialValue at the first step (t == 0) and
// passes the input through for t > 0. Useful to seed a signal (e.g. to break an
// algebraic loop's initial value). Pure feedthrough after t = 0, element-wise.
//=============================================================================
bool
Nelson::NFlow::handleInitialCondition(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const double ic = bd.paramDouble("InitialValue", 0.0);
    const bool initial = (ctx.t <= 0.0);
    SigView u = getInputSig(ctx, b.nid, 0);
    return emitElementwise(ctx, b.nid, [&](int i) -> double { return initial ? ic : sigAt(u, i); });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCInitialCondition()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.line("out_" + a.id + " = (t > 0.0) ? " + a.in[0] + " : "
            + a.fmt(bd.paramDouble("InitialValue", 0.0)) + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustInitialCondition()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.line("out_" + a.id + " = if t > 0.0 { " + a.in[0] + " } else { "
            + a.fmt(bd.paramDouble("InitialValue", 0.0)) + " };");
    };
    return t;
}
//=============================================================================
