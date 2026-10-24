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
Nelson::NFlow::handleQuantizer(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double interval = std::abs(bd.paramDouble(nflow::kQuantizationInterval, 1.0));
    if (interval < 1e-12) {
        interval = 1.0;
    }
    SigView u = getInputSig(ctx, b.nid, 0);
    return emitElementwise(
        ctx, b.nid, [&](int i) { return interval * std::round(sigAt(u, i) / interval); });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCQuantizer()
{
    BlockCodegenTemplate t;
    // The interval parameter is normalized (abs, zero-guard): native emitter.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double interval = std::abs(bd.paramDouble(nflow::kQuantizationInterval, 1.0));
        if (interval < 1e-12) {
            interval = 1.0;
        }
        a.line("out_" + a.id + " = " + a.fmt(interval) + " * round(" + a.in[0] + " / "
            + a.fmt(interval) + ");");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustQuantizer()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double interval = std::abs(bd.paramDouble(nflow::kQuantizationInterval, 1.0));
        if (interval < 1e-12) {
            interval = 1.0;
        }
        a.line("out_" + a.id + " = " + a.fmt(interval) + " * libm::round(" + a.in[0] + " / "
            + a.fmt(interval) + ");");
    };
    return t;
}
//=============================================================================
