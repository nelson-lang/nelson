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
//=============================================================================
bool
Nelson::NFlow::handleImpulse(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double tp = bd.paramDouble(nflow::kTime, 0.0);
    double amp = bd.paramDouble(nflow::kAmp, 0.0);
    double out = (std::abs(ctx.t - tp) <= ctx.dt / 2.0) ? amp / std::max(ctx.dt, 1e-6) : 0.0;
    setOutput(ctx, b.nid, out);
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCImpulse()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = (fabs(t - {param:Time:0.0}) <= dt * 0.5 ? {param:Amplitude:0.0} / "
             "fmax(dt, 1e-6) : 0.0);";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustImpulse()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = if libm::fabs(t - {param:Time:0.0}) <= dt * 0.5_f64 { "
             "{param:Amplitude:0.0} / libm::fmax(dt, 1e-6_f64) } else { 0.0_f64 };";
    return t;
}
//=============================================================================
