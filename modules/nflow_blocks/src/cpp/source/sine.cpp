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
Nelson::NFlow::handleSine(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double amp = bd.paramDouble(nflow::kAmp, 0.0);
    double freq = bd.paramDouble(nflow::kFreq, 0.0);
    double phase_ = bd.paramDouble(nflow::kPhase, 0.0);
    setOutput(ctx, b.nid, amp * std::sin(2.0 * M_PI * freq * ctx.t + phase_));
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCSine()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {param:Amplitude:0.0} * sin(2.0 * M_PI * {param:Frequency:0.0} * t + "
             "{param:Phase:0.0});";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustSine()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {param:Amplitude:0.0} * libm::sin(2.0_f64 * core::f64::consts::PI * "
             "{param:Frequency:0.0} * t + {param:Phase:0.0});";
    return t;
}
//=============================================================================
