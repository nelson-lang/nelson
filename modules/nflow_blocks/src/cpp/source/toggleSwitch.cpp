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
Nelson::NFlow::handleToggleSwitch(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double state = bd.paramDouble(nflow::kState, 0.0);
    double onVal = bd.paramDouble(nflow::kOnValue, 1.0);
    double offVal = bd.paramDouble(nflow::kOffValue, 0.0);
    setOutput(ctx, b.nid, (state != 0.0) ? onVal : offVal);
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCToggleSwitch()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = ({param:State:0.0} ? {param:OnValue:1.0} : {param:OffValue:0.0});";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustToggleSwitch()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = if {param:State:0.0} != 0.0_f64 { {param:OnValue:1.0} } else { "
             "{param:OffValue:0.0} };";
    return t;
}
//=============================================================================
