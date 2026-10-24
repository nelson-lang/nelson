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
#include <cstdint>
#include <cmath>
#include "logic_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleNot(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    SigView u = getInputSig(ctx, b.nid, 0);
    return emitElementwise(ctx, b.nid, [&](int i) {
        double v = sigAt(u, i);
        bool truthy = !std::isnan(v) && v != 0.0;
        return truthy ? 0.0 : 1.0;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCNot()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = ({in0} == 0.0) ? 1.0 : 0.0;";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustNot()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = if {in0} == 0.0_f64 { 1.0_f64 } else { 0.0_f64 };";
    return t;
}
//=============================================================================
