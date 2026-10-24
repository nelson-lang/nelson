//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// wrapToZero: outputs 0 when the input is at or above Threshold, else passes the
// input through unchanged (Wrap To Zero). Pure algebraic feedthrough, element-
// wise over the input width; C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "math_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleWrapToZero(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const double threshold = bd.paramDouble("Threshold", 255.0);
    SigView u = getInputSig(ctx, b.nid, 0);
    return emitElementwise(ctx, b.nid, [&](int i) {
        // The threshold itself is NOT above the threshold: an input landing
        // exactly on it passes through, and only what exceeds it wraps.
        const double v = sigAt(u, i);
        return (v > threshold) ? 0.0 : v;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCWrapToZero()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = ({in0} > {param:Threshold:255.0}) ? 0.0 : {in0};";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustWrapToZero()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = if {in0} > {param:Threshold:255.0} { 0.0_f64 } else { {in0} };";
    return t;
}
//=============================================================================
