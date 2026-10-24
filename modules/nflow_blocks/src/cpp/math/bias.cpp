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
#include "NFlowCodegenTyped.hpp"
#include "math_blocks.hpp"
#include <cmath>
#include <algorithm>
//=============================================================================
bool
Nelson::NFlow::handleBias(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const double biasVal = bd.paramDouble(nflow::kBias, 0.0);
    SigView u = getInputSig(ctx, b.nid, 0);
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        const bool sat = blockSaturates(ctx, b.nid);
        const long long k = quantizeDoubleToI64(biasVal, ty);
        return emitElementwiseI64(
            ctx, b.nid, [&](int i) { return i64ApplyAdd(sigAtI64(u, i, ty), k, ty, sat); });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return sigAt(u, i) + biasVal; });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCBias()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0} + {param:Bias:0.0};";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const bool uns = codegenIsU64(a.outType);
        const long long k = quantizeDoubleToI64(
            bd.paramDouble(nflow::kBias, 0.0), uns ? SigType::UInt64 : SigType::Int64);
        a.line("out_" + a.id + " = "
            + codegenI64BinOp(
                "add", a.in[0], codegenI64Literal(k, uns, true), uns, codegenSaturates(a), true)
            + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustBias()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0} + {param:Bias:0.0};";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const bool uns = codegenIsU64(a.outType);
        const long long k = quantizeDoubleToI64(
            bd.paramDouble(nflow::kBias, 0.0), uns ? SigType::UInt64 : SigType::Int64);
        a.line("out_" + a.id + " = "
            + codegenI64BinOp(
                "add", a.in[0], codegenI64Literal(k, uns, false), uns, codegenSaturates(a), false)
            + ";");
    };
    return t;
}
//=============================================================================
