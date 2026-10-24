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
Nelson::NFlow::handleNegate(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    SigView u = getInputSig(ctx, b.nid, 0);
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        const bool sat = blockSaturates(ctx, b.nid);
        return emitElementwiseI64(
            ctx, b.nid, [&](int i) { return i64ApplyNeg(sigAtI64(u, i, ty), ty, sat); });
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int i) { return -sigAtC(u, i); });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return -sigAt(u, i); });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCNegate()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = -{in0};";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        const bool uns = codegenIsU64(a.outType);
        if (uns && codegenSaturates(a)) {
            a.line("out_" + a.id + " = 0; /* -u64 saturates to 0 */");
            return;
        }
        a.line("out_" + a.id + " = "
            + codegenI64BinOp("sub", uns ? "0ULL" : "0LL", a.in[0], uns, codegenSaturates(a), true)
            + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustNegate()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = -{in0w};";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        const bool uns = codegenIsU64(a.outType);
        if (uns && codegenSaturates(a)) {
            a.line("out_" + a.id + " = 0_u64; // -u64 saturates to 0");
            return;
        }
        a.line("out_" + a.id + " = "
            + codegenI64BinOp(
                "sub", uns ? "0_u64" : "0_i64", a.in[0], uns, codegenSaturates(a), false)
            + ";");
    };
    return t;
}
//=============================================================================
