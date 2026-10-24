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
Nelson::NFlow::handleMin(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0) || !hasInput(ctx, b.nid, 1)) {
        return false;
    }
    SigView a = getInputSig(ctx, b.nid, 0);
    SigView c = getInputSig(ctx, b.nid, 1);
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        return emitElementwiseI64(ctx, b.nid, [&](int i) {
            const long long x = sigAtI64(a, i, ty);
            const long long y = sigAtI64(c, i, ty);
            return i64Less(x, y, ty) ? x : y;
        });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return std::min(sigAt(a, i), sigAt(c, i)); });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCMin()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = fmin({in0}, {in1});";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = (" + a.in[0] + " < " + a.in[1] + ") ? " + a.in[0] + " : "
            + a.in[1] + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustMin()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = libm::fmin({in0}, {in1});";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = core::cmp::min(" + a.in[0] + ", " + a.in[1] + ");");
    };
    return t;
}
//=============================================================================
