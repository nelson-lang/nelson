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
Nelson::NFlow::handleAbs(SimCtx& ctx, const Block& b, Phase phase)
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
            ctx, b.nid, [&](int i) { return i64ApplyAbs(sigAtI64(u, i, ty), ty, sat); });
    }
    if (u.isComplex) {
        // Complex magnitude: real output (forcesComplexOut == 0).
        return emitElementwise(ctx, b.nid, [&](int i) { return std::abs(sigAtC(u, i)); });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return std::abs(sigAt(u, i)); });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCAbs()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = fabs({in0});";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        if (codegenIsU64(a.outType)) {
            a.line("out_" + a.id + " = " + a.in[0] + "; /* |u64| is the identity */");
            return;
        }
        if (codegenSaturates(a)) {
            a.line("out_" + a.id + " = (" + a.in[0] + " < 0) ? nflow_i64_sat_sub(0, " + a.in[0]
                + ") : " + a.in[0] + ";");
        } else {
            a.line("out_" + a.id + " = (" + a.in[0] + " < 0) ? (int64_t)(0ULL - (uint64_t)"
                + a.in[0] + ") : " + a.in[0] + ";");
        }
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustAbs()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = libm::fabs({in0});";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        if (codegenIsU64(a.outType)) {
            a.line("out_" + a.id + " = " + a.in[0] + "; // |u64| is the identity");
            return;
        }
        const char* m = codegenSaturates(a) ? "saturating_abs" : "wrapping_abs";
        a.line("out_" + a.id + " = " + a.in[0] + "." + m + "();");
    };
    return t;
}
//=============================================================================
