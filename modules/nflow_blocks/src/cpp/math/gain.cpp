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
Nelson::NFlow::handleGain(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        // Disconnected: hold previous output
        setOutput(ctx, b.nid, ctx.blockState[b.nid].output);
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double gainVal = bd.paramDouble(nflow::kGain, 1.0);
    SigView u = getInputSig(ctx, b.nid, 0);
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        const bool sat = blockSaturates(ctx, b.nid);
        const long long k = quantizeDoubleToI64(gainVal, ty);
        return emitElementwiseI64(
            ctx, b.nid, [&](int i) { return i64ApplyMul(sigAtI64(u, i, ty), k, ty, sat); });
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int i) { return sigAtC(u, i) * gainVal; });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return sigAt(u, i) * gainVal; });
}
//=============================================================================
namespace {
Nelson::NFlow::BlockCodegenEmitFn
gainTypedEmitter(bool cLang, double dflt)
{
    using namespace Nelson::NFlow;
    return [cLang, dflt](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const bool uns = codegenIsU64(a.outType);
        const bool sat = codegenSaturates(a);
        const long long k = quantizeDoubleToI64(
            bd.paramDouble(nflow::kGain, dflt), uns ? SigType::UInt64 : SigType::Int64);
        a.line("out_" + a.id + " = "
            + codegenI64BinOp("mul", a.in[0], codegenI64Literal(k, uns, cLang), uns, sat, cLang)
            + ";");
    };
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCGain()
{
    BlockCodegenTemplate t;
    // Absent Gain defaults to 1.0 (unity passthrough), as in the interpreter
    // (handleGain) and the Rust backend.
    t.step = "out_{id} = {in0} * {param:Gain:1.0};";
    t.emitStepTyped = gainTypedEmitter(true, 1.0);
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustGain()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {param:Gain:1.0} * {in0};";
    t.emitStepTyped = gainTypedEmitter(false, 1.0);
    return t;
}
//=============================================================================
