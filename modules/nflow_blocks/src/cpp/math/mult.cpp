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
Nelson::NFlow::handleMult(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    int n = numInputs(ctx, b.nid);
    std::vector<SigView> ins(n);
    for (int i = 0; i < n; ++i) {
        ins[i] = getInputSig(ctx, b.nid, i);
    }
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        const bool sat = blockSaturates(ctx, b.nid);
        return emitElementwiseI64(ctx, b.nid, [&](int k) {
            long long out = 1;
            for (int i = 0; i < n; ++i) {
                out = i64ApplyMul(out, sigAtI64(ins[i], k, ty, 1), ty, sat);
            }
            return out;
        });
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int k) {
            std::complex<double> out { 1.0, 0.0 };
            for (int i = 0; i < n; ++i) {
                out *= sigAtC(ins[i], k, 1.0);
            }
            return out;
        });
    }
    return emitElementwise(ctx, b.nid, [&](int k) {
        double out = 1.0;
        for (int i = 0; i < n; ++i) {
            out *= sigAt(ins[i], k, 1.0);
        }
        return out;
    });
}
//=============================================================================
namespace {
// Exact-64 product emission: fold the connected inputs through
// saturating/wrapping multiplication.
Nelson::NFlow::BlockCodegenEmitFn
multTypedEmitter(bool cLang)
{
    using namespace Nelson::NFlow;
    return [cLang](const BlockCodegenArgs& a) {
        const bool uns = codegenIsU64(a.outType);
        const bool sat = codegenSaturates(a);
        std::string acc;
        for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
            if (a.inputIds[i].empty()) {
                continue;
            }
            const std::string expr = (i < (int)a.in.size()) ? a.in[i] : "";
            if (acc.empty()) {
                acc = expr;
                continue;
            }
            acc = codegenI64BinOp("mul", acc, expr, uns, sat, cLang);
        }
        if (acc.empty()) {
            acc = cLang ? (uns ? "1ULL" : "1LL") : (uns ? "1_u64" : "1_i64");
        }
        a.line("out_" + a.id + " = " + acc + ";");
    };
}
//=============================================================================
} // namespace
//=============================================================================
// Variable-arity product over the connected inputs (the empty product is 1),
// as in the interpreter's fold. A fixed "{in0} * {in1} * {in2}" template drops
// inputs past a hard-coded count, and a different count per language.
static Nelson::NFlow::BlockCodegenTemplate
makeMultCodegen(bool cLang)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    const std::string one = cLang ? "1.0" : "1.0_f64";
    t.inputDefault = one;
    t.emitStep = [one](const BlockCodegenArgs& a) {
        std::vector<std::string> factors;
        for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
            if (a.inputIds[i].empty()) {
                continue;
            }
            // a.in[i] is the per-port input expression (already wrapped).
            factors.push_back((i < static_cast<int>(a.in.size())) ? a.in[i] : one);
        }
        if (factors.empty()) {
            factors.push_back(one);
        }
        std::string joined;
        for (size_t i = 0; i < factors.size(); ++i) {
            if (i > 0) {
                joined += " * ";
            }
            joined += factors[i];
        }
        a.line("out_" + a.id + " = " + joined + ";");
    };
    t.emitStepTyped = multTypedEmitter(cLang);
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCMult()
{
    return makeMultCodegen(true);
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustMult()
{
    return makeMultCodegen(false);
}
//=============================================================================
