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
#include "NFlowCodegenHelpers.hpp"
#include "NFlowCodegenTyped.hpp"
#include "math_blocks.hpp"
#include <cmath>
#include <algorithm>
//=============================================================================
namespace {
// Exact-64 sum emission shared by both languages: fold the connected inputs
// through saturating/wrapping add/sub per the signs string.
Nelson::NFlow::BlockCodegenEmitFn
sumTypedEmitter(bool cLang)
{
    using namespace Nelson::NFlow;
    return [cLang](const BlockCodegenArgs& a) {
        const bool uns = codegenIsU64(a.outType);
        const bool sat = codegenSaturates(a);
        std::string signs = nflow::jstr(*asJson(a.params), nflow::kSumInputs, "");
        std::string acc;
        bool first = true;
        for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
            if (a.inputIds[i].empty()) {
                continue;
            }
            const std::string expr = (i < (int)a.in.size()) ? a.in[i] : "";
            const bool minus = (i < (int)signs.size() && signs[i] == '-');
            if (first) {
                if (minus) {
                    acc = codegenI64BinOp("sub",
                        cLang ? (uns ? "0ULL" : "0LL") : (uns ? "0_u64" : "0_i64"), expr, uns, sat,
                        cLang);
                } else {
                    acc = expr;
                }
                first = false;
                continue;
            }
            acc = codegenI64BinOp(minus ? "sub" : "add", acc, expr, uns, sat, cLang);
        }
        if (acc.empty()) {
            acc = cLang ? (uns ? "0ULL" : "0LL") : (uns ? "0_u64" : "0_i64");
        }
        a.line("out_" + a.id + " = " + acc + ";");
    };
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleSum(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    int n = numInputs(ctx, b.nid);

    std::string signs;
    if (asJson(b.params).contains(nflow::kSumInputs)
        && asJson(b.params)[nflow::kSumInputs].is_string()) {
        signs = asJson(b.params)[nflow::kSumInputs].get<std::string>();
    }

    std::vector<SigView> ins(n);
    for (int i = 0; i < n; ++i) {
        ins[i] = getInputSig(ctx, b.nid, i);
    }
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        const bool sat = blockSaturates(ctx, b.nid);
        return emitElementwiseI64(ctx, b.nid, [&](int k) {
            long long out = 0;
            for (int i = 0; i < n; ++i) {
                const long long v = sigAtI64(ins[i], k, ty);
                const bool minus = (i < (int)signs.size() && signs[i] == '-');
                out = minus ? i64ApplySub(out, v, ty, sat) : i64ApplyAdd(out, v, ty, sat);
            }
            return out;
        });
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int k) {
            std::complex<double> out { 0.0, 0.0 };
            for (int i = 0; i < n; ++i) {
                const double sign = (i < (int)signs.size() && signs[i] == '-') ? -1.0 : 1.0;
                out += sigAtC(ins[i], k) * sign;
            }
            return out;
        });
    }
    return emitElementwise(ctx, b.nid, [&](int k) {
        double out = 0.0;
        for (int i = 0; i < n; ++i) {
            double sign = (i < (int)signs.size() && signs[i] == '-') ? -1.0 : 1.0;
            out += sigAt(ins[i], k) * sign;
        }
        return out;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCSum()
{
    BlockCodegenTemplate t;
    // Variable-arity emission with per-input signs: needs a native emitter.
    t.emitStep = [](const BlockCodegenArgs& a) {
        std::string signs = nflow::jstr(*asJson(a.params), nflow::kSumInputs, "");
        std::vector<std::string> terms;
        for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
            if (a.inputIds[i].empty()) {
                continue;
            }
            bool minus = (i < static_cast<int>(signs.size()) && signs[i] == '-');
            // a.in[i] is the per-port input expression (already wrapped).
            std::string expr = (i < static_cast<int>(a.in.size())) ? a.in[i] : "0.0";
            terms.push_back(minus ? "-" + expr : expr);
        }
        if (terms.empty()) {
            terms.push_back("0.0");
        }
        std::string joined;
        for (size_t i = 0; i < terms.size(); ++i) {
            if (i > 0) {
                joined += " + ";
            }
            joined += terms[i];
        }
        a.line("out_" + a.id + " = " + joined + ";");
    };
    t.emitStepTyped = sumTypedEmitter(true);
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustSum()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        std::vector<std::string> inList;
        for (size_t i = 0; i < a.inputIds.size(); ++i) {
            // a.in[i] is the per-port input expression.
            inList.push_back(a.inputIds[i].empty() || i >= a.in.size() ? "0.0_f64" : a.in[i]);
        }
        std::string expr = "0.0_f64";
        std::string signs = nflow::jstr(*asJson(a.params), nflow::kSumInputs, "");
        for (int i = 0; i < static_cast<int>(inList.size()); ++i) {
            char sign = (i < static_cast<int>(signs.size()) && signs[i] == '-') ? '-' : '+';
            expr = (i == 0 && sign == '+') ? inList[i] : expr + " " + sign + " " + inList[i];
        }
        if (inList.empty()) {
            expr = "0.0_f64";
        }
        a.line("out_" + a.id + " = " + expr + ";");
    };
    t.emitStepTyped = sumTypedEmitter(false);
    return t;
}
//=============================================================================
