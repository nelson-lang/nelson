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
#include <cmath>
#include <algorithm>
#include "source_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleConstant(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const PortSig* ps = portSigOf(ctx, b.nid, 0);
    const int w = ps ? ps->width() : 1;
    const SigType ty = ps ? ps->type : SigType::Double;
    if (ps && portUsesI64(*ps)) {
        // Exact-64 constant: read Value straight from the JSON (integer literals
        // stay exact) and fill both lanes.
        auto exactAt = [&](size_t i) -> long long {
            const json* v = nullptr;
            if (b.params.contains(nflow::kValue)) {
                const json& jv = b.params[nflow::kValue];
                if (jv.is_array()) {
                    if (jv.empty()) {
                        return 0;
                    }
                    v = (i < jv.size()) ? &jv[i] : &jv.back();
                } else {
                    v = &jv;
                }
            }
            if (!v) {
                return 0;
            }
            if (v->is_number_unsigned()) {
                return static_cast<long long>(v->get<unsigned long long>());
            }
            if (v->is_number_integer()) {
                return v->get<long long>();
            }
            if (v->is_number_float()) {
                return quantizeDoubleToI64(v->get<double>(), ty);
            }
            return quantizeDoubleToI64(bd.paramDouble(nflow::kValue, 0.0), ty);
        };
        long long* di = outputSliceI64(ctx, b.nid, 0);
        double* y = outputSlice(ctx, b.nid, 0);
        for (int i = 0; i < w; ++i) {
            const long long v = exactAt((size_t)i);
            if (di) {
                di[i] = v;
            }
            y[i] = i64ToDouble(v, ty);
        }
        return false;
    }
    if (w <= 1) {
        double v = bd.paramDouble(nflow::kValue, 0.0);
        if (ty != SigType::Double) {
            v = quantizeToType(v, ty);
        }
        setOutput(ctx, b.nid, v);
        return false;
    }
    // Vector constant: the Value parameter is a list; the port was sized to
    // its length by the dimension-propagation pass.
    std::vector<double> lst = bd.paramList(nflow::kValue);
    double* y = outputSlice(ctx, b.nid, 0);
    for (int i = 0; i < w; ++i) {
        double v = (i < (int)lst.size()) ? lst[i] : (lst.empty() ? 0.0 : lst.back());
        y[i] = (ty != SigType::Double) ? quantizeToType(v, ty) : v;
    }
    return false;
}
//=============================================================================
namespace {
// Exact-64 constant emission: read Value straight from the JSON (integer
// literals stay exact).
Nelson::NFlow::BlockCodegenEmitFn
constantTypedEmitter(bool cLang)
{
    using namespace Nelson::NFlow;
    return [cLang](const BlockCodegenArgs& a) {
        const bool uns = codegenIsU64(a.outType);
        long long v = 0;
        if (a.params && a.params->contains(nflow::kValue)) {
            const json& jv = (*a.params)[nflow::kValue];
            const json* e = jv.is_array() ? (jv.empty() ? nullptr : &jv[0]) : &jv;
            if (e) {
                if (e->is_number_unsigned()) {
                    v = static_cast<long long>(e->get<unsigned long long>());
                } else if (e->is_number_integer()) {
                    v = e->get<long long>();
                } else if (e->is_number_float()) {
                    v = quantizeDoubleToI64(
                        e->get<double>(), uns ? SigType::UInt64 : SigType::Int64);
                }
            }
        }
        a.line("out_" + a.id + " = " + codegenI64Literal(v, uns, cLang) + ";");
    };
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCConstant()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {param:Value:0.0};";
    t.emitStepTyped = constantTypedEmitter(true);
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustConstant()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {param:Value:0.0};";
    t.emitStepTyped = constantTypedEmitter(false);
    return t;
}
//=============================================================================
