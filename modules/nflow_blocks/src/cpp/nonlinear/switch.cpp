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
#include "NFlowCodegenLang.hpp"
#include "NFlowCodegenTyped.hpp"
#include <cmath>
#include <algorithm>
#include "nonlinear_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleSwitch(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase == Phase::ZERO_CROSSING) {
        // Surface where the control crosses the threshold and the switch flips
        // its selected branch, one per signal element; the solver stops there.
        double* g = blockG(ctx, b.nid);
        if (g) {
            nflow::BlockDescriptor zbd(b, ctx.variables);
            double thr = zbd.paramDouble(nflow::kThreshold, 0.0);
            SigView cond = getInputSig(ctx, b.nid, 1);
            const int w = std::max(1, outputWidth(ctx, b.nid, 0));
            for (int i = 0; i < w; ++i) {
                g[i] = sigAt(cond, i) - thr;
            }
        }
        return false;
    }
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    std::string condition = nflow::kGe;
    if (asJson(b.params).contains(nflow::kCondition) && asJson(b.params)[nflow::kCondition].is_string()) {
        condition = asJson(b.params)[nflow::kCondition].get<std::string>();
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double threshold = bd.paramDouble(nflow::kThreshold, 0.0);

    SigView top = getInputSig(ctx, b.nid, 0);
    SigView cond = getInputSig(ctx, b.nid, 1);
    SigView bottom = getInputSig(ctx, b.nid, 2);
    auto takeTopAt = [&](int i) {
        const double condV = sigAt(cond, i);
        if (condition == nflow::kGt) {
            return condV > threshold;
        }
        if (condition == nflow::kNe) {
            return condV != threshold;
        }
        return condV >= threshold;
    };
    if (const PortSig* pex = exact64OutPort(ctx, b.nid)) {
        const SigType ty = pex->type;
        return emitElementwiseI64(ctx, b.nid,
            [&](int i) { return takeTopAt(i) ? sigAtI64(top, i, ty) : sigAtI64(bottom, i, ty); });
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(
            ctx, b.nid, [&](int i) { return takeTopAt(i) ? sigAtC(top, i) : sigAtC(bottom, i); });
    }
    return emitElementwise(
        ctx, b.nid, [&](int i) { return takeTopAt(i) ? sigAt(top, i) : sigAt(bottom, i); });
}
//=============================================================================
// Both targets use the simulator's comparison (Threshold and Condition). A
// typed (exact-64) control signal is compared in double space like the
// simulator's sigAt view; Rust needs the explicit cast to compile.
static Nelson::NFlow::BlockCodegenTemplate
makeSwitchCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    t.emitStep = [L](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        std::string condition = nflow::jstr(*asJson(a.params), nflow::kCondition, nflow::kGe);
        std::string threshold = nflow::formatNumber(bd.paramDouble(nflow::kThreshold, 0.0));
        std::string op = " >= ";
        if (condition == nflow::kGt) {
            op = " > ";
        } else if (condition == nflow::kNe) {
            op = " != ";
        }
        std::string cond = a.in[1];
        const std::string condType = a.inTypes.size() > 1 ? a.inTypes[1] : "double";
        if (L.rust && codegenIsExact64(condType)) {
            cond = "(" + cond + " as f64)";
        }
        if (L.rust) {
            a.line("out_" + a.id + " = if " + cond + op + threshold + " { " + a.in[0] + " } else { "
                + a.in[2] + " };");
        } else {
            a.line("out_" + a.id + " = (" + cond + op + threshold + ") ? " + a.in[0] + " : "
                + a.in[2] + ";");
        }
    };
    // Zero-crossing seam: one surface, control - Threshold (sim
    // ZERO_CROSSING parity: the solver stops where the branch flips).
    t.emitRk4 = [](const BlockCodegenArgs& a) {
        if (a.rk4Op != Rk4Crossing) {
            return;
        }
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.line(a.rk4Arr + "[" + std::to_string(a.crossingOffset) + "] = (" + a.in[1] + ") - "
            + a.fmt(bd.paramDouble(nflow::kThreshold, 0.0)) + ";");
    };
    t.crossingCount = [](const BlockCodegenArgs&) -> int { return 1; };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCSwitch()
{
    return makeSwitchCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustSwitch()
{
    return makeSwitchCodegen({ true });
}
//=============================================================================
