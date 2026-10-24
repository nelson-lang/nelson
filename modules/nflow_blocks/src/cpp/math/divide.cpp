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
Nelson::NFlow::handleDivide(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0) || !hasInput(ctx, b.nid, 1)) {
        return false;
    }
    SigView num = getInputSig(ctx, b.nid, 0);
    SigView den = getInputSig(ctx, b.nid, 1);
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        const bool sat = blockSaturates(ctx, b.nid);
        // Integer division rounds per the Rounding parameter (floor by default).
        nflow::BlockDescriptor dbd(b, ctx.variables);
        const std::string rname = dbd.paramStr(nflow::kRounding, "floor");
        SigRounding rounding = SigRounding::Floor;
        if (rname == "nearest") {
            rounding = SigRounding::Nearest;
        } else if (rname == "ceiling" || rname == "ceil") {
            rounding = SigRounding::Ceiling;
        } else if (rname == "zero" || rname == "fix") {
            rounding = SigRounding::Zero;
        }
        return emitElementwiseI64(ctx, b.nid, [&](int i) {
            return i64ApplyDiv(sigAtI64(num, i, ty), sigAtI64(den, i, ty, 1), ty, sat, rounding);
        });
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int i) {
            std::complex<double> denom = sigAtC(den, i, 1.0);
            if (std::abs(denom) < 1e-300) {
                denom = { 1e-300, 0.0 };
            }
            return sigAtC(num, i) / denom;
        });
    }
    return emitElementwise(ctx, b.nid, [&](int i) {
        double denom = sigAt(den, i, 1.0);
        if (std::abs(denom) < 1e-300) {
            denom = denom < 0.0 ? -1e-300 : 1e-300;
        }
        return sigAt(num, i) / denom;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCDivide()
{
    BlockCodegenTemplate t;
    t.step = "{ double denom = {in1};\n"
             "  if (fabs(denom) < 1e-300) denom = denom < 0.0 ? -1e-300 : 1e-300;\n"
             "  out_{id} = {in0} / denom; }";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        // Integer division rounds per the block's Rounding parameter
        // (floor by default, matching the runtime).
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string rname = bd.paramStr(nflow::kRounding, "floor");
        const bool uns = codegenIsU64(a.outType);
        std::string mode = "floor";
        if (rname == "zero" || rname == "fix") {
            mode = uns ? "floor" : "trunc"; // unsigned trunc == floor
        } else if (rname == "ceiling" || rname == "ceil") {
            mode = "ceil";
        } else if (rname == "nearest") {
            mode = "round";
        }
        const std::string fn = std::string(uns ? "nflow_u64_div_" : "nflow_i64_div_") + mode;
        a.line("out_" + a.id + " = " + fn + "(" + a.in[0] + ", " + a.in[1] + ");");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustDivide()
{
    BlockCodegenTemplate t;
    // Preserve the divisor's sign, matching the interpreter (handleDivide) and
    // the C backend.
    t.step = "{ let denom = {in1};\n"
             "  let denom = if libm::fabs(denom) < 1e-300_f64 { if denom < 0.0_f64 { -1e-300_f64 } "
             "else { 1e-300_f64 } } else { denom };\n"
             "  out_{id} = {in0} / denom; }";
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        // Integer division rounds per the block's Rounding parameter
        // (floor by default, matching the runtime).
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string rname = bd.paramStr(nflow::kRounding, "floor");
        const bool uns = codegenIsU64(a.outType);
        if (uns) {
            std::string adjust = "na / nb"; // floor == trunc for unsigned
            if (rname == "ceiling" || rname == "ceil") {
                adjust = "na / nb + if na % nb != 0 { 1 } else { 0 }";
            } else if (rname == "nearest") {
                adjust = "na / nb + if 2 * (na % nb) >= nb { 1 } else { 0 }";
            }
            a.line("out_" + a.id + " = { let na: u64 = " + a.in[0] + "; let nb: u64 = " + a.in[1]
                + "; if nb == 0 { if na == 0 { 0 } else { u64::MAX } } else { " + adjust + " } };");
            return;
        }
        std::string adjust;
        if (rname == "zero" || rname == "fix") {
            adjust = "q";
        } else if (rname == "ceiling" || rname == "ceil") {
            adjust = "if r != 0 && ((na < 0) == (nb < 0)) { q + 1 } else { q }";
        } else if (rname == "nearest") {
            adjust = "if r != 0 && 2 * r.unsigned_abs() >= nb.unsigned_abs() { if (na < 0) != "
                     "(nb < 0) { q - 1 } else { q + 1 } } else { q }";
        } else { // floor
            adjust = "if r != 0 && ((na < 0) != (nb < 0)) { q - 1 } else { q }";
        }
        a.line("out_" + a.id + " = { let na: i64 = " + a.in[0] + "; let nb: i64 = " + a.in[1]
            + "; if nb == 0 { if na == 0 { 0 } else if na > 0 { i64::MAX } else { i64::MIN } }"
            + " else if na == i64::MIN && nb == -1 { i64::MAX }"
            + " else { let q = na / nb; let r = na % nb; " + adjust + " } };");
    };
    return t;
}
//=============================================================================
