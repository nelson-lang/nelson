//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "routing_blocks.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenTyped.hpp"
//=============================================================================
// convert: explicit signal-type cast (OutDataType), with per-block overflow
// policy (SaturateOnOverflow, default saturate) and rounding mode (Rounding:
// nearest / floor / ceiling / zero).
//=============================================================================
bool
Nelson::NFlow::handleConvert(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    SigType ty = SigType::Double;
    sigTypeFromString(bd.paramStr(nflow::kOutDataType, "double"), ty);
    const bool saturate = blockSaturates(ctx, b.nid);
    SigRounding rounding = SigRounding::Nearest;
    const std::string rname = bd.paramStr(nflow::kRounding, "nearest");
    if (rname == "floor") {
        rounding = SigRounding::Floor;
    } else if (rname == "ceiling" || rname == "ceil") {
        rounding = SigRounding::Ceiling;
    } else if (rname == "zero" || rname == "fix") {
        rounding = SigRounding::Zero;
    }
    SigView u = getInputSig(ctx, b.nid, 0);
    if (sigTypeIsExact64(ty)) {
        // Exact-64 target: integer-space cast from an exact source,
        // quantizeDoubleToI64 from a double-backed one.
        return emitElementwiseI64(ctx, b.nid, [&](int i) {
            if (u.idata) {
                return requantizeI64(sigAtI64(u, i, u.type), u.type, ty, saturate);
            }
            return quantizeDoubleToI64(sigAt(u, i), ty, saturate, rounding);
        });
    }
    // Exact-64 source into a non-exact target: the double lane already carries the
    // projection, so the double path below is the cast (loss above 2^53 is inherent
    // to the narrowing). emitElementwise re-quantizes with round-to-nearest, but the
    // convert block owns its rounding mode, so quantize here (idempotent on a second pass).
    return emitElementwise(
        ctx, b.nid, [&](int i) { return quantizeToType(sigAt(u, i), ty, saturate, rounding); });
}
//=============================================================================
// Scalar-path code generation. The generated model stores signals as double;
// convert emits the round + saturate/wrap of a scalar cast (double result).
namespace {
std::string
roundExpr(const std::string& lang, const std::string& rname, const std::string& x)
{
    const bool c = (lang == "c");
    if (rname == "floor") {
        return c ? "floor(" + x + ")" : "libm::floor(" + x + ")";
    }
    if (rname == "ceiling" || rname == "ceil") {
        return c ? "ceil(" + x + ")" : "libm::ceil(" + x + ")";
    }
    if (rname == "zero" || rname == "fix") {
        return c ? "trunc(" + x + ")" : "libm::trunc(" + x + ")";
    }
    return c ? "round(" + x + ")" : "libm::round(" + x + ")";
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
convertTemplate(const std::string& lang)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    const bool c = (lang == "c");
    t.emitStep = [c, lang](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        SigType ty = SigType::Double;
        sigTypeFromString(bd.paramStr(nflow::kOutDataType, "double"), ty);
        const std::string in = a.in[0];
        const std::string zero = c ? "0.0" : "0.0_f64";
        if (ty == SigType::Double) {
            a.line("out_" + a.id + " = " + in + ";");
            return;
        }
        if (ty == SigType::Single) {
            a.line(c ? "out_" + a.id + " = (double)(float)(" + in + ");"
                     : "out_" + a.id + " = (" + in + " as f32) as f64;");
            return;
        }
        if (ty == SigType::Boolean) {
            a.line(c ? "out_" + a.id + " = (" + in + " != 0.0) ? 1.0 : 0.0;"
                     : "out_" + a.id + " = if " + in + " != 0.0_f64 { 1.0_f64 } else { 0.0_f64 };");
            return;
        }
        (void)zero;
        // Integer types: round then saturate (default) or leave to wrap.
        double lo = 0.0, hi = 0.0;
        sigTypeRange(ty, lo, hi);
        const std::string rname = bd.paramStr(nflow::kRounding, "nearest");
        const std::string r = roundExpr(lang, rname, in);
        const bool saturate = !(asJson(a.params) && asJson(a.params)->contains(nflow::kSaturateOnOverflow)
            && (*asJson(a.params))[nflow::kSaturateOnOverflow].is_boolean()
            && !(*asJson(a.params))[nflow::kSaturateOnOverflow].get<bool>());
        if (!saturate) {
            // Emit the rounded raw value. The small-type quantization pass appends the
            // modular wrap after this block (nflow_quant_i with sat=0).
            a.line("out_" + a.id + " = " + r + ";");
            return;
        }
        if (c) {
            a.line(
                "out_" + a.id + " = fmax(" + a.fmt(lo) + ", fmin(" + a.fmt(hi) + ", " + r + "));");
        } else {
            a.line("out_" + a.id + " = libm::fmax(" + a.fmt(lo) + ", libm::fmin(" + a.fmt(hi) + ", "
                + r + "));");
        }
    };
    // Exact-64 casts (either endpoint int64/uint64). The C generator
    // emits its nflow_d_to_* helpers whenever an exact-64 port exists.
    t.emitStepTyped = [c, lang](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        SigType ty = SigType::Double;
        sigTypeFromString(bd.paramStr(nflow::kOutDataType, "double"), ty);
        const std::string dstName = sigTypeName(ty);
        const std::string srcName = a.inTypes.empty() ? "double" : a.inTypes[0];
        const bool dst64 = codegenIsExact64(dstName);
        const bool src64 = codegenIsExact64(srcName);
        const bool sat = codegenSaturates(a);
        const std::string in = a.in[0];
        const std::string rname = bd.paramStr(nflow::kRounding, "nearest");
        if (dst64 && src64) {
            const bool dstU = codegenIsU64(dstName);
            const bool srcU = codegenIsU64(srcName);
            if (dstU == srcU) {
                a.line("out_" + a.id + " = " + in + ";");
            } else if (dstU) { // int64 -> uint64
                if (c) {
                    a.line("out_" + a.id + " = "
                        + (sat ? "(" + in + " < 0) ? 0ULL : (uint64_t)" + in : "(uint64_t)" + in)
                        + ";");
                } else {
                    a.line("out_" + a.id + " = "
                        + (sat ? "if " + in + " < 0 { 0_u64 } else { " + in + " as u64 }"
                               : in + " as u64")
                        + ";");
                }
            } else { // uint64 -> int64
                if (c) {
                    a.line("out_" + a.id + " = "
                        + (sat ? "(" + in + " > (uint64_t)INT64_MAX) ? INT64_MAX : (int64_t)" + in
                               : "(int64_t)" + in)
                        + ";");
                } else {
                    a.line("out_" + a.id + " = "
                        + (sat ? "if " + in + " > i64::MAX as u64 { i64::MAX } else { " + in
                                    + " as i64 }"
                               : in + " as i64")
                        + ";");
                }
            }
            return;
        }
        if (dst64) {
            // double-backed source -> exact-64: round, then saturating
            // conversion (wrap-on-overflow is not modeled here either).
            const std::string r = roundExpr(lang, rname, in);
            const bool dstU = codegenIsU64(dstName);
            if (c) {
                a.line("out_" + a.id + " = "
                    + std::string(dstU ? "nflow_d_to_u64_sat(" : "nflow_d_to_i64_sat(") + r + ")"
                    + (sat ? "" : "; /* nflow: wrap not modeled for 64-bit casts */")
                    + (sat ? ";" : ""));
            } else {
                // Rust float->int `as` saturates (NaN -> 0) by language rule.
                a.line("out_" + a.id + " = " + r + " as " + (dstU ? "u64" : "i64")
                    + (sat ? ";" : "; // nflow: wrap not modeled for 64-bit casts"));
            }
            return;
        }
        // exact-64 source -> double-backed target: project to double,
        // then the standard double-space quantization.
        const std::string proj = c ? "(double)" + in : "(" + in + " as f64)";
        if (ty == SigType::Double) {
            a.line("out_" + a.id + " = " + proj + ";");
            return;
        }
        if (ty == SigType::Single) {
            a.line(c ? "out_" + a.id + " = (double)(float)" + proj + ";"
                     : "out_" + a.id + " = (" + proj + " as f32) as f64;");
            return;
        }
        if (ty == SigType::Boolean) {
            a.line(
                c ? "out_" + a.id + " = (" + proj + " != 0.0) ? 1.0 : 0.0;"
                  : "out_" + a.id + " = if " + proj + " != 0.0_f64 { 1.0_f64 } else { 0.0_f64 };");
            return;
        }
        double lo = 0.0, hi = 0.0;
        sigTypeRange(ty, lo, hi);
        const std::string r = roundExpr(lang, rname, proj);
        if (c) {
            a.line(
                "out_" + a.id + " = fmax(" + a.fmt(lo) + ", fmin(" + a.fmt(hi) + ", " + r + "));");
        } else {
            a.line("out_" + a.id + " = libm::fmax(" + a.fmt(lo) + ", libm::fmin(" + a.fmt(hi) + ", "
                + r + "));");
        }
    };
    return t;
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCConvert()
{
    return convertTemplate("c");
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustConvert()
{
    return convertTemplate("rust");
}
//=============================================================================
