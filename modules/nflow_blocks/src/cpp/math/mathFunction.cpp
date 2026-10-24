//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// mathFunction: a single elementwise math block whose params.Function selects
// the applied function (sin/cos/tan/asin/acos/atan/sinh/cosh/tanh/exp/log/
// log10/sqrt/sign). Covers the whole Coselica Blocks.Math scalar-function set
// in one palette entry. Feedthrough (ALGEBRAIC phase); real double signals; has
// C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "math_blocks.hpp"
#include <cmath>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        double
        applyMathFn(const std::string& fn, double x)
        {
            if (fn == "sin") {
                return std::sin(x);
            }
            if (fn == "cos") {
                return std::cos(x);
            }
            if (fn == "tan") {
                return std::tan(x);
            }
            if (fn == "asin") {
                return std::asin(x);
            }
            if (fn == "acos") {
                return std::acos(x);
            }
            if (fn == "atan") {
                return std::atan(x);
            }
            if (fn == "sinh") {
                return std::sinh(x);
            }
            if (fn == "cosh") {
                return std::cosh(x);
            }
            if (fn == "tanh") {
                return std::tanh(x);
            }
            if (fn == "exp") {
                return std::exp(x);
            }
            if (fn == "log") {
                return std::log(x);
            }
            if (fn == "log10") {
                return std::log10(x);
            }
            if (fn == "10^u") {
                return std::pow(10.0, x);
            }
            if (fn == "square" || fn == "magnitude^2") {
                // 'magnitude^2' is another tool's name for the same thing on a real signal.
                // Without it the block falls through to the identity below.
                return x * x;
            }
            if (fn == "sqrt") {
                return std::sqrt(x);
            }
            if (fn == "reciprocal") {
                return 1.0 / x;
            }
            if (fn == "sign") {
                return (x > 0.0) ? 1.0 : ((x < 0.0) ? -1.0 : 0.0);
            }
            return x;
        }
        //=============================================================================
        bool
        isBinaryFn(const std::string& fn)
        {
            return fn == "pow" || fn == "hypot" || fn == "rem" || fn == "mod";
        }
        //=============================================================================
        // Every name applyMathFn / applyBinaryFn implement. An unknown name would fall
        // through to the identity below and turn the block into a wire. Unlike its
        // siblings (trigFunction, roundingFunction, sqrt), 'Math Function' has no natural
        // default operation, so an unknown name is reported.
        bool
        isKnownMathFn(const std::string& fn)
        {
            static const char* kNames[] = { "sin", "cos", "tan", "asin", "acos", "atan", "sinh",
                "cosh", "tanh", "exp", "log", "log10", "10^u", "square", "magnitude^2", "sqrt",
                "reciprocal", "sign" };
            for (const char* n : kNames) {
                if (fn == n) {
                    return true;
                }
            }
            return isBinaryFn(fn);
        }
        //=============================================================================
        // Two-input functions. rem keeps the sign of the dividend (fmod); mod
        // keeps the sign of the divisor (floor modulo), with mod(a,0)=a.
        double
        applyBinaryFn(const std::string& fn, double a, double b)
        {
            if (fn == "pow") {
                return std::pow(a, b);
            }
            if (fn == "hypot") {
                return std::hypot(a, b);
            }
            if (fn == "rem") {
                return std::fmod(a, b);
            }
            if (b == 0.0) {
                return a;
            }
            double r = std::fmod(a, b);
            if (r != 0.0 && ((r < 0.0) != (b < 0.0))) {
                r += b;
            }
            return r;
        }
        //=============================================================================
        std::string
        codegenExpr(const std::string& fn, const std::string& arg, bool rust)
        {
            const std::string p = rust ? "libm::" : "";
            if (fn == "sin" || fn == "cos" || fn == "tan" || fn == "asin" || fn == "acos"
                || fn == "atan" || fn == "sinh" || fn == "cosh" || fn == "tanh" || fn == "exp"
                || fn == "log" || fn == "log10" || fn == "sqrt") {
                return p + fn + "(" + arg + ")";
            }
            if (fn == "10^u") {
                return p + "pow(10.0, " + arg + ")";
            }
            if (fn == "square" || fn == "magnitude^2") {
                return "((" + arg + ") * (" + arg + "))";
            }
            if (fn == "reciprocal") {
                return "(1.0 / (" + arg + "))";
            }
            if (fn == "sign") {
                // branchless sign as (x>0) - (x<0)
                if (rust) {
                    return "(((" + arg + " > 0.0) as i32 - (" + arg + " < 0.0) as i32) as f64)";
                }
                return "((double)((" + arg + " > 0.0) - (" + arg + " < 0.0)))";
            }
            return arg;
        }
        //=============================================================================
        BlockCodegenTemplate
        makeCodegen(bool rust)
        {
            BlockCodegenTemplate t;
            t.emitStep = [rust](const BlockCodegenArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const std::string fn = bd.paramStr("Function", "sin");
                const std::string p = rust ? "libm::" : "";
                if (fn == "pow" || fn == "hypot") {
                    a.line("out_" + a.id + " = " + p + fn + "(" + a.in[0] + ", " + a.in[1] + ");");
                    return;
                }
                if (fn == "rem") {
                    a.line("out_" + a.id + " = " + p + "fmod(" + a.in[0] + ", " + a.in[1] + ");");
                    return;
                }
                if (fn == "mod") {
                    // floor modulo: sign of the divisor, mod(a,0)=a.
                    if (rust) {
                        a.line("let a_" + a.id + " = " + a.in[0] + ";");
                        a.line("let b_" + a.id + " = " + a.in[1] + ";");
                        a.line(
                            "let mut r_" + a.id + " = libm::fmod(a_" + a.id + ", b_" + a.id + ");");
                        a.line("if r_" + a.id + " != 0.0 && (r_" + a.id + " < 0.0) != (b_" + a.id
                            + " < 0.0) { r_" + a.id + " += b_" + a.id + "; }");
                        a.line("out_" + a.id + " = if b_" + a.id + " == 0.0 { a_" + a.id
                            + " } else { r_" + a.id + " };");
                    } else {
                        a.line("double a_" + a.id + " = " + a.in[0] + ";");
                        a.line("double b_" + a.id + " = " + a.in[1] + ";");
                        a.line("double r_" + a.id + " = fmod(a_" + a.id + ", b_" + a.id + ");");
                        a.line("if (r_" + a.id + " != 0.0 && ((r_" + a.id + " < 0.0) != (b_" + a.id
                            + " < 0.0))) r_" + a.id + " += b_" + a.id + ";");
                        a.line("out_" + a.id + " = (b_" + a.id + " == 0.0) ? a_" + a.id + " : r_"
                            + a.id + ";");
                    }
                    return;
                }
                a.line("out_" + a.id + " = " + codegenExpr(fn, a.in[0], rust) + ";");
            };
            return t;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handleMathFunction(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase == Phase::INIT) {
            nflow::BlockDescriptor bdInit(b, ctx.variables);
            const std::string fnInit = bdInit.paramStr("Function", "sin");
            if (!isKnownMathFn(fnInit) && ctx.diag) {
                SimDiagnostic d;
                d.code = "math_function_unknown";
                d.blockId = b.id;
                d.message = std::string("block '") + b.id + "' (mathFunction) has no function '"
                    + fnInit
                    + "'. Supported: sin cos tan asin acos atan sinh cosh tanh exp log log10 "
                      "10^u square magnitude^2 sqrt reciprocal sign, and the two-input pow "
                      "hypot rem mod.";
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
            return false;
        }
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::string fn = bd.paramStr("Function", "sin");
        if (isBinaryFn(fn)) {
            if (!hasInput(ctx, b.nid, 1)) {
                return false;
            }
            SigView a = getInputSig(ctx, b.nid, 0);
            SigView bb = getInputSig(ctx, b.nid, 1);
            return emitElementwise(
                ctx, b.nid, [&](int i) { return applyBinaryFn(fn, sigAt(a, i), sigAt(bb, i)); });
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) { return applyMathFn(fn, sigAt(u, i)); });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCMathFunction()
    {
        return makeCodegen(false);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustMathFunction()
    {
        return makeCodegen(true);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
