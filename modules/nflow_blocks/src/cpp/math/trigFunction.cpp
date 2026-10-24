//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// trigFunction: element-wise trigonometric block whose params.Function selects
// the applied function:
//   sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh  (1 in / 1 out)
//   atan2  -> atan2(u1, u2) with u1 on port 0 and u2 on port 1   (2 in / 1 out)
//   sincos -> sin(u) on port 0 and cos(u) on port 1              (1 in / 2 out)
// (the complex cos+jsin variant is handled separately). Feedthrough
// (ALGEBRAIC), real double signals, vector element-wise; has C / Rust code
// generation via the libm intrinsics.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "math_blocks.hpp"
#include <cmath>
#include <string>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        double
        applyTrigFn(const std::string& fn, double x)
        {
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
            if (fn == "asinh") {
                return std::asinh(x);
            }
            if (fn == "acosh") {
                return std::acosh(x);
            }
            if (fn == "atanh") {
                return std::atanh(x);
            }
            return std::sin(x); // "sin" (default)
        }
        //=============================================================================
        // Every supported function maps directly to the libm intrinsic of the
        // same name (bare in C, libm:: in Rust #![no_std]).
        std::string
        trigIntrinsic(const std::string& fn)
        {
            if (fn == "cos" || fn == "tan" || fn == "asin" || fn == "acos" || fn == "atan"
                || fn == "sinh" || fn == "cosh" || fn == "tanh" || fn == "asinh" || fn == "acosh"
                || fn == "atanh") {
                return fn;
            }
            return "sin";
        }
        //=============================================================================
        // Two-output extractor helper: writes pairFn(i).first to port 0 and
        // .second to port 1 (both real), demux-style change detection.
        template <typename PairFn>
        bool
        emitTwoRealOutputs(SimCtx& ctx, BlockId nid, int w, PairFn pairFn)
        {
            double* y0 = outputSlice(ctx, nid, 0);
            double* y1 = outputSlice(ctx, nid, 1);
            bool changed = false;
            for (int i = 0; i < w; ++i) {
                const auto pv = pairFn(i);
                if (y0[i] != pv.first) {
                    y0[i] = pv.first;
                    changed = true;
                }
                if (y1[i] != pv.second) {
                    y1[i] = pv.second;
                    changed = true;
                }
            }
            return changed;
        }
        //=============================================================================
        BlockCodegenTemplate
        makeCodegen(bool rust)
        {
            BlockCodegenTemplate t;
            t.emitStep = [rust](const BlockCodegenArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const std::string raw = bd.paramStr("Function", "sin");
                const std::string p = rust ? "libm::" : "";
                if (raw == "atan2") {
                    a.line("out_" + a.id + " = " + p + "atan2(" + a.in[0] + ", " + a.in[1] + ");");
                    return;
                }
                if (raw == "sincos") {
                    a.line("out_" + a.id + " = " + p + "sin(" + a.in[0] + ");");
                    a.line("out_" + a.id + "_p1 = " + p + "cos(" + a.in[0] + ");");
                    return;
                }
                a.line("out_" + a.id + " = " + p + trigIntrinsic(raw) + "(" + a.in[0] + ");");
            };
            return t;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handleTrigFunction(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::string fn = bd.paramStr("Function", "sin");
        if (fn == "atan2") {
            if (!hasInput(ctx, b.nid, 1)) {
                return false;
            }
            SigView y = getInputSig(ctx, b.nid, 0);
            SigView x = getInputSig(ctx, b.nid, 1);
            return emitElementwise(
                ctx, b.nid, [&](int i) { return std::atan2(sigAt(y, i), sigAt(x, i)); });
        }
        if (fn == "sincos") {
            SigView u = getInputSig(ctx, b.nid, 0);
            const int w = std::max(1, outputWidth(ctx, b.nid, 0));
            return emitTwoRealOutputs(ctx, b.nid, w, [&](int i) {
                const double v = sigAt(u, i);
                return std::pair<double, double> { std::sin(v), std::cos(v) };
            });
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) { return applyTrigFn(fn, sigAt(u, i)); });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCTrigFunction()
    {
        return makeCodegen(false);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustTrigFunction()
    {
        return makeCodegen(true);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
