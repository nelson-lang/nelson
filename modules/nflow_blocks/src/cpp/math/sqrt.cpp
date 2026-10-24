//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// sqrt: element-wise square-root family selected by params.Function:
//   sqrt        -> sqrt(u)           (u<0 -> NaN in the real path)
//   signedSqrt  -> sign(u)*sqrt(|u|) (always real)
//   rSqrt       -> 1/sqrt(u)         (reciprocal square root)
// Feedthrough (ALGEBRAIC), real double signals, vector element-wise; has
// C / Rust code generation. AlgorithmType/OutputSignalType are V1 = Exact/real.
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
        applySqrt(const std::string& fn, double x)
        {
            if (fn == "signedSqrt") {
                const double s = (x > 0.0) ? 1.0 : ((x < 0.0) ? -1.0 : 0.0);
                return s * std::sqrt(std::fabs(x));
            }
            if (fn == "rSqrt") {
                return 1.0 / std::sqrt(x);
            }
            return std::sqrt(x); // "sqrt" (default)
        }
        //=============================================================================
        BlockCodegenTemplate
        makeCodegen(bool rust)
        {
            BlockCodegenTemplate t;
            t.emitStep = [rust](const BlockCodegenArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const std::string fn = bd.paramStr("Function", "sqrt");
                const std::string sq = rust ? "libm::sqrt" : "sqrt";
                const std::string ab = rust ? "libm::fabs" : "fabs";
                const std::string u = a.in[0];
                if (fn == "signedSqrt") {
                    const std::string s = rust
                        ? "(((" + u + " > 0.0) as i32 - (" + u + " < 0.0) as i32) as f64)"
                        : "((double)((" + u + " > 0.0) - (" + u + " < 0.0)))";
                    a.line("out_" + a.id + " = " + s + " * " + sq + "(" + ab + "(" + u + "));");
                } else if (fn == "rSqrt") {
                    a.line("out_" + a.id + " = 1.0 / " + sq + "(" + u + ");");
                } else {
                    a.line("out_" + a.id + " = " + sq + "(" + u + ");");
                }
            };
            return t;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handleSqrt(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::string fn = bd.paramStr("Function", "sqrt");
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) { return applySqrt(fn, sigAt(u, i)); });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCSqrt()
    {
        return makeCodegen(false);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustSqrt()
    {
        return makeCodegen(true);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
