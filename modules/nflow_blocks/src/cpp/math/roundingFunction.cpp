//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// roundingFunction: element-wise rounding block whose params.Operator selects
// the rounding mode (floor/ceil/round/fix). Feedthrough (ALGEBRAIC), real
// double signals, vector element-wise; has C / Rust code generation.
//   floor -> toward -inf     ceil -> toward +inf
//   round -> nearest, ties away from zero     fix -> truncate toward zero
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
        applyRounding(const std::string& op, double x)
        {
            if (op == "ceil") {
                return std::ceil(x);
            }
            if (op == "round") {
                return std::round(x); // ties away from zero
            }
            if (op == "fix") {
                return std::trunc(x);
            }
            return std::floor(x); // "floor" (default)
        }
        //=============================================================================
        std::string
        roundingFn(const std::string& op, bool rust)
        {
            const std::string p = rust ? "libm::" : "";
            if (op == "ceil") {
                return p + "ceil";
            }
            if (op == "round") {
                return p + "round";
            }
            if (op == "fix") {
                return p + "trunc";
            }
            return p + "floor";
        }
        //=============================================================================
        BlockCodegenTemplate
        makeCodegen(bool rust)
        {
            BlockCodegenTemplate t;
            t.emitStep = [rust](const BlockCodegenArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const std::string op = bd.paramStr("Operator", "floor");
                a.line("out_" + a.id + " = " + roundingFn(op, rust) + "(" + a.in[0] + ");");
            };
            return t;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handleRoundingFunction(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::string op = bd.paramStr("Operator", "floor");
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) { return applyRounding(op, sigAt(u, i)); });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCRoundingFunction()
    {
        return makeCodegen(false);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustRoundingFunction()
    {
        return makeCodegen(true);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
