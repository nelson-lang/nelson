//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// polynomial: evaluates a polynomial with constant Coefficients (highest power
// first, descending-power polyval order) at the input using Horner's method (Polynomial
// block). For Coefficients = [a b c], out = a*u^2 + b*u + c. Pure algebraic
// feedthrough, element-wise over the input width; C / Rust code generation
// (the Horner form is unrolled over the static coefficients).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "math_blocks.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handlePolynomial(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::vector<double> c = bd.paramList("Coefficients");
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) {
            const double x = sigAt(u, i);
            double acc = c.empty() ? 0.0 : c[0];
            for (size_t k = 1; k < c.size(); ++k) {
                acc = acc * x + c[k];
            }
            return acc;
        });
    }
    //=============================================================================
    static void
    emitPolynomial(const BlockCodegenArgs& a, bool rust)
    {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::vector<double> c = bd.paramList("Coefficients");
        const std::string suffix = rust ? "_f64" : "";
        if (c.empty()) {
            a.line("out_" + a.id + " = 0.0" + suffix + ";");
            return;
        }
        // Horner: (((c0)*x + c1)*x + c2) ... unrolled over the static coefficients.
        std::string expr = a.fmt(c[0]);
        for (size_t k = 1; k < c.size(); ++k) {
            expr = "(" + expr + ") * " + a.in[0] + " + " + a.fmt(c[k]);
        }
        a.line("out_" + a.id + " = " + expr + ";");
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCPolynomial()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitPolynomial(a, false); };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustPolynomial()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitPolynomial(a, true); };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
