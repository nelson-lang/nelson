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
#include "ExprEngine.hpp"
#include <stdexcept>
#include "userdefined_blocks.hpp"
//=============================================================================
// expression: evaluates a restricted math expression of u (plus t, dt and the
// diagram variables) at every step, through the module's own ExprEngine - no
// interpreter involved, so this block simulates everywhere AND generates
// C/Rust code (the generators reconstruct the expression from its AST).
// Scalar signals only, one input, one output, stateless.
//=============================================================================
bool
Nelson::NFlow::handleExpression(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::INIT && phase != Phase::ALGEBRAIC) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const std::string expr = bd.paramStr(nflow::kExpr, "u");
    if (phase == Phase::INIT) {
        // Validate once so a bad expression fails with a clear diagnostic
        // instead of erroring at every step.
        std::unordered_map<std::string, double> vars = ctx.variables;
        vars["u"] = 0.0;
        vars["t"] = 0.0;
        vars["dt"] = ctx.dt;
        try {
            (void)evalExpressionValue(expr, vars);
        } catch (const std::exception& e) {
            if (ctx.diag) {
                SimDiagnostic d;
                d.code = "expression_invalid";
                d.blockId = b.id;
                d.message = "expression block: invalid expression '" + expr + "': " + e.what();
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
        }
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        setOutput(ctx, b.nid, ctx.blockState[b.nid].output);
        return false;
    }
    std::unordered_map<std::string, double> vars = ctx.variables;
    vars["u"] = getInput(ctx, b.nid, 0, 0.0);
    vars["t"] = ctx.t;
    vars["dt"] = ctx.dt;
    double y = 0.0;
    try {
        y = evalExpressionValue(expr, vars);
    } catch (const std::exception&) {
        y = 0.0; // validated at INIT; runtime failures degrade to 0
    }
    return emitElementwise(ctx, b.nid, [&](int) { return y; });
}
//=============================================================================
