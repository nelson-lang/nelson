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
#include <cstdint>
#include <cmath>
#include "logic_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleRelationalOperator(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0) || !hasInput(ctx, b.nid, 1)) {
        return false;
    }
    const std::string op = operatorParam(b, nflow::kOperator, nflow::kGe);
    SigView a = getInputSig(ctx, b.nid, 0);
    SigView c = getInputSig(ctx, b.nid, 1);
    if (a.idata || c.idata) {
        // Exact-64 operands compare in integer space (the double projection
        // would collapse neighbours above 2^53).
        const SigType ty = a.idata ? a.type : c.type;
        return emitElementwise(ctx, b.nid, [&](int i) {
            return i64CompareOp(sigAtI64(a, i, ty), sigAtI64(c, i, ty), ty, op) ? 1.0 : 0.0;
        });
    }
    return emitElementwise(
        ctx, b.nid, [&](int i) { return compareValues(sigAt(a, i), sigAt(c, i), op) ? 1.0 : 0.0; });
}
//=============================================================================
// One emitter for both languages (CodegenLang). Operands of the SAME exact-64
// kind compare in integer space (doubles collapse neighbours above 2^53); a
// mixed pairing compares as double (Rust casts the typed side).
static Nelson::NFlow::BlockCodegenTemplate
makeRelationalOperatorCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    t.emitStep = [L](const BlockCodegenArgs& a) {
        const std::string op = nflow::jstr(*asJson(a.params), nflow::kOperator, nflow::kGe);
        codegenEmitTypedCompare(a, op, a.in[0], a.in[1], !L.rust);
    };
    t.emitStepTyped = [L](const BlockCodegenArgs& a) {
        const std::string op = nflow::jstr(*asJson(a.params), nflow::kOperator, nflow::kGe);
        const std::string t0 = a.inTypes.size() > 0 ? a.inTypes[0] : "double";
        const std::string t1 = a.inTypes.size() > 1 ? a.inTypes[1] : "double";
        if (codegenIsExact64(t0) && t0 == t1) {
            codegenEmitTypedCompare(a, op, a.in[0], a.in[1], !L.rust);
            return;
        }
        // Mixed operands: double-space comparison; Rust needs an explicit cast on
        // the typed side.
        auto asDouble = [&L](const std::string& expr, const std::string& tn) {
            if (L.rust && codegenIsExact64(tn)) {
                return "(" + expr + " as f64)";
            }
            return expr;
        };
        codegenEmitTypedCompare(a, op, asDouble(a.in[0], t0), asDouble(a.in[1], t1), !L.rust);
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCRelationalOperator()
{
    return makeRelationalOperatorCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustRelationalOperator()
{
    return makeRelationalOperatorCodegen({ true });
}
//=============================================================================
