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
#include "NFlowCodegenTyped.hpp"
#include <cstdint>
#include <cmath>
#include "logic_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleCompareToConstant(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const double cst = bd.paramDouble(nflow::kConst, 0.0);
    const std::string op = operatorParam(b, nflow::kRelop, nflow::kGe);
    SigView u = getInputSig(ctx, b.nid, 0);
    if (u.idata) {
        const long long icst = quantizeDoubleToI64(cst, u.type);
        return emitElementwise(ctx, b.nid, [&](int i) {
            return i64CompareOp(sigAtI64(u, i, u.type), icst, u.type, op) ? 1.0 : 0.0;
        });
    }
    return emitElementwise(
        ctx, b.nid, [&](int i) { return compareValues(sigAt(u, i), cst, op) ? 1.0 : 0.0; });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCCompareToConstant()
{
    BlockCodegenTemplate t;
    // The comparison operator comes from the operator parameter: native emitter.
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        std::string op = nflow::jstr(*a.params, nflow::kRelop, nflow::kGe);
        std::string lhs = a.in[0];
        std::string rhs = a.fmt(bd.paramDouble(nflow::kConst, 0.0));
        std::string cmp;
        if (op == "gt" || op == ">") {
            cmp = lhs + " > " + rhs;
        } else if (op == "le" || op == "<=") {
            cmp = lhs + " <= " + rhs;
        } else if (op == "lt" || op == "<") {
            cmp = lhs + " < " + rhs;
        } else if (op == "eq" || op == "==") {
            cmp = lhs + " == " + rhs;
        } else if (op == "ne" || op == "!=" || op == "~=") {
            cmp = lhs + " != " + rhs;
        } else {
            cmp = lhs + " >= " + rhs;
        }
        a.line("out_" + a.id + " = (" + cmp + ") ? 1.0 : 0.0;");
    };
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string op = nflow::jstr(*a.params, nflow::kRelop, nflow::kGe);
        const std::string srcName = a.inTypes.empty() ? "double" : a.inTypes[0];
        const bool uns = codegenIsU64(srcName);
        const long long k = quantizeDoubleToI64(
            bd.paramDouble(nflow::kConst, 0.0), uns ? SigType::UInt64 : SigType::Int64);
        codegenEmitTypedCompare(a, op, a.in[0], codegenI64Literal(k, uns, true), true);
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustCompareToConstant()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        std::string op = nflow::jstr(*a.params, nflow::kRelop, nflow::kGe);
        std::string lhs = a.in[0];
        std::string rhs = a.fmt(bd.paramDouble(nflow::kConst, 0.0));
        std::string cmp;
        if (op == "gt" || op == ">") {
            cmp = lhs + " > " + rhs;
        } else if (op == "le" || op == "<=") {
            cmp = lhs + " <= " + rhs;
        } else if (op == "lt" || op == "<") {
            cmp = lhs + " < " + rhs;
        } else if (op == "eq" || op == "==") {
            cmp = lhs + " == " + rhs;
        } else if (op == "ne" || op == "!=" || op == "~=") {
            cmp = lhs + " != " + rhs;
        } else {
            cmp = lhs + " >= " + rhs;
        }
        a.line("out_" + a.id + " = if " + cmp + " { 1.0_f64 } else { 0.0_f64 };");
    };
    t.emitStepTyped = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string op = nflow::jstr(*a.params, nflow::kRelop, nflow::kGe);
        const std::string srcName = a.inTypes.empty() ? "double" : a.inTypes[0];
        const bool uns = codegenIsU64(srcName);
        const long long k = quantizeDoubleToI64(
            bd.paramDouble(nflow::kConst, 0.0), uns ? SigType::UInt64 : SigType::Int64);
        codegenEmitTypedCompare(a, op, a.in[0], codegenI64Literal(k, uns, false), false);
    };
    return t;
}
//=============================================================================
