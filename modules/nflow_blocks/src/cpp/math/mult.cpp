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
#include "NFlowMatrixProduct.hpp"
#include <cmath>
#include <algorithm>
//=============================================================================
namespace {
struct IntegerProductMatrix
{
    int rows = 1, cols = 1;
    std::vector<long long> values { 1 };
};
IntegerProductMatrix
multiplyIntegerMatrices(const IntegerProductMatrix& a, const IntegerProductMatrix& b,
    Nelson::NFlow::SigType type, bool saturate)
{
    using namespace Nelson::NFlow;
    IntegerProductMatrix out;
    if (a.values.size() == 1 || b.values.size() == 1) {
        out = a.values.size() == 1 ? b : a;
        const long long scalar = a.values.size() == 1 ? a.values[0] : b.values[0];
        for (auto& value : out.values) {
            value = i64ApplyMul(value, scalar, type, saturate);
        }
        return out;
    }
    out.rows = a.rows;
    out.cols = b.cols;
    out.values.assign(out.rows * out.cols, 0);
    for (int col = 0; col < b.cols; ++col) {
        for (int row = 0; row < a.rows; ++row) {
            for (int k = 0; k < a.cols; ++k) {
                auto& v = out.values[col * out.rows + row];
                v = i64ApplyAdd(v,
                    i64ApplyMul(
                        a.values[k * a.rows + row], b.values[col * b.rows + k], type, saturate),
                    type, saturate);
            }
        }
    }
    return out;
}
bool
handleIntegerMatrixProduct(Nelson::NFlow::SimCtx& ctx, const Nelson::NFlow::Block& b,
    const std::string& operations, const Nelson::NFlow::PortSig& output)
{
    using namespace Nelson::NFlow;
    IntegerProductMatrix result;
    const bool saturate = blockSaturates(ctx, b.nid);
    nflow::BlockDescriptor descriptor(b, ctx.variables);
    const std::string mode = descriptor.paramStr(nflow::kRounding, "floor");
    SigRounding rounding = SigRounding::Floor;
    if (mode == "nearest") {
        rounding = SigRounding::Nearest;
    } else if (mode == "ceiling" || mode == "ceil") {
        rounding = SigRounding::Ceiling;
    } else if (mode == "zero" || mode == "fix") {
        rounding = SigRounding::Zero;
    }
    for (int i = 0; i < numInputs(ctx, b.nid); ++i) {
        const SigView input = getInputSig(ctx, b.nid, i);
        if (input.width <= 0) {
            continue;
        }
        const bool divide = i < static_cast<int>(operations.size()) && operations[i] == '/';
        if (divide && input.width > 1) {
            if (ctx.diag) {
                SimDiagnostic d;
                d.code = "product_integer_matrix_division";
                d.blockId = b.id;
                d.message = "block '" + b.id
                    + "': integer matrix division is not supported; convert inputs to single or "
                      "double";
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
            return false;
        }
        if (divide) {
            const long long divisor = sigAtI64(input, 0, output.type, 1);
            for (auto& value : result.values) {
                value = i64ApplyDiv(value, divisor, output.type, saturate, rounding);
            }
            continue;
        }
        IntegerProductMatrix next;
        next.rows = input.rows;
        next.cols = input.cols;
        next.values.resize(input.width);
        for (int k = 0; k < input.width; ++k) {
            next.values[k] = sigAtI64(input, k, output.type, 1);
        }
        result = multiplyIntegerMatrices(result, next, output.type, saturate);
    }
    return emitElementwiseI64(ctx, b.nid, [&](int i) { return result.values[i]; });
}
bool
handleMatrixProduct(Nelson::NFlow::SimCtx& ctx, const Nelson::NFlow::Block& b)
{
    using namespace Nelson::NFlow;
    const auto& params = asJson(b.params);
    const std::string operations = params.contains("Inputs") && params["Inputs"].is_string()
        ? params["Inputs"].get<std::string>()
        : std::string();
    if (const PortSig* output = exact64OutPort(ctx, b.nid)) {
        return handleIntegerMatrixProduct(ctx, b, operations, *output);
    }
    const PortSig* output = portSigOf(ctx, b.nid, 0);
    const SigType outputType = output ? output->type : SigType::Double;
    const bool saturate = blockSaturates(ctx, b.nid);
    ProductMatrix result;
    for (int i = 0; i < numInputs(ctx, b.nid); ++i) {
        const SigView input = getInputSig(ctx, b.nid, i);
        if (input.width <= 0) {
            continue;
        }
        ProductMatrix next;
        next.rows = input.rows;
        next.cols = input.cols;
        next.values.resize(input.width);
        for (int k = 0; k < input.width; ++k) {
            next.values[k] = sigAtC(input, k);
        }
        if (i < static_cast<int>(operations.size()) && operations[i] == '/') {
            if (input.width == 1 && std::abs(next.values[0]) != 0.0) {
                for (auto& value : result.values) {
                    value = quantizeProductValue(value / next.values[0], outputType, saturate);
                }
                continue;
            }
            if (!invertProductMatrix(next, outputType, saturate)) {
                if (ctx.diag) {
                    SimDiagnostic d;
                    d.code = "product_singular";
                    d.blockId = b.id;
                    d.message = "block '" + b.id + "': matrix Product divisor is singular";
                    d.time = ctx.t;
                    ctx.diag->fail(d);
                }
                return false;
            }
        }
        result = multiplyProductMatrices(result, next, outputType, saturate);
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int i) { return result.values[i]; });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return result.values[i].real(); });
}
}
//=============================================================================
bool
Nelson::NFlow::handleMult(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (isMatrixProduct(b)) {
        return handleMatrixProduct(ctx, b);
    }
    int n = numInputs(ctx, b.nid);
    std::string operations;
    if (asJson(b.params).contains(nflow::kSumInputs)
        && asJson(b.params)[nflow::kSumInputs].is_string()) {
        operations = asJson(b.params)[nflow::kSumInputs].get<std::string>();
    }
    const auto divides = [&](int index) {
        return index < static_cast<int>(operations.size()) && operations[index] == '/';
    };
    std::vector<SigView> ins(n);
    for (int i = 0; i < n; ++i) {
        ins[i] = getInputSig(ctx, b.nid, i);
    }
    if (const PortSig* ps = exact64OutPort(ctx, b.nid)) {
        const SigType ty = ps->type;
        const bool sat = blockSaturates(ctx, b.nid);
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::string roundingName = bd.paramStr(nflow::kRounding, "floor");
        SigRounding rounding = SigRounding::Floor;
        if (roundingName == "nearest") {
            rounding = SigRounding::Nearest;
        } else if (roundingName == "ceiling" || roundingName == "ceil") {
            rounding = SigRounding::Ceiling;
        } else if (roundingName == "zero" || roundingName == "fix") {
            rounding = SigRounding::Zero;
        }
        return emitElementwiseI64(ctx, b.nid, [&](int k) {
            long long out = 1;
            for (int i = 0; i < n; ++i) {
                const long long value = sigAtI64(ins[i], k, ty, 1);
                out = divides(i) ? i64ApplyDiv(out, value, ty, sat, rounding)
                                 : i64ApplyMul(out, value, ty, sat);
            }
            return out;
        });
    }
    if (complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int k) {
            std::complex<double> out { 1.0, 0.0 };
            for (int i = 0; i < n; ++i) {
                std::complex<double> value = sigAtC(ins[i], k, 1.0);
                if (divides(i)) {
                    if (std::abs(value) < 1e-300) {
                        value = { 1e-300, 0.0 };
                    }
                    out /= value;
                } else {
                    out *= value;
                }
            }
            return out;
        });
    }
    return emitElementwise(ctx, b.nid, [&](int k) {
        double out = 1.0;
        for (int i = 0; i < n; ++i) {
            double value = sigAt(ins[i], k, 1.0);
            if (divides(i)) {
                if (std::abs(value) < 1e-300) {
                    value = value < 0.0 ? -1e-300 : 1e-300;
                }
                out /= value;
            } else {
                out *= value;
            }
        }
        return out;
    });
}
//=============================================================================
namespace {
// Exact-64 product emission: fold the connected inputs through
// saturating/wrapping multiplication.
Nelson::NFlow::BlockCodegenEmitFn
multTypedEmitter(bool cLang)
{
    using namespace Nelson::NFlow;
    return [cLang](const BlockCodegenArgs& a) {
        const bool uns = codegenIsU64(a.outType);
        const bool sat = codegenSaturates(a);
        const std::string operations = nflow::jstr(*asJson(a.params), nflow::kSumInputs, "");
        const std::string output = "out_" + a.id;
        const std::string one = cLang ? (uns ? "1ULL" : "1LL") : (uns ? "1_u64" : "1_i64");
        if (operations.find('/') == std::string::npos) {
            std::string acc;
            for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
                if (a.inputIds[i].empty()) {
                    continue;
                }
                const std::string expr = i < static_cast<int>(a.in.size()) ? a.in[i] : one;
                acc = acc.empty() ? expr : codegenI64BinOp("mul", acc, expr, uns, sat, cLang);
            }
            a.line(output + " = " + (acc.empty() ? one : acc) + ";");
            return;
        }
        a.line(output + " = " + one + ";");
        const auto divide = cLang ? getCodeGenCDivide() : getCodeGenRustDivide();
        for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
            if (a.inputIds[i].empty()) {
                continue;
            }
            const std::string expr = (i < static_cast<int>(a.in.size())) ? a.in[i] : one;
            if (i < static_cast<int>(operations.size()) && operations[i] == '/') {
                BlockCodegenArgs args = a;
                args.in = { output, expr };
                divide.emitStepTyped(args);
            } else {
                a.line(
                    output + " = " + codegenI64BinOp("mul", output, expr, uns, sat, cLang) + ";");
            }
        }
    };
}
//=============================================================================
} // namespace
//=============================================================================
// Variable-arity product over the connected inputs (the empty product is 1),
// as in the interpreter's fold. A fixed "{in0} * {in1} * {in2}" template drops
// inputs past a hard-coded count, and a different count per language.
static Nelson::NFlow::BlockCodegenTemplate
makeMultCodegen(bool cLang)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    const std::string one = cLang ? "1.0" : "1.0_f64";
    t.inputDefault = one;
    t.emitStep = [one, cLang](const BlockCodegenArgs& a) {
        const std::string operations = nflow::jstr(*asJson(a.params), nflow::kSumInputs, "");
        const auto& params = *asJson(a.params);
        const std::string mode = params.value("Multiplication", std::string());
        const bool matrixPivot
            = params.value("MatrixInversePivot", false) || mode == "Matrix(*)" || mode == "matrix";
        const std::string output = "out_" + a.id;
        if (operations.find('/') == std::string::npos) {
            std::string joined;
            for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
                if (a.inputIds[i].empty()) {
                    continue;
                }
                if (!joined.empty()) {
                    joined += " * ";
                }
                joined += i < static_cast<int>(a.in.size()) ? a.in[i] : one;
            }
            a.line(output + " = " + (joined.empty() ? one : joined) + ";");
            return;
        }
        a.line(output + " = " + one + ";");
        for (int i = 0; i < static_cast<int>(a.inputIds.size()); ++i) {
            if (a.inputIds[i].empty()) {
                continue;
            }
            const std::string expr = (i < static_cast<int>(a.in.size())) ? a.in[i] : one;
            if (i < static_cast<int>(operations.size()) && operations[i] == '/') {
                if (matrixPivot) {
                    a.line(cLang ? output + " = (" + expr + " == 0.0 ? NAN : " + output + " / "
                                + expr + ");"
                                 : output + " = if " + expr + " == 0.0_f64 { f64::NAN } else { "
                                + output + " / " + expr + " };");
                    continue;
                }
                if (cLang) {
                    a.line("{ double den = " + expr
                        + "; if (fabs(den) < 1e-300) den = den < 0.0 ? -1e-300 : 1e-300; " + output
                        + " /= den; }");
                } else {
                    a.line("{ let den = " + expr
                        + "; let den = if libm::fabs(den) < 1e-300_f64 { if den < 0.0_f64 { "
                          "-1e-300_f64 } else { 1e-300_f64 } } else { den }; "
                        + output + " /= den; }");
                }
            } else {
                a.line(output + " *= " + expr + ";");
            }
        }
    };
    t.emitStepTyped = multTypedEmitter(cLang);
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCMult()
{
    return makeMultCodegen(true);
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustMult()
{
    return makeMultCodegen(false);
}
//=============================================================================
