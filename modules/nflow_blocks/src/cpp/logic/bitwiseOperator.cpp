//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// bitwiseOperator: bit-wise logic between the (integer-valued) input and a
// constant BitMask (Bitwise Operator, mask form). Operation is one of AND, OR,
// XOR, NAND, NOR, NOT (NOT ignores the mask). Values are reinterpreted as
// unsigned integers of NumBits width (default 32); the result is masked back to
// that width and returned as a double. Pure algebraic feedthrough, element-wise
// over the input width. C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenHelpers.hpp"
#include <cmath>
#include <cstdint>
#include <string>
#include "logic_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Operation codes: 0 AND, 1 OR, 2 XOR, 3 NAND, 4 NOR, 5 NOT.
    static int
    bwOp(const nflow::BlockDescriptor& bd)
    {
        const std::string s = bd.paramStr("Operation", "AND");
        if (s == "OR") {
            return 1;
        }
        if (s == "XOR") {
            return 2;
        }
        if (s == "NAND") {
            return 3;
        }
        if (s == "NOR") {
            return 4;
        }
        if (s == "NOT") {
            return 5;
        }
        return 0;
    }
    //=============================================================================
    static uint64_t
    bwFullMask(const nflow::BlockDescriptor& bd)
    {
        double nb = std::floor(bd.paramDouble("NumBits", 32.0));
        if (nb < 1.0) {
            nb = 1.0;
        }
        if (nb >= 64.0) {
            return ~0ULL;
        }
        return (1ULL << static_cast<unsigned>(nb)) - 1ULL;
    }
    //=============================================================================
    static uint64_t
    bwApply(int op, uint64_t x, uint64_t m, uint64_t fm)
    {
        switch (op) {
        case 1:
            return (x | m) & fm;
        case 2:
            return (x ^ m) & fm;
        case 3:
            return (~(x & m)) & fm;
        case 4:
            return (~(x | m)) & fm;
        case 5:
            return (~x) & fm;
        case 0:
        default:
            return (x & m) & fm;
        }
    }
    //=============================================================================
    bool
    handleBitwiseOperator(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const int op = bwOp(bd);
        const uint64_t fm = bwFullMask(bd);
        const uint64_t mask
            = static_cast<uint64_t>(std::llround(bd.paramDouble("BitMask", 0.0))) & fm;
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) -> double {
            const uint64_t x = static_cast<uint64_t>(std::llround(sigAt(u, i))) & fm;
            return static_cast<double>(bwApply(op, x, mask, fm));
        });
    }
    //=============================================================================
    static std::string
    bwExprC(int op, const std::string& xu, const std::string& m, const std::string& fm)
    {
        switch (op) {
        case 1:
            return "(double)((" + xu + " | " + m + ") & " + fm + ")";
        case 2:
            return "(double)((" + xu + " ^ " + m + ") & " + fm + ")";
        case 3:
            return "(double)((~(" + xu + " & " + m + ")) & " + fm + ")";
        case 4:
            return "(double)((~(" + xu + " | " + m + ")) & " + fm + ")";
        case 5:
            return "(double)((~" + xu + ") & " + fm + ")";
        case 0:
        default:
            return "(double)((" + xu + " & " + m + ") & " + fm + ")";
        }
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCBitwiseOperator()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const int op = bwOp(bd);
            const uint64_t fm = bwFullMask(bd);
            const uint64_t mask
                = static_cast<uint64_t>(std::llround(bd.paramDouble("BitMask", 0.0))) & fm;
            const std::string xu = "((unsigned long long)llround(" + a.in[0] + "))";
            const std::string m = std::to_string(mask) + "ULL";
            const std::string fms = std::to_string(fm) + "ULL";
            a.line("out_" + a.id + " = " + bwExprC(op, xu, m, fms) + ";");
        };
        return t;
    }
    //=============================================================================
    static std::string
    bwExprRust(int op, const std::string& xu, const std::string& m, const std::string& fm)
    {
        switch (op) {
        case 1:
            return "(((" + xu + " | " + m + ") & " + fm + ") as f64)";
        case 2:
            return "(((" + xu + " ^ " + m + ") & " + fm + ") as f64)";
        case 3:
            return "(((!(" + xu + " & " + m + ")) & " + fm + ") as f64)";
        case 4:
            return "(((!(" + xu + " | " + m + ")) & " + fm + ") as f64)";
        case 5:
            return "(((!" + xu + ") & " + fm + ") as f64)";
        case 0:
        default:
            return "(((" + xu + " & " + m + ") & " + fm + ") as f64)";
        }
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustBitwiseOperator()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const int op = bwOp(bd);
            const uint64_t fm = bwFullMask(bd);
            const uint64_t mask
                = static_cast<uint64_t>(std::llround(bd.paramDouble("BitMask", 0.0))) & fm;
            // Wrap negatives via i64 -> u64 (two's complement), matching the
            // interpreter and the C backend; a bare `as u64` saturates negative
            // inputs to 0 in Rust.
            const std::string xu = "((libm::round(" + a.in[0] + ") as i64) as u64)";
            const std::string m = std::to_string(mask) + "u64";
            const std::string fms = std::to_string(fm) + "u64";
            a.line("out_" + a.id + " = " + bwExprRust(op, xu, m, fms) + ";");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
