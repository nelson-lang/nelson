//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// logicalOperator: a single configurable logical block (Logical Operator).
// Operator is AND / OR / NAND / NOR / XOR / XNOR / NOT; it reduces the N boolean
// inputs element-wise (a nonzero input is true). NOT takes a single input and
// negates it. Pure algebraic feedthrough over the input width; C / Rust code
// generation. Complements the fixed and / or / xor / not blocks with one dispatchable block.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenHelpers.hpp"
#include <cmath>
#include <string>
#include <vector>
#include "logic_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Operator codes: 0 AND, 1 OR, 2 NAND, 3 NOR, 4 XOR, 5 XNOR, 6 NOT.
    static int
    logOp(const nflow::BlockDescriptor& bd)
    {
        const std::string s = bd.paramStr("Operator", "AND");
        if (s == "OR") {
            return 1;
        }
        if (s == "NAND") {
            return 2;
        }
        if (s == "NOR") {
            return 3;
        }
        if (s == "XOR") {
            return 4;
        }
        if (s == "XNOR") {
            return 5;
        }
        if (s == "NOT") {
            return 6;
        }
        return 0;
    }
    //=============================================================================
    bool
    handleLogicalOperator(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const int op = logOp(bd);
        const int n = numInputs(ctx, b.nid);
        std::vector<SigView> ins(n);
        for (int i = 0; i < n; ++i) {
            ins[i] = getInputSig(ctx, b.nid, i);
        }
        return emitElementwise(ctx, b.nid, [&](int k) -> double {
            if (op == 6) { // NOT (single input)
                const double v = (n > 0) ? sigAt(ins[0], k) : 0.0;
                return (v == 0.0) ? 1.0 : 0.0;
            }
            int trueCount = 0;
            bool anyTrue = false;
            bool allTrue = true;
            for (int i = 0; i < n; ++i) {
                const double v = sigAt(ins[i], k);
                const bool t = !(std::isnan(v) || v == 0.0);
                anyTrue = anyTrue || t;
                allTrue = allTrue && t;
                if (t) {
                    ++trueCount;
                }
            }
            switch (op) {
            case 1:
                return anyTrue ? 1.0 : 0.0; // OR
            case 2:
                return allTrue ? 0.0 : 1.0; // NAND
            case 3:
                return anyTrue ? 0.0 : 1.0; // NOR
            case 4:
                return (trueCount & 1) ? 1.0 : 0.0; // XOR
            case 5:
                return (trueCount & 1) ? 0.0 : 1.0; // XNOR
            case 0:
            default:
                return allTrue ? 1.0 : 0.0; // AND
            }
        });
    }
    //=============================================================================
    static std::string
    logExprC(int op, const std::vector<std::string>& in)
    {
        const int n = (int)in.size();
        if (op == 6) {
            const std::string x = n > 0 ? in[0] : std::string("0.0");
            return "(" + x + " == 0.0) ? 1.0 : 0.0";
        }
        std::string joined;
        for (int i = 0; i < n; ++i) {
            const std::string b = "(" + in[i] + " != 0.0)";
            if (op == 0 || op == 2) { // AND / NAND
                joined += (i ? " && " : "") + b;
            } else if (op == 1 || op == 3) { // OR / NOR
                joined += (i ? " || " : "") + b;
            } else { // XOR / XNOR: parity of the boolean count
                joined += (i ? " ^ " : "") + std::string("(int)") + b;
            }
        }
        if (op == 0) {
            return "(" + joined + ") ? 1.0 : 0.0";
        }
        if (op == 1) {
            return "(" + joined + ") ? 1.0 : 0.0";
        }
        if (op == 2) {
            return "(" + joined + ") ? 0.0 : 1.0";
        }
        if (op == 3) {
            return "(" + joined + ") ? 0.0 : 1.0";
        }
        if (op == 4) {
            return "((" + joined + ") & 1) ? 1.0 : 0.0";
        }
        return "((" + joined + ") & 1) ? 0.0 : 1.0"; // XNOR
    }
    //=============================================================================
    static std::string
    logExprRust(int op, const std::vector<std::string>& in)
    {
        const int n = (int)in.size();
        if (op == 6) {
            const std::string x = n > 0 ? in[0] : std::string("0.0_f64");
            return "if " + x + " == 0.0_f64 { 1.0_f64 } else { 0.0_f64 }";
        }
        std::string joined;
        for (int i = 0; i < n; ++i) {
            const std::string b = "(" + in[i] + " != 0.0_f64)";
            if (op == 0 || op == 2) {
                joined += (i ? " && " : "") + b;
            } else if (op == 1 || op == 3) {
                joined += (i ? " || " : "") + b;
            } else {
                joined += (i ? " ^ " : "") + std::string("(") + b + " as i64)";
            }
        }
        if (op == 0 || op == 1) {
            return "if " + joined + " { 1.0_f64 } else { 0.0_f64 }";
        }
        if (op == 2 || op == 3) {
            return "if " + joined + " { 0.0_f64 } else { 1.0_f64 }";
        }
        if (op == 4) {
            return "if ((" + joined + ") & 1) != 0 { 1.0_f64 } else { 0.0_f64 }";
        }
        return "if ((" + joined + ") & 1) != 0 { 0.0_f64 } else { 1.0_f64 }";
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCLogicalOperator()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            a.line("out_" + a.id + " = " + logExprC(logOp(bd), a.in) + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustLogicalOperator()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            a.line("out_" + a.id + " = " + logExprRust(logOp(bd), a.in) + ";");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
