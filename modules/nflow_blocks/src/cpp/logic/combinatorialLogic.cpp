//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// combinatorialLogic: a truth-table lookup (Combinatorial Logic). The single
// input is a vector of N boolean elements forming a binary index (element 0 is
// the most-significant bit); the output is TruthTable[index], where TruthTable
// is a 2^N-entry column read from the parameter list. Non-zero inputs count as
// 1. Pure algebraic feedthrough with a scalar output. An out-of-range index or
// an empty table yields 0.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenLang.hpp"
#include <cmath>
#include <cstdint>
#include <vector>
#include "logic_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleCombinatorialLogic(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::vector<double> table = bd.paramList("TruthTable");
        SigView u = getInputSig(ctx, b.nid, 0);
        const int n = (u.width > 0) ? u.width : 1;
        uint64_t index = 0;
        for (int i = 0; i < n && i < 63; ++i) {
            index = (index << 1) | ((sigAt(u, i) != 0.0) ? 1ULL : 0ULL);
        }
        double out = 0.0;
        if (index < table.size()) {
            out = table[static_cast<size_t>(index)];
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    // The vector expansion keeps ONE block whose input elements become N
    // scalar ports (MSB-first, the runtime's bit-fold order). The truth
    // table is baked as a constant array and indexed at runtime; an index
    // beyond the table yields 0.0 like the handler.
    //=============================================================================
    static BlockCodegenTemplate
    makeCombinatorialLogicCodegen(CodegenLang L)
    {
        BlockCodegenTemplate t;
        t.emitStep = [L](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::vector<double> table = bd.paramList("TruthTable");
            if (table.empty()) {
                // No table: every index misses, like the handler.
                a.line("out_" + a.id + " = 0.0;");
                return;
            }
            const size_t n = a.inputIds.empty() ? 1 : a.inputIds.size();
            std::string tbl;
            for (size_t k = 0; k < table.size(); ++k) {
                if (k > 0) {
                    tbl += ", ";
                }
                tbl += a.fmt(table[k]);
            }
            const std::string sz = std::to_string(table.size());
            if (L.rust) {
                a.line("{");
                a.line("    let cl_tbl: [f64; " + sz + "] = [" + tbl + "];");
                a.line("    let mut cl_i: usize = 0;");
                for (size_t i = 0; i < n; ++i) {
                    a.line(
                        "    cl_i = (cl_i << 1) | (if " + a.in[i] + " != 0.0 { 1 } else { 0 });");
                }
                a.line(
                    "    out_" + a.id + " = if cl_i < " + sz + " { cl_tbl[cl_i] } else { 0.0 };");
                a.line("}");
            } else {
                a.line("{");
                a.line("  static const double cl_tbl_" + a.id + "[" + sz + "] = { " + tbl + " };");
                a.line("  unsigned long cl_i = 0;");
                for (size_t i = 0; i < n; ++i) {
                    a.line("  cl_i = (cl_i << 1) | ((" + a.in[i] + ") != 0.0 ? 1u : 0u);");
                }
                a.line(
                    "  out_" + a.id + " = (cl_i < " + sz + "u) ? cl_tbl_" + a.id + "[cl_i] : 0.0;");
                a.line("}");
            }
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCCombinatorialLogic()
    {
        return makeCombinatorialLogicCodegen({ false });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustCombinatorialLogic()
    {
        return makeCombinatorialLogicCodegen({ true });
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
