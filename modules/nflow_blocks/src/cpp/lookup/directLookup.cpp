//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// directLookup: Direct Lookup Table (n-D), Element mode -- no interpolation.
// N integer index inputs select one element of a static column-major Table.
// The per-dimension sizes are given by TableDimensions = [n0, n1, ...]; the
// flat offset is sum_k idx_k * stride_k (stride_0 = 1, stride_k = prod_{m<k}
// n_m), with each index rounded to the nearest integer and clamped to
// [0, n_k-1] (zero-based). ALGEBRAIC feedthrough, real double, no state.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "lookup_blocks.hpp"
#include <cmath>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        int
        numDims(const nflow::BlockDescriptor& bd)
        {
            const int n = (int)bd.paramDouble("NumberOfTableDimensions", 0.0);
            return (n >= 1) ? n : 0;
        }
        //=============================================================================
        int
        clampIndex(double u, int n)
        {
            int idx = (int)std::lround(u);
            if (idx < 0) {
                idx = 0;
            }
            if (idx > n - 1) {
                idx = n - 1;
            }
            return idx;
        }
        //=============================================================================
        // BlockDescriptor resolves both JSON arrays and string expressions.
        std::vector<double>
        paramVec(const BlockCodegenArgs& a, const char* key)
        {
            if (!a.block || !a.variables) {
                return {};
            }
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            return bd.paramList(key);
        }
        //=============================================================================
        void
        emitDirectLookup(const BlockCodegenArgs& a, bool rust)
        {
            nflow::BlockDescriptor bdp(*a.block, *a.variables);
            int N = (int)bdp.paramDouble("NumberOfTableDimensions", 0.0);
            const std::vector<double> dimsD = paramVec(a, "TableDimensions");
            const std::vector<double> tbl = paramVec(a, "Table");
            size_t expected = 1;
            for (int k = 0; k < (int)dimsD.size(); ++k) {
                expected *= std::max<size_t>(1, (size_t)dimsD[k]);
            }
            if (N < 1 || (int)dimsD.size() != N || tbl.size() != expected) {
                a.line("out_" + a.id + (rust ? " = 0.0_f64;" : " = 0.0;"));
                return;
            }
            std::vector<int> dims(N, 1), stride(N, 1);
            for (int k = 0; k < N; ++k) {
                dims[k] = std::max(1, (int)dimsD[k]);
            }
            for (int k = 1; k < N; ++k) {
                stride[k] = stride[k - 1] * dims[k - 1];
            }
            const std::string tv = "__dl_tbl_" + a.id;
            std::string td = rust ? "let " + tv + " = [" : "static const double " + tv + "[] = {";
            for (size_t i = 0; i < tbl.size(); ++i) {
                td += (i ? ", " : "") + a.fmt(tbl[i]);
            }
            td += rust ? "];" : "};";
            a.line(td);
            std::string offExpr;
            for (int k = 0; k < N; ++k) {
                const std::string iv = "__di_" + a.id + "_" + std::to_string(k);
                const std::string hi = std::to_string(dims[k] - 1);
                if (rust) {
                    a.line("let mut " + iv + ": i32 = (" + a.in[k] + ").round() as i32;");
                    a.line("if " + iv + " < 0 { " + iv + " = 0; } if " + iv + " > " + hi + " { "
                        + iv + " = " + hi + "; }");
                } else {
                    a.line("int " + iv + " = (int)lround(" + a.in[k] + ");");
                    a.line("if (" + iv + " < 0) " + iv + " = 0; if (" + iv + " > " + hi + ") " + iv
                        + " = " + hi + ";");
                }
                offExpr += (k ? " + " : "") + iv + " * " + std::to_string(stride[k]);
            }
            const std::string off = "__doff_" + a.id;
            a.line((rust ? "let " : "int ") + off + " = " + offExpr + ";");
            a.line("out_" + a.id + " = " + tv + "[" + off + (rust ? " as usize" : "") + "];");
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handleDirectLookup(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const int N = numDims(bd);
        if (N < 1) {
            return false;
        }
        const std::vector<double> dimsD = bd.paramList("TableDimensions");
        if ((int)dimsD.size() != N) {
            return false;
        }
        std::vector<int> dims(N, 1), stride(N, 1);
        for (int k = 0; k < N; ++k) {
            dims[k] = std::max(1, (int)dimsD[k]);
            if (!hasInput(ctx, b.nid, k)) {
                return false;
            }
        }
        for (int k = 1; k < N; ++k) {
            stride[k] = stride[k - 1] * dims[k - 1];
        }
        const std::vector<double> tbl = bd.paramList("Table");
        std::vector<SigView> in(N);
        for (int k = 0; k < N; ++k) {
            in[k] = getInputSig(ctx, b.nid, k);
        }
        return emitElementwise(ctx, b.nid, [&](int e) {
            int off = 0;
            for (int k = 0; k < N; ++k) {
                off += clampIndex(sigAt(in[k], e), dims[k]) * stride[k];
            }
            return (off >= 0 && off < (int)tbl.size()) ? tbl[off] : 0.0;
        });
    }
    //=============================================================================
    bool
    resolveDirectLookupDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err)
    {
        nflow::BlockDescriptor bd(b, vars);
        const int N = numDims(bd);
        if (N < 1) {
            err = "directLookup requires NumberOfTableDimensions >= 1";
            return false;
        }
        const std::vector<double> dimsD = bd.paramList("TableDimensions");
        if ((int)dimsD.size() != N) {
            err = "directLookup requires TableDimensions with one size per dimension";
            return false;
        }
        size_t expected = 1;
        for (int k = 0; k < N; ++k) {
            expected *= std::max<size_t>(1, (size_t)dimsD[k]);
        }
        const std::vector<double> tbl = bd.paramList("Table");
        if (tbl.size() != expected) {
            err = "directLookup table size must equal the product of TableDimensions";
            return false;
        }
        int w = 1;
        for (const auto& s : inSigs) {
            w = std::max(w, s.width());
        }
        outSigs[0].setVector(w);
        outSigs[0].type = SigType::Double;
        return true;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCDirectLookup()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitDirectLookup(a, false); };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustDirectLookup()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitDirectLookup(a, true); };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
