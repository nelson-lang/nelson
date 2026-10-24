//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// lookupND: n-D lookup table (V1: 1..4 dimensions). N inputs (one coordinate
// per dimension) index a static column-major Table: element (i0,i1,...) at
// sum_k i_k * stride_k with stride_0 = 1, stride_k = prod_{m<k} n_m.
//
// Multilinear interpolation as the weighted blend of the 2^N corners: with a
// per-axis weight w_k in [0,1] (Flat -> 0, Nearest -> round(f), Linear -> f),
// the corner c contributes prod_k (bit_k ? w_k : 1-w_k) of Table[offset(c)].
// ALGEBRAIC feedthrough, real double, no state.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "lookup_blocks.hpp"
#include "lookup_spline.hpp"
#include <string>
#include <vector>
#include <functional>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        void
        axisLocate(const std::vector<double>& bp, double u, bool linExtrap, int& i, double& f)
        {
            const int n = (int)bp.size();
            if (u <= bp[0]) {
                i = 0;
                f = linExtrap ? (u - bp[0]) / (bp[1] - bp[0]) : 0.0;
                return;
            }
            if (u >= bp[n - 1]) {
                i = n - 2;
                f = linExtrap ? (u - bp[n - 2]) / (bp[n - 1] - bp[n - 2]) : 1.0;
                return;
            }
            i = n - 2;
            while (i > 0 && u < bp[i]) {
                --i;
            }
            f = (u - bp[i]) / (bp[i + 1] - bp[i]);
        }
        //=============================================================================
        double
        axisWeight(double f, const std::string& interp)
        {
            if (interp == "Flat") {
                return 0.0;
            }
            if (interp == "Nearest") {
                return (f < 0.5) ? 0.0 : 1.0;
            }
            return f;
        }
        //=============================================================================
        bool
        isStrictlyIncreasing(const std::vector<double>& v)
        {
            for (size_t i = 1; i < v.size(); ++i) {
                if (!(v[i] > v[i - 1])) {
                    return false;
                }
            }
            return true;
        }
        //=============================================================================
        int
        numDims(const nflow::BlockDescriptor& bd)
        {
            const int n = (int)bd.paramDouble("NumberOfTableDimensions", 0.0);
            return (n >= 1) ? n : 0;
        }
        //=============================================================================
        std::string
        dimKey(int k)
        {
            return "BreakpointsForDimension" + std::to_string(k + 1);
        }
        //=============================================================================
        // Parse via BlockDescriptor so JSON arrays and string expressions both resolve.
        std::vector<double>
        paramVec(const BlockCodegenArgs& a, const std::string& key)
        {
            if (!a.block || !a.variables) {
                return {};
            }
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            return bd.paramList(key.c_str());
        }
        //=============================================================================
        std::string
        emitArray(const BlockCodegenArgs& a, const std::string& name, const std::vector<double>& v,
            bool rust)
        {
            std::string s
                = rust ? "let " + name + " = [" : "static const double " + name + "[] = {";
            for (size_t i = 0; i < v.size(); ++i) {
                s += (i ? ", " : "") + a.fmt(v[i]);
            }
            s += rust ? "];" : "};";
            return s;
        }
        //=============================================================================
        void
        emitLocate(const BlockCodegenArgs& a, bool rust, const std::string& arr,
            const std::string& uv, const std::string& iv, const std::string& fv, int n,
            bool linExtrap)
        {
            const std::string N1 = std::to_string(n - 1);
            const std::string N2 = std::to_string(n - 2);
            const std::string ix = rust ? iv + " as usize" : iv;
            const std::string belowF = linExtrap
                ? "(" + uv + " - " + arr + "[0]) / (" + arr + "[1] - " + arr + "[0])"
                : (rust ? "0.0_f64" : "0.0");
            const std::string aboveF = linExtrap ? "(" + uv + " - " + arr + "[" + N2 + "]) / ("
                    + arr + "[" + N1 + "] - " + arr + "[" + N2 + "])"
                                                 : (rust ? "1.0_f64" : "1.0");
            if (rust) {
                a.line("let " + iv + ": i32; let " + fv + ": f64;");
                a.line("if " + uv + " <= " + arr + "[0] { " + iv + " = 0; " + fv + " = " + belowF
                    + "; }");
                a.line("else if " + uv + " >= " + arr + "[" + N1 + "] { " + iv + " = " + N2 + "; "
                    + fv + " = " + aboveF + "; }");
                a.line("else { let mut __k2 = " + N2 + "i32; while __k2 > 0 && " + uv + " < " + arr
                    + "[__k2 as usize] { __k2 -= 1; } " + iv + " = __k2; " + fv + " = (" + uv
                    + " - " + arr + "[__k2 as usize]) / (" + arr + "[(__k2+1) as usize] - " + arr
                    + "[__k2 as usize]); }");
            } else {
                a.line("int " + iv + "; double " + fv + ";");
                a.line("if (" + uv + " <= " + arr + "[0]) { " + iv + " = 0; " + fv + " = " + belowF
                    + "; }");
                a.line("else if (" + uv + " >= " + arr + "[" + N1 + "]) { " + iv + " = " + N2 + "; "
                    + fv + " = " + aboveF + "; }");
                a.line("else { " + iv + " = " + N2 + "; while (" + iv + " > 0 && " + uv + " < "
                    + arr + "[" + iv + "]) " + iv + "--; " + fv + " = (" + uv + " - " + arr + "["
                    + iv + "]) / (" + arr + "[" + iv + "+1] - " + arr + "[" + iv + "]); }");
            }
            (void)ix;
        }
        //=============================================================================
        std::string
        weightExpr(const std::string& fv, const std::string& interp, bool rust)
        {
            if (interp == "Flat") {
                return rust ? "0.0_f64" : "0.0";
            }
            if (interp == "Nearest") {
                return rust ? "(if " + fv + " < 0.5 { 0.0 } else { 1.0 })"
                            : "((" + fv + " < 0.5) ? 0.0 : 1.0)";
            }
            return fv;
        }
        //=============================================================================
        void
        emitLookupND(const BlockCodegenArgs& a, bool rust)
        {
            nflow::BlockDescriptor bdp(*a.block, *a.variables);
            int N = (int)bdp.paramDouble("NumberOfTableDimensions", 0.0);
            const std::string interp = bdp.paramStr("InterpMethod", "Linear point-slope");
            const bool linExtrap = (bdp.paramStr("ExtrapMethod", "Linear") == "Linear");
            std::vector<std::vector<double>> bps;
            size_t expected = 1;
            for (int k = 0; k < N; ++k) {
                std::vector<double> bp = paramVec(a, dimKey(k));
                if (bp.size() < 2) {
                    N = 0;
                    break;
                }
                expected *= bp.size();
                bps.push_back(std::move(bp));
            }
            const std::vector<double> tbl = paramVec(a, "Table");
            if (N < 1 || tbl.size() != expected) {
                a.line("out_" + a.id + (rust ? " = 0.0_f64;" : " = 0.0;"));
                return;
            }
            if (lookupspline::isSpline(interp)) {
                // Separable tensor-product spline: axis-0 coefficients are
                // baked per innermost column; every outer axis reduces at run
                // time (per-column dense solve) via the shared helpers.
                lookupspline::emitSplineLookupCodegen(
                    a, rust, bps, tbl, interp, bdp.paramStr("ExtrapMethod", "Linear"));
                return;
            }
            const std::string tv = "__tblN_" + a.id;
            a.line(emitArray(a, tv, tbl, rust));
            std::vector<std::string> idxNames, wNames;
            std::vector<int> strides(N, 1);
            for (int k = 1; k < N; ++k) {
                strides[k] = strides[k - 1] * (int)bps[k - 1].size();
            }
            for (int k = 0; k < N; ++k) {
                const std::string ks = std::to_string(k);
                const std::string bpv = "__bpN_" + a.id + "_" + ks;
                a.line(emitArray(a, bpv, bps[k], rust));
                const std::string uv = "__uN_" + a.id + "_" + ks;
                const std::string iv = "__iN_" + a.id + "_" + ks;
                const std::string fv = "__fN_" + a.id + "_" + ks;
                const std::string wv = "__wN_" + a.id + "_" + ks;
                a.line((rust ? "let " : "double ") + uv + " = " + a.in[k] + ";");
                emitLocate(a, rust, bpv, uv, iv, fv, (int)bps[k].size(), linExtrap);
                a.line(
                    (rust ? "let " : "double ") + wv + " = " + weightExpr(fv, interp, rust) + ";");
                idxNames.push_back(iv);
                wNames.push_back(wv);
            }
            const std::string y = "__yN_" + a.id;
            std::string idxArr, wArr, strArr;
            for (int k = 0; k < N; ++k) {
                idxArr += (k ? ", " : "") + idxNames[k];
                wArr += (k ? ", " : "") + wNames[k];
                strArr += (k ? ", " : "") + std::to_string(strides[k]);
            }
            const std::string Ns = std::to_string(N);
            if (rust) {
                a.line("let __idxN_" + a.id + ": [i32; " + Ns + "] = [" + idxArr + "];");
                a.line("let __wArrN_" + a.id + ": [f64; " + Ns + "] = [" + wArr + "];");
                a.line("let __strideN_" + a.id + ": [i32; " + Ns + "] = [" + strArr + "];");
                a.line("let mut " + y + " = 0.0_f64;");
                a.line("for __c in 0..(1i32 << " + Ns + ") {");
                a.line("  let mut __cw = 1.0_f64; let mut __off: i32 = 0;");
                a.line("  for __k in 0.." + Ns + " {");
                a.line("    let __bit = (__c >> __k) & 1;");
                a.line("    __cw *= if __bit == 1 { __wArrN_" + a.id
                    + "[__k] } else { 1.0 - __wArrN_" + a.id + "[__k] };");
                a.line("    __off += (__idxN_" + a.id + "[__k] + __bit) * __strideN_" + a.id
                    + "[__k];");
                a.line("  }");
                a.line("  " + y + " += __cw * " + tv + "[__off as usize];");
                a.line("}");
                a.line("out_" + a.id + " = " + y + ";");
            } else {
                a.line("int __idxN_" + a.id + "[" + Ns + "] = {" + idxArr + "};");
                a.line("double __wArrN_" + a.id + "[" + Ns + "] = {" + wArr + "};");
                a.line("static const int __strideN_" + a.id + "[" + Ns + "] = {" + strArr + "};");
                a.line("double " + y + " = 0.0;");
                a.line("for (int __c = 0; __c < (1 << " + Ns + "); __c++) {");
                a.line("  double __cw = 1.0; int __off = 0;");
                a.line("  for (int __k = 0; __k < " + Ns + "; __k++) {");
                a.line("    int __bit = (__c >> __k) & 1;");
                a.line("    __cw *= __bit ? __wArrN_" + a.id + "[__k] : (1.0 - __wArrN_" + a.id
                    + "[__k]);");
                a.line("    __off += (__idxN_" + a.id + "[__k] + __bit) * __strideN_" + a.id
                    + "[__k];");
                a.line("  }");
                a.line("  " + y + " += __cw * " + tv + "[__off];");
                a.line("}");
                a.line("out_" + a.id + " = " + y + ";");
            }
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handleLookupND(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const int N = numDims(bd);
        if (N < 1) {
            return false;
        }
        std::vector<std::vector<double>> bps(N);
        std::vector<int> stride(N, 1);
        for (int k = 0; k < N; ++k) {
            bps[k] = bd.paramList(dimKey(k).c_str());
            if (!hasInput(ctx, b.nid, k) || (int)bps[k].size() < 2) {
                return false;
            }
        }
        for (int k = 1; k < N; ++k) {
            stride[k] = stride[k - 1] * (int)bps[k - 1].size();
        }
        const std::vector<double> tbl = bd.paramList("Table");
        const std::string interp = bd.paramStr("InterpMethod", "Linear point-slope");
        const std::string extrap = bd.paramStr("ExtrapMethod", "Linear");
        const bool linExtrap = (extrap == "Linear");
        std::vector<SigView> in(N);
        for (int k = 0; k < N; ++k) {
            in[k] = getInputSig(ctx, b.nid, k);
        }
        if (lookupspline::isSpline(interp)) {
            // Separable tensor-product spline: interpolate along dimension 0
            // (innermost stride) first, recursing outward one axis at a time.
            return emitElementwise(ctx, b.nid, [&](int e) {
                std::vector<double> uv(N);
                for (int k = 0; k < N; ++k) {
                    uv[k] = sigAt(in[k], e);
                }
                std::function<double(int, int)> evalDim = [&](int dim, int off) -> double {
                    const int nk = (int)bps[dim].size();
                    std::vector<double> col((size_t)nk, 0.0);
                    for (int i = 0; i < nk; ++i) {
                        const int o = off + i * stride[dim];
                        col[(size_t)i] = (dim == 0)
                            ? ((o >= 0 && o < (int)tbl.size()) ? tbl[(size_t)o] : 0.0)
                            : evalDim(dim - 1, o);
                    }
                    return lookupspline::spline1DValue(bps[dim], col, uv[dim], interp, extrap);
                };
                return evalDim(N - 1, 0);
            });
        }
        return emitElementwise(ctx, b.nid, [&](int e) {
            std::vector<int> idx(N);
            std::vector<double> w(N);
            for (int k = 0; k < N; ++k) {
                int i = 0;
                double f = 0.0;
                axisLocate(bps[k], sigAt(in[k], e), linExtrap, i, f);
                idx[k] = i;
                w[k] = axisWeight(f, interp);
            }
            double y = 0.0;
            const int corners = 1 << N;
            for (int c = 0; c < corners; ++c) {
                double cw = 1.0;
                int off = 0;
                for (int k = 0; k < N; ++k) {
                    const int bit = (c >> k) & 1;
                    cw *= bit ? w[k] : (1.0 - w[k]);
                    off += (idx[k] + bit) * stride[k];
                }
                if (off >= 0 && off < (int)tbl.size()) {
                    y += cw * tbl[off];
                }
            }
            return y;
        });
    }
    //=============================================================================
    bool
    resolveLookupNDDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err)
    {
        nflow::BlockDescriptor bd(b, vars);
        const int N = numDims(bd);
        if (N < 1) {
            err = "lookupND requires NumberOfTableDimensions >= 1";
            return false;
        }
        size_t expected = 1;
        for (int k = 0; k < N; ++k) {
            const std::vector<double> bp = bd.paramList(dimKey(k).c_str());
            if (bp.size() < 2) {
                err = "lookupND requires at least 2 breakpoints per dimension";
                return false;
            }
            if (!isStrictlyIncreasing(bp)) {
                err = "lookupND breakpoints must be strictly increasing";
                return false;
            }
            expected *= bp.size();
        }
        const std::vector<double> tbl = bd.paramList("Table");
        if (tbl.size() != expected) {
            err = "lookupND table size must equal the product of the breakpoint counts";
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
    // Shared spline helpers, emitted once per target when any lookup block in
    // the model interpolates with a spline (same `once` key as lookup2D).
    static void
    emitLookupNDShared(const BlockCodegenStateArgs& a, bool rust)
    {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        if (!lookupspline::isSpline(bd.paramStr("InterpMethod", "Linear point-slope"))) {
            return;
        }
        if (!a.once("nflow:spline:helpers")) {
            return;
        }
        lookupspline::emitSplineCodegenHelpers(a, rust);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCLookupND()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitLookupND(a, false); };
        t.emitShared = [](const BlockCodegenStateArgs& a) { emitLookupNDShared(a, false); };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustLookupND()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitLookupND(a, true); };
        t.emitShared = [](const BlockCodegenStateArgs& a) { emitLookupNDShared(a, true); };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
