//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// lookup2D: 2-D lookup table. Two inputs (row coordinate on port 0, column
// coordinate on port 1) index a static matrix Table stored column-major:
// Table(i,j) at index i + j*R (R = number of row breakpoints). Bilinear /
// Flat / Nearest interpolation with Clip / Linear extrapolation, element-wise
// with scalar expansion. ALGEBRAIC feedthrough, real double, no state.
//
// The interpolation is expressed with a per-axis weight w in [0,1] (Flat -> 0,
// Nearest -> round(f), Linear -> f); the output is the four-corner blend
//   T(i,j)(1-w1)(1-w2) + T(i+1,j) w1 (1-w2) + T(i,j+1)(1-w1) w2 + T(i+1,j+1) w1 w2
// which reduces to a single corner for Flat / Nearest.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "lookup_blocks.hpp"
#include "lookup_spline.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        // Locate the lower index i and raw fraction f for value u on a strictly
        // increasing axis. Clip holds f in [0,1] at the edges; Linear lets f run
        // past the edge segments so the caller extrapolates.
        void
        axisLocate(const std::vector<double>& bp, double u, bool linearExtrap, int& i, double& f)
        {
            const int n = (int)bp.size();
            if (u <= bp[0]) {
                i = 0;
                f = linearExtrap ? (u - bp[0]) / (bp[1] - bp[0]) : 0.0;
                return;
            }
            if (u >= bp[n - 1]) {
                i = n - 2;
                f = linearExtrap ? (u - bp[n - 2]) / (bp[n - 1] - bp[n - 2]) : 1.0;
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
        // Parse via BlockDescriptor so JSON arrays and string expressions both resolve.
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
        // Emit the per-axis (index, fraction) locate for a strictly increasing
        // breakpoint array of length n, into integer var `iv` and double var
        // `fv`. `linExtrap` extends the edge segment's slope; otherwise clip.
        void
        emitLocate(const BlockCodegenArgs& a, bool rust, const std::string& arr,
            const std::string& uv, const std::string& iv, const std::string& fv, int n,
            bool linExtrap)
        {
            const std::string N1 = std::to_string(n - 1);
            const std::string N2 = std::to_string(n - 2);
            const std::string belowF = linExtrap
                ? "(" + uv + " - " + arr + "[0]) / (" + arr + "[1] - " + arr + "[0])"
                : (rust ? "0.0_f64" : "0.0");
            const std::string aboveF = linExtrap ? "(" + uv + " - " + arr + "[" + N2 + "]) / ("
                    + arr + "[" + N1 + "] - " + arr + "[" + N2 + "])"
                                                 : (rust ? "1.0_f64" : "1.0");
            if (rust) {
                a.line("let " + iv + ": usize; let " + fv + ": f64;");
                a.line("if " + uv + " <= " + arr + "[0] { " + iv + " = 0; " + fv + " = " + belowF
                    + "; }");
                a.line("else if " + uv + " >= " + arr + "[" + N1 + "] { " + iv + " = " + N2 + "; "
                    + fv + " = " + aboveF + "; }");
                a.line("else { let mut __k = " + N2 + "usize; while __k > 0 && " + uv + " < " + arr
                    + "[__k] { __k -= 1; } " + iv + " = __k; " + fv + " = (" + uv + " - " + arr
                    + "[__k]) / (" + arr + "[__k+1] - " + arr + "[__k]); }");
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
        emitLookup2D(const BlockCodegenArgs& a, bool rust)
        {
            const std::vector<double> bp1 = paramVec(a, "BreakpointsForDimension1");
            const std::vector<double> bp2 = paramVec(a, "BreakpointsForDimension2");
            const std::vector<double> tbl = paramVec(a, "Table");
            nflow::BlockDescriptor bdp(*a.block, *a.variables);
            const std::string interp = bdp.paramStr("InterpMethod", "Linear point-slope");
            const bool linExtrap = (bdp.paramStr("ExtrapMethod", "Linear") == "Linear");
            const int R = (int)bp1.size();
            const int C = (int)bp2.size();
            if (R < 2 || C < 2 || (int)tbl.size() != R * C) {
                a.line("out_" + a.id + (rust ? " = 0.0_f64;" : " = 0.0;"));
                return;
            }
            if (lookupspline::isSpline(interp)) {
                // Separable tensor-product spline: dimension-1 coefficients are
                // baked per table column; the dimension-2 pass solves its dense
                // system at run time through the shared generated helpers.
                lookupspline::emitSplineLookupCodegen(
                    a, rust, { bp1, bp2 }, tbl, interp, bdp.paramStr("ExtrapMethod", "Linear"));
                return;
            }
            const std::string bp1v = "__lu2_bp1_" + a.id;
            const std::string bp2v = "__lu2_bp2_" + a.id;
            const std::string tv = "__lu2_tbl_" + a.id;
            a.line(emitArray(a, bp1v, bp1, rust));
            a.line(emitArray(a, bp2v, bp2, rust));
            a.line(emitArray(a, tv, tbl, rust));
            const std::string u1 = "__u1_" + a.id, u2 = "__u2_" + a.id;
            const std::string i1 = "__i1_" + a.id, i2 = "__j1_" + a.id;
            const std::string f1 = "__f1_" + a.id, f2 = "__f2_" + a.id;
            a.line((rust ? "let " : "double ") + u1 + " = " + a.in[0] + ";");
            a.line((rust ? "let " : "double ") + u2 + " = " + a.in[1] + ";");
            emitLocate(a, rust, bp1v, u1, i1, f1, R, linExtrap);
            emitLocate(a, rust, bp2v, u2, i2, f2, C, linExtrap);
            const std::string w1 = "__w1_" + a.id, w2 = "__w2_" + a.id;
            a.line((rust ? "let " : "double ") + w1 + " = " + weightExpr(f1, interp, rust) + ";");
            a.line((rust ? "let " : "double ") + w2 + " = " + weightExpr(f2, interp, rust) + ";");
            // T(ii,jj) = tv[ii + jj*R]
            const std::string Rs = std::to_string(R);
            auto T = [&](const std::string& ii, const std::string& jj) {
                return tv + "[" + ii + " + (" + jj + ") * " + Rs + "]";
            };
            const std::string ip1 = i1 + "+1", jp1 = i2 + "+1";
            const std::string blend = T(i1, i2) + " * (1.0 - " + w1 + ") * (1.0 - " + w2 + ") + "
                + T(ip1, i2) + " * " + w1 + " * (1.0 - " + w2 + ") + " + T(i1, jp1) + " * (1.0 - "
                + w1 + ") * " + w2 + " + " + T(ip1, jp1) + " * " + w1 + " * " + w2;
            a.line("out_" + a.id + " = " + blend + ";");
        }
        //=============================================================================
        double
        interp2d(const std::vector<double>& bp1, const std::vector<double>& bp2,
            const std::vector<double>& tbl, double u1, double u2, const std::string& interp,
            bool linExtrap, const std::string& extrap)
        {
            const int R = (int)bp1.size();
            if (lookupspline::isSpline(interp)) {
                const int C = (int)bp2.size();
                std::vector<double> v((size_t)C, 0.0);
                std::vector<double> col((size_t)R, 0.0);
                for (int jj = 0; jj < C; ++jj) {
                    for (int ii = 0; ii < R; ++ii) {
                        col[(size_t)ii] = tbl[(size_t)ii + (size_t)jj * (size_t)R];
                    }
                    v[(size_t)jj] = lookupspline::spline1DValue(bp1, col, u1, interp, extrap);
                }
                return lookupspline::spline1DValue(bp2, v, u2, interp, extrap);
            }
            int i = 0, j = 0;
            double f1 = 0.0, f2 = 0.0;
            axisLocate(bp1, u1, linExtrap, i, f1);
            axisLocate(bp2, u2, linExtrap, j, f2);
            const double w1 = axisWeight(f1, interp);
            const double w2 = axisWeight(f2, interp);
            auto T = [&](int ii, int jj) { return tbl[(size_t)ii + (size_t)jj * (size_t)R]; };
            return T(i, j) * (1.0 - w1) * (1.0 - w2) + T(i + 1, j) * w1 * (1.0 - w2)
                + T(i, j + 1) * (1.0 - w1) * w2 + T(i + 1, j + 1) * w1 * w2;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handleLookup2D(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0) || !hasInput(ctx, b.nid, 1)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::vector<double> bp1 = bd.paramList("BreakpointsForDimension1");
        const std::vector<double> bp2 = bd.paramList("BreakpointsForDimension2");
        const std::vector<double> tbl = bd.paramList("Table");
        const std::string interp = bd.paramStr("InterpMethod", "Linear point-slope");
        const std::string extrap = bd.paramStr("ExtrapMethod", "Linear");
        const bool linExtrap = (extrap == "Linear");
        SigView u1 = getInputSig(ctx, b.nid, 0);
        SigView u2 = getInputSig(ctx, b.nid, 1);
        return emitElementwise(ctx, b.nid, [&](int k) {
            return interp2d(bp1, bp2, tbl, sigAt(u1, k), sigAt(u2, k), interp, linExtrap, extrap);
        });
    }
    //=============================================================================
    bool
    resolveLookup2DDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err)
    {
        nflow::BlockDescriptor bd(b, vars);
        const std::vector<double> bp1 = bd.paramList("BreakpointsForDimension1");
        const std::vector<double> bp2 = bd.paramList("BreakpointsForDimension2");
        const std::vector<double> tbl = bd.paramList("Table");
        if (bp1.size() < 2 || bp2.size() < 2) {
            err = "lookup2D requires at least 2 breakpoints per dimension";
            return false;
        }
        if (tbl.size() != bp1.size() * bp2.size()) {
            err = "lookup2D table size must be rows*cols";
            return false;
        }
        if (!isStrictlyIncreasing(bp1) || !isStrictlyIncreasing(bp2)) {
            err = "lookup2D breakpoints must be strictly increasing";
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
    // the model interpolates with a spline (same `once` key as lookupND).
    static void
    emitLookup2DShared(const BlockCodegenStateArgs& a, bool rust)
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
    getCodeGenCLookup2D()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitLookup2D(a, false); };
        t.emitShared = [](const BlockCodegenStateArgs& a) { emitLookup2DShared(a, false); };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustLookup2D()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitLookup2D(a, true); };
        t.emitShared = [](const BlockCodegenStateArgs& a) { emitLookup2DShared(a, true); };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
