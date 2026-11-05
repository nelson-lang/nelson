//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// luScalar: a code-generation-only scalar helper block. It never exists in a
// user diagram and has no interpreter handler; the vector-expansion pass
// (NFlowVectorExpand) lowers prelookup / interpolationPrelookup / lookupDynamic
// into luScalar blocks so their scalar output participates in ordinary scalar
// code generation. The "mode" parameter selects the emitted body:
//   prelookupK : in[0]=u; param Breakpoints  -> interval index k (as double)
//   prelookupF : in[0]=u; param Breakpoints  -> interval fraction f
//   interp     : in[0]=k, in[1]=f; param Table -> tbl[k] + f*(tbl[k+1]-tbl[k])
//   dynamic    : in[0]=x, in[1..W]=xdat, in[W+1..2W]=ydat; param W -> 1-D interp
// Static tables (Breakpoints / Table) are baked as constant arrays; the dynamic
// breakpoint / table wires are baked into local arrays initialised from the
// per-element input expressions. C and Rust bodies share the structure.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "lookup_blocks.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        std::vector<double>
        luParamVec(const BlockCodegenArgs& a, const char* key)
        {
            if (!a.block || !a.variables) {
                return {};
            }
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            return bd.paramList(key);
        }
        //=============================================================================
        std::string
        luMode(const BlockCodegenArgs& a)
        {
            if (!a.block || !a.variables) {
                return "";
            }
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            return bd.paramStr("mode", "");
        }
        //=============================================================================
        std::string
        cArr(const BlockCodegenArgs& a, const std::string& name, const std::vector<double>& v)
        {
            std::string s = "static const double " + name + "[] = { ";
            for (size_t i = 0; i < v.size(); ++i) {
                s += (i ? ", " : "") + a.fmt(v[i]);
            }
            s += " };";
            return s;
        }
        //=============================================================================
        void
        emitC(const BlockCodegenArgs& a)
        {
            const std::string mode = luMode(a);
            const std::string id = a.id;
            const std::string out = "out_" + id;
            if (mode == "prelookupK" || mode == "prelookupF") {
                const std::vector<double> bp = luParamVec(a, "Breakpoints");
                const int n = (int)bp.size();
                a.line("{");
                a.line("double nf_u_" + id + " = " + a.in[0] + ";");
                a.line(cArr(a, "nf_bp_" + id, bp));
                if (n < 2) {
                    a.line(out + " = 0.0;");
                    a.line("}");
                    return;
                }
                a.line("int nf_k_" + id + ";");
                a.line("if (nf_u_" + id + " <= nf_bp_" + id + "[0]) nf_k_" + id + " = 0;");
                a.line("else if (nf_u_" + id + " >= nf_bp_" + id + "[" + std::to_string(n - 1)
                    + "]) nf_k_" + id + " = " + std::to_string(n - 2) + ";");
                a.line("else { int nf_i = " + std::to_string(n - 2) + "; while (nf_i > 0 && nf_u_"
                    + id + " < nf_bp_" + id + "[nf_i]) nf_i--; nf_k_" + id + " = nf_i; }");
                if (mode == "prelookupK") {
                    a.line(out + " = (double)nf_k_" + id + ";");
                } else {
                    a.line("if (nf_u_" + id + " <= nf_bp_" + id + "[0]) " + out + " = 0.0;");
                    a.line("else if (nf_u_" + id + " >= nf_bp_" + id + "[" + std::to_string(n - 1)
                        + "]) " + out + " = 1.0;");
                    a.line("else { double nf_d = nf_bp_" + id + "[nf_k_" + id + "+1] - nf_bp_" + id
                        + "[nf_k_" + id + "]; " + out + " = (nf_d != 0.0) ? (nf_u_" + id
                        + " - nf_bp_" + id + "[nf_k_" + id + "]) / nf_d : 0.0; }");
                }
                a.line("}");
                return;
            }
            if (mode == "interp") {
                const std::vector<double> tbl = luParamVec(a, "Table");
                const int m = (int)tbl.size();
                a.line("{");
                a.line("double nf_k_" + id + " = " + a.in[0] + ";");
                a.line("double nf_f_" + id + " = " + a.in[1] + ";");
                a.line(cArr(a, "nf_t_" + id, tbl));
                if (m < 2) {
                    a.line(out + " = " + (m == 1 ? a.fmt(tbl[0]) : std::string("0.0")) + ";");
                    a.line("}");
                    return;
                }
                a.line("int nf_ki_" + id + " = (int)(nf_k_" + id + " + 0.5);");
                a.line("if (nf_ki_" + id + " < 0) nf_ki_" + id + " = 0;");
                a.line("if (nf_ki_" + id + " > " + std::to_string(m - 2) + ") nf_ki_" + id + " = "
                    + std::to_string(m - 2) + ";");
                a.line(out + " = nf_t_" + id + "[nf_ki_" + id + "] + nf_f_" + id + " * (nf_t_" + id
                    + "[nf_ki_" + id + "+1] - nf_t_" + id + "[nf_ki_" + id + "]);");
                a.line("}");
                return;
            }
            if (mode == "dynamic") {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const int W = (int)bd.paramDouble("W", 0.0);
                a.line("{");
                a.line("double nf_x_" + id + " = " + a.in[0] + ";");
                if (W < 1) {
                    a.line(out + " = 0.0;");
                    a.line("}");
                    return;
                }
                std::string xd = "double nf_xd_" + id + "[] = { ";
                std::string yd = "double nf_yd_" + id + "[] = { ";
                for (int j = 0; j < W; ++j) {
                    xd += (j ? ", " : "") + a.in[1 + j];
                    yd += (j ? ", " : "") + a.in[1 + W + j];
                }
                xd += " };";
                yd += " };";
                a.line(xd);
                a.line(yd);
                const std::string wl = std::to_string(W - 1);
                if (W == 1) {
                    a.line(out + " = nf_yd_" + id + "[0];");
                    a.line("}");
                    return;
                }
                a.line("if (nf_x_" + id + " <= nf_xd_" + id + "[0]) " + out + " = nf_yd_" + id
                    + "[0];");
                a.line("else if (nf_x_" + id + " >= nf_xd_" + id + "[" + wl + "]) " + out
                    + " = nf_yd_" + id + "[" + wl + "];");
                a.line("else { int nf_i = " + std::to_string(W - 2) + "; while (nf_i > 0 && nf_x_"
                    + id + " < nf_xd_" + id + "[nf_i]) nf_i--; double nf_d = nf_xd_" + id
                    + "[nf_i+1] - nf_xd_" + id + "[nf_i]; double nf_f = (nf_d != 0.0) ? (nf_x_" + id
                    + " - nf_xd_" + id + "[nf_i]) / nf_d : 0.0; " + out + " = nf_yd_" + id
                    + "[nf_i] + nf_f * (nf_yd_" + id + "[nf_i+1] - nf_yd_" + id + "[nf_i]); }");
                a.line("}");
                return;
            }
            a.line(out + " = 0.0;");
        }
        //=============================================================================
        std::string
        rArr(const BlockCodegenArgs& a, const std::string& name, const std::vector<double>& v)
        {
            std::string s = "let " + name + ": [f64; " + std::to_string(v.size()) + "] = [";
            for (size_t i = 0; i < v.size(); ++i) {
                s += (i ? ", " : "") + a.fmt(v[i]);
            }
            s += "];";
            return s;
        }
        //=============================================================================
        void
        emitRust(const BlockCodegenArgs& a)
        {
            const std::string mode = luMode(a);
            const std::string id = a.id;
            const std::string out = "out_" + id;
            if (mode == "prelookupK" || mode == "prelookupF") {
                const std::vector<double> bp = luParamVec(a, "Breakpoints");
                const int n = (int)bp.size();
                a.line("{");
                a.line("let nf_u = " + a.in[0] + ";");
                a.line(rArr(a, "nf_bp", bp));
                if (n < 2) {
                    a.line(out + " = 0.0_f64;");
                    a.line("}");
                    return;
                }
                a.line("let mut nf_k: usize = 0;");
                a.line("if nf_u <= nf_bp[0] { nf_k = 0; }");
                a.line("else if nf_u >= nf_bp[" + std::to_string(n - 1)
                    + "] { nf_k = " + std::to_string(n - 2) + "; }");
                a.line("else { let mut nf_i: usize = " + std::to_string(n - 2)
                    + "; while nf_i > 0 && nf_u < nf_bp[nf_i] { nf_i -= 1; } nf_k = nf_i; }");
                if (mode == "prelookupK") {
                    a.line(out + " = nf_k as f64;");
                } else {
                    a.line("if nf_u <= nf_bp[0] { " + out + " = 0.0_f64; }");
                    a.line("else if nf_u >= nf_bp[" + std::to_string(n - 1) + "] { " + out
                        + " = 1.0_f64; }");
                    a.line("else { let nf_d = nf_bp[nf_k+1] - nf_bp[nf_k]; " + out
                        + " = if nf_d != 0.0_f64 { (nf_u - nf_bp[nf_k]) / nf_d } else { 0.0_f64 }; "
                          "}");
                }
                a.line("}");
                return;
            }
            if (mode == "interp") {
                const std::vector<double> tbl = luParamVec(a, "Table");
                const int m = (int)tbl.size();
                a.line("{");
                a.line("let nf_k = " + a.in[0] + ";");
                a.line("let nf_f = " + a.in[1] + ";");
                a.line(rArr(a, "nf_t", tbl));
                if (m < 2) {
                    a.line(out + " = " + (m == 1 ? a.fmt(tbl[0]) : std::string("0.0_f64")) + ";");
                    a.line("}");
                    return;
                }
                a.line("let mut nf_ki: i64 = (nf_k + 0.5) as i64;");
                a.line("if nf_ki < 0 { nf_ki = 0; }");
                a.line("if nf_ki > " + std::to_string(m - 2) + " { nf_ki = " + std::to_string(m - 2)
                    + "; }");
                a.line("let nf_ku = nf_ki as usize;");
                a.line(out + " = nf_t[nf_ku] + nf_f * (nf_t[nf_ku+1] - nf_t[nf_ku]);");
                a.line("}");
                return;
            }
            if (mode == "dynamic") {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const int W = (int)bd.paramDouble("W", 0.0);
                a.line("{");
                a.line("let nf_x = " + a.in[0] + ";");
                if (W < 1) {
                    a.line(out + " = 0.0_f64;");
                    a.line("}");
                    return;
                }
                std::string xd = "let nf_xd: [f64; " + std::to_string(W) + "] = [";
                std::string yd = "let nf_yd: [f64; " + std::to_string(W) + "] = [";
                for (int j = 0; j < W; ++j) {
                    xd += (j ? ", " : "") + a.in[1 + j];
                    yd += (j ? ", " : "") + a.in[1 + W + j];
                }
                xd += "];";
                yd += "];";
                a.line(xd);
                a.line(yd);
                const std::string wl = std::to_string(W - 1);
                if (W == 1) {
                    a.line(out + " = nf_yd[0];");
                    a.line("}");
                    return;
                }
                a.line("if nf_x <= nf_xd[0] { " + out + " = nf_yd[0]; }");
                a.line("else if nf_x >= nf_xd[" + wl + "] { " + out + " = nf_yd[" + wl + "]; }");
                a.line("else { let mut nf_i: usize = " + std::to_string(W - 2)
                    + "; while nf_i > 0 && nf_x < nf_xd[nf_i] { nf_i -= 1; } let nf_d = "
                      "nf_xd[nf_i+1] - "
                      "nf_xd[nf_i]; let nf_f = if nf_d != 0.0_f64 { (nf_x - nf_xd[nf_i]) / nf_d } "
                      "else { "
                      "0.0_f64 }; "
                    + out + " = nf_yd[nf_i] + nf_f * (nf_yd[nf_i+1] - nf_yd[nf_i]); }");
                a.line("}");
                return;
            }
            a.line(out + " = 0.0_f64;");
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCLuScalar()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitC(a); };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustLuScalar()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) { emitRust(a); };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
