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
                a.line("double __u_" + id + " = " + a.in[0] + ";");
                a.line(cArr(a, "__bp_" + id, bp));
                if (n < 2) {
                    a.line(out + " = 0.0;");
                    a.line("}");
                    return;
                }
                a.line("int __k_" + id + ";");
                a.line("if (__u_" + id + " <= __bp_" + id + "[0]) __k_" + id + " = 0;");
                a.line("else if (__u_" + id + " >= __bp_" + id + "[" + std::to_string(n - 1)
                    + "]) __k_" + id + " = " + std::to_string(n - 2) + ";");
                a.line("else { int __i = " + std::to_string(n - 2) + "; while (__i > 0 && __u_" + id
                    + " < __bp_" + id + "[__i]) __i--; __k_" + id + " = __i; }");
                if (mode == "prelookupK") {
                    a.line(out + " = (double)__k_" + id + ";");
                } else {
                    a.line("if (__u_" + id + " <= __bp_" + id + "[0]) " + out + " = 0.0;");
                    a.line("else if (__u_" + id + " >= __bp_" + id + "[" + std::to_string(n - 1)
                        + "]) " + out + " = 1.0;");
                    a.line("else { double __d = __bp_" + id + "[__k_" + id + "+1] - __bp_" + id
                        + "[__k_" + id + "]; " + out + " = (__d != 0.0) ? (__u_" + id + " - __bp_"
                        + id + "[__k_" + id + "]) / __d : 0.0; }");
                }
                a.line("}");
                return;
            }
            if (mode == "interp") {
                const std::vector<double> tbl = luParamVec(a, "Table");
                const int m = (int)tbl.size();
                a.line("{");
                a.line("double __k_" + id + " = " + a.in[0] + ";");
                a.line("double __f_" + id + " = " + a.in[1] + ";");
                a.line(cArr(a, "__t_" + id, tbl));
                if (m < 2) {
                    a.line(out + " = " + (m == 1 ? a.fmt(tbl[0]) : std::string("0.0")) + ";");
                    a.line("}");
                    return;
                }
                a.line("int __ki_" + id + " = (int)(__k_" + id + " + 0.5);");
                a.line("if (__ki_" + id + " < 0) __ki_" + id + " = 0;");
                a.line("if (__ki_" + id + " > " + std::to_string(m - 2) + ") __ki_" + id + " = "
                    + std::to_string(m - 2) + ";");
                a.line(out + " = __t_" + id + "[__ki_" + id + "] + __f_" + id + " * (__t_" + id
                    + "[__ki_" + id + "+1] - __t_" + id + "[__ki_" + id + "]);");
                a.line("}");
                return;
            }
            if (mode == "dynamic") {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const int W = (int)bd.paramDouble("W", 0.0);
                a.line("{");
                a.line("double __x_" + id + " = " + a.in[0] + ";");
                if (W < 1) {
                    a.line(out + " = 0.0;");
                    a.line("}");
                    return;
                }
                std::string xd = "double __xd_" + id + "[] = { ";
                std::string yd = "double __yd_" + id + "[] = { ";
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
                    a.line(out + " = __yd_" + id + "[0];");
                    a.line("}");
                    return;
                }
                a.line(
                    "if (__x_" + id + " <= __xd_" + id + "[0]) " + out + " = __yd_" + id + "[0];");
                a.line("else if (__x_" + id + " >= __xd_" + id + "[" + wl + "]) " + out + " = __yd_"
                    + id + "[" + wl + "];");
                a.line("else { int __i = " + std::to_string(W - 2) + "; while (__i > 0 && __x_" + id
                    + " < __xd_" + id + "[__i]) __i--; double __d = __xd_" + id + "[__i+1] - __xd_"
                    + id + "[__i]; double __f = (__d != 0.0) ? (__x_" + id + " - __xd_" + id
                    + "[__i]) / __d : 0.0; " + out + " = __yd_" + id + "[__i] + __f * (__yd_" + id
                    + "[__i+1] - __yd_" + id + "[__i]); }");
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
                a.line("let __u = " + a.in[0] + ";");
                a.line(rArr(a, "__bp", bp));
                if (n < 2) {
                    a.line(out + " = 0.0_f64;");
                    a.line("}");
                    return;
                }
                a.line("let mut __k: usize = 0;");
                a.line("if __u <= __bp[0] { __k = 0; }");
                a.line("else if __u >= __bp[" + std::to_string(n - 1)
                    + "] { __k = " + std::to_string(n - 2) + "; }");
                a.line("else { let mut __i: usize = " + std::to_string(n - 2)
                    + "; while __i > 0 && __u < __bp[__i] { __i -= 1; } __k = __i; }");
                if (mode == "prelookupK") {
                    a.line(out + " = __k as f64;");
                } else {
                    a.line("if __u <= __bp[0] { " + out + " = 0.0_f64; }");
                    a.line("else if __u >= __bp[" + std::to_string(n - 1) + "] { " + out
                        + " = 1.0_f64; }");
                    a.line("else { let __d = __bp[__k+1] - __bp[__k]; " + out
                        + " = if __d != 0.0_f64 { (__u - __bp[__k]) / __d } else { 0.0_f64 }; }");
                }
                a.line("}");
                return;
            }
            if (mode == "interp") {
                const std::vector<double> tbl = luParamVec(a, "Table");
                const int m = (int)tbl.size();
                a.line("{");
                a.line("let __k = " + a.in[0] + ";");
                a.line("let __f = " + a.in[1] + ";");
                a.line(rArr(a, "__t", tbl));
                if (m < 2) {
                    a.line(out + " = " + (m == 1 ? a.fmt(tbl[0]) : std::string("0.0_f64")) + ";");
                    a.line("}");
                    return;
                }
                a.line("let mut __ki: i64 = (__k + 0.5) as i64;");
                a.line("if __ki < 0 { __ki = 0; }");
                a.line("if __ki > " + std::to_string(m - 2) + " { __ki = " + std::to_string(m - 2)
                    + "; }");
                a.line("let __ku = __ki as usize;");
                a.line(out + " = __t[__ku] + __f * (__t[__ku+1] - __t[__ku]);");
                a.line("}");
                return;
            }
            if (mode == "dynamic") {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const int W = (int)bd.paramDouble("W", 0.0);
                a.line("{");
                a.line("let __x = " + a.in[0] + ";");
                if (W < 1) {
                    a.line(out + " = 0.0_f64;");
                    a.line("}");
                    return;
                }
                std::string xd = "let __xd: [f64; " + std::to_string(W) + "] = [";
                std::string yd = "let __yd: [f64; " + std::to_string(W) + "] = [";
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
                    a.line(out + " = __yd[0];");
                    a.line("}");
                    return;
                }
                a.line("if __x <= __xd[0] { " + out + " = __yd[0]; }");
                a.line("else if __x >= __xd[" + wl + "] { " + out + " = __yd[" + wl + "]; }");
                a.line("else { let mut __i: usize = " + std::to_string(W - 2)
                    + "; while __i > 0 && __x < __xd[__i] { __i -= 1; } let __d = __xd[__i+1] - "
                      "__xd[__i]; let __f = if __d != 0.0_f64 { (__x - __xd[__i]) / __d } else { "
                      "0.0_f64 }; "
                    + out + " = __yd[__i] + __f * (__yd[__i+1] - __yd[__i]); }");
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
