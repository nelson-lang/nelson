//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenSolver.hpp"
#include "BlockRegistry.hpp" // Rk4Op values
//=============================================================================
namespace nflow {
//=============================================================================
namespace {
    // Tableau fractions are written once: frac("3.0","40") gives "3.0/40" (C) or
    // "3.0/40.0" (Rust).
    std::string
    frac(bool rust, const char* num, const char* den)
    {
        return std::string(num) + "/" + den + (rust ? ".0" : "");
    }
    //=============================================================================
    // "<ind>__nflow_rhs(...)" with the per-language call shape: the C rhs takes a
    // nullable ModelOutput*, the Rust rhs a __report flag. `gArg` (zero-crossing
    // mode only) appends the trailing surface-array argument.
    std::string
    rhsCall(bool rust, const std::string& ind, const std::string& tExpr, const std::string& xArr,
        const std::string& kArr, bool report, const std::string& gArg = std::string())
    {
        const std::string tail = gArg.empty() ? "" : (", " + gArg);
        if (rust) {
            return ind + "__nflow_rhs(s, inp, out, " + tExpr + ", &" + xArr + ", &mut " + kArr
                + ", " + (report ? "true" : "false") + tail + ");";
        }
        return ind + "__nflow_rhs(s, in, " + (report ? "out" : "(ModelOutput*)0") + ", " + tExpr
            + ", " + xArr + ", " + kArr + tail + ");";
    }
    //=============================================================================
    std::string
    gNull(bool rust)
    {
        return rust ? "None" : "(double*)0";
    }
    //=============================================================================
    std::string
    forAssign(bool rust, const std::string& ind, const std::string& N, const std::string& target,
        const std::string& expr)
    {
        if (rust) {
            return ind + "for __i in 0.." + N + " { " + target + "[__i] = " + expr + "; }";
        }
        return ind + "for (int __i = 0; __i < " + N + "; ++__i) " + target + "[__i] = " + expr
            + ";";
    }
    //=============================================================================
    bool
    hasCrossings(const SolverEmitSpec& spec)
    {
        return !spec.crossingCount.empty() && spec.crossingCount != "0";
    }
    //=============================================================================
    // The seven Dormand-Prince stages plus the embedded error expression, shared by
    // the plain ode45 stepper (gArg empty) and the zero-crossing variant (gArg = the
    // null __g form).
    std::string
    emitDp45Stages(const SolverEmitSpec& spec, const std::string& ind3, const std::string& gArg)
    {
        auto& lines = *spec.lines;
        const bool ru = spec.rust;
        const std::string& N = spec.stateCount;
        auto F = [&](const char* num, const char* den) { return frac(ru, num, den); };
        lines.push_back(rhsCall(ru, ind3, "__tc", "__x", "__k1", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "__xt", "__x[__i] + __h * (0.2 * __k1[__i])"));
        lines.push_back(rhsCall(ru, ind3, "__tc + 0.2 * __h", "__xt", "__k2", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "__xt",
            "__x[__i] + __h * (" + F("3.0", "40") + " * __k1[__i] + " + F("9.0", "40")
                + " * __k2[__i])"));
        lines.push_back(rhsCall(ru, ind3, "__tc + 0.3 * __h", "__xt", "__k3", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "__xt",
            "__x[__i] + __h * (" + F("44.0", "45") + " * __k1[__i] - " + F("56.0", "15")
                + " * __k2[__i] + " + F("32.0", "9") + " * __k3[__i])"));
        lines.push_back(rhsCall(ru, ind3, "__tc + 0.8 * __h", "__xt", "__k4", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "__xt",
            "__x[__i] + __h * (" + F("19372.0", "6561") + " * __k1[__i] - " + F("25360.0", "2187")
                + " * __k2[__i] + " + F("64448.0", "6561") + " * __k3[__i] - " + F("212.0", "729")
                + " * __k4[__i])"));
        lines.push_back(
            rhsCall(ru, ind3, "__tc + " + F("8.0", "9") + " * __h", "__xt", "__k5", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "__xt",
            "__x[__i] + __h * (" + F("9017.0", "3168") + " * __k1[__i] - " + F("355.0", "33")
                + " * __k2[__i] + " + F("46732.0", "5247") + " * __k3[__i] + " + F("49.0", "176")
                + " * __k4[__i] - " + F("5103.0", "18656") + " * __k5[__i])"));
        lines.push_back(rhsCall(ru, ind3, "__tc + __h", "__xt", "__k6", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "__x5",
            "__x[__i] + __h * (" + F("35.0", "384") + " * __k1[__i] + " + F("500.0", "1113")
                + " * __k3[__i] + " + F("125.0", "192") + " * __k4[__i] - " + F("2187.0", "6784")
                + " * __k5[__i] + " + F("11.0", "84") + " * __k6[__i])"));
        lines.push_back(rhsCall(ru, ind3, "__tc + __h", "__x5", "__k7", false, gArg));
        return "__h * ((" + F("35.0", "384") + " - " + F("5179.0", "57600") + ") * __k1[__i] + ("
            + F("500.0", "1113") + " - " + F("7571.0", "16695") + ") * __k3[__i] + ("
            + F("125.0", "192") + " - " + F("393.0", "640") + ") * __k4[__i] + (-"
            + F("2187.0", "6784") + " - (-" + F("92097.0", "339200") + ")) * __k5[__i] + ("
            + F("11.0", "84") + " - " + F("187.0", "2100") + ") * __k6[__i] + (0.0 - "
            + F("1.0", "40") + ") * __k7[__i])";
    }
    //=============================================================================
    // Fixed-RK4 trial step (__nflow_rk4sub), used by the crossing stepper's advance
    // and by every bisection probe. Same as the simulator's integrateFixedTo: k1
    // recomputed at the step start, 0.5h / 0.5h / h stages, no output report, no
    // surface writes.
    void
    emitRk4Sub(const SolverEmitSpec& spec)
    {
        auto& lines = *spec.lines;
        const bool ru = spec.rust;
        const std::string& N = spec.stateCount;

        if (ru) {
            lines.push_back("#[allow(unused_variables, clippy::needless_range_loop, "
                            "clippy::too_many_arguments)]");
            lines.push_back("fn __nflow_rk4sub(s: &mut ModelState, inp: &ModelInput, out: &mut "
                            "ModelOutput, __t: f64, __hh: f64, __xa: &[f64; "
                + N + "], __xb: &mut [f64; " + N + "]) {");
            for (const char* v : { "__k1", "__k2", "__k3", "__k4", "__ts" }) {
                lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
            }
            lines.push_back("    __nflow_rhs(s, inp, out, __t, __xa, &mut __k1, false, None);");
            lines.push_back(
                "    for __i in 0.." + N + " { __ts[__i] = __xa[__i] + 0.5 * __hh * __k1[__i]; }");
            lines.push_back(
                "    __nflow_rhs(s, inp, out, __t + 0.5 * __hh, &__ts, &mut __k2, false, None);");
            lines.push_back(
                "    for __i in 0.." + N + " { __ts[__i] = __xa[__i] + 0.5 * __hh * __k2[__i]; }");
            lines.push_back(
                "    __nflow_rhs(s, inp, out, __t + 0.5 * __hh, &__ts, &mut __k3, false, None);");
            lines.push_back(
                "    for __i in 0.." + N + " { __ts[__i] = __xa[__i] + __hh * __k3[__i]; }");
            lines.push_back(
                "    __nflow_rhs(s, inp, out, __t + __hh, &__ts, &mut __k4, false, None);");
            lines.push_back("    for __i in 0.." + N
                + " { __xb[__i] = __xa[__i] + (__hh / 6.0) * (__k1[__i] + 2.0 * __k2[__i] + 2.0 "
                  "* __k3[__i] + __k4[__i]); }");
            lines.push_back("}");
        } else {
            lines.push_back("static void __nflow_rk4sub(ModelState* s, const ModelInput* in, "
                            "double __t, double __hh, const double* __xa, double* __xb) {");
            lines.push_back("  double __k1[" + N + "], __k2[" + N + "], __k3[" + N + "], __k4[" + N
                + "], __ts[" + N + "];");
            lines.push_back("  __nflow_rhs(s, in, (ModelOutput*)0, __t, __xa, __k1, (double*)0);");
            lines.push_back("  for (int __i = 0; __i < " + N
                + "; ++__i) __ts[__i] = __xa[__i] + 0.5 * __hh * __k1[__i];");
            lines.push_back("  __nflow_rhs(s, in, (ModelOutput*)0, __t + 0.5 * __hh, __ts, __k2, "
                            "(double*)0);");
            lines.push_back("  for (int __i = 0; __i < " + N
                + "; ++__i) __ts[__i] = __xa[__i] + 0.5 * __hh * __k2[__i];");
            lines.push_back("  __nflow_rhs(s, in, (ModelOutput*)0, __t + 0.5 * __hh, __ts, __k3, "
                            "(double*)0);");
            lines.push_back("  for (int __i = 0; __i < " + N
                + "; ++__i) __ts[__i] = __xa[__i] + __hh * __k3[__i];");
            lines.push_back(
                "  __nflow_rhs(s, in, (ModelOutput*)0, __t + __hh, __ts, __k4, (double*)0);");
            lines.push_back("  for (int __i = 0; __i < " + N
                + "; ++__i) __xb[__i] = __xa[__i] + (__hh / 6.0) * (__k1[__i] + 2.0 * __k2[__i] "
                  "+ 2.0 * __k3[__i] + __k4[__i]);");
            lines.push_back("}");
        }
        lines.push_back("");
    }
    //=============================================================================
    // Sign-change scan and bisection, shared by the ode4 and ode45 crossing
    // steppers. Follows the simulator's firstCrossing (a zero counts only against a
    // nonzero previous value) and localizeRoot (60 iterations / 1e-12, fresh
    // fixed-RK4 probe from the interval start, state left at the LAST probe, time at
    // the midpoint). Two known differences: rootDirection filtering is not applied
    // (no generatable block declares directions; trigger subsystems do, and they are
    // rejected), and the surface ORDER follows the flattened block array, not the
    // runtime's hierarchical walk, so two surfaces crossing in the same sub-step may
    // tie-break differently.
    void
    emitCrossingScanAndBisect(const SolverEmitSpec& spec, const std::string& ind,
        const std::string& tStart, const std::string& hVar, const std::string& xStart,
        const std::string& xProbe, const std::string& onRoot,
        const std::string& tailStmt = "continue;")
    {
        auto& lines = *spec.lines;
        const bool ru = spec.rust;
        const std::string& NG = spec.crossingCount;
        if (ru) {
            lines.push_back(ind + "let mut __hit: i32 = -1;");
            lines.push_back(ind + "for __i in 0.." + NG + " {");
            lines.push_back(ind + "    if __g0[__i] == 0.0 || __g1[__i] == 0.0 {");
            lines.push_back(ind
                + "        if __g1[__i] == 0.0 && __g0[__i] != 0.0 { __hit = __i as i32; break; }");
            lines.push_back(ind + "        continue;");
            lines.push_back(ind + "    }");
            lines.push_back(
                ind + "    if __g0[__i] * __g1[__i] < 0.0 { __hit = __i as i32; break; }");
            lines.push_back(ind + "}");
            lines.push_back(ind + "if __hit >= 0 {");
            lines.push_back(ind + "    let __ga = __g0[__hit as usize];");
            lines.push_back(ind + "    let mut __lo = " + tStart + ";");
            lines.push_back(ind + "    let mut __hi = " + tStart + " + " + hVar + ";");
            lines.push_back(ind + "    let mut __it = 0;");
            lines.push_back(ind + "    while __it < 60 && (__hi - __lo) > 1e-12 {");
            lines.push_back(ind + "        let __mid = 0.5 * (__lo + __hi);");
            lines.push_back(ind + "        __nflow_rk4sub(s, inp, out, " + tStart + ", __mid - "
                + tStart + ", &" + xStart + ", &mut " + xProbe + ");");
            lines.push_back(ind + "        __nflow_rhs(s, inp, out, __mid, &" + xProbe
                + ", &mut __ks, false, Some(&mut __g1[..]));");
            lines.push_back(ind + "        let __gm = __g1[__hit as usize];");
            lines.push_back(ind
                + "        if (__ga <= 0.0 && __gm <= 0.0) || (__ga > 0.0 && "
                  "__gm > 0.0) { __lo = __mid; } else { __hi = __mid; }");
            lines.push_back(ind + "        __it += 1;");
            lines.push_back(ind + "    }");
            lines.push_back(ind + "    for __i in 0.." + spec.stateCount + " { " + xStart
                + "[__i] = " + xProbe + "[__i]; }");
            lines.push_back(ind + "    " + onRoot);
            lines.push_back(ind + "    " + tailStmt);
            lines.push_back(ind + "}");
        } else {
            lines.push_back(ind + "int __hit = -1;");
            lines.push_back(ind + "for (int __i = 0; __i < " + NG + "; ++__i) {");
            lines.push_back(ind + "  if (__g0[__i] == 0.0 || __g1[__i] == 0.0) {");
            lines.push_back(
                ind + "    if (__g1[__i] == 0.0 && __g0[__i] != 0.0) { __hit = __i; break; }");
            lines.push_back(ind + "    continue;");
            lines.push_back(ind + "  }");
            lines.push_back(ind + "  if (__g0[__i] * __g1[__i] < 0.0) { __hit = __i; break; }");
            lines.push_back(ind + "}");
            lines.push_back(ind + "if (__hit >= 0) {");
            lines.push_back(ind + "  const double __ga = __g0[__hit];");
            lines.push_back(
                ind + "  double __lo = " + tStart + ", __hi = " + tStart + " + " + hVar + ";");
            lines.push_back(ind
                + "  for (int __it = 0; __it < 60 && (__hi - __lo) > 1e-12; "
                  "++__it) {");
            lines.push_back(ind + "    const double __mid = 0.5 * (__lo + __hi);");
            lines.push_back(ind + "    __nflow_rk4sub(s, in, " + tStart + ", __mid - " + tStart
                + ", " + xStart + ", " + xProbe + ");");
            lines.push_back(ind + "    __nflow_rhs(s, in, (ModelOutput*)0, __mid, " + xProbe
                + ", __ks, __g1);");
            lines.push_back(ind + "    const double __gm = __g1[__hit];");
            lines.push_back(ind
                + "    if ((__ga <= 0.0 && __gm <= 0.0) || (__ga > 0.0 && __gm > "
                  "0.0)) __lo = __mid; else __hi = __mid;");
            lines.push_back(ind + "  }");
            lines.push_back(ind + "  for (int __i = 0; __i < " + spec.stateCount + "; ++__i) "
                + xStart + "[__i] = " + xProbe + "[__i];");
            lines.push_back(ind + "  " + onRoot);
            lines.push_back(ind + "  " + tailStmt);
            lines.push_back(ind + "}");
        }
    }
    //=============================================================================
    // ode4 stepper with zero-crossing localization: sub-advance the communication
    // step, bisect to each crossing and restart there (like the simulator's
    // FixedStepBackend advance/localizeRoot pair).
    void
    emitRk4StepperWithRoots(const SolverEmitSpec& spec)
    {
        emitRk4Sub(spec);
        auto& lines = *spec.lines;
        const bool ru = spec.rust;
        const std::string& N = spec.stateCount;
        const std::string& NG = spec.crossingCount;
        if (ru) {
            lines.push_back("#[allow(unused_variables, unused_assignments, unused_mut, "
                            "clippy::needless_range_loop)]");
            lines.push_back("pub fn nflow_step(s: &mut ModelState,");
            lines.push_back("                  inp: &ModelInput,");
            lines.push_back("                  out: &mut ModelOutput,");
            lines.push_back("                  t: f64) {");
            lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
            for (const char* v : { "__x0", "__x1", "__ks" }) {
                lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
            }
            lines.push_back("    let mut __g0 = [0.0f64; " + NG + "];");
            lines.push_back("    let mut __g1 = [0.0f64; " + NG + "];");
        } else {
            lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                            "out, double t) {");
            lines.push_back("  const double dt = " + spec.dtLiteral + ";");
            lines.push_back("  double __x0[" + N + "], __x1[" + N + "], __ks[" + N + "];");
            lines.push_back("  double __g0[" + NG + "], __g1[" + NG + "];");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Gather, "__x0");
        // The reporting evaluation doubles as the initial g0 fill; g0 is
        // re-evaluated only after a bisection restart moved (__tc, __x0).
        lines.push_back(rhsCall(ru, ru ? "    " : "  ", "t", "__x0", "__ks", true,
            ru ? "Some(&mut __g0[..])" : "__g0"));
        if (spec.resetReseed) {
            // See emitRk4Stepper: read back what a reset re-seeded, and refill
            // the surfaces from it (they were filled at the discarded state).
            spec.emitSeam(Nelson::NFlow::Rk4Gather, "__x0");
            lines.push_back(rhsCall(ru, ru ? "    " : "  ", "t", "__x0", "__ks", false,
                ru ? "Some(&mut __g0[..])" : "__g0"));
        }
        // The restart budget (1000 roots per communication step, far above any
        // physical model) never truncates the interval: once exhausted, the rest is
        // integrated in one plain sub-step without root checks. The stepper always
        // reaches __tend, so a chattering surface cannot hang it.
        if (ru) {
            lines.push_back("    let mut __tc: f64 = t;");
            lines.push_back("    let __tend: f64 = t + dt;");
            lines.push_back("    let mut __rst = 0;");
            lines.push_back("    let mut __rchk = true;");
            lines.push_back("    let mut __rgot = true;");
            lines.push_back("    while __tc < __tend - 1e-12 * dt {");
            lines.push_back("        let __h = __tend - __tc;");
            lines.push_back("        if __rchk {");
            lines.push_back("            if !__rgot { __nflow_rhs(s, inp, out, __tc, &__x0, &mut "
                            "__ks, false, Some(&mut __g0[..])); }");
            lines.push_back("            __rgot = false;");
            lines.push_back(
                "            __nflow_rk4sub(s, inp, out, __tc, __h, &__x0, &mut __x1);");
            lines.push_back("            __nflow_rhs(s, inp, out, __tc + __h, &__x1, &mut __ks, "
                            "false, Some(&mut __g1[..]));");
            emitCrossingScanAndBisect(spec, "            ", "__tc", "__h", "__x0", "__x1",
                "__tc = 0.5 * (__lo + __hi); __rst += 1; if __rst >= 1000 { __rchk = false; }");
            lines.push_back("        } else {");
            lines.push_back(
                "            __nflow_rk4sub(s, inp, out, __tc, __h, &__x0, &mut __x1);");
            lines.push_back("        }");
            lines.push_back("        for __i in 0.." + N + " { __x0[__i] = __x1[__i]; }");
            lines.push_back("        __tc = __tend;");
            lines.push_back("    }");
        } else {
            lines.push_back("  double __tc = t;");
            lines.push_back("  const double __tend = t + dt;");
            lines.push_back("  int __rst = 0;");
            lines.push_back("  int __rchk = 1;");
            lines.push_back("  int __rgot = 1;");
            lines.push_back("  while (__tc < __tend - 1e-12 * dt) {");
            lines.push_back("    const double __h = __tend - __tc;");
            lines.push_back("    if (__rchk) {");
            lines.push_back("      if (!__rgot) __nflow_rhs(s, in, (ModelOutput*)0, __tc, __x0, "
                            "__ks, __g0);");
            lines.push_back("      __rgot = 0;");
            lines.push_back("      __nflow_rk4sub(s, in, __tc, __h, __x0, __x1);");
            lines.push_back(
                "      __nflow_rhs(s, in, (ModelOutput*)0, __tc + __h, __x1, __ks, __g1);");
            emitCrossingScanAndBisect(spec, "      ", "__tc", "__h", "__x0", "__x1",
                "__tc = 0.5 * (__lo + __hi); if (++__rst >= 1000) __rchk = 0;");
            lines.push_back("    } else {");
            lines.push_back("      __nflow_rk4sub(s, in, __tc, __h, __x0, __x1);");
            lines.push_back("    }");
            lines.push_back("    for (int __i = 0; __i < " + N + "; ++__i) __x0[__i] = __x1[__i];");
            lines.push_back("    __tc = __tend;");
            lines.push_back("  }");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Scatter, "__x0");
        spec.emitSeam(Nelson::NFlow::Rk4Clamp, "");
        lines.push_back("}");
    }
    //=============================================================================
    // ode45 stepper with zero-crossing localization: accepted-step / root-check /
    // bisect-and-restart flow like the simulator's DormandPrinceBackend.
    // localizeRoot uses fixed-RK4 probes; the step size is NOT regrown on a root,
    // only a rootless accepted step grows it.
    void
    emitDp45StepperWithRoots(const SolverEmitSpec& spec)
    {
        emitRk4Sub(spec);
        auto& lines = *spec.lines;
        const bool ru = spec.rust;
        const std::string& N = spec.stateCount;
        const std::string& NG = spec.crossingCount;
        const std::string ind = ru ? "    " : "  ";
        const std::string ind2 = ru ? "        " : "    ";
        const std::string ind3 = ru ? "            " : "      ";
        if (ru) {
            lines.push_back("#[allow(unused_variables, unused_assignments, unused_mut, "
                            "clippy::needless_range_loop)]");
            lines.push_back("pub fn nflow_step(s: &mut ModelState,");
            lines.push_back("                  inp: &ModelInput,");
            lines.push_back("                  out: &mut ModelOutput,");
            lines.push_back("                  t: f64) {");
            lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
            lines.push_back("    let __rtol: f64 = " + spec.rtolLiteral + ";");
            lines.push_back("    let __atol: f64 = " + spec.atolLiteral + ";");
            lines.push_back("    let __hmax: f64 = " + spec.hmaxLiteral + ";");
            for (const char* v : { "__x0", "__x", "__xt", "__x5", "__xsv", "__ks", "__k1", "__k2",
                     "__k3", "__k4", "__k5", "__k6", "__k7" }) {
                lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
            }
            lines.push_back("    let mut __g0 = [0.0f64; " + NG + "];");
            lines.push_back("    let mut __g1 = [0.0f64; " + NG + "];");
        } else {
            lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                            "out, double t) {");
            lines.push_back("  const double dt = " + spec.dtLiteral + ";");
            lines.push_back("  const double __rtol = " + spec.rtolLiteral + ";");
            lines.push_back("  const double __atol = " + spec.atolLiteral + ";");
            lines.push_back("  const double __hmax = " + spec.hmaxLiteral + ";");
            lines.push_back("  double __x0[" + N + "], __x[" + N + "], __xt[" + N + "], __x5[" + N
                + "], __xsv[" + N + "], __ks[" + N + "];");
            lines.push_back("  double __k1[" + N + "], __k2[" + N + "], __k3[" + N + "], __k4[" + N
                + "], __k5[" + N + "], __k6[" + N + "], __k7[" + N + "];");
            lines.push_back("  double __g0[" + NG + "], __g1[" + NG + "];");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Gather, "__x0");
        lines.push_back(rhsCall(ru, ind, "t", "__x0", "__k1", true, gNull(ru)));
        lines.push_back(forAssign(ru, ind, N, "__x", "__x0[__i]"));
        if (ru) {
            lines.push_back("    __nflow_rhs(s, inp, out, t, &__x, &mut __ks, false, Some(&mut "
                            "__g0));");
            lines.push_back("    let mut __tc: f64 = t;");
            lines.push_back("    let __tend: f64 = t + dt;");
            lines.push_back("    let mut __h: f64 = dt;");
            lines.push_back("    if __hmax > 0.0 && __h > __hmax { __h = __hmax; }");
            lines.push_back("    let mut __rooted = false;");
            lines.push_back("    let mut __rst = 0;");
            lines.push_back("    let mut __rchk = true;");
            lines.push_back("    while __tc < __tend - 1e-12 * dt {");
            lines.push_back("        if __h > __tend - __tc { __h = __tend - __tc; }");
            lines.push_back("        if __hmax > 0.0 && __h > __hmax { __h = __hmax; }");
            lines.push_back("        for _ in 0..50 {");
        } else {
            lines.push_back("  __nflow_rhs(s, in, (ModelOutput*)0, t, __x, __ks, __g0);");
            lines.push_back("  double __tc = t;");
            lines.push_back("  const double __tend = t + dt;");
            lines.push_back("  double __h = dt;");
            lines.push_back("  if (__hmax > 0.0 && __h > __hmax) __h = __hmax;");
            lines.push_back("  int __rooted = 0;");
            lines.push_back("  int __rst = 0;");
            lines.push_back("  int __rchk = 1;");
            lines.push_back("  while (__tc < __tend - 1e-12 * dt) {");
            lines.push_back("    if (__h > __tend - __tc) __h = __tend - __tc;");
            lines.push_back("    if (__hmax > 0.0 && __h > __hmax) __h = __hmax;");
            lines.push_back("    for (int __att = 0; __att < 50; ++__att) {");
        }
        const std::string errExpr = emitDp45Stages(spec, ind3, gNull(ru));
        if (ru) {
            lines.push_back("            let mut __acc: f64 = 0.0;");
            lines.push_back("            for __i in 0.." + N + " {");
            lines.push_back("                let __e = " + errExpr + ";");
            lines.push_back("                let __sc = __atol + __rtol * "
                            "libm::fmax(libm::fabs(__x[__i]), libm::fabs(__x5[__i]));");
            lines.push_back("                let __r = if __sc > 0.0 { __e / __sc } else { 0.0 };");
            lines.push_back("                __acc += __r * __r;");
            lines.push_back("            }");
            lines.push_back("            let __err = libm::sqrt(__acc / (" + N + " as f64));");
            lines.push_back("            if __err <= 1.0 || __h <= 1e-13 {");
            lines.push_back("                if __rchk {");
            lines.push_back(
                "                    for __i in 0.." + N + " { __xsv[__i] = __x[__i]; }");
            lines.push_back("                    __nflow_rhs(s, inp, out, __tc + __h, &__x5, &mut "
                            "__ks, false, Some(&mut __g1[..]));");
            emitCrossingScanAndBisect(spec, "                    ", "__tc", "__h", "__xsv", "__xt",
                "__tc = 0.5 * (__lo + __hi); __rst += 1; if __rst >= 1000 { __rchk = false; } "
                "for __i in 0.."
                    + N
                    + " { __x[__i] = __xsv[__i]; } __nflow_rhs(s, inp, out, __tc, &__x, &mut "
                      "__ks, false, Some(&mut __g0[..]));",
                "__rooted = true; break;");
            lines.push_back(
                "                    for __i in 0.." + NG + " { __g0[__i] = __g1[__i]; }");
            lines.push_back("                }");
            lines.push_back("                __tc += __h;");
            lines.push_back("                for __i in 0.." + N + " { __x[__i] = __x5[__i]; }");
            lines.push_back("                let mut __grow = 0.9 * libm::pow(libm::fmax(__err, "
                            "1e-10), -0.2);");
            lines.push_back("                if __grow < 0.2 { __grow = 0.2; }");
            lines.push_back("                if __grow > 5.0 { __grow = 5.0; }");
            lines.push_back("                __h *= __grow;");
            lines.push_back("                break;");
            lines.push_back("            } else {");
            lines.push_back("                let mut __fac = 0.9 * libm::pow(__err, -0.2);");
            lines.push_back("                if __fac < 0.2 { __fac = 0.2; }");
            lines.push_back("                if __fac > 1.0 { __fac = 1.0; }");
            lines.push_back("                __h *= __fac;");
            lines.push_back("                if __hmax > 0.0 && __h > __hmax { __h = __hmax; }");
            lines.push_back("            }");
            lines.push_back("        }");
            lines.push_back("        if __rooted { __rooted = false; continue; }");
            lines.push_back("    }");
        } else {
            lines.push_back("      double __acc = 0.0;");
            lines.push_back("      for (int __i = 0; __i < " + N + "; ++__i) {");
            lines.push_back("        double __e = " + errExpr + ";");
            lines.push_back("        double __xa = __x[__i] < 0.0 ? -__x[__i] : __x[__i];");
            lines.push_back("        double __x5a = __x5[__i] < 0.0 ? -__x5[__i] : __x5[__i];");
            lines.push_back(
                "        double __sc = __atol + __rtol * (__xa > __x5a ? __xa : __x5a);");
            lines.push_back("        double __r = (__sc > 0.0) ? __e / __sc : 0.0;");
            lines.push_back("        __acc += __r * __r;");
            lines.push_back("      }");
            lines.push_back("      double __err = sqrt(__acc / (double)" + N + ");");
            lines.push_back("      if (__err <= 1.0 || __h <= 1e-13) {");
            lines.push_back("        if (__rchk) {");
            lines.push_back(
                "          for (int __i = 0; __i < " + N + "; ++__i) __xsv[__i] = __x[__i];");
            lines.push_back(
                "          __nflow_rhs(s, in, (ModelOutput*)0, __tc + __h, __x5, __ks, __g1);");
            emitCrossingScanAndBisect(spec, "          ", "__tc", "__h", "__xsv", "__xt",
                "__tc = 0.5 * (__lo + __hi); if (++__rst >= 1000) __rchk = 0; "
                "for (int __i = 0; __i < "
                    + N
                    + "; ++__i) __x[__i] = __xsv[__i]; __nflow_rhs(s, in, (ModelOutput*)0, __tc, "
                      "__x, __ks, __g0);",
                "__rooted = 1; break;");
            lines.push_back(
                "          for (int __i = 0; __i < " + NG + "; ++__i) __g0[__i] = __g1[__i];");
            lines.push_back("        }");
            lines.push_back("        __tc += __h;");
            lines.push_back(
                "        for (int __i = 0; __i < " + N + "; ++__i) __x[__i] = __x5[__i];");
            lines.push_back(
                "        double __grow = 0.9 * pow(__err > 1e-10 ? __err : 1e-10, -0.2);");
            lines.push_back("        if (__grow < 0.2) __grow = 0.2;");
            lines.push_back("        if (__grow > 5.0) __grow = 5.0;");
            lines.push_back("        __h *= __grow;");
            lines.push_back("        break;");
            lines.push_back("      } else {");
            lines.push_back("        double __fac = 0.9 * pow(__err, -0.2);");
            lines.push_back("        if (__fac < 0.2) __fac = 0.2;");
            lines.push_back("        if (__fac > 1.0) __fac = 1.0;");
            lines.push_back("        __h *= __fac;");
            lines.push_back("        if (__hmax > 0.0 && __h > __hmax) __h = __hmax;");
            lines.push_back("      }");
            lines.push_back("    }");
            lines.push_back("    if (__rooted) { __rooted = 0; continue; }");
            lines.push_back("  }");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Scatter, "__x");
        spec.emitSeam(Nelson::NFlow::Rk4Clamp, "");
        lines.push_back("}");
    }
    //=============================================================================
} // namespace
//=============================================================================
void
emitRk4Stepper(const SolverEmitSpec& spec)
{
    if (hasCrossings(spec)) {
        emitRk4StepperWithRoots(spec);
        return;
    }
    auto& lines = *spec.lines;
    const bool ru = spec.rust;
    const std::string& N = spec.stateCount;
    const std::string ind = ru ? "    " : "  ";
    if (ru) {
        lines.push_back("#[allow(unused_variables, unused_assignments, unused_mut, "
                        "clippy::needless_range_loop)]");
        lines.push_back("pub fn nflow_step(s: &mut ModelState,");
        lines.push_back("                  inp: &ModelInput,");
        lines.push_back("                  out: &mut ModelOutput,");
        lines.push_back("                  t: f64) {");
        lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
        for (const char* v : { "__x0", "__k1", "__k2", "__k3", "__k4", "__xs", "__x1" }) {
            lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
        }
    } else {
        lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                        "out, double t) {");
        lines.push_back("  const double dt = " + spec.dtLiteral + ";");
        lines.push_back("  double __x0[" + N + "], __k1[" + N + "], __k2[" + N + "], __k3[" + N
            + "], __k4[" + N + "], __xs[" + N + "], __x1[" + N + "];");
    }
    spec.emitSeam(Nelson::NFlow::Rk4Gather, "__x0");
    lines.push_back(rhsCall(ru, ind, "t", "__x0", "__k1", true));
    if (spec.resetReseed) {
        // A reset fired by the reporting evaluation re-seeded its members' state
        // fields. Pick those up and evaluate the first stage again, so the step starts
        // from the new initial value and not from the discarded one.
        spec.emitSeam(Nelson::NFlow::Rk4Gather, "__x0");
        lines.push_back(rhsCall(ru, ind, "t", "__x0", "__k1", false));
    }
    lines.push_back(forAssign(ru, ind, N, "__xs", "__x0[__i] + 0.5 * dt * __k1[__i]"));
    lines.push_back(rhsCall(ru, ind, "t + 0.5 * dt", "__xs", "__k2", false));
    lines.push_back(forAssign(ru, ind, N, "__xs", "__x0[__i] + 0.5 * dt * __k2[__i]"));
    lines.push_back(rhsCall(ru, ind, "t + 0.5 * dt", "__xs", "__k3", false));
    lines.push_back(forAssign(ru, ind, N, "__xs", "__x0[__i] + dt * __k3[__i]"));
    lines.push_back(rhsCall(ru, ind, "t + dt", "__xs", "__k4", false));
    lines.push_back(forAssign(ru, ind, N, "__x1",
        "__x0[__i] + (dt / 6.0) * (__k1[__i] + 2.0 * __k2[__i] + 2.0 * __k3[__i] + __k4[__i])"));
    spec.emitSeam(Nelson::NFlow::Rk4Scatter, "__x1");
    // Post-step clamp for bounded states (no-op for the others).
    spec.emitSeam(Nelson::NFlow::Rk4Clamp, "");
    lines.push_back("}");
}
//=============================================================================
void
emitDp45Stepper(const SolverEmitSpec& spec)
{
    if (hasCrossings(spec)) {
        emitDp45StepperWithRoots(spec);
        return;
    }
    auto& lines = *spec.lines;
    const bool ru = spec.rust;
    const std::string& N = spec.stateCount;
    const std::string ind = ru ? "    " : "  ";
    const std::string ind2 = ru ? "        " : "    ";
    const std::string ind3 = ru ? "            " : "      ";
    const std::string ind4 = ru ? "                " : "        ";
    if (ru) {
        lines.push_back("#[allow(unused_variables, unused_assignments, unused_mut, "
                        "clippy::needless_range_loop)]");
        lines.push_back("pub fn nflow_step(s: &mut ModelState,");
        lines.push_back("                  inp: &ModelInput,");
        lines.push_back("                  out: &mut ModelOutput,");
        lines.push_back("                  t: f64) {");
        lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
        lines.push_back("    let __rtol: f64 = " + spec.rtolLiteral + ";");
        lines.push_back("    let __atol: f64 = " + spec.atolLiteral + ";");
        lines.push_back("    let __hmax: f64 = " + spec.hmaxLiteral + ";");
        for (const char* v : { "__x0", "__x", "__xt", "__x5", "__k1", "__k2", "__k3", "__k4",
                 "__k5", "__k6", "__k7" }) {
            lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
        }
    } else {
        lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                        "out, double t) {");
        lines.push_back("  const double dt = " + spec.dtLiteral + ";");
        lines.push_back("  const double __rtol = " + spec.rtolLiteral + ";");
        lines.push_back("  const double __atol = " + spec.atolLiteral + ";");
        lines.push_back("  const double __hmax = " + spec.hmaxLiteral + ";");
        lines.push_back(
            "  double __x0[" + N + "], __x[" + N + "], __xt[" + N + "], __x5[" + N + "];");
        lines.push_back("  double __k1[" + N + "], __k2[" + N + "], __k3[" + N + "], __k4[" + N
            + "], __k5[" + N + "], __k6[" + N + "], __k7[" + N + "];");
    }
    spec.emitSeam(Nelson::NFlow::Rk4Gather, "__x0");
    // Report the pre-step output at t (report-then-advance, like ode4).
    lines.push_back(rhsCall(ru, ind, "t", "__x0", "__k1", true));
    lines.push_back(forAssign(ru, ind, N, "__x", "__x0[__i]"));
    if (ru) {
        lines.push_back("    let mut __tc: f64 = t;");
        lines.push_back("    let __tend: f64 = t + dt;");
        lines.push_back("    let mut __h: f64 = dt;");
        lines.push_back("    if __hmax > 0.0 && __h > __hmax { __h = __hmax; }");
        lines.push_back("    while __tc < __tend - 1e-12 * dt {");
        lines.push_back("        if __h > __tend - __tc { __h = __tend - __tc; }");
        lines.push_back("        if __hmax > 0.0 && __h > __hmax { __h = __hmax; }");
        lines.push_back("        for _ in 0..50 {");
    } else {
        lines.push_back("  double __tc = t;");
        lines.push_back("  const double __tend = t + dt;");
        lines.push_back("  double __h = dt;");
        lines.push_back("  if (__hmax > 0.0 && __h > __hmax) __h = __hmax;");
        lines.push_back("  while (__tc < __tend - 1e-12 * dt) {");
        lines.push_back("    if (__h > __tend - __tc) __h = __tend - __tc;");
        lines.push_back("    if (__hmax > 0.0 && __h > __hmax) __h = __hmax;");
        lines.push_back("    for (int __att = 0; __att < 50; ++__att) {");
    }
    const std::string errExpr = emitDp45Stages(spec, ind3, std::string());
    if (ru) {
        lines.push_back("            let mut __acc: f64 = 0.0;");
        lines.push_back("            for __i in 0.." + N + " {");
        lines.push_back("                let __e = " + errExpr + ";");
        lines.push_back("                let __sc = __atol + __rtol * "
                        "libm::fmax(libm::fabs(__x[__i]), libm::fabs(__x5[__i]));");
        lines.push_back("                let __r = if __sc > 0.0 { __e / __sc } else { 0.0 };");
        lines.push_back("                __acc += __r * __r;");
        lines.push_back("            }");
        lines.push_back("            let __err = libm::sqrt(__acc / (" + N + " as f64));");
        lines.push_back("            if __err <= 1.0 || __h <= 1e-13 {");
        lines.push_back("                __tc += __h;");
        lines.push_back("                for __i in 0.." + N + " { __x[__i] = __x5[__i]; }");
        lines.push_back("                let mut __grow = 0.9 * libm::pow(libm::fmax(__err, "
                        "1e-10), -0.2);");
        lines.push_back("                if __grow < 0.2 { __grow = 0.2; }");
        lines.push_back("                if __grow > 5.0 { __grow = 5.0; }");
        lines.push_back("                __h *= __grow;");
        lines.push_back("                break;");
        lines.push_back("            } else {");
        lines.push_back("                let mut __fac = 0.9 * libm::pow(__err, -0.2);");
        lines.push_back("                if __fac < 0.2 { __fac = 0.2; }");
        lines.push_back("                if __fac > 1.0 { __fac = 1.0; }");
        lines.push_back("                __h *= __fac;");
        lines.push_back("                if __hmax > 0.0 && __h > __hmax { __h = __hmax; }");
        lines.push_back("            }");
        lines.push_back("        }");
        lines.push_back("    }");
    } else {
        lines.push_back("      double __acc = 0.0;");
        lines.push_back("      for (int __i = 0; __i < " + N + "; ++__i) {");
        lines.push_back("        double __e = " + errExpr + ";");
        lines.push_back("        double __xa = __x[__i] < 0.0 ? -__x[__i] : __x[__i];");
        lines.push_back("        double __x5a = __x5[__i] < 0.0 ? -__x5[__i] : __x5[__i];");
        lines.push_back("        double __sc = __atol + __rtol * (__xa > __x5a ? __xa : __x5a);");
        lines.push_back("        double __r = (__sc > 0.0) ? __e / __sc : 0.0;");
        lines.push_back("        __acc += __r * __r;");
        lines.push_back("      }");
        lines.push_back("      double __err = sqrt(__acc / (double)" + N + ");");
        lines.push_back("      if (__err <= 1.0 || __h <= 1e-13) {");
        lines.push_back("        __tc += __h;");
        lines.push_back("        for (int __i = 0; __i < " + N + "; ++__i) __x[__i] = __x5[__i];");
        lines.push_back("        double __grow = 0.9 * pow(__err > 1e-10 ? __err : 1e-10, -0.2);");
        lines.push_back("        if (__grow < 0.2) __grow = 0.2;");
        lines.push_back("        if (__grow > 5.0) __grow = 5.0;");
        lines.push_back("        __h *= __grow;");
        lines.push_back("        break;");
        lines.push_back("      } else {");
        lines.push_back("        double __fac = 0.9 * pow(__err, -0.2);");
        lines.push_back("        if (__fac < 0.2) __fac = 0.2;");
        lines.push_back("        if (__fac > 1.0) __fac = 1.0;");
        lines.push_back("        __h *= __fac;");
        lines.push_back("        if (__hmax > 0.0 && __h > __hmax) __h = __hmax;");
        lines.push_back("      }");
        lines.push_back("    }");
        lines.push_back("  }");
    }
    spec.emitSeam(Nelson::NFlow::Rk4Scatter, "__x");
    // Post-step clamp for bounded states (no-op for the others).
    spec.emitSeam(Nelson::NFlow::Rk4Clamp, "");
    lines.push_back("}");
}
//=============================================================================
} // namespace nflow
//=============================================================================
