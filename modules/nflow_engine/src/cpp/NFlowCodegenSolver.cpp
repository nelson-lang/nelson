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
    // "<ind>nflow_rhs(...)" with the per-language call shape: the C rhs takes a
    // nullable ModelOutput*, the Rust rhs a nf_report flag. `gArg` (zero-crossing
    // mode only) appends the trailing surface-array argument.
    std::string
    rhsCall(bool rust, const std::string& ind, const std::string& tExpr, const std::string& xArr,
        const std::string& kArr, bool report, const std::string& gArg = std::string())
    {
        const std::string tail = gArg.empty() ? "" : (", " + gArg);
        if (rust) {
            return ind + "nflow_rhs(s, inp, out, " + tExpr + ", &" + xArr + ", &mut " + kArr + ", "
                + (report ? "true" : "false") + tail + ");";
        }
        return ind + "nflow_rhs(s, in, " + (report ? "out" : "(ModelOutput*)0") + ", " + tExpr
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
            return ind + "for nf_i in 0.." + N + " { " + target + "[nf_i] = " + expr + "; }";
        }
        return ind + "for (int nf_i = 0; nf_i < " + N + "; ++nf_i) " + target + "[nf_i] = " + expr
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
    // null nf_g form).
    std::string
    emitDp45Stages(const SolverEmitSpec& spec, const std::string& ind3, const std::string& gArg)
    {
        auto& lines = *spec.lines;
        const bool ru = spec.rust;
        const std::string& N = spec.stateCount;
        auto F = [&](const char* num, const char* den) { return frac(ru, num, den); };
        lines.push_back(rhsCall(ru, ind3, "nf_tc", "nf_x", "nf_k1", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "nf_xt", "nf_x[nf_i] + nf_h * (0.2 * nf_k1[nf_i])"));
        lines.push_back(rhsCall(ru, ind3, "nf_tc + 0.2 * nf_h", "nf_xt", "nf_k2", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "nf_xt",
            "nf_x[nf_i] + nf_h * (" + F("3.0", "40") + " * nf_k1[nf_i] + " + F("9.0", "40")
                + " * nf_k2[nf_i])"));
        lines.push_back(rhsCall(ru, ind3, "nf_tc + 0.3 * nf_h", "nf_xt", "nf_k3", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "nf_xt",
            "nf_x[nf_i] + nf_h * (" + F("44.0", "45") + " * nf_k1[nf_i] - " + F("56.0", "15")
                + " * nf_k2[nf_i] + " + F("32.0", "9") + " * nf_k3[nf_i])"));
        lines.push_back(rhsCall(ru, ind3, "nf_tc + 0.8 * nf_h", "nf_xt", "nf_k4", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "nf_xt",
            "nf_x[nf_i] + nf_h * (" + F("19372.0", "6561") + " * nf_k1[nf_i] - "
                + F("25360.0", "2187") + " * nf_k2[nf_i] + " + F("64448.0", "6561")
                + " * nf_k3[nf_i] - " + F("212.0", "729") + " * nf_k4[nf_i])"));
        lines.push_back(rhsCall(
            ru, ind3, "nf_tc + " + F("8.0", "9") + " * nf_h", "nf_xt", "nf_k5", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "nf_xt",
            "nf_x[nf_i] + nf_h * (" + F("9017.0", "3168") + " * nf_k1[nf_i] - " + F("355.0", "33")
                + " * nf_k2[nf_i] + " + F("46732.0", "5247") + " * nf_k3[nf_i] + "
                + F("49.0", "176") + " * nf_k4[nf_i] - " + F("5103.0", "18656")
                + " * nf_k5[nf_i])"));
        lines.push_back(rhsCall(ru, ind3, "nf_tc + nf_h", "nf_xt", "nf_k6", false, gArg));
        lines.push_back(forAssign(ru, ind3, N, "nf_x5",
            "nf_x[nf_i] + nf_h * (" + F("35.0", "384") + " * nf_k1[nf_i] + " + F("500.0", "1113")
                + " * nf_k3[nf_i] + " + F("125.0", "192") + " * nf_k4[nf_i] - "
                + F("2187.0", "6784") + " * nf_k5[nf_i] + " + F("11.0", "84") + " * nf_k6[nf_i])"));
        lines.push_back(rhsCall(ru, ind3, "nf_tc + nf_h", "nf_x5", "nf_k7", false, gArg));
        return "nf_h * ((" + F("35.0", "384") + " - " + F("5179.0", "57600") + ") * nf_k1[nf_i] + ("
            + F("500.0", "1113") + " - " + F("7571.0", "16695") + ") * nf_k3[nf_i] + ("
            + F("125.0", "192") + " - " + F("393.0", "640") + ") * nf_k4[nf_i] + (-"
            + F("2187.0", "6784") + " - (-" + F("92097.0", "339200") + ")) * nf_k5[nf_i] + ("
            + F("11.0", "84") + " - " + F("187.0", "2100") + ") * nf_k6[nf_i] + (0.0 - "
            + F("1.0", "40") + ") * nf_k7[nf_i])";
    }
    //=============================================================================
    // Fixed-RK4 trial step (nflow_rk4sub), used by the crossing stepper's advance
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
            lines.push_back("#[allow(clippy::needless_range_loop, clippy::too_many_arguments)]");
            lines.push_back("fn nflow_rk4sub(s: &mut ModelState, inp: &ModelInput, out: &mut "
                            "ModelOutput, nf_t: f64, nf_hh: f64, nf_xa: &[f64; "
                + N + "], nf_xb: &mut [f64; " + N + "]) {");
            for (const char* v : { "nf_k1", "nf_k2", "nf_k3", "nf_k4", "nf_ts" }) {
                lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
            }
            lines.push_back("    nflow_rhs(s, inp, out, nf_t, nf_xa, &mut nf_k1, false, None);");
            lines.push_back("    for nf_i in 0.." + N
                + " { nf_ts[nf_i] = nf_xa[nf_i] + 0.5 * nf_hh * nf_k1[nf_i]; }");
            lines.push_back(
                "    nflow_rhs(s, inp, out, nf_t + 0.5 * nf_hh, &nf_ts, &mut nf_k2, false, None);");
            lines.push_back("    for nf_i in 0.." + N
                + " { nf_ts[nf_i] = nf_xa[nf_i] + 0.5 * nf_hh * nf_k2[nf_i]; }");
            lines.push_back(
                "    nflow_rhs(s, inp, out, nf_t + 0.5 * nf_hh, &nf_ts, &mut nf_k3, false, None);");
            lines.push_back("    for nf_i in 0.." + N
                + " { nf_ts[nf_i] = nf_xa[nf_i] + nf_hh * nf_k3[nf_i]; }");
            lines.push_back(
                "    nflow_rhs(s, inp, out, nf_t + nf_hh, &nf_ts, &mut nf_k4, false, None);");
            lines.push_back("    for nf_i in 0.." + N
                + " { nf_xb[nf_i] = nf_xa[nf_i] + (nf_hh / 6.0) * (nf_k1[nf_i] + 2.0 * nf_k2[nf_i] "
                  "+ 2.0 "
                  "* nf_k3[nf_i] + nf_k4[nf_i]); }");
            lines.push_back("}");
        } else {
            lines.push_back("static void nflow_rk4sub(ModelState* s, const ModelInput* in, "
                            "double nf_t, double nf_hh, const double* nf_xa, double* nf_xb) {");
            lines.push_back("  double nf_k1[" + N + "], nf_k2[" + N + "], nf_k3[" + N + "], nf_k4["
                + N + "], nf_ts[" + N + "];");
            lines.push_back("  nflow_rhs(s, in, (ModelOutput*)0, nf_t, nf_xa, nf_k1, (double*)0);");
            lines.push_back("  for (int nf_i = 0; nf_i < " + N
                + "; ++nf_i) nf_ts[nf_i] = nf_xa[nf_i] + 0.5 * nf_hh * nf_k1[nf_i];");
            lines.push_back("  nflow_rhs(s, in, (ModelOutput*)0, nf_t + 0.5 * nf_hh, nf_ts, nf_k2, "
                            "(double*)0);");
            lines.push_back("  for (int nf_i = 0; nf_i < " + N
                + "; ++nf_i) nf_ts[nf_i] = nf_xa[nf_i] + 0.5 * nf_hh * nf_k2[nf_i];");
            lines.push_back("  nflow_rhs(s, in, (ModelOutput*)0, nf_t + 0.5 * nf_hh, nf_ts, nf_k3, "
                            "(double*)0);");
            lines.push_back("  for (int nf_i = 0; nf_i < " + N
                + "; ++nf_i) nf_ts[nf_i] = nf_xa[nf_i] + nf_hh * nf_k3[nf_i];");
            lines.push_back(
                "  nflow_rhs(s, in, (ModelOutput*)0, nf_t + nf_hh, nf_ts, nf_k4, (double*)0);");
            lines.push_back("  for (int nf_i = 0; nf_i < " + N
                + "; ++nf_i) nf_xb[nf_i] = nf_xa[nf_i] + (nf_hh / 6.0) * (nf_k1[nf_i] + 2.0 * "
                  "nf_k2[nf_i] "
                  "+ 2.0 * nf_k3[nf_i] + nf_k4[nf_i]);");
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
            lines.push_back(ind + "let mut nf_hit: i32 = -1;");
            lines.push_back(ind + "for nf_i in 0.." + NG + " {");
            lines.push_back(ind + "    if nf_g0[nf_i] == 0.0 || nf_g1[nf_i] == 0.0 {");
            lines.push_back(ind
                + "        if nf_g1[nf_i] == 0.0 && nf_g0[nf_i] != 0.0 { nf_hit = nf_i as i32; "
                  "break; }");
            lines.push_back(ind + "        continue;");
            lines.push_back(ind + "    }");
            lines.push_back(
                ind + "    if nf_g0[nf_i] * nf_g1[nf_i] < 0.0 { nf_hit = nf_i as i32; break; }");
            lines.push_back(ind + "}");
            lines.push_back(ind + "if nf_hit >= 0 {");
            lines.push_back(ind + "    let nf_ga = nf_g0[nf_hit as usize];");
            lines.push_back(ind + "    let mut nf_lo = " + tStart + ";");
            lines.push_back(ind + "    let mut nf_hi = " + tStart + " + " + hVar + ";");
            lines.push_back(ind + "    let mut nf_it = 0;");
            lines.push_back(ind + "    while nf_it < 60 && (nf_hi - nf_lo) > 1e-12 {");
            lines.push_back(ind + "        let nf_mid = 0.5 * (nf_lo + nf_hi);");
            lines.push_back(ind + "        nflow_rk4sub(s, inp, out, " + tStart + ", nf_mid - "
                + tStart + ", &" + xStart + ", &mut " + xProbe + ");");
            lines.push_back(ind + "        nflow_rhs(s, inp, out, nf_mid, &" + xProbe
                + ", &mut nf_ks, false, Some(&mut nf_g1[..]));");
            lines.push_back(ind + "        let nf_gm = nf_g1[nf_hit as usize];");
            lines.push_back(ind
                + "        if (nf_ga <= 0.0 && nf_gm <= 0.0) || (nf_ga > 0.0 && "
                  "nf_gm > 0.0) { nf_lo = nf_mid; } else { nf_hi = nf_mid; }");
            lines.push_back(ind + "        nf_it += 1;");
            lines.push_back(ind + "    }");
            lines.push_back(ind + "    for nf_i in 0.." + spec.stateCount + " { " + xStart
                + "[nf_i] = " + xProbe + "[nf_i]; }");
            lines.push_back(ind + "    " + onRoot);
            lines.push_back(ind + "    " + tailStmt);
            lines.push_back(ind + "}");
        } else {
            lines.push_back(ind + "int nf_hit = -1;");
            lines.push_back(ind + "for (int nf_i = 0; nf_i < " + NG + "; ++nf_i) {");
            lines.push_back(ind + "  if (nf_g0[nf_i] == 0.0 || nf_g1[nf_i] == 0.0) {");
            lines.push_back(ind
                + "    if (nf_g1[nf_i] == 0.0 && nf_g0[nf_i] != 0.0) { nf_hit = nf_i; break; }");
            lines.push_back(ind + "    continue;");
            lines.push_back(ind + "  }");
            lines.push_back(
                ind + "  if (nf_g0[nf_i] * nf_g1[nf_i] < 0.0) { nf_hit = nf_i; break; }");
            lines.push_back(ind + "}");
            lines.push_back(ind + "if (nf_hit >= 0) {");
            lines.push_back(ind + "  const double nf_ga = nf_g0[nf_hit];");
            lines.push_back(
                ind + "  double nf_lo = " + tStart + ", nf_hi = " + tStart + " + " + hVar + ";");
            lines.push_back(ind
                + "  for (int nf_it = 0; nf_it < 60 && (nf_hi - nf_lo) > 1e-12; "
                  "++nf_it) {");
            lines.push_back(ind + "    const double nf_mid = 0.5 * (nf_lo + nf_hi);");
            lines.push_back(ind + "    nflow_rk4sub(s, in, " + tStart + ", nf_mid - " + tStart
                + ", " + xStart + ", " + xProbe + ");");
            lines.push_back(ind + "    nflow_rhs(s, in, (ModelOutput*)0, nf_mid, " + xProbe
                + ", nf_ks, nf_g1);");
            lines.push_back(ind + "    const double nf_gm = nf_g1[nf_hit];");
            lines.push_back(ind
                + "    if ((nf_ga <= 0.0 && nf_gm <= 0.0) || (nf_ga > 0.0 && nf_gm > "
                  "0.0)) nf_lo = nf_mid; else nf_hi = nf_mid;");
            lines.push_back(ind + "  }");
            lines.push_back(ind + "  for (int nf_i = 0; nf_i < " + spec.stateCount + "; ++nf_i) "
                + xStart + "[nf_i] = " + xProbe + "[nf_i];");
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
            lines.push_back("#[allow(clippy::needless_range_loop)]");
            lines.push_back("pub fn nflow_step(s: &mut ModelState,");
            lines.push_back("                  inp: &ModelInput,");
            lines.push_back("                  out: &mut ModelOutput,");
            lines.push_back("                  t: f64) {");
            lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
            for (const char* v : { "nf_x0", "nf_x1", "nf_ks" }) {
                lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
            }
            lines.push_back("    let mut nf_g0 = [0.0f64; " + NG + "];");
            lines.push_back("    let mut nf_g1 = [0.0f64; " + NG + "];");
        } else {
            lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                            "out, double t) {");
            lines.push_back("  const double dt = " + spec.dtLiteral + ";");
            lines.push_back("  double nf_x0[" + N + "], nf_x1[" + N + "], nf_ks[" + N + "];");
            lines.push_back("  double nf_g0[" + NG + "], nf_g1[" + NG + "];");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Gather, "nf_x0");
        // The reporting evaluation doubles as the initial g0 fill; g0 is
        // re-evaluated only after a bisection restart moved (nf_tc, nf_x0).
        lines.push_back(rhsCall(ru, ru ? "    " : "  ", "t", "nf_x0", "nf_ks", true,
            ru ? "Some(&mut nf_g0[..])" : "nf_g0"));
        if (spec.resetReseed) {
            // See emitRk4Stepper: read back what a reset re-seeded, and refill
            // the surfaces from it (they were filled at the discarded state).
            spec.emitSeam(Nelson::NFlow::Rk4Gather, "nf_x0");
            lines.push_back(rhsCall(ru, ru ? "    " : "  ", "t", "nf_x0", "nf_ks", false,
                ru ? "Some(&mut nf_g0[..])" : "nf_g0"));
        }
        // The restart budget (1000 roots per communication step, far above any
        // physical model) never truncates the interval: once exhausted, the rest is
        // integrated in one plain sub-step without root checks. The stepper always
        // reaches nf_tend, so a chattering surface cannot hang it.
        if (ru) {
            lines.push_back("    let mut nf_tc: f64 = t;");
            lines.push_back("    let nf_tend: f64 = t + dt;");
            lines.push_back("    let mut nf_rst = 0;");
            lines.push_back("    let mut nf_rchk = true;");
            lines.push_back("    let mut nf_rgot = true;");
            lines.push_back("    while nf_tc < nf_tend - 1e-12 * dt {");
            lines.push_back("        let nf_h = nf_tend - nf_tc;");
            lines.push_back("        if nf_rchk {");
            lines.push_back("            if !nf_rgot { nflow_rhs(s, inp, out, nf_tc, &nf_x0, &mut "
                            "nf_ks, false, Some(&mut nf_g0[..])); }");
            lines.push_back("            nf_rgot = false;");
            lines.push_back(
                "            nflow_rk4sub(s, inp, out, nf_tc, nf_h, &nf_x0, &mut nf_x1);");
            lines.push_back("            nflow_rhs(s, inp, out, nf_tc + nf_h, &nf_x1, &mut nf_ks, "
                            "false, Some(&mut nf_g1[..]));");
            emitCrossingScanAndBisect(spec, "            ", "nf_tc", "nf_h", "nf_x0", "nf_x1",
                "nf_tc = 0.5 * (nf_lo + nf_hi); nf_rst += 1; if nf_rst >= 1000 { nf_rchk = false; "
                "}");
            lines.push_back("        } else {");
            lines.push_back(
                "            nflow_rk4sub(s, inp, out, nf_tc, nf_h, &nf_x0, &mut nf_x1);");
            lines.push_back("        }");
            lines.push_back("        for nf_i in 0.." + N + " { nf_x0[nf_i] = nf_x1[nf_i]; }");
            lines.push_back("        nf_tc = nf_tend;");
            lines.push_back("    }");
        } else {
            lines.push_back("  double nf_tc = t;");
            lines.push_back("  const double nf_tend = t + dt;");
            lines.push_back("  int nf_rst = 0;");
            lines.push_back("  int nf_rchk = 1;");
            lines.push_back("  int nf_rgot = 1;");
            lines.push_back("  while (nf_tc < nf_tend - 1e-12 * dt) {");
            lines.push_back("    const double nf_h = nf_tend - nf_tc;");
            lines.push_back("    if (nf_rchk) {");
            lines.push_back("      if (!nf_rgot) nflow_rhs(s, in, (ModelOutput*)0, nf_tc, nf_x0, "
                            "nf_ks, nf_g0);");
            lines.push_back("      nf_rgot = 0;");
            lines.push_back("      nflow_rk4sub(s, in, nf_tc, nf_h, nf_x0, nf_x1);");
            lines.push_back(
                "      nflow_rhs(s, in, (ModelOutput*)0, nf_tc + nf_h, nf_x1, nf_ks, nf_g1);");
            emitCrossingScanAndBisect(spec, "      ", "nf_tc", "nf_h", "nf_x0", "nf_x1",
                "nf_tc = 0.5 * (nf_lo + nf_hi); if (++nf_rst >= 1000) nf_rchk = 0;");
            lines.push_back("    } else {");
            lines.push_back("      nflow_rk4sub(s, in, nf_tc, nf_h, nf_x0, nf_x1);");
            lines.push_back("    }");
            lines.push_back(
                "    for (int nf_i = 0; nf_i < " + N + "; ++nf_i) nf_x0[nf_i] = nf_x1[nf_i];");
            lines.push_back("    nf_tc = nf_tend;");
            lines.push_back("  }");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Scatter, "nf_x0");
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
            lines.push_back("#[allow(clippy::needless_range_loop)]");
            lines.push_back("pub fn nflow_step(s: &mut ModelState,");
            lines.push_back("                  inp: &ModelInput,");
            lines.push_back("                  out: &mut ModelOutput,");
            lines.push_back("                  t: f64) {");
            lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
            lines.push_back("    let nf_rtol: f64 = " + spec.rtolLiteral + ";");
            lines.push_back("    let nf_atol: f64 = " + spec.atolLiteral + ";");
            lines.push_back("    let nf_hmax: f64 = " + spec.hmaxLiteral + ";");
            for (const char* v : { "nf_x0", "nf_x", "nf_xt", "nf_x5", "nf_xsv", "nf_ks", "nf_k1",
                     "nf_k2", "nf_k3", "nf_k4", "nf_k5", "nf_k6", "nf_k7" }) {
                lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
            }
            lines.push_back("    let mut nf_g0 = [0.0f64; " + NG + "];");
            lines.push_back("    let mut nf_g1 = [0.0f64; " + NG + "];");
        } else {
            lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                            "out, double t) {");
            lines.push_back("  const double dt = " + spec.dtLiteral + ";");
            lines.push_back("  const double nf_rtol = " + spec.rtolLiteral + ";");
            lines.push_back("  const double nf_atol = " + spec.atolLiteral + ";");
            lines.push_back("  const double nf_hmax = " + spec.hmaxLiteral + ";");
            lines.push_back("  double nf_x0[" + N + "], nf_x[" + N + "], nf_xt[" + N + "], nf_x5["
                + N + "], nf_xsv[" + N + "], nf_ks[" + N + "];");
            lines.push_back("  double nf_k1[" + N + "], nf_k2[" + N + "], nf_k3[" + N + "], nf_k4["
                + N + "], nf_k5[" + N + "], nf_k6[" + N + "], nf_k7[" + N + "];");
            lines.push_back("  double nf_g0[" + NG + "], nf_g1[" + NG + "];");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Gather, "nf_x0");
        lines.push_back(rhsCall(ru, ind, "t", "nf_x0", "nf_k1", true, gNull(ru)));
        lines.push_back(forAssign(ru, ind, N, "nf_x", "nf_x0[nf_i]"));
        if (ru) {
            lines.push_back("    nflow_rhs(s, inp, out, t, &nf_x, &mut nf_ks, false, Some(&mut "
                            "nf_g0));");
            lines.push_back("    let mut nf_tc: f64 = t;");
            lines.push_back("    let nf_tend: f64 = t + dt;");
            lines.push_back("    let mut nf_h: f64 = dt;");
            lines.push_back("    if nf_hmax > 0.0 && nf_h > nf_hmax { nf_h = nf_hmax; }");
            lines.push_back("    let mut nf_rooted = false;");
            lines.push_back("    let mut nf_rst = 0;");
            lines.push_back("    let mut nf_rchk = true;");
            lines.push_back("    while nf_tc < nf_tend - 1e-12 * dt {");
            lines.push_back("        if nf_h > nf_tend - nf_tc { nf_h = nf_tend - nf_tc; }");
            lines.push_back("        if nf_hmax > 0.0 && nf_h > nf_hmax { nf_h = nf_hmax; }");
            lines.push_back("        for _ in 0..50 {");
        } else {
            lines.push_back("  nflow_rhs(s, in, (ModelOutput*)0, t, nf_x, nf_ks, nf_g0);");
            lines.push_back("  double nf_tc = t;");
            lines.push_back("  const double nf_tend = t + dt;");
            lines.push_back("  double nf_h = dt;");
            lines.push_back("  if (nf_hmax > 0.0 && nf_h > nf_hmax) nf_h = nf_hmax;");
            lines.push_back("  int nf_rooted = 0;");
            lines.push_back("  int nf_rst = 0;");
            lines.push_back("  int nf_rchk = 1;");
            lines.push_back("  while (nf_tc < nf_tend - 1e-12 * dt) {");
            lines.push_back("    if (nf_h > nf_tend - nf_tc) nf_h = nf_tend - nf_tc;");
            lines.push_back("    if (nf_hmax > 0.0 && nf_h > nf_hmax) nf_h = nf_hmax;");
            lines.push_back("    for (int nf_att = 0; nf_att < 50; ++nf_att) {");
        }
        const std::string errExpr = emitDp45Stages(spec, ind3, gNull(ru));
        if (ru) {
            lines.push_back("            let mut nf_acc: f64 = 0.0;");
            lines.push_back("            for nf_i in 0.." + N + " {");
            lines.push_back("                let nf_e = " + errExpr + ";");
            lines.push_back("                let nf_sc = nf_atol + nf_rtol * "
                            "libm::fmax(libm::fabs(nf_x[nf_i]), libm::fabs(nf_x5[nf_i]));");
            lines.push_back(
                "                let nf_r = if nf_sc > 0.0 { nf_e / nf_sc } else { 0.0 };");
            lines.push_back("                nf_acc += nf_r * nf_r;");
            lines.push_back("            }");
            lines.push_back("            let nf_err = libm::sqrt(nf_acc / (" + N + " as f64));");
            lines.push_back("            if nf_err <= 1.0 || nf_h <= 1e-13 {");
            lines.push_back("                if nf_rchk {");
            lines.push_back(
                "                    for nf_i in 0.." + N + " { nf_xsv[nf_i] = nf_x[nf_i]; }");
            lines.push_back("                    nflow_rhs(s, inp, out, nf_tc + nf_h, &nf_x5, &mut "
                            "nf_ks, false, Some(&mut nf_g1[..]));");
            emitCrossingScanAndBisect(spec, "                    ", "nf_tc", "nf_h", "nf_xsv",
                "nf_xt",
                "nf_tc = 0.5 * (nf_lo + nf_hi); nf_rst += 1; if nf_rst >= 1000 { nf_rchk = false; "
                "} "
                "for nf_i in 0.."
                    + N
                    + " { nf_x[nf_i] = nf_xsv[nf_i]; } nflow_rhs(s, inp, out, nf_tc, &nf_x, &mut "
                      "nf_ks, false, Some(&mut nf_g0[..]));",
                "nf_rooted = true; break;");
            lines.push_back(
                "                    for nf_i in 0.." + NG + " { nf_g0[nf_i] = nf_g1[nf_i]; }");
            lines.push_back("                }");
            lines.push_back("                nf_tc += nf_h;");
            lines.push_back(
                "                for nf_i in 0.." + N + " { nf_x[nf_i] = nf_x5[nf_i]; }");
            lines.push_back("                let mut nf_grow = 0.9 * libm::pow(libm::fmax(nf_err, "
                            "1e-10), -0.2);");
            lines.push_back("                if nf_grow < 0.2 { nf_grow = 0.2; }");
            lines.push_back("                if nf_grow > 5.0 { nf_grow = 5.0; }");
            lines.push_back("                nf_h *= nf_grow;");
            lines.push_back("                break;");
            lines.push_back("            } else {");
            lines.push_back("                let mut nf_fac = 0.9 * libm::pow(nf_err, -0.2);");
            lines.push_back("                if nf_fac < 0.2 { nf_fac = 0.2; }");
            lines.push_back("                if nf_fac > 1.0 { nf_fac = 1.0; }");
            lines.push_back("                nf_h *= nf_fac;");
            lines.push_back(
                "                if nf_hmax > 0.0 && nf_h > nf_hmax { nf_h = nf_hmax; }");
            lines.push_back("            }");
            lines.push_back("        }");
            lines.push_back("        if nf_rooted { nf_rooted = false; continue; }");
            lines.push_back("    }");
        } else {
            lines.push_back("      double nf_acc = 0.0;");
            lines.push_back("      for (int nf_i = 0; nf_i < " + N + "; ++nf_i) {");
            lines.push_back("        double nf_e = " + errExpr + ";");
            lines.push_back("        double nf_xa = nf_x[nf_i] < 0.0 ? -nf_x[nf_i] : nf_x[nf_i];");
            lines.push_back(
                "        double nf_x5a = nf_x5[nf_i] < 0.0 ? -nf_x5[nf_i] : nf_x5[nf_i];");
            lines.push_back(
                "        double nf_sc = nf_atol + nf_rtol * (nf_xa > nf_x5a ? nf_xa : nf_x5a);");
            lines.push_back("        double nf_r = (nf_sc > 0.0) ? nf_e / nf_sc : 0.0;");
            lines.push_back("        nf_acc += nf_r * nf_r;");
            lines.push_back("      }");
            lines.push_back("      double nf_err = sqrt(nf_acc / (double)" + N + ");");
            lines.push_back("      if (nf_err <= 1.0 || nf_h <= 1e-13) {");
            lines.push_back("        if (nf_rchk) {");
            lines.push_back("          for (int nf_i = 0; nf_i < " + N
                + "; ++nf_i) nf_xsv[nf_i] = nf_x[nf_i];");
            lines.push_back(
                "          nflow_rhs(s, in, (ModelOutput*)0, nf_tc + nf_h, nf_x5, nf_ks, nf_g1);");
            emitCrossingScanAndBisect(spec, "          ", "nf_tc", "nf_h", "nf_xsv", "nf_xt",
                "nf_tc = 0.5 * (nf_lo + nf_hi); if (++nf_rst >= 1000) nf_rchk = 0; "
                "for (int nf_i = 0; nf_i < "
                    + N
                    + "; ++nf_i) nf_x[nf_i] = nf_xsv[nf_i]; nflow_rhs(s, in, (ModelOutput*)0, "
                      "nf_tc, "
                      "nf_x, nf_ks, nf_g0);",
                "nf_rooted = 1; break;");
            lines.push_back("          for (int nf_i = 0; nf_i < " + NG
                + "; ++nf_i) nf_g0[nf_i] = nf_g1[nf_i];");
            lines.push_back("        }");
            lines.push_back("        nf_tc += nf_h;");
            lines.push_back(
                "        for (int nf_i = 0; nf_i < " + N + "; ++nf_i) nf_x[nf_i] = nf_x5[nf_i];");
            lines.push_back(
                "        double nf_grow = 0.9 * pow(nf_err > 1e-10 ? nf_err : 1e-10, -0.2);");
            lines.push_back("        if (nf_grow < 0.2) nf_grow = 0.2;");
            lines.push_back("        if (nf_grow > 5.0) nf_grow = 5.0;");
            lines.push_back("        nf_h *= nf_grow;");
            lines.push_back("        break;");
            lines.push_back("      } else {");
            lines.push_back("        double nf_fac = 0.9 * pow(nf_err, -0.2);");
            lines.push_back("        if (nf_fac < 0.2) nf_fac = 0.2;");
            lines.push_back("        if (nf_fac > 1.0) nf_fac = 1.0;");
            lines.push_back("        nf_h *= nf_fac;");
            lines.push_back("        if (nf_hmax > 0.0 && nf_h > nf_hmax) nf_h = nf_hmax;");
            lines.push_back("      }");
            lines.push_back("    }");
            lines.push_back("    if (nf_rooted) { nf_rooted = 0; continue; }");
            lines.push_back("  }");
        }
        spec.emitSeam(Nelson::NFlow::Rk4Scatter, "nf_x");
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
        lines.push_back("#[allow(clippy::needless_range_loop)]");
        lines.push_back("pub fn nflow_step(s: &mut ModelState,");
        lines.push_back("                  inp: &ModelInput,");
        lines.push_back("                  out: &mut ModelOutput,");
        lines.push_back("                  t: f64) {");
        lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
        for (const char* v : { "nf_x0", "nf_k1", "nf_k2", "nf_k3", "nf_k4", "nf_xs", "nf_x1" }) {
            lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
        }
    } else {
        lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                        "out, double t) {");
        lines.push_back("  const double dt = " + spec.dtLiteral + ";");
        lines.push_back("  double nf_x0[" + N + "], nf_k1[" + N + "], nf_k2[" + N + "], nf_k3[" + N
            + "], nf_k4[" + N + "], nf_xs[" + N + "], nf_x1[" + N + "];");
    }
    spec.emitSeam(Nelson::NFlow::Rk4Gather, "nf_x0");
    lines.push_back(rhsCall(ru, ind, "t", "nf_x0", "nf_k1", true));
    if (spec.resetReseed) {
        // A reset fired by the reporting evaluation re-seeded its members' state
        // fields. Pick those up and evaluate the first stage again, so the step starts
        // from the new initial value and not from the discarded one.
        spec.emitSeam(Nelson::NFlow::Rk4Gather, "nf_x0");
        lines.push_back(rhsCall(ru, ind, "t", "nf_x0", "nf_k1", false));
    }
    lines.push_back(forAssign(ru, ind, N, "nf_xs", "nf_x0[nf_i] + 0.5 * dt * nf_k1[nf_i]"));
    lines.push_back(rhsCall(ru, ind, "t + 0.5 * dt", "nf_xs", "nf_k2", false));
    lines.push_back(forAssign(ru, ind, N, "nf_xs", "nf_x0[nf_i] + 0.5 * dt * nf_k2[nf_i]"));
    lines.push_back(rhsCall(ru, ind, "t + 0.5 * dt", "nf_xs", "nf_k3", false));
    lines.push_back(forAssign(ru, ind, N, "nf_xs", "nf_x0[nf_i] + dt * nf_k3[nf_i]"));
    lines.push_back(rhsCall(ru, ind, "t + dt", "nf_xs", "nf_k4", false));
    lines.push_back(forAssign(ru, ind, N, "nf_x1",
        "nf_x0[nf_i] + (dt / 6.0) * (nf_k1[nf_i] + 2.0 * nf_k2[nf_i] + 2.0 * nf_k3[nf_i] + "
        "nf_k4[nf_i])"));
    spec.emitSeam(Nelson::NFlow::Rk4Scatter, "nf_x1");
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
        lines.push_back("#[allow(clippy::needless_range_loop)]");
        lines.push_back("pub fn nflow_step(s: &mut ModelState,");
        lines.push_back("                  inp: &ModelInput,");
        lines.push_back("                  out: &mut ModelOutput,");
        lines.push_back("                  t: f64) {");
        lines.push_back("    let dt: f64 = " + spec.dtLiteral + ";");
        lines.push_back("    let nf_rtol: f64 = " + spec.rtolLiteral + ";");
        lines.push_back("    let nf_atol: f64 = " + spec.atolLiteral + ";");
        lines.push_back("    let nf_hmax: f64 = " + spec.hmaxLiteral + ";");
        for (const char* v : { "nf_x0", "nf_x", "nf_xt", "nf_x5", "nf_k1", "nf_k2", "nf_k3",
                 "nf_k4", "nf_k5", "nf_k6", "nf_k7" }) {
            lines.push_back("    let mut " + std::string(v) + " = [0.0f64; " + N + "];");
        }
    } else {
        lines.push_back("void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* "
                        "out, double t) {");
        lines.push_back("  const double dt = " + spec.dtLiteral + ";");
        lines.push_back("  const double nf_rtol = " + spec.rtolLiteral + ";");
        lines.push_back("  const double nf_atol = " + spec.atolLiteral + ";");
        lines.push_back("  const double nf_hmax = " + spec.hmaxLiteral + ";");
        lines.push_back(
            "  double nf_x0[" + N + "], nf_x[" + N + "], nf_xt[" + N + "], nf_x5[" + N + "];");
        lines.push_back("  double nf_k1[" + N + "], nf_k2[" + N + "], nf_k3[" + N + "], nf_k4[" + N
            + "], nf_k5[" + N + "], nf_k6[" + N + "], nf_k7[" + N + "];");
    }
    spec.emitSeam(Nelson::NFlow::Rk4Gather, "nf_x0");
    // Report the pre-step output at t (report-then-advance, like ode4).
    lines.push_back(rhsCall(ru, ind, "t", "nf_x0", "nf_k1", true));
    lines.push_back(forAssign(ru, ind, N, "nf_x", "nf_x0[nf_i]"));
    if (ru) {
        lines.push_back("    let mut nf_tc: f64 = t;");
        lines.push_back("    let nf_tend: f64 = t + dt;");
        lines.push_back("    let mut nf_h: f64 = dt;");
        lines.push_back("    if nf_hmax > 0.0 && nf_h > nf_hmax { nf_h = nf_hmax; }");
        lines.push_back("    while nf_tc < nf_tend - 1e-12 * dt {");
        lines.push_back("        if nf_h > nf_tend - nf_tc { nf_h = nf_tend - nf_tc; }");
        lines.push_back("        if nf_hmax > 0.0 && nf_h > nf_hmax { nf_h = nf_hmax; }");
        lines.push_back("        for _ in 0..50 {");
    } else {
        lines.push_back("  double nf_tc = t;");
        lines.push_back("  const double nf_tend = t + dt;");
        lines.push_back("  double nf_h = dt;");
        lines.push_back("  if (nf_hmax > 0.0 && nf_h > nf_hmax) nf_h = nf_hmax;");
        lines.push_back("  while (nf_tc < nf_tend - 1e-12 * dt) {");
        lines.push_back("    if (nf_h > nf_tend - nf_tc) nf_h = nf_tend - nf_tc;");
        lines.push_back("    if (nf_hmax > 0.0 && nf_h > nf_hmax) nf_h = nf_hmax;");
        lines.push_back("    for (int nf_att = 0; nf_att < 50; ++nf_att) {");
    }
    const std::string errExpr = emitDp45Stages(spec, ind3, std::string());
    if (ru) {
        lines.push_back("            let mut nf_acc: f64 = 0.0;");
        lines.push_back("            for nf_i in 0.." + N + " {");
        lines.push_back("                let nf_e = " + errExpr + ";");
        lines.push_back("                let nf_sc = nf_atol + nf_rtol * "
                        "libm::fmax(libm::fabs(nf_x[nf_i]), libm::fabs(nf_x5[nf_i]));");
        lines.push_back("                let nf_r = if nf_sc > 0.0 { nf_e / nf_sc } else { 0.0 };");
        lines.push_back("                nf_acc += nf_r * nf_r;");
        lines.push_back("            }");
        lines.push_back("            let nf_err = libm::sqrt(nf_acc / (" + N + " as f64));");
        lines.push_back("            if nf_err <= 1.0 || nf_h <= 1e-13 {");
        lines.push_back("                nf_tc += nf_h;");
        lines.push_back("                for nf_i in 0.." + N + " { nf_x[nf_i] = nf_x5[nf_i]; }");
        lines.push_back("                let mut nf_grow = 0.9 * libm::pow(libm::fmax(nf_err, "
                        "1e-10), -0.2);");
        lines.push_back("                if nf_grow < 0.2 { nf_grow = 0.2; }");
        lines.push_back("                if nf_grow > 5.0 { nf_grow = 5.0; }");
        lines.push_back("                nf_h *= nf_grow;");
        lines.push_back("                break;");
        lines.push_back("            } else {");
        lines.push_back("                let mut nf_fac = 0.9 * libm::pow(nf_err, -0.2);");
        lines.push_back("                if nf_fac < 0.2 { nf_fac = 0.2; }");
        lines.push_back("                if nf_fac > 1.0 { nf_fac = 1.0; }");
        lines.push_back("                nf_h *= nf_fac;");
        lines.push_back("                if nf_hmax > 0.0 && nf_h > nf_hmax { nf_h = nf_hmax; }");
        lines.push_back("            }");
        lines.push_back("        }");
        lines.push_back("    }");
    } else {
        lines.push_back("      double nf_acc = 0.0;");
        lines.push_back("      for (int nf_i = 0; nf_i < " + N + "; ++nf_i) {");
        lines.push_back("        double nf_e = " + errExpr + ";");
        lines.push_back("        double nf_xa = nf_x[nf_i] < 0.0 ? -nf_x[nf_i] : nf_x[nf_i];");
        lines.push_back("        double nf_x5a = nf_x5[nf_i] < 0.0 ? -nf_x5[nf_i] : nf_x5[nf_i];");
        lines.push_back(
            "        double nf_sc = nf_atol + nf_rtol * (nf_xa > nf_x5a ? nf_xa : nf_x5a);");
        lines.push_back("        double nf_r = (nf_sc > 0.0) ? nf_e / nf_sc : 0.0;");
        lines.push_back("        nf_acc += nf_r * nf_r;");
        lines.push_back("      }");
        lines.push_back("      double nf_err = sqrt(nf_acc / (double)" + N + ");");
        lines.push_back("      if (nf_err <= 1.0 || nf_h <= 1e-13) {");
        lines.push_back("        nf_tc += nf_h;");
        lines.push_back(
            "        for (int nf_i = 0; nf_i < " + N + "; ++nf_i) nf_x[nf_i] = nf_x5[nf_i];");
        lines.push_back(
            "        double nf_grow = 0.9 * pow(nf_err > 1e-10 ? nf_err : 1e-10, -0.2);");
        lines.push_back("        if (nf_grow < 0.2) nf_grow = 0.2;");
        lines.push_back("        if (nf_grow > 5.0) nf_grow = 5.0;");
        lines.push_back("        nf_h *= nf_grow;");
        lines.push_back("        break;");
        lines.push_back("      } else {");
        lines.push_back("        double nf_fac = 0.9 * pow(nf_err, -0.2);");
        lines.push_back("        if (nf_fac < 0.2) nf_fac = 0.2;");
        lines.push_back("        if (nf_fac > 1.0) nf_fac = 1.0;");
        lines.push_back("        nf_h *= nf_fac;");
        lines.push_back("        if (nf_hmax > 0.0 && nf_h > nf_hmax) nf_h = nf_hmax;");
        lines.push_back("      }");
        lines.push_back("    }");
        lines.push_back("  }");
    }
    spec.emitSeam(Nelson::NFlow::Rk4Scatter, "nf_x");
    // Post-step clamp for bounded states (no-op for the others).
    spec.emitSeam(Nelson::NFlow::Rk4Clamp, "");
    lines.push_back("}");
}
//=============================================================================
} // namespace nflow
//=============================================================================
