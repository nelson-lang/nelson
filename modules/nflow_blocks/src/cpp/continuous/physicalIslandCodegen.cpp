//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// physicalIsland code generation: embedded-Newton lowering for a nonlinear
// island. The generated code assembles the same DAE residual as
// handlePhysicalIsland and solves a backward-Euler step each time step: the
// differential rows use x'_i = (x_i - x_prev_i)/dt, the algebraic rows the raw
// residual. The coupled system is Newton-solved (finite-difference Jacobian,
// Gaussian elimination with partial pivoting). A purely algebraic island
// reduces to F(x)=0 and matches the IDAS/dae simulation; an island with a
// differential C/L state is a fixed-step approximation of the dae run.
//=============================================================================
#include "physicalIslandShared.hpp"
#include "BlockRegistry.hpp"
#include "continuous_blocks.hpp"
#include <functional>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    using namespace physisland;
    //=============================================================================
    namespace {
        // C expression for a node potential inside the residual helper (ground and
        // stateless nodes are 0; others read the packed unknown x[eIndex]).
        static std::string
        islandEpot(const IslandLayout& L, int node)
        {
            if (node <= 0 || node >= L.numNodes) {
                return "0.0";
            }
            const int si = L.eIndex[static_cast<size_t>(node)];
            return si < 0 ? std::string("0.0") : ("x[" + std::to_string(si) + "]");
        }
        //=============================================================================
        // Emit the residual helper body. It first assembles the raw DAE output G
        // (identical to handlePhysicalIsland's `xdot`: the derivative value for a
        // differential state, the constraint residual for an algebraic one), then
        // forms the backward-Euler residual F: differential rows (id==1) become
        // (x_i - xprev_i)/dt - G_i, algebraic rows (id==0) stay G_i. For a purely
        // algebraic island every row is G_i and xprev/dt are unused.
        static void
        emitIslandResidualBody(const IslandLayout& L, const std::function<std::string(double)>& fmt,
            const std::function<void(const std::string&)>& line)
        {
            const int N = L.numStates;
            line("  double G[" + std::to_string(N) + "];");
            line("  for (int nf_i = 0; nf_i < " + std::to_string(N) + "; ++nf_i) G[nf_i] = 0.0;");
            line("  double kcl[" + std::to_string(L.numNodes) + "];");
            line("  for (int nf_n = 0; nf_n < " + std::to_string(L.numNodes)
                + "; ++nf_n) kcl[nf_n] = 0.0;");
            for (const IslandComp& c : L.comps) {
                const std::string ea = islandEpot(L, c.a);
                const std::string eb = islandEpot(L, c.b);
                const std::string a = std::to_string(c.a);
                const std::string b = std::to_string(c.b);
                const std::string si = std::to_string(c.stateIndex);
                const std::string si1 = std::to_string(c.stateIndex + 1);
                switch (c.kind) {
                case Kind::Resistor:
                    line("  { double nf_i = ((" + ea + ") - (" + eb + ")) / " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += nf_i; kcl[" + b + "] -= nf_i; }");
                    break;
                case Kind::Conductor:
                    line("  { double nf_i = ((" + ea + ") - (" + eb + ")) * " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += nf_i; kcl[" + b + "] -= nf_i; }");
                    break;
                case Kind::Diode:
                    line("  { double nf_vd = (" + ea + ") - (" + eb + ");"
                        + " double nf_arg = nf_vd / " + fmt(c.vt)
                        + "; if (nf_arg > 40.0) nf_arg = 40.0;" + " double nf_i = " + fmt(c.value)
                        + " * (exp(nf_arg) - 1.0);" + " kcl[" + a + "] += nf_i; kcl[" + b
                        + "] -= nf_i; }");
                    break;
                case Kind::ZDiode:
                    line("  { double nf_vd = (" + ea + ") - (" + eb + ");"
                        + " double nf_af = nf_vd / " + fmt(c.vt)
                        + "; if (nf_af > 40.0) nf_af = 40.0;" + " double nf_ar = -(nf_vd + "
                        + fmt(c.value2) + ") / " + fmt(c.vt) + "; if (nf_ar > 40.0) nf_ar = 40.0;"
                        + " double nf_i = " + fmt(c.value) + " * (exp(nf_af) - 1.0) - "
                        + fmt(c.value) + " * (exp(nf_ar) - 1.0);" + " kcl[" + a + "] += nf_i; kcl["
                        + b + "] -= nf_i; }");
                    break;
                case Kind::IdealDiode:
                    // Hybrid branch on its state s, same modes as the native residual.
                    line("  { double nf_s = x[" + si + "]; int nf_off = (nf_s < 0.0);"
                        + " double nf_vs = " + fmt(c.value3) + " + nf_s * (nf_off ? 1.0 : "
                        + fmt(c.value) + ");" + " double nf_is = (nf_off ? " + fmt(c.value2)
                        + " * nf_s : nf_s) + " + fmt(c.value2) + " * " + fmt(c.value3) + ";" + " G["
                        + si + "] = ((" + ea + ") - (" + eb + ")) - nf_vs;" + " kcl[" + a
                        + "] += nf_is; kcl[" + b + "] -= nf_is; }");
                    break;
                case Kind::Npn:
                case Kind::Pnp: {
                    // Ebers-Moll BJT (a=C, b=E, c=B), s = +1 NPN / -1 PNP.
                    const std::string ec = islandEpot(L, c.c);
                    const std::string cc = std::to_string(c.c);
                    const std::string sgn = (c.kind == Kind::Npn) ? "1.0" : "-1.0";
                    line("  { double nf_s = " + sgn + ";" + " double nf_abe = nf_s * ((" + ec
                        + ") - (" + eb + ")) / " + fmt(c.vt) + ";" + " double nf_abc = nf_s * (("
                        + ec + ") - (" + ea + ")) / " + fmt(c.vt) + ";"
                        + " if (nf_abe > 40.0) nf_abe = 40.0; if (nf_abc > 40.0) nf_abc = 40.0;"
                        + " double nf_ef = exp(nf_abe) - 1.0; double nf_er = exp(nf_abc) - 1.0;"
                        + " double nf_ic = nf_s * (" + fmt(c.value) + " * (nf_ef - nf_er) - "
                        + fmt(c.value / c.value3) + " * nf_er);" + " double nf_ib = nf_s * ("
                        + fmt(c.value / c.value2) + " * nf_ef + " + fmt(c.value / c.value3)
                        + " * nf_er);" + " kcl[" + a + "] += nf_ic; kcl[" + cc + "] += nf_ib;"
                        + " kcl[" + b + "] -= (nf_ic + nf_ib); }");
                    break;
                }
                case Kind::Nmos:
                case Kind::Pmos: {
                    // Square-law MOSFET (a=D, b=S, c=G), s = +1 NMOS / -1 PMOS.
                    const std::string ec = islandEpot(L, c.c);
                    const std::string sgn = (c.kind == Kind::Nmos) ? "1.0" : "-1.0";
                    line("  { double nf_s = " + sgn + ";" + " double nf_vgs = nf_s * ((" + ec
                        + ") - (" + eb + "));" + " double nf_vds = nf_s * ((" + ea + ") - (" + eb
                        + "));" + " double nf_vov = nf_vgs - " + fmt(c.value2)
                        + "; double nf_id = 0.0;"
                        + " if (nf_vov > 0.0) { nf_id = (nf_vds < nf_vov) ? " + fmt(c.value)
                        + " * (nf_vov * nf_vds - 0.5 * nf_vds * nf_vds) : 0.5 * " + fmt(c.value)
                        + " * nf_vov * nf_vov; }" + " kcl[" + a + "] += nf_s * nf_id; kcl[" + b
                        + "] -= nf_s * nf_id; }");
                    break;
                }
                case Kind::Radiation:
                    // Stefan-Boltzmann: q = Gr (T_a^4 - T_b^4), the node potentials
                    // being temperatures (thermal domain). Smooth nonlinear stamp.
                    line("  { double nf_ta = (" + ea + "); double nf_tb = (" + eb + ");"
                        + " double nf_q = " + fmt(c.value)
                        + " * (nf_ta * nf_ta * nf_ta * nf_ta - nf_tb * nf_tb * nf_tb * nf_tb);"
                        + " kcl[" + a + "] += nf_q; kcl[" + b + "] -= nf_q; }");
                    break;
                case Kind::Capacitor:
                    // state si = v_C (differential), si+1 = i_C (algebraic):
                    //   G[si] = i_C / C ; G[si+1] = v_C - (e_a - e_b) ; i_C into KCL.
                    line("  { double nf_vC = x[" + si + "]; double nf_iC = x[" + si1 + "];" + " G["
                        + si + "] = nf_iC / " + fmt(c.value) + ";" + " G[" + si1 + "] = nf_vC - (("
                        + ea + ") - (" + eb + "));" + " kcl[" + a + "] += nf_iC; kcl[" + b
                        + "] -= nf_iC; }");
                    break;
                case Kind::Inductor:
                    // state si = i_L (differential): G[si] = v_L / L ; i_L into KCL.
                    line("  { double nf_iL = x[" + si + "];" + " G[" + si + "] = ((" + ea + ") - ("
                        + eb + ")) / " + fmt(c.value) + ";" + " kcl[" + a + "] += nf_iL; kcl[" + b
                        + "] -= nf_iL; }");
                    break;
                case Kind::VSource:
                case Kind::CurrentSensor: {
                    const std::string V
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line("  { double nf_ib = x[" + si + "];" + " kcl[" + a + "] += nf_ib; kcl[" + b
                        + "] -= nf_ib;" + " G[" + si + "] = ((" + ea + ") - (" + eb + ")) - (" + V
                        + "); }");
                    break;
                }
                case Kind::ISource: {
                    const std::string I
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line("  { double nf_i = " + I + "; kcl[" + a + "] += nf_i; kcl[" + b
                        + "] -= nf_i; }");
                    break;
                }
                case Kind::Vccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double nf_i = " + fmt(c.value) + " * ((" + ec + ") - (" + ed + "));"
                        + " kcl[" + a + "] += nf_i; kcl[" + b + "] -= nf_i; }");
                    break;
                }
                case Kind::Vcvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double nf_ib = x[" + si + "]; kcl[" + a + "] += nf_ib; kcl[" + b
                        + "] -= nf_ib; G[" + si + "] = ((" + ea + ") - (" + eb + ")) - "
                        + fmt(c.value) + " * ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::Ccvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double nf_io = x[" + si + "]; double nf_is = x[" + si1 + "];"
                        + " kcl[" + a + "] += nf_io; kcl[" + b + "] -= nf_io;" + " kcl["
                        + std::to_string(c.c) + "] += nf_is; kcl[" + std::to_string(c.d)
                        + "] -= nf_is; G[" + si + "] = ((" + ea + ") - (" + eb + ")) - "
                        + fmt(c.value) + " * nf_is; G[" + si1 + "] = ((" + ec + ") - (" + ed
                        + ")); }");
                    break;
                }
                case Kind::Cccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double nf_is = x[" + si + "]; double nf_io = " + fmt(c.value)
                        + " * nf_is;" + " kcl[" + a + "] += nf_io; kcl[" + b + "] -= nf_io;"
                        + " kcl[" + std::to_string(c.c) + "] += nf_is; kcl[" + std::to_string(c.d)
                        + "] -= nf_is; G[" + si + "] = ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::OpAmp:
                    line("  { double nf_io = x[" + si + "]; G[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + std::to_string(c.c) + "] += nf_io; }");
                    break;
                case Kind::Short:
                    line("  { double nf_ib = x[" + si + "]; G[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + a + "] += nf_ib; kcl[" + b + "] -= nf_ib; }");
                    break;
                case Kind::Idle:
                    line("  { double nf_ib = x[" + si + "]; G[" + si + "] = nf_ib;" + " kcl[" + a
                        + "] += nf_ib; kcl[" + b + "] -= nf_ib; }");
                    break;
                case Kind::Transformer: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double nf_i1 = x[" + si + "]; double nf_i2 = -" + fmt(c.value)
                        + " * nf_i1; G[" + si + "] = ((" + ea + ") - (" + eb + ")) - "
                        + fmt(c.value) + " * ((" + ec + ") - (" + ed + "));" + " kcl[" + a
                        + "] += nf_i1; kcl[" + b + "] -= nf_i1; kcl[" + std::to_string(c.c)
                        + "] += nf_i2; kcl[" + std::to_string(c.d) + "] -= nf_i2; }");
                    break;
                }
                case Kind::Gyrator: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double nf_v1 = (" + ea + ") - (" + eb + "); double nf_v2 = (" + ec
                        + ") - (" + ed + "); double nf_i1 = " + fmt(c.value2) + " * nf_v2;"
                        + " double nf_i2 = -" + fmt(c.value) + " * nf_v1;" + " kcl[" + a
                        + "] += nf_i1; kcl[" + b + "] -= nf_i1; kcl[" + std::to_string(c.c)
                        + "] += nf_i2; kcl[" + std::to_string(c.d) + "] -= nf_i2; }");
                    break;
                }
                case Kind::Switch: {
                    const std::string ctrl
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : std::string("1.0");
                    line("  { double nf_ib = x[" + si + "]; G[" + si + "] = (" + ctrl
                        + " > 0.5) ? ((" + ea + ") - (" + eb + ")) : nf_ib;" + " kcl[" + a
                        + "] += nf_ib; kcl[" + b + "] -= nf_ib; }");
                    break;
                }
                default:
                    break; // sensors contribute no residual; others filtered out
                }
            }
            for (int n = 1; n < L.numNodes; ++n) {
                const int si = L.eIndex[static_cast<size_t>(n)];
                if (si >= 0) {
                    line("  G[" + std::to_string(si) + "] = kcl[" + std::to_string(n) + "];");
                }
            }
            // Backward-Euler residual per state (id baked in at generation time).
            for (int i = 0; i < N; ++i) {
                const std::string is = std::to_string(i);
                if (i < static_cast<int>(L.id.size()) && L.id[static_cast<size_t>(i)] > 0.5) {
                    line("  F[" + is + "] = (x[" + is + "] - xprev[" + is + "]) / dt - G[" + is
                        + "];");
                } else {
                    line("  F[" + is + "] = G[" + is + "];");
                }
            }
        }
        //=============================================================================
        static void
        emitIslandStepC(const BlockCodegenArgs& a, const IslandLayout& L)
        {
            const std::string id = a.id;
            const int N = L.numStates;
            const int np = islandPortCount(L);
            const std::string Ns = std::to_string(N);
            const std::string xs = "s->isl_x_" + id;
            a.line("{");
            a.line("  double* nf_X = " + xs + ";");
            if (np > 0) {
                a.line("  double nf_u[" + std::to_string(np) + "];");
                for (int p = 0; p < np; ++p) {
                    const std::string e = (p < static_cast<int>(a.in.size()))
                        ? a.in[static_cast<size_t>(p)]
                        : "0.0";
                    a.line("  nf_u[" + std::to_string(p) + "] = " + e + ";");
                }
            } else {
                a.line("  double nf_u[1]; nf_u[0] = 0.0; (void)nf_u;");
            }
            // Backward-Euler: freeze the previous-step state for the implicit
            // residual (unused when the island is purely algebraic).
            a.line("  double nf_xprev[" + Ns + "]; for (int nf_i = 0; nf_i < " + Ns
                + "; ++nf_i) nf_xprev[nf_i] = nf_X[nf_i];");
            a.line("  for (int nf_it = 0; nf_it < 60; ++nf_it) {");
            a.line("    double nf_F[" + Ns + "]; nflow_isl_res_" + id
                + "(nf_X, nf_xprev, nf_u, dt, nf_F);");
            a.line("    double nf_nrm = 0.0; for (int nf_i = 0; nf_i < " + Ns
                + "; ++nf_i) nf_nrm += nf_F[nf_i] * nf_F[nf_i];");
            a.line("    if (nf_nrm < 1e-24) break;");
            a.line("    double nf_J[" + std::to_string(N * N) + "];");
            a.line("    for (int nf_j = 0; nf_j < " + Ns + "; ++nf_j) {");
            a.line("      double nf_xj = nf_X[nf_j]; double nf_h = 1e-7 * (nf_xj < 0 ? -nf_xj : "
                   "nf_xj) + "
                   "1e-9;");
            a.line("      nf_X[nf_j] = nf_xj + nf_h; double nf_Fp[" + Ns + "]; nflow_isl_res_" + id
                + "(nf_X, nf_xprev, nf_u, dt, nf_Fp); nf_X[nf_j] = nf_xj;");
            a.line("      for (int nf_i = 0; nf_i < " + Ns + "; ++nf_i) nf_J[nf_i * " + Ns
                + " + nf_j] = (nf_Fp[nf_i] - nf_F[nf_i]) / nf_h;");
            a.line("    }");
            a.line("    double nf_b[" + Ns + "]; for (int nf_i = 0; nf_i < " + Ns
                + "; ++nf_i) nf_b[nf_i] = -nf_F[nf_i];");
            a.line("    for (int nf_k = 0; nf_k < " + Ns + "; ++nf_k) {");
            a.line("      int nf_p = nf_k; double nf_mx = nf_J[nf_k * " + Ns + " + nf_k];"
                + " if (nf_mx < 0) nf_mx = -nf_mx;");
            a.line("      for (int nf_r = nf_k + 1; nf_r < " + Ns + "; ++nf_r) {"
                + " double nf_v = nf_J[nf_r * " + Ns + " + nf_k]; if (nf_v < 0) nf_v = -nf_v;"
                + " if (nf_v > nf_mx) { nf_mx = nf_v; nf_p = nf_r; } }");
            a.line("      if (nf_p != nf_k) { for (int nf_col = 0; nf_col < " + Ns + "; ++nf_col) {"
                + " double nf_t = nf_J[nf_k * " + Ns + " + nf_col]; nf_J[nf_k * " + Ns
                + " + nf_col] = nf_J[nf_p * " + Ns + " + nf_col]; nf_J[nf_p * " + Ns
                + " + nf_col] = nf_t; }"
                + " double nf_tb = nf_b[nf_k]; nf_b[nf_k] = nf_b[nf_p]; nf_b[nf_p] = nf_tb; }");
            a.line("      double nf_d = nf_J[nf_k * " + Ns
                + " + nf_k]; if (nf_d < 1e-30 && nf_d > -1e-30) nf_d = 1e-30;");
            a.line("      for (int nf_r = nf_k + 1; nf_r < " + Ns + "; ++nf_r) {"
                + " double nf_f = nf_J[nf_r * " + Ns + " + nf_k] / nf_d;"
                + " for (int nf_col = nf_k; nf_col < " + Ns + "; ++nf_col) nf_J[nf_r * " + Ns
                + " + nf_col] -= nf_f * nf_J[nf_k * " + Ns + " + nf_col];"
                + " nf_b[nf_r] -= nf_f * nf_b[nf_k]; }");
            a.line("    }");
            a.line("    for (int nf_i = " + std::to_string(N - 1) + "; nf_i >= 0; --nf_i) {"
                + " double nf_s = nf_b[nf_i];" + " for (int nf_col = nf_i + 1; nf_col < " + Ns
                + "; ++nf_col) nf_s -= nf_J[nf_i * " + Ns + " + nf_col] * nf_b[nf_col];"
                + " double nf_d = nf_J[nf_i * " + Ns
                + " + nf_i]; if (nf_d < 1e-30 && nf_d > -1e-30) nf_d = 1e-30;"
                + " nf_b[nf_i] = nf_s / nf_d; }");
            a.line("    for (int nf_i = 0; nf_i < " + Ns + "; ++nf_i) nf_X[nf_i] += nf_b[nf_i];");
            a.line("  }");
            // Sensor outputs. The node potential here reads the persistent state
            // array (isl_x_<id>), NOT the residual helper's `x` parameter.
            auto epotStep = [&](int node) -> std::string {
                if (node <= 0 || node >= L.numNodes) {
                    return "0.0";
                }
                const int si = L.eIndex[static_cast<size_t>(node)];
                return si < 0 ? std::string("0.0") : (xs + "[" + std::to_string(si) + "]");
            };
            for (const IslandComp& c : L.comps) {
                if (c.outPort < 0) {
                    continue;
                }
                const std::string ov = c.outPort > 0
                    ? ("out_" + id + "_p" + std::to_string(c.outPort))
                    : ("out_" + id);
                if (c.kind == Kind::VoltageSensor) {
                    a.line("  " + ov + " = (" + epotStep(c.a) + ") - (" + epotStep(c.b) + ");");
                } else if (c.kind == Kind::PotentialSensor) {
                    a.line("  " + ov + " = " + epotStep(c.a) + ";");
                } else if (c.kind == Kind::CurrentSensor) {
                    a.line("  " + ov + " = " + xs + "[" + std::to_string(c.stateIndex) + "];");
                }
            }
            a.line("}");
        }
        //=============================================================================
        // Rust mirror of the embedded-Newton island: same residual, backward Euler
        // and Newton solve as the C emitter, in no_std Rust (libm for exp/fabs).
        // Per-scalar state fields are gathered into a local array each step.
        static void
        emitIslandResidualBodyRust(const IslandLayout& L,
            const std::function<std::string(double)>& fmt,
            const std::function<void(const std::string&)>& line)
        {
            const int N = L.numStates;
            line("  let mut g = [0.0f64; " + std::to_string(N) + "];");
            line("  let mut kcl = [0.0f64; " + std::to_string(L.numNodes) + "];");
            for (const IslandComp& c : L.comps) {
                const std::string ea = islandEpot(L, c.a);
                const std::string eb = islandEpot(L, c.b);
                const std::string a = std::to_string(c.a);
                const std::string b = std::to_string(c.b);
                const std::string si = std::to_string(c.stateIndex);
                const std::string si1 = std::to_string(c.stateIndex + 1);
                switch (c.kind) {
                case Kind::Resistor:
                    line("  { let nf_i = ((" + ea + ") - (" + eb + ")) / " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += nf_i; kcl[" + b + "] -= nf_i; }");
                    break;
                case Kind::Conductor:
                    line("  { let nf_i = ((" + ea + ") - (" + eb + ")) * " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += nf_i; kcl[" + b + "] -= nf_i; }");
                    break;
                case Kind::Diode:
                    line("  { let nf_vd = (" + ea + ") - (" + eb + ");"
                        + " let mut nf_arg = nf_vd / " + fmt(c.vt)
                        + "; if nf_arg > 40.0 { nf_arg = 40.0; }" + " let nf_i = " + fmt(c.value)
                        + " * (libm::exp(nf_arg) - 1.0);" + " kcl[" + a + "] += nf_i; kcl[" + b
                        + "] -= nf_i; }");
                    break;
                case Kind::ZDiode:
                    line("  { let nf_vd = (" + ea + ") - (" + eb + ");"
                        + " let mut nf_af = nf_vd / " + fmt(c.vt)
                        + "; if nf_af > 40.0 { nf_af = 40.0; }" + " let mut nf_ar = -(nf_vd + "
                        + fmt(c.value2) + ") / " + fmt(c.vt) + "; if nf_ar > 40.0 { nf_ar = 40.0; }"
                        + " let nf_i = " + fmt(c.value) + " * (libm::exp(nf_af) - 1.0) - "
                        + fmt(c.value) + " * (libm::exp(nf_ar) - 1.0);" + " kcl[" + a
                        + "] += nf_i; kcl[" + b + "] -= nf_i; }");
                    break;
                case Kind::IdealDiode:
                    line("  { let nf_s = x[" + si + "]; let nf_off = nf_s < 0.0;"
                        + " let nf_vs = " + fmt(c.value3) + " + nf_s * (if nf_off { 1.0 } else { "
                        + fmt(c.value) + " });" + " let nf_is = (if nf_off { " + fmt(c.value2)
                        + " * nf_s } else { nf_s }) + " + fmt(c.value2) + " * " + fmt(c.value3)
                        + ";" + " g[" + si + "] = ((" + ea + ") - (" + eb + ")) - nf_vs;" + " kcl["
                        + a + "] += nf_is; kcl[" + b + "] -= nf_is; }");
                    break;
                case Kind::Capacitor:
                    line("  { let nf_vc = x[" + si + "]; let nf_ic = x[" + si1 + "];" + " g[" + si
                        + "] = nf_ic / " + fmt(c.value) + ";" + " g[" + si1 + "] = nf_vc - ((" + ea
                        + ") - (" + eb + "));" + " kcl[" + a + "] += nf_ic; kcl[" + b
                        + "] -= nf_ic; }");
                    break;
                case Kind::Inductor:
                    line("  { let nf_il = x[" + si + "];" + " g[" + si + "] = ((" + ea + ") - ("
                        + eb + ")) / " + fmt(c.value) + ";" + " kcl[" + a + "] += nf_il; kcl[" + b
                        + "] -= nf_il; }");
                    break;
                case Kind::VSource:
                case Kind::CurrentSensor: {
                    const std::string V
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line("  { let nf_ib = x[" + si + "];" + " kcl[" + a + "] += nf_ib; kcl[" + b
                        + "] -= nf_ib;" + " g[" + si + "] = ((" + ea + ") - (" + eb + ")) - (" + V
                        + "); }");
                    break;
                }
                case Kind::ISource: {
                    const std::string I
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line("  { let nf_i = " + I + "; kcl[" + a + "] += nf_i; kcl[" + b
                        + "] -= nf_i; }");
                    break;
                }
                case Kind::Npn:
                case Kind::Pnp: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string cc = std::to_string(c.c);
                    const std::string sgn = (c.kind == Kind::Npn) ? "1.0" : "-1.0";
                    line("  { let nf_s: f64 = " + sgn + ";" + " let mut nf_abe = nf_s * ((" + ec
                        + ") - (" + eb + ")) / " + fmt(c.vt) + ";" + " let mut nf_abc = nf_s * (("
                        + ec + ") - (" + ea + ")) / " + fmt(c.vt) + ";"
                        + " if nf_abe > 40.0 { nf_abe = 40.0; } if nf_abc > 40.0 { nf_abc = 40.0; }"
                        + " let nf_ef = libm::exp(nf_abe) - 1.0; let nf_er = libm::exp(nf_abc) - "
                          "1.0;"
                        + " let nf_ic = nf_s * (" + fmt(c.value) + " * (nf_ef - nf_er) - "
                        + fmt(c.value / c.value3) + " * nf_er);" + " let nf_ib = nf_s * ("
                        + fmt(c.value / c.value2) + " * nf_ef + " + fmt(c.value / c.value3)
                        + " * nf_er);" + " kcl[" + a + "] += nf_ic; kcl[" + cc + "] += nf_ib;"
                        + " kcl[" + b + "] -= (nf_ic + nf_ib); }");
                    break;
                }
                case Kind::Nmos:
                case Kind::Pmos: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string sgn = (c.kind == Kind::Nmos) ? "1.0" : "-1.0";
                    line("  { let nf_s: f64 = " + sgn + ";" + " let nf_vgs = nf_s * ((" + ec
                        + ") - (" + eb + "));" + " let nf_vds = nf_s * ((" + ea + ") - (" + eb
                        + "));" + " let nf_vov = nf_vgs - " + fmt(c.value2)
                        + "; let mut nf_id = 0.0;"
                        + " if nf_vov > 0.0 { nf_id = if nf_vds < nf_vov { " + fmt(c.value)
                        + " * (nf_vov * nf_vds - 0.5 * nf_vds * nf_vds) } else { 0.5 * "
                        + fmt(c.value) + " * nf_vov * nf_vov }; }" + " kcl[" + a
                        + "] += nf_s * nf_id; kcl[" + b + "] -= nf_s * nf_id; }");
                    break;
                }
                case Kind::Radiation:
                    line("  { let nf_ta = (" + ea + "); let nf_tb = (" + eb + ");"
                        + " let nf_q = " + fmt(c.value)
                        + " * (nf_ta * nf_ta * nf_ta * nf_ta - nf_tb * nf_tb * nf_tb * nf_tb);"
                        + " kcl[" + a + "] += nf_q; kcl[" + b + "] -= nf_q; }");
                    break;
                case Kind::Vccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let nf_i = " + fmt(c.value) + " * ((" + ec + ") - (" + ed + "));"
                        + " kcl[" + a + "] += nf_i; kcl[" + b + "] -= nf_i; }");
                    break;
                }
                case Kind::Vcvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let nf_ib = x[" + si + "]; kcl[" + a + "] += nf_ib; kcl[" + b
                        + "] -= nf_ib; g[" + si + "] = ((" + ea + ") - (" + eb + ")) - "
                        + fmt(c.value) + " * ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::Ccvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let nf_io = x[" + si + "]; let nf_is = x[" + si1 + "];" + " kcl[" + a
                        + "] += nf_io; kcl[" + b + "] -= nf_io;" + " kcl[" + std::to_string(c.c)
                        + "] += nf_is; kcl[" + std::to_string(c.d) + "] -= nf_is; g[" + si
                        + "] = ((" + ea + ") - (" + eb + ")) - " + fmt(c.value) + " * nf_is; g["
                        + si1 + "] = ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::Cccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let nf_is = x[" + si + "]; let nf_io = " + fmt(c.value) + " * nf_is;"
                        + " kcl[" + a + "] += nf_io; kcl[" + b + "] -= nf_io;" + " kcl["
                        + std::to_string(c.c) + "] += nf_is; kcl[" + std::to_string(c.d)
                        + "] -= nf_is; g[" + si + "] = ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::OpAmp:
                    line("  { let nf_io = x[" + si + "]; g[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + std::to_string(c.c) + "] += nf_io; }");
                    break;
                case Kind::Short:
                    line("  { let nf_ib = x[" + si + "]; g[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + a + "] += nf_ib; kcl[" + b + "] -= nf_ib; }");
                    break;
                case Kind::Idle:
                    line("  { let nf_ib = x[" + si + "]; g[" + si + "] = nf_ib;" + " kcl[" + a
                        + "] += nf_ib; kcl[" + b + "] -= nf_ib; }");
                    break;
                case Kind::Transformer: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let nf_i1 = x[" + si + "]; let nf_i2 = -" + fmt(c.value)
                        + " * nf_i1; g[" + si + "] = ((" + ea + ") - (" + eb + ")) - "
                        + fmt(c.value) + " * ((" + ec + ") - (" + ed + "));" + " kcl[" + a
                        + "] += nf_i1; kcl[" + b + "] -= nf_i1; kcl[" + std::to_string(c.c)
                        + "] += nf_i2; kcl[" + std::to_string(c.d) + "] -= nf_i2; }");
                    break;
                }
                case Kind::Gyrator: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let nf_v1 = (" + ea + ") - (" + eb + "); let nf_v2 = (" + ec + ") - ("
                        + ed + "); let nf_i1 = " + fmt(c.value2) + " * nf_v2;" + " let nf_i2 = -"
                        + fmt(c.value) + " * nf_v1;" + " kcl[" + a + "] += nf_i1; kcl[" + b
                        + "] -= nf_i1; kcl[" + std::to_string(c.c) + "] += nf_i2; kcl["
                        + std::to_string(c.d) + "] -= nf_i2; }");
                    break;
                }
                case Kind::Switch: {
                    const std::string ctrl
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : std::string("1.0");
                    line("  { let nf_ib = x[" + si + "]; g[" + si + "] = if " + ctrl + " > 0.5 { ("
                        + ea + ") - (" + eb + ") } else { nf_ib };" + " kcl[" + a
                        + "] += nf_ib; kcl[" + b + "] -= nf_ib; }");
                    break;
                }
                default:
                    break;
                }
            }
            for (int n = 1; n < L.numNodes; ++n) {
                const int si = L.eIndex[static_cast<size_t>(n)];
                if (si >= 0) {
                    line("  g[" + std::to_string(si) + "] = kcl[" + std::to_string(n) + "];");
                }
            }
            for (int i = 0; i < N; ++i) {
                const std::string is = std::to_string(i);
                if (i < static_cast<int>(L.id.size()) && L.id[static_cast<size_t>(i)] > 0.5) {
                    line("  fout[" + is + "] = (x[" + is + "] - xprev[" + is + "]) / dt - g[" + is
                        + "];");
                } else {
                    line("  fout[" + is + "] = g[" + is + "];");
                }
            }
        }
        //=============================================================================
        static void
        emitIslandStepRust(const BlockCodegenArgs& a, const IslandLayout& L)
        {
            const std::string id = a.id;
            const int N = L.numStates;
            const int np = islandPortCount(L);
            const std::string Ns = std::to_string(N);
            a.line("{");
            a.line("  let mut nf_x = [0.0f64; " + Ns + "];");
            for (int i = 0; i < N; ++i) {
                a.line("  nf_x[" + std::to_string(i) + "] = s.isl_x_" + id + "_" + std::to_string(i)
                    + ";");
            }
            if (np > 0) {
                a.line("  let mut nf_u = [0.0f64; " + std::to_string(np) + "];");
                for (int p = 0; p < np; ++p) {
                    const std::string e = (p < static_cast<int>(a.in.size()))
                        ? a.in[static_cast<size_t>(p)]
                        : "0.0";
                    a.line("  nf_u[" + std::to_string(p) + "] = " + e + ";");
                }
            } else {
                a.line("  let nf_u = [0.0f64; 1];");
            }
            a.line("  let mut nf_xprev = [0.0f64; " + Ns + "];");
            a.line("  for nf_i in 0usize.." + Ns + " { nf_xprev[nf_i] = nf_x[nf_i]; }");
            a.line("  for _ in 0..60 {");
            a.line("    let mut nf_f = [0.0f64; " + Ns + "]; nflow_isl_res_" + id
                + "(&nf_x, &nf_xprev, &nf_u, dt, &mut nf_f);");
            a.line("    let mut nf_nrm = 0.0f64; for nf_i in 0usize.." + Ns
                + " { nf_nrm += nf_f[nf_i] * nf_f[nf_i]; }");
            a.line("    if nf_nrm < 1e-24 { break; }");
            a.line("    let mut nf_jac = [0.0f64; " + std::to_string(N * N) + "];");
            a.line("    for nf_col in 0usize.." + Ns + " {");
            a.line("      let nf_xj = nf_x[nf_col]; let nf_h = 1e-7 * libm::fabs(nf_xj) + 1e-9;");
            a.line("      nf_x[nf_col] = nf_xj + nf_h; let mut nf_fp = [0.0f64; " + Ns
                + "]; nflow_isl_res_" + id
                + "(&nf_x, &nf_xprev, &nf_u, dt, &mut nf_fp); nf_x[nf_col] = nf_xj;");
            a.line("      for nf_i in 0usize.." + Ns + " { nf_jac[nf_i * " + Ns
                + " + nf_col] = (nf_fp[nf_i] - nf_f[nf_i]) / nf_h; }");
            a.line("    }");
            a.line("    let mut nf_b = [0.0f64; " + Ns + "]; for nf_i in 0usize.." + Ns
                + " { nf_b[nf_i] = -nf_f[nf_i]; }");
            a.line("    for nf_k in 0usize.." + Ns + " {");
            a.line("      let mut nf_p = nf_k; let mut nf_mx = libm::fabs(nf_jac[nf_k * " + Ns
                + " + nf_k]);");
            a.line("      for nf_r in (nf_k + 1).." + Ns + " { let nf_v = libm::fabs(nf_jac[nf_r * "
                + Ns + " + nf_k]); if nf_v > nf_mx { nf_mx = nf_v; nf_p = nf_r; } }");
            a.line("      if nf_p != nf_k { for nf_col in 0usize.." + Ns
                + " { let nf_t = nf_jac[nf_k * " + Ns + " + nf_col]; nf_jac[nf_k * " + Ns
                + " + nf_col] = nf_jac[nf_p * " + Ns + " + nf_col]; nf_jac[nf_p * " + Ns
                + " + nf_col] = nf_t; } let nf_tb = nf_b[nf_k]; nf_b[nf_k] = nf_b[nf_p]; "
                  "nf_b[nf_p] = nf_tb; "
                  "}");
            a.line("      let mut nf_d = nf_jac[nf_k * " + Ns
                + " + nf_k]; if nf_d < 1e-30 && nf_d > -1e-30 { nf_d = 1e-30; }");
            a.line("      for nf_r in (nf_k + 1).." + Ns + " { let nf_fac = nf_jac[nf_r * " + Ns
                + " + nf_k] / nf_d; for nf_col in nf_k.." + Ns + " { nf_jac[nf_r * " + Ns
                + " + nf_col] -= nf_fac * nf_jac[nf_k * " + Ns
                + " + nf_col]; } nf_b[nf_r] -= nf_fac * nf_b[nf_k]; }");
            a.line("    }");
            a.line("    for nf_i in (0usize.." + Ns
                + ").rev() { let mut nf_s = nf_b[nf_i]; for nf_col in (nf_i + 1).." + Ns
                + " { nf_s -= nf_jac[nf_i * " + Ns
                + " + nf_col] * nf_b[nf_col]; } let mut nf_d = nf_jac[nf_i * " + Ns
                + " + nf_i]; if nf_d < 1e-30 && nf_d > -1e-30 { nf_d = 1e-30; } nf_b[nf_i] = nf_s "
                  "/ nf_d; "
                  "}");
            a.line("    for nf_i in 0usize.." + Ns + " { nf_x[nf_i] += nf_b[nf_i]; }");
            a.line("  }");
            for (int i = 0; i < N; ++i) {
                a.line("  s.isl_x_" + id + "_" + std::to_string(i) + " = nf_x[" + std::to_string(i)
                    + "];");
            }
            auto epotStep = [&](int node) -> std::string {
                if (node <= 0 || node >= L.numNodes) {
                    return "0.0";
                }
                const int si = L.eIndex[static_cast<size_t>(node)];
                return si < 0 ? std::string("0.0") : ("nf_x[" + std::to_string(si) + "]");
            };
            for (const IslandComp& c : L.comps) {
                if (c.outPort < 0) {
                    continue;
                }
                const std::string ov = c.outPort > 0
                    ? ("out_" + id + "_p" + std::to_string(c.outPort))
                    : ("out_" + id);
                if (c.kind == Kind::VoltageSensor) {
                    a.line("  " + ov + " = (" + epotStep(c.a) + ") - (" + epotStep(c.b) + ");");
                } else if (c.kind == Kind::PotentialSensor) {
                    a.line("  " + ov + " = " + epotStep(c.a) + ";");
                } else if (c.kind == Kind::CurrentSensor) {
                    a.line("  " + ov + " = nf_x[" + std::to_string(c.stateIndex) + "];");
                }
            }
            a.line("}");
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCPhysicalIsland()
    {
        BlockCodegenTemplate t;
        t.emitState = [](const BlockCodegenStateArgs& a) {
            const IslandLayout L = buildLayout(*asJson(a.params));
            if (L.numStates <= 0 || !islandNewtonSupported(L)) {
                return; // unsupported island: no state (emitStep emits a marker)
            }
            a.declState("double isl_x_" + a.id + "[" + std::to_string(L.numStates) + "];");
            for (int i = 0; i < L.numStates; ++i) {
                const double ic
                    = (i < static_cast<int>(L.ic.size())) ? L.ic[static_cast<size_t>(i)] : 0.0;
                a.addInit("s->isl_x_" + a.id + "[" + std::to_string(i) + "] = " + a.fmt(ic) + ";");
            }
        };
        t.emitShared = [](const BlockCodegenStateArgs& a) {
            const IslandLayout L = buildLayout(*asJson(a.params));
            if (L.numStates <= 0 || !islandNewtonSupported(L)) {
                return;
            }
            if (!a.once("island_res_" + a.id)) {
                return;
            }
            a.addHelper("static void nflow_isl_res_" + a.id
                + "(const double* x, const double* xprev, const double* u, double dt, double* F) "
                  "{");
            a.addHelper("  (void)u; (void)xprev; (void)dt;");
            emitIslandResidualBody(L, a.fmt, [&](const std::string& l) { a.addHelper(l); });
            a.addHelper("}");
        };
        // The island runs its own implicit (backward-Euler) step in the fixed-step
        // body, so it contributes no state to the global RK4 packed vector.
        t.continuousWidth = [](const BlockCodegenArgs&) -> int { return 0; };
        t.emitStep = [](const BlockCodegenArgs& a) {
            const IslandLayout L = buildLayout(*asJson(a.params));
            if (L.numStates <= 0 || !islandNewtonSupported(L)) {
                a.line("out_" + a.id
                    + " = 0.0; /* nflow: this physicalIsland uses a component kind outside the "
                      "embedded-Newton codegen tier; simulate it with the 'dae' solver */");
                return;
            }
            emitIslandStepC(a, L);
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustPhysicalIsland()
    {
        BlockCodegenTemplate t;
        // Per-scalar state fields (isl_x_<id>_<i>: f64); the step gathers them into
        // a local array for the Newton solve.
        t.emitState = [](const BlockCodegenStateArgs& a) {
            const IslandLayout L = buildLayout(*asJson(a.params));
            if (L.numStates <= 0 || !islandNewtonSupported(L)) {
                return;
            }
            for (int i = 0; i < L.numStates; ++i) {
                const double ic
                    = (i < static_cast<int>(L.ic.size())) ? L.ic[static_cast<size_t>(i)] : 0.0;
                a.addState("isl_x_" + a.id + "_" + std::to_string(i), a.fmt(ic), "");
            }
        };
        t.emitShared = [](const BlockCodegenStateArgs& a) {
            const IslandLayout L = buildLayout(*asJson(a.params));
            if (L.numStates <= 0 || !islandNewtonSupported(L)) {
                return;
            }
            if (!a.once("island_res_rust_" + a.id)) {
                return;
            }
            a.addHelper("fn nflow_isl_res_" + a.id
                + "(x: &[f64], xprev: &[f64], u: &[f64], dt: f64, fout: &mut [f64]) {");
            emitIslandResidualBodyRust(L, a.fmt, [&](const std::string& l) { a.addHelper(l); });
            a.addHelper("}");
        };
        t.continuousWidth = [](const BlockCodegenArgs&) -> int { return 0; };
        t.emitStep = [](const BlockCodegenArgs& a) {
            const IslandLayout L = buildLayout(*asJson(a.params));
            if (L.numStates <= 0 || !islandNewtonSupported(L)) {
                a.line("out_" + a.id
                    + " = 0.0; // nflow: this physicalIsland uses a component kind outside the "
                      "embedded-Newton codegen tier; simulate it with the 'dae' solver");
                return;
            }
            emitIslandStepRust(a, L);
        };
        return t;
    }
    //=============================================================================
    // True when every component of the island is in the embedded-Newton supported
    // set. The code generator's non-generatable gate uses it: a Newton-generatable
    // island is allowed (C target), an unsupported-kind island is rejected with
    // the typed "simulate with dae" message.
    bool
    physicalIslandAlgebraicGeneratable(const json& params)
    {
        const IslandLayout L = buildLayout(params);
        return L.numStates > 0 && islandNewtonSupported(L);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
