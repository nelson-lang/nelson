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
            line("  for (int __i = 0; __i < " + std::to_string(N) + "; ++__i) G[__i] = 0.0;");
            line("  double kcl[" + std::to_string(L.numNodes) + "];");
            line("  for (int __n = 0; __n < " + std::to_string(L.numNodes)
                + "; ++__n) kcl[__n] = 0.0;");
            for (const IslandComp& c : L.comps) {
                const std::string ea = islandEpot(L, c.a);
                const std::string eb = islandEpot(L, c.b);
                const std::string a = std::to_string(c.a);
                const std::string b = std::to_string(c.b);
                const std::string si = std::to_string(c.stateIndex);
                const std::string si1 = std::to_string(c.stateIndex + 1);
                switch (c.kind) {
                case Kind::Resistor:
                    line("  { double __i = ((" + ea + ") - (" + eb + ")) / " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                case Kind::Conductor:
                    line("  { double __i = ((" + ea + ") - (" + eb + ")) * " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                case Kind::Diode:
                    line("  { double __vd = (" + ea + ") - (" + eb + ");"
                        + " double __arg = __vd / " + fmt(c.vt)
                        + "; if (__arg > 40.0) __arg = 40.0;" + " double __i = " + fmt(c.value)
                        + " * (exp(__arg) - 1.0);" + " kcl[" + a + "] += __i; kcl[" + b
                        + "] -= __i; }");
                    break;
                case Kind::ZDiode:
                    line("  { double __vd = (" + ea + ") - (" + eb + ");" + " double __af = __vd / "
                        + fmt(c.vt) + "; if (__af > 40.0) __af = 40.0;" + " double __ar = -(__vd + "
                        + fmt(c.value2) + ") / " + fmt(c.vt) + "; if (__ar > 40.0) __ar = 40.0;"
                        + " double __i = " + fmt(c.value) + " * (exp(__af) - 1.0) - " + fmt(c.value)
                        + " * (exp(__ar) - 1.0);" + " kcl[" + a + "] += __i; kcl[" + b
                        + "] -= __i; }");
                    break;
                case Kind::IdealDiode:
                    // Hybrid branch on its state s, same modes as the native residual.
                    line("  { double __s = x[" + si + "]; int __off = (__s < 0.0);"
                        + " double __vs = " + fmt(c.value3) + " + __s * (__off ? 1.0 : "
                        + fmt(c.value) + ");" + " double __is = (__off ? " + fmt(c.value2)
                        + " * __s : __s) + " + fmt(c.value2) + " * " + fmt(c.value3) + ";" + " G["
                        + si + "] = ((" + ea + ") - (" + eb + ")) - __vs;" + " kcl[" + a
                        + "] += __is; kcl[" + b + "] -= __is; }");
                    break;
                case Kind::Npn:
                case Kind::Pnp: {
                    // Ebers-Moll BJT (a=C, b=E, c=B), s = +1 NPN / -1 PNP.
                    const std::string ec = islandEpot(L, c.c);
                    const std::string cc = std::to_string(c.c);
                    const std::string sgn = (c.kind == Kind::Npn) ? "1.0" : "-1.0";
                    line("  { double __s = " + sgn + ";" + " double __abe = __s * ((" + ec + ") - ("
                        + eb + ")) / " + fmt(c.vt) + ";" + " double __abc = __s * ((" + ec + ") - ("
                        + ea + ")) / " + fmt(c.vt) + ";"
                        + " if (__abe > 40.0) __abe = 40.0; if (__abc > 40.0) __abc = 40.0;"
                        + " double __ef = exp(__abe) - 1.0; double __er = exp(__abc) - 1.0;"
                        + " double __ic = __s * (" + fmt(c.value) + " * (__ef - __er) - "
                        + fmt(c.value / c.value3) + " * __er);" + " double __ib = __s * ("
                        + fmt(c.value / c.value2) + " * __ef + " + fmt(c.value / c.value3)
                        + " * __er);" + " kcl[" + a + "] += __ic; kcl[" + cc + "] += __ib;"
                        + " kcl[" + b + "] -= (__ic + __ib); }");
                    break;
                }
                case Kind::Nmos:
                case Kind::Pmos: {
                    // Square-law MOSFET (a=D, b=S, c=G), s = +1 NMOS / -1 PMOS.
                    const std::string ec = islandEpot(L, c.c);
                    const std::string sgn = (c.kind == Kind::Nmos) ? "1.0" : "-1.0";
                    line("  { double __s = " + sgn + ";" + " double __vgs = __s * ((" + ec + ") - ("
                        + eb + "));" + " double __vds = __s * ((" + ea + ") - (" + eb + "));"
                        + " double __vov = __vgs - " + fmt(c.value2) + "; double __id = 0.0;"
                        + " if (__vov > 0.0) { __id = (__vds < __vov) ? " + fmt(c.value)
                        + " * (__vov * __vds - 0.5 * __vds * __vds) : 0.5 * " + fmt(c.value)
                        + " * __vov * __vov; }" + " kcl[" + a + "] += __s * __id; kcl[" + b
                        + "] -= __s * __id; }");
                    break;
                }
                case Kind::Radiation:
                    // Stefan-Boltzmann: q = Gr (T_a^4 - T_b^4), the node potentials
                    // being temperatures (thermal domain). Smooth nonlinear stamp.
                    line("  { double __ta = (" + ea + "); double __tb = (" + eb + ");"
                        + " double __q = " + fmt(c.value)
                        + " * (__ta * __ta * __ta * __ta - __tb * __tb * __tb * __tb);" + " kcl["
                        + a + "] += __q; kcl[" + b + "] -= __q; }");
                    break;
                case Kind::Capacitor:
                    // state si = v_C (differential), si+1 = i_C (algebraic):
                    //   G[si] = i_C / C ; G[si+1] = v_C - (e_a - e_b) ; i_C into KCL.
                    line("  { double __vC = x[" + si + "]; double __iC = x[" + si1 + "];" + " G["
                        + si + "] = __iC / " + fmt(c.value) + ";" + " G[" + si1 + "] = __vC - (("
                        + ea + ") - (" + eb + "));" + " kcl[" + a + "] += __iC; kcl[" + b
                        + "] -= __iC; }");
                    break;
                case Kind::Inductor:
                    // state si = i_L (differential): G[si] = v_L / L ; i_L into KCL.
                    line("  { double __iL = x[" + si + "];" + " G[" + si + "] = ((" + ea + ") - ("
                        + eb + ")) / " + fmt(c.value) + ";" + " kcl[" + a + "] += __iL; kcl[" + b
                        + "] -= __iL; }");
                    break;
                case Kind::VSource:
                case Kind::CurrentSensor: {
                    const std::string V
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line("  { double __ib = x[" + si + "];" + " kcl[" + a + "] += __ib; kcl[" + b
                        + "] -= __ib;" + " G[" + si + "] = ((" + ea + ") - (" + eb + ")) - (" + V
                        + "); }");
                    break;
                }
                case Kind::ISource: {
                    const std::string I
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line("  { double __i = " + I + "; kcl[" + a + "] += __i; kcl[" + b
                        + "] -= __i; }");
                    break;
                }
                case Kind::Vccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double __i = " + fmt(c.value) + " * ((" + ec + ") - (" + ed + "));"
                        + " kcl[" + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                }
                case Kind::Vcvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double __ib = x[" + si + "]; kcl[" + a + "] += __ib; kcl[" + b
                        + "] -= __ib; G[" + si + "] = ((" + ea + ") - (" + eb + ")) - "
                        + fmt(c.value) + " * ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::Ccvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double __io = x[" + si + "]; double __is = x[" + si1 + "];" + " kcl["
                        + a + "] += __io; kcl[" + b + "] -= __io;" + " kcl[" + std::to_string(c.c)
                        + "] += __is; kcl[" + std::to_string(c.d) + "] -= __is; G[" + si + "] = (("
                        + ea + ") - (" + eb + ")) - " + fmt(c.value) + " * __is; G[" + si1
                        + "] = ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::Cccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double __is = x[" + si + "]; double __io = " + fmt(c.value)
                        + " * __is;" + " kcl[" + a + "] += __io; kcl[" + b + "] -= __io;" + " kcl["
                        + std::to_string(c.c) + "] += __is; kcl[" + std::to_string(c.d)
                        + "] -= __is; G[" + si + "] = ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::OpAmp:
                    line("  { double __io = x[" + si + "]; G[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + std::to_string(c.c) + "] += __io; }");
                    break;
                case Kind::Short:
                    line("  { double __ib = x[" + si + "]; G[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + a + "] += __ib; kcl[" + b + "] -= __ib; }");
                    break;
                case Kind::Idle:
                    line("  { double __ib = x[" + si + "]; G[" + si + "] = __ib;" + " kcl[" + a
                        + "] += __ib; kcl[" + b + "] -= __ib; }");
                    break;
                case Kind::Transformer: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double __i1 = x[" + si + "]; double __i2 = -" + fmt(c.value)
                        + " * __i1; G[" + si + "] = ((" + ea + ") - (" + eb + ")) - " + fmt(c.value)
                        + " * ((" + ec + ") - (" + ed + "));" + " kcl[" + a + "] += __i1; kcl[" + b
                        + "] -= __i1; kcl[" + std::to_string(c.c) + "] += __i2; kcl["
                        + std::to_string(c.d) + "] -= __i2; }");
                    break;
                }
                case Kind::Gyrator: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { double __v1 = (" + ea + ") - (" + eb + "); double __v2 = (" + ec
                        + ") - (" + ed + "); double __i1 = " + fmt(c.value2) + " * __v2;"
                        + " double __i2 = -" + fmt(c.value) + " * __v1;" + " kcl[" + a
                        + "] += __i1; kcl[" + b + "] -= __i1; kcl[" + std::to_string(c.c)
                        + "] += __i2; kcl[" + std::to_string(c.d) + "] -= __i2; }");
                    break;
                }
                case Kind::Switch: {
                    const std::string ctrl
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : std::string("1.0");
                    line("  { double __ib = x[" + si + "]; G[" + si + "] = (" + ctrl
                        + " > 0.5) ? ((" + ea + ") - (" + eb + ")) : __ib;" + " kcl[" + a
                        + "] += __ib; kcl[" + b + "] -= __ib; }");
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
            a.line("  double* __X = " + xs + ";");
            if (np > 0) {
                a.line("  double __u[" + std::to_string(np) + "];");
                for (int p = 0; p < np; ++p) {
                    const std::string e = (p < static_cast<int>(a.in.size()))
                        ? a.in[static_cast<size_t>(p)]
                        : "0.0";
                    a.line("  __u[" + std::to_string(p) + "] = " + e + ";");
                }
            } else {
                a.line("  double __u[1]; __u[0] = 0.0; (void)__u;");
            }
            // Backward-Euler: freeze the previous-step state for the implicit
            // residual (unused when the island is purely algebraic).
            a.line("  double __xprev[" + Ns + "]; for (int __i = 0; __i < " + Ns
                + "; ++__i) __xprev[__i] = __X[__i];");
            a.line("  for (int __it = 0; __it < 60; ++__it) {");
            a.line("    double __F[" + Ns + "]; __isl_res_" + id + "(__X, __xprev, __u, dt, __F);");
            a.line("    double __nrm = 0.0; for (int __i = 0; __i < " + Ns
                + "; ++__i) __nrm += __F[__i] * __F[__i];");
            a.line("    if (__nrm < 1e-24) break;");
            a.line("    double __J[" + std::to_string(N * N) + "];");
            a.line("    for (int __j = 0; __j < " + Ns + "; ++__j) {");
            a.line("      double __xj = __X[__j]; double __h = 1e-7 * (__xj < 0 ? -__xj : __xj) + "
                   "1e-9;");
            a.line("      __X[__j] = __xj + __h; double __Fp[" + Ns + "]; __isl_res_" + id
                + "(__X, __xprev, __u, dt, __Fp); __X[__j] = __xj;");
            a.line("      for (int __i = 0; __i < " + Ns + "; ++__i) __J[__i * " + Ns
                + " + __j] = (__Fp[__i] - __F[__i]) / __h;");
            a.line("    }");
            a.line("    double __b[" + Ns + "]; for (int __i = 0; __i < " + Ns
                + "; ++__i) __b[__i] = -__F[__i];");
            a.line("    for (int __k = 0; __k < " + Ns + "; ++__k) {");
            a.line("      int __p = __k; double __mx = __J[__k * " + Ns + " + __k];"
                + " if (__mx < 0) __mx = -__mx;");
            a.line("      for (int __r = __k + 1; __r < " + Ns + "; ++__r) {"
                + " double __v = __J[__r * " + Ns + " + __k]; if (__v < 0) __v = -__v;"
                + " if (__v > __mx) { __mx = __v; __p = __r; } }");
            a.line("      if (__p != __k) { for (int __col = 0; __col < " + Ns + "; ++__col) {"
                + " double __t = __J[__k * " + Ns + " + __col]; __J[__k * " + Ns
                + " + __col] = __J[__p * " + Ns + " + __col]; __J[__p * " + Ns
                + " + __col] = __t; }"
                + " double __tb = __b[__k]; __b[__k] = __b[__p]; __b[__p] = __tb; }");
            a.line("      double __d = __J[__k * " + Ns
                + " + __k]; if (__d < 1e-30 && __d > -1e-30) __d = 1e-30;");
            a.line("      for (int __r = __k + 1; __r < " + Ns + "; ++__r) {"
                + " double __f = __J[__r * " + Ns + " + __k] / __d;"
                + " for (int __col = __k; __col < " + Ns + "; ++__col) __J[__r * " + Ns
                + " + __col] -= __f * __J[__k * " + Ns + " + __col];"
                + " __b[__r] -= __f * __b[__k]; }");
            a.line("    }");
            a.line("    for (int __i = " + std::to_string(N - 1) + "; __i >= 0; --__i) {"
                + " double __s = __b[__i];" + " for (int __col = __i + 1; __col < " + Ns
                + "; ++__col) __s -= __J[__i * " + Ns + " + __col] * __b[__col];"
                + " double __d = __J[__i * " + Ns
                + " + __i]; if (__d < 1e-30 && __d > -1e-30) __d = 1e-30;"
                + " __b[__i] = __s / __d; }");
            a.line("    for (int __i = 0; __i < " + Ns + "; ++__i) __X[__i] += __b[__i];");
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
                    line("  { let __i = ((" + ea + ") - (" + eb + ")) / " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                case Kind::Conductor:
                    line("  { let __i = ((" + ea + ") - (" + eb + ")) * " + fmt(c.value) + ";"
                        + " kcl[" + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                case Kind::Diode:
                    line("  { let __vd = (" + ea + ") - (" + eb + ");" + " let mut __arg = __vd / "
                        + fmt(c.vt) + "; if __arg > 40.0 { __arg = 40.0; }"
                        + " let __i = " + fmt(c.value) + " * (libm::exp(__arg) - 1.0);" + " kcl["
                        + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                case Kind::ZDiode:
                    line("  { let __vd = (" + ea + ") - (" + eb + ");" + " let mut __af = __vd / "
                        + fmt(c.vt) + "; if __af > 40.0 { __af = 40.0; }"
                        + " let mut __ar = -(__vd + " + fmt(c.value2) + ") / " + fmt(c.vt)
                        + "; if __ar > 40.0 { __ar = 40.0; }" + " let __i = " + fmt(c.value)
                        + " * (libm::exp(__af) - 1.0) - " + fmt(c.value)
                        + " * (libm::exp(__ar) - 1.0);" + " kcl[" + a + "] += __i; kcl[" + b
                        + "] -= __i; }");
                    break;
                case Kind::IdealDiode:
                    line("  { let __s = x[" + si + "]; let __off = __s < 0.0;"
                        + " let __vs = " + fmt(c.value3) + " + __s * (if __off { 1.0 } else { "
                        + fmt(c.value) + " });" + " let __is = (if __off { " + fmt(c.value2)
                        + " * __s } else { __s }) + " + fmt(c.value2) + " * " + fmt(c.value3) + ";"
                        + " g[" + si + "] = ((" + ea + ") - (" + eb + ")) - __vs;" + " kcl[" + a
                        + "] += __is; kcl[" + b + "] -= __is; }");
                    break;
                case Kind::Capacitor:
                    line("  { let __vc = x[" + si + "]; let __ic = x[" + si1 + "];" + " g[" + si
                        + "] = __ic / " + fmt(c.value) + ";" + " g[" + si1 + "] = __vc - ((" + ea
                        + ") - (" + eb + "));" + " kcl[" + a + "] += __ic; kcl[" + b
                        + "] -= __ic; }");
                    break;
                case Kind::Inductor:
                    line("  { let __il = x[" + si + "];" + " g[" + si + "] = ((" + ea + ") - (" + eb
                        + ")) / " + fmt(c.value) + ";" + " kcl[" + a + "] += __il; kcl[" + b
                        + "] -= __il; }");
                    break;
                case Kind::VSource:
                case Kind::CurrentSensor: {
                    const std::string V
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line("  { let __ib = x[" + si + "];" + " kcl[" + a + "] += __ib; kcl[" + b
                        + "] -= __ib;" + " g[" + si + "] = ((" + ea + ") - (" + eb + ")) - (" + V
                        + "); }");
                    break;
                }
                case Kind::ISource: {
                    const std::string I
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : fmt(c.value);
                    line(
                        "  { let __i = " + I + "; kcl[" + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                }
                case Kind::Npn:
                case Kind::Pnp: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string cc = std::to_string(c.c);
                    const std::string sgn = (c.kind == Kind::Npn) ? "1.0" : "-1.0";
                    line("  { let __s: f64 = " + sgn + ";" + " let mut __abe = __s * ((" + ec
                        + ") - (" + eb + ")) / " + fmt(c.vt) + ";" + " let mut __abc = __s * (("
                        + ec + ") - (" + ea + ")) / " + fmt(c.vt) + ";"
                        + " if __abe > 40.0 { __abe = 40.0; } if __abc > 40.0 { __abc = 40.0; }"
                        + " let __ef = libm::exp(__abe) - 1.0; let __er = libm::exp(__abc) - 1.0;"
                        + " let __ic = __s * (" + fmt(c.value) + " * (__ef - __er) - "
                        + fmt(c.value / c.value3) + " * __er);" + " let __ib = __s * ("
                        + fmt(c.value / c.value2) + " * __ef + " + fmt(c.value / c.value3)
                        + " * __er);" + " kcl[" + a + "] += __ic; kcl[" + cc + "] += __ib;"
                        + " kcl[" + b + "] -= (__ic + __ib); }");
                    break;
                }
                case Kind::Nmos:
                case Kind::Pmos: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string sgn = (c.kind == Kind::Nmos) ? "1.0" : "-1.0";
                    line("  { let __s: f64 = " + sgn + ";" + " let __vgs = __s * ((" + ec + ") - ("
                        + eb + "));" + " let __vds = __s * ((" + ea + ") - (" + eb + "));"
                        + " let __vov = __vgs - " + fmt(c.value2) + "; let mut __id = 0.0;"
                        + " if __vov > 0.0 { __id = if __vds < __vov { " + fmt(c.value)
                        + " * (__vov * __vds - 0.5 * __vds * __vds) } else { 0.5 * " + fmt(c.value)
                        + " * __vov * __vov }; }" + " kcl[" + a + "] += __s * __id; kcl[" + b
                        + "] -= __s * __id; }");
                    break;
                }
                case Kind::Radiation:
                    line("  { let __ta = (" + ea + "); let __tb = (" + eb + ");"
                        + " let __q = " + fmt(c.value)
                        + " * (__ta * __ta * __ta * __ta - __tb * __tb * __tb * __tb);" + " kcl["
                        + a + "] += __q; kcl[" + b + "] -= __q; }");
                    break;
                case Kind::Vccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let __i = " + fmt(c.value) + " * ((" + ec + ") - (" + ed + "));"
                        + " kcl[" + a + "] += __i; kcl[" + b + "] -= __i; }");
                    break;
                }
                case Kind::Vcvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let __ib = x[" + si + "]; kcl[" + a + "] += __ib; kcl[" + b
                        + "] -= __ib; g[" + si + "] = ((" + ea + ") - (" + eb + ")) - "
                        + fmt(c.value) + " * ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::Ccvs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let __io = x[" + si + "]; let __is = x[" + si1 + "];" + " kcl[" + a
                        + "] += __io; kcl[" + b + "] -= __io;" + " kcl[" + std::to_string(c.c)
                        + "] += __is; kcl[" + std::to_string(c.d) + "] -= __is; g[" + si + "] = (("
                        + ea + ") - (" + eb + ")) - " + fmt(c.value) + " * __is; g[" + si1
                        + "] = ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::Cccs: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let __is = x[" + si + "]; let __io = " + fmt(c.value) + " * __is;"
                        + " kcl[" + a + "] += __io; kcl[" + b + "] -= __io;" + " kcl["
                        + std::to_string(c.c) + "] += __is; kcl[" + std::to_string(c.d)
                        + "] -= __is; g[" + si + "] = ((" + ec + ") - (" + ed + ")); }");
                    break;
                }
                case Kind::OpAmp:
                    line("  { let __io = x[" + si + "]; g[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + std::to_string(c.c) + "] += __io; }");
                    break;
                case Kind::Short:
                    line("  { let __ib = x[" + si + "]; g[" + si + "] = (" + ea + ") - (" + eb
                        + "); kcl[" + a + "] += __ib; kcl[" + b + "] -= __ib; }");
                    break;
                case Kind::Idle:
                    line("  { let __ib = x[" + si + "]; g[" + si + "] = __ib;" + " kcl[" + a
                        + "] += __ib; kcl[" + b + "] -= __ib; }");
                    break;
                case Kind::Transformer: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let __i1 = x[" + si + "]; let __i2 = -" + fmt(c.value) + " * __i1; g["
                        + si + "] = ((" + ea + ") - (" + eb + ")) - " + fmt(c.value) + " * ((" + ec
                        + ") - (" + ed + "));" + " kcl[" + a + "] += __i1; kcl[" + b
                        + "] -= __i1; kcl[" + std::to_string(c.c) + "] += __i2; kcl["
                        + std::to_string(c.d) + "] -= __i2; }");
                    break;
                }
                case Kind::Gyrator: {
                    const std::string ec = islandEpot(L, c.c);
                    const std::string ed = islandEpot(L, c.d);
                    line("  { let __v1 = (" + ea + ") - (" + eb + "); let __v2 = (" + ec + ") - ("
                        + ed + "); let __i1 = " + fmt(c.value2) + " * __v2;" + " let __i2 = -"
                        + fmt(c.value) + " * __v1;" + " kcl[" + a + "] += __i1; kcl[" + b
                        + "] -= __i1; kcl[" + std::to_string(c.c) + "] += __i2; kcl["
                        + std::to_string(c.d) + "] -= __i2; }");
                    break;
                }
                case Kind::Switch: {
                    const std::string ctrl
                        = c.port >= 0 ? ("u[" + std::to_string(c.port) + "]") : std::string("1.0");
                    line("  { let __ib = x[" + si + "]; g[" + si + "] = if " + ctrl + " > 0.5 { ("
                        + ea + ") - (" + eb + ") } else { __ib };" + " kcl[" + a + "] += __ib; kcl["
                        + b + "] -= __ib; }");
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
            a.line("  let mut __x = [0.0f64; " + Ns + "];");
            for (int i = 0; i < N; ++i) {
                a.line("  __x[" + std::to_string(i) + "] = s.isl_x_" + id + "_" + std::to_string(i)
                    + ";");
            }
            if (np > 0) {
                a.line("  let mut __u = [0.0f64; " + std::to_string(np) + "];");
                for (int p = 0; p < np; ++p) {
                    const std::string e = (p < static_cast<int>(a.in.size()))
                        ? a.in[static_cast<size_t>(p)]
                        : "0.0";
                    a.line("  __u[" + std::to_string(p) + "] = " + e + ";");
                }
            } else {
                a.line("  let __u = [0.0f64; 1];");
            }
            a.line("  let mut __xprev = [0.0f64; " + Ns + "];");
            a.line("  for __i in 0usize.." + Ns + " { __xprev[__i] = __x[__i]; }");
            a.line("  for _ in 0..60 {");
            a.line("    let mut __f = [0.0f64; " + Ns + "]; __isl_res_" + id
                + "(&__x, &__xprev, &__u, dt, &mut __f);");
            a.line("    let mut __nrm = 0.0f64; for __i in 0usize.." + Ns
                + " { __nrm += __f[__i] * __f[__i]; }");
            a.line("    if __nrm < 1e-24 { break; }");
            a.line("    let mut __jac = [0.0f64; " + std::to_string(N * N) + "];");
            a.line("    for __col in 0usize.." + Ns + " {");
            a.line("      let __xj = __x[__col]; let __h = 1e-7 * libm::fabs(__xj) + 1e-9;");
            a.line("      __x[__col] = __xj + __h; let mut __fp = [0.0f64; " + Ns + "]; __isl_res_"
                + id + "(&__x, &__xprev, &__u, dt, &mut __fp); __x[__col] = __xj;");
            a.line("      for __i in 0usize.." + Ns + " { __jac[__i * " + Ns
                + " + __col] = (__fp[__i] - __f[__i]) / __h; }");
            a.line("    }");
            a.line("    let mut __b = [0.0f64; " + Ns + "]; for __i in 0usize.." + Ns
                + " { __b[__i] = -__f[__i]; }");
            a.line("    for __k in 0usize.." + Ns + " {");
            a.line("      let mut __p = __k; let mut __mx = libm::fabs(__jac[__k * " + Ns
                + " + __k]);");
            a.line("      for __r in (__k + 1).." + Ns + " { let __v = libm::fabs(__jac[__r * " + Ns
                + " + __k]); if __v > __mx { __mx = __v; __p = __r; } }");
            a.line("      if __p != __k { for __col in 0usize.." + Ns + " { let __t = __jac[__k * "
                + Ns + " + __col]; __jac[__k * " + Ns + " + __col] = __jac[__p * " + Ns
                + " + __col]; __jac[__p * " + Ns
                + " + __col] = __t; } let __tb = __b[__k]; __b[__k] = __b[__p]; __b[__p] = __tb; "
                  "}");
            a.line("      let mut __d = __jac[__k * " + Ns
                + " + __k]; if __d < 1e-30 && __d > -1e-30 { __d = 1e-30; }");
            a.line("      for __r in (__k + 1).." + Ns + " { let __fac = __jac[__r * " + Ns
                + " + __k] / __d; for __col in __k.." + Ns + " { __jac[__r * " + Ns
                + " + __col] -= __fac * __jac[__k * " + Ns
                + " + __col]; } __b[__r] -= __fac * __b[__k]; }");
            a.line("    }");
            a.line("    for __i in (0usize.." + Ns
                + ").rev() { let mut __s = __b[__i]; for __col in (__i + 1).." + Ns
                + " { __s -= __jac[__i * " + Ns
                + " + __col] * __b[__col]; } let mut __d = __jac[__i * " + Ns
                + " + __i]; if __d < 1e-30 && __d > -1e-30 { __d = 1e-30; } __b[__i] = __s / __d; "
                  "}");
            a.line("    for __i in 0usize.." + Ns + " { __x[__i] += __b[__i]; }");
            a.line("  }");
            for (int i = 0; i < N; ++i) {
                a.line("  s.isl_x_" + id + "_" + std::to_string(i) + " = __x[" + std::to_string(i)
                    + "];");
            }
            auto epotStep = [&](int node) -> std::string {
                if (node <= 0 || node >= L.numNodes) {
                    return "0.0";
                }
                const int si = L.eIndex[static_cast<size_t>(node)];
                return si < 0 ? std::string("0.0") : ("__x[" + std::to_string(si) + "]");
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
                    a.line("  " + ov + " = __x[" + std::to_string(c.stateIndex) + "];");
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
            a.addHelper("static void __isl_res_" + a.id
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
            a.addHelper("#[allow(unused_variables, unused_mut, unused_assignments)]");
            a.addHelper("fn __isl_res_" + a.id
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
