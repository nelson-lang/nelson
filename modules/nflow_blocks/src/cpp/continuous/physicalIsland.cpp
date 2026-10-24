//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// physicalIsland (acausal palette N2). A single native block that carries an
// assembled electrical netlist (nodes + components) and feeds the DAE solver a
// modified-nodal-analysis residual over its own continuous-state slice, the
// C++ vehicle for the acausal palette, the fast counterpart of the N1 `.m`
// lowering (nflow_acausal_lower.m), which stays as the executable spec.
//
// Formulation (index-1 by curation), identical to the N1 POC but assembled in
// one block with no interpreter round-trip in the residual:
//   - non-ground node potential e_n           ALGEBRAIC, residual = KCL at n
//   - capacitor: v_C DIFFERENTIAL (v_C'=i_C/C) + i_C ALGEBRAIC (v_C-(e_a-e_b))
//   - inductor:  i_L DIFFERENTIAL (i_L'=(e_a-e_b)/L)
//   - vsource / ammeter: i branch ALGEBRAIC (residual (e_a-e_b)-V, ammeter V=0)
//   - resistor / current source: pure KCL contributions (no own state)
// Signal-driven sources read an input port; sensors write an output port.
//
// Under the `dae` solver the residual pieces are written into the block's xdot
// slice during DERIVATIVE; the solver-loop assembly reads id[k] (1 diff / 0
// alg, supplied by BlockMetadata::algebraicStateMask) and forms
// r = id ? (x'-f) : f, so a differential state contributes x'-f and an
// algebraic one contributes f = g.
//
// Under every OTHER loop (the legacy fixed-step macro loop and the native
// explicit solvers ode1/ode4/ode45, SimCtx::islandImplicitStep) the island
// integrates itself: DERIVATIVE freezes its slice (zero rates, so an explicit
// stage never integrates a constraint residual as a derivative) and OUTPUT
// advances the state once per major step with the same embedded implicit
// (backward-Euler + Newton) solve the generated code runs; the first pass
// solves the algebraic components with the differential states pinned at
// their initial conditions (an IDACalcIC-style consistency solve), each later
// pass one backward-Euler step reading the drive at the sample instant.
// See physicalIslandLayout.cpp (params -> layout), physicalIslandCodegen.cpp
// (embedded-Newton C / Rust emitters) and physicalIslandShared.hpp (types).
//
// Node 0 is the ground reference (e = 0). Grounds need no component; any node
// index not otherwise used is an isolated reference.
//
// Domain-generic: the block is a pure across/through DAE solver, so the THERMAL
// domain (across = temperature T, through = heat flow Q) maps onto the same
// kinds with no new code: HeatCapacitor = capacitor to the 0-reference,
// ThermalConductor = conductor, Fixed/PrescribedTemperature = vsource,
// Fixed/PrescribedHeatFlow = isource, and the three temperature/heat-flow
// sensors = potential/voltage/current sensors. The `domain` param is
// informational here; the future component registry owns the naming/units.
// (Sign convention: an isource current flows a -> b, so to inject Q into a
// node the source runs ground -> node.)
//=============================================================================
#include "physicalIslandShared.hpp"
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "continuous_blocks.hpp"
#include <cmath>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    using namespace physisland;
    //=============================================================================
    // Raw DAE output G over an arbitrary state view: the derivative value for a
    // differential state, the constraint residual for an algebraic one, the
    // single source of truth shared by the dae DERIVATIVE pass and the embedded
    // implicit advance. Reads states from xArr when non-null, else `fallback`
    // (the legacy per-block vector), inputs live from ctx.
    static void
    islandStampRaw(SimCtx& ctx, const Block& b, const IslandLayout* L, const double* xArr,
        const std::vector<double>& fallback, double* out);
    // One embedded implicit advance (consistency solve at the first pass,
    // backward-Euler + Newton after), guarded to once per major step. With
    // seedOnly the consistency solve runs unconditionally and leaves the
    // once-per-step bookkeeping untouched (the DAE path's t = 0 seed).
    static void
    islandImplicitAdvance(
        SimCtx& ctx, const Block& b, BlockState& st, IslandLayout* L, bool seedOnly = false);
    //=============================================================================
    int
    physicalIslandStateCount(const Block& b, const ValMap&)
    {
        return buildLayout(b.params).numStates;
    }
    //=============================================================================
    void
    physicalIslandIdMask(const Block& b, const ValMap&, std::vector<double>& ids)
    {
        IslandLayout L = buildLayout(b.params);
        for (size_t k = 0; k < ids.size() && k < L.id.size(); ++k) {
            ids[k] = L.id[k];
        }
    }
    //=============================================================================
    int
    physicalIslandZeroCrossingCount(const Block& b, const ValMap&)
    {
        // One zero-crossing surface per ideal switch (the control threshold)
        // and per ideal diode (its mode state s).
        IslandLayout L = buildLayout(b.params);
        int n = 0;
        for (const IslandComp& c : L.comps) {
            if (c.kind == Kind::Switch || c.kind == Kind::IdealDiode) {
                ++n;
            }
        }
        return n;
    }
    //=============================================================================
    bool
    handlePhysicalIsland(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);

        if (phase == Phase::INIT) {
            IslandLayout L = buildLayout(b.params);
            // Seed the block's initial state (gatherStates copies st.vec into the
            // global x slice); IDACalcIC then corrects the algebraic entries.
            st.vec.assign(L.ic.begin(), L.ic.end());
            st.external.reset(new IslandLayout(std::move(L)));
            return false;
        }

        IslandLayout* L = dynamic_cast<IslandLayout*>(st.external.get());
        if (!L) {
            return false;
        }

        if (phase == Phase::ZERO_CROSSING) {
            // Surface where an ideal switch's control crosses its 0.5 threshold
            // and the branch flips (open <-> short). Reporting it lets the DAE
            // locate the crossing and reinit there instead of failing to step
            // across the topology jump (IDASolve flag -1). One per switch.
            // An ideal diode reports its mode state s (off < 0 <= on) the same way.
            double* g = blockG(ctx, b.nid);
            if (g) {
                const double* x = blockX(ctx, b.nid);
                int gi = 0;
                for (const IslandComp& c : L->comps) {
                    if (c.kind == Kind::Switch) {
                        const double ctrl = (c.port >= 0) ? getInput(ctx, b.nid, c.port, 1.0) : 1.0;
                        g[gi++] = ctrl - 0.5;
                    } else if (c.kind == Kind::IdealDiode) {
                        // Off: the knee gap v_d - Vknee (turns on when it goes
                        // positive). On: the branch parameter s (turns off when
                        // the current goes negative).
                        const size_t ci = static_cast<size_t>(&c - L->comps.data());
                        const bool off = (ci < L->diodeMode.size()) && (L->diodeMode[ci] == 0);
                        if (off) {
                            g[gi++] = islandDiodeKneeGap(L, c, x, st.vec);
                        } else {
                            const int si = c.stateIndex;
                            double s = 0.0;
                            if (x) {
                                s = x[si];
                            } else if (si >= 0 && si < (int)st.vec.size()) {
                                s = st.vec[si];
                            }
                            g[gi++] = s;
                        }
                    }
                }
            }
            return false;
        }

        if (phase == Phase::EVENT) {
            // A located root: toggle the ideal diode(s) sitting at their knee
            // (the surface that crossed), then re-solve the algebraic state in
            // the new modes so the reinit (IDACalcIC) starts consistent instead
            // of discovering the mode change under a stale Jacobian.
            if (L->hasIdealDiode) {
                const double* x = blockX(ctx, b.nid);
                for (size_t ci = 0; ci < L->comps.size() && ci < L->diodeMode.size(); ++ci) {
                    const IslandComp& c = L->comps[ci];
                    if (c.kind == Kind::IdealDiode
                        && std::fabs(islandDiodeKneeGap(L, c, x, st.vec)) <= kDiodeKneeTolV) {
                        L->diodeMode[ci] = (L->diodeMode[ci] == 0) ? 1 : 0;
                    }
                }
                islandImplicitAdvance(ctx, b, st, L, true);
            }
            return false;
        }

        if (phase == Phase::OUTPUT) {
            if (ctx.islandImplicitStep && !ctx.sideEffectFree) {
                // Non-DAE loops: the island advances itself here, once per major
                // step (never during the solver's side-effect-free rhs / roots
                // evaluations, whose stage vectors must not be written).
                islandImplicitAdvance(ctx, b, st, L);
            } else if (ctx.initialStateSeed && L->hasIdealDiode) {
                // DAE path, t = 0 seed evaluation: settle the latched diode modes
                // against the live inputs (unlike INIT) and hand IDACalcIC a
                // consistent start. Islands without a latched mode keep their
                // original cold start, so their trajectories are untouched.
                islandImplicitAdvance(ctx, b, st, L, true);
            }
            const double* x = blockX(ctx, b.nid);
            // Node potential accessor: ground (node 0) is 0; a non-ground node
            // reads its algebraic state. Falls back to the per-block vector when
            // no global x is active (the fixed-step loop).
            auto ePot = [&](int node) -> double {
                if (node <= 0) {
                    return 0.0;
                }
                const int si = L->eIndex[static_cast<size_t>(node)];
                if (si < 0) {
                    return 0.0;
                }
                if (x) {
                    return x[si];
                }
                return (si < (int)st.vec.size()) ? st.vec[si] : 0.0;
            };
            auto xat = [&](int si) -> double {
                if (si < 0) {
                    return 0.0;
                }
                if (x) {
                    return x[si];
                }
                return (si < (int)st.vec.size()) ? st.vec[si] : 0.0;
            };
            for (const IslandComp& c : L->comps) {
                if (c.outPort < 0) {
                    continue;
                }
                switch (c.kind) {
                case Kind::VoltageSensor:
                    setOutput(ctx, b.nid, ePot(c.a) - ePot(c.b), c.outPort);
                    break;
                case Kind::PotentialSensor:
                    setOutput(ctx, b.nid, ePot(c.a), c.outPort);
                    break;
                case Kind::CurrentSensor:
                    setOutput(ctx, b.nid, xat(c.stateIndex), c.outPort);
                    break;
                default:
                    break;
                }
            }
            return false;
        }

        if (phase == Phase::DERIVATIVE) {
            double* xdot = blockXdot(ctx, b.nid);
            if (!xdot) {
                return false;
            }
            if (ctx.islandImplicitStep) {
                // Explicit solver loop: the OUTPUT advance owns the state; a
                // zero rate keeps the RK stages from integrating the constraint
                // residuals as if they were derivatives (the very failure this
                // mode exists to prevent).
                for (int k = 0; k < L->numStates; ++k) {
                    xdot[k] = 0.0;
                }
                return false;
            }
            islandStampRaw(ctx, b, L, blockX(ctx, b.nid), st.vec, xdot);
            return false;
        }
        return false;
    }
    //=============================================================================
    static void
    islandStampRaw(SimCtx& ctx, const Block& b, const IslandLayout* L, const double* xArr,
        const std::vector<double>& fallback, double* out)
    {
        auto ePot = [&](int node) -> double {
            if (node <= 0) {
                return 0.0;
            }
            const int si = L->eIndex[static_cast<size_t>(node)];
            if (si < 0) {
                return 0.0;
            }
            if (xArr) {
                return xArr[si];
            }
            return (si < (int)fallback.size()) ? fallback[si] : 0.0;
        };
        auto xat = [&](int si) -> double {
            if (si < 0) {
                return 0.0;
            }
            if (xArr) {
                return xArr[si];
            }
            return (si < (int)fallback.size()) ? fallback[si] : 0.0;
        };
        std::vector<double> kcl(static_cast<size_t>(L->numNodes), 0.0);
        // SPICE-style GMIN: a tiny conductance placed across every nonlinear
        // junction (diode / BJT / MOSFET). It keeps the junction Jacobian
        // non-singular at a cold start (where exp(vd/Vt) has a near-zero slope
        // for a small Is), so the Newton/IDACalcIC operating-point solve can
        // climb to a strongly forward-biased bias point instead of stalling.
        // Small enough (1e-9 S) to leave the physical solution unchanged.
        const double GMIN = 1e-9;
        for (const IslandComp& c : L->comps) {
            switch (c.kind) {
            case Kind::Resistor: {
                // Floor R to a tiny value so an ideal jumper (R -> 0) does not
                // divide by zero in the residual.
                double R = c.value;
                if (std::fabs(R) < 1e-12) {
                    R = (R < 0.0) ? -1e-12 : 1e-12;
                }
                const double i = (ePot(c.a) - ePot(c.b)) / R;
                kcl[static_cast<size_t>(c.a)] += i;
                kcl[static_cast<size_t>(c.b)] -= i;
                break;
            }
            case Kind::Conductor: {
                const double i = (ePot(c.a) - ePot(c.b)) * c.value; // G*(e_a-e_b)
                kcl[static_cast<size_t>(c.a)] += i;
                kcl[static_cast<size_t>(c.b)] -= i;
                break;
            }
            case Kind::VariableResistor: {
                // Resistance from a signal input; guard against R -> 0.
                double R = (c.port >= 0) ? getInput(ctx, b.nid, c.port, 1.0) : 1.0;
                if (std::fabs(R) < 1e-12) {
                    R = (R < 0.0) ? -1e-12 : 1e-12;
                }
                const double i = (ePot(c.a) - ePot(c.b)) / R;
                kcl[static_cast<size_t>(c.a)] += i;
                kcl[static_cast<size_t>(c.b)] -= i;
                break;
            }
            case Kind::VariableConductor: {
                const double G = (c.port >= 0) ? getInput(ctx, b.nid, c.port, 1.0) : 1.0;
                const double i = (ePot(c.a) - ePot(c.b)) * G;
                kcl[static_cast<size_t>(c.a)] += i;
                kcl[static_cast<size_t>(c.b)] -= i;
                break;
            }
            case Kind::VariableCapacitor: {
                // Exact charge formulation: Q' = i_C, v = Q/C (no dC/dt
                // approximation). C from the input signal, guarded.
                const double Q = xat(c.stateIndex);
                const double iC = xat(c.stateIndex + 1);
                double C = (c.port >= 0) ? getInput(ctx, b.nid, c.port, 1.0) : 1.0;
                if (std::fabs(C) < 1e-15) {
                    C = (C < 0.0) ? -1e-15 : 1e-15;
                }
                out[c.stateIndex] = iC;
                out[c.stateIndex + 1] = (Q / C) - (ePot(c.a) - ePot(c.b));
                kcl[static_cast<size_t>(c.a)] += iC;
                kcl[static_cast<size_t>(c.b)] -= iC;
                break;
            }
            case Kind::VariableInductor: {
                // Exact flux formulation: phi' = v, i = phi/L (no dL/dt
                // approximation). L from the input signal, guarded.
                const double phi = xat(c.stateIndex);
                const double iL = xat(c.stateIndex + 1);
                double Lval = (c.port >= 0) ? getInput(ctx, b.nid, c.port, 1.0) : 1.0;
                if (std::fabs(Lval) < 1e-15) {
                    Lval = (Lval < 0.0) ? -1e-15 : 1e-15;
                }
                out[c.stateIndex] = ePot(c.a) - ePot(c.b);
                out[c.stateIndex + 1] = iL - (phi / Lval);
                kcl[static_cast<size_t>(c.a)] += iL;
                kcl[static_cast<size_t>(c.b)] -= iL;
                break;
            }
            case Kind::Vccs: {
                // current gain*(e_c - e_d) injected a -> b
                const double i = c.value * (ePot(c.c) - ePot(c.d));
                kcl[static_cast<size_t>(c.a)] += i;
                kcl[static_cast<size_t>(c.b)] -= i;
                break;
            }
            case Kind::OpAmp: {
                // Ideal op-amp (nullor): inputs a=+, b=-, output c. The
                // residual forces the virtual short e_a = e_b; the output
                // current i_out (the algebraic state) is injected at node c
                // and drawn from the reference. Input pins draw no current.
                const double iout = xat(c.stateIndex);
                out[c.stateIndex] = ePot(c.a) - ePot(c.b);
                kcl[static_cast<size_t>(c.c)] += iout;
                break;
            }
            case Kind::Short: {
                // Ideal short: residual e_a - e_b = 0; branch current free.
                const double iBr = xat(c.stateIndex);
                out[c.stateIndex] = ePot(c.a) - ePot(c.b);
                kcl[static_cast<size_t>(c.a)] += iBr;
                kcl[static_cast<size_t>(c.b)] -= iBr;
                break;
            }
            case Kind::Idle: {
                // Ideal open branch: residual i = 0; voltage free.
                const double iBr = xat(c.stateIndex);
                out[c.stateIndex] = iBr;
                kcl[static_cast<size_t>(c.a)] += iBr;
                kcl[static_cast<size_t>(c.b)] -= iBr;
                break;
            }
            case Kind::Gyrator: {
                // Gyrator: i1 = G2 v2, i2 = -G1 v1 (no own state; the port
                // currents are explicit functions of the other port voltage).
                const double v1 = ePot(c.a) - ePot(c.b);
                const double v2 = ePot(c.c) - ePot(c.d);
                const double i1 = c.value2 * v2; // G2 * v2
                const double i2 = -c.value * v1; // -G1 * v1
                kcl[static_cast<size_t>(c.a)] += i1;
                kcl[static_cast<size_t>(c.b)] -= i1;
                kcl[static_cast<size_t>(c.c)] += i2;
                kcl[static_cast<size_t>(c.d)] -= i2;
                break;
            }
            case Kind::Switch: {
                // Ideal switch: control from input port. Closed (control >
                // 0.5) -> short, residual v = e_a - e_b = 0; open -> residual
                // i = 0. i is the algebraic branch state either way.
                const double iBr = xat(c.stateIndex);
                double ctrl = 1.0;
                if (c.port >= 0) {
                    ctrl = getInput(ctx, b.nid, c.port, 1.0);
                }
                if (ctrl > 0.5) {
                    out[c.stateIndex] = ePot(c.a) - ePot(c.b);
                } else {
                    out[c.stateIndex] = iBr;
                }
                kcl[static_cast<size_t>(c.a)] += iBr;
                kcl[static_cast<size_t>(c.b)] -= iBr;
                break;
            }
            case Kind::Diode: {
                // Shockley: i = Is (exp(vd/Vt) - 1), vd = e_a - e_b, a -> b.
                // Clamp the exponent (SPICE-style) so a large forward bias
                // during a Newton overshoot cannot overflow the residual.
                const double vd = ePot(c.a) - ePot(c.b);
                double arg = vd / c.vt;
                if (arg > 40.0) {
                    arg = 40.0;
                }
                const double i = c.value * (std::exp(arg) - 1.0) + GMIN * vd;
                kcl[static_cast<size_t>(c.a)] += i;
                kcl[static_cast<size_t>(c.b)] -= i;
                break;
            }
            case Kind::ZDiode: {
                // Zener: forward Shockley plus a reverse breakdown branch that
                // turns on for vd < -Vz. i = Is(exp(vd/Vt)-1) - Is(exp(-(vd+Vz)/Vt)-1).
                const double vd = ePot(c.a) - ePot(c.b);
                double af = vd / c.vt;
                if (af > 40.0) {
                    af = 40.0;
                }
                double ar = -(vd + c.value2) / c.vt;
                if (ar > 40.0) {
                    ar = 40.0;
                }
                const double i
                    = c.value * (std::exp(af) - 1.0) - c.value * (std::exp(ar) - 1.0) + GMIN * vd;
                kcl[static_cast<size_t>(c.a)] += i;
                kcl[static_cast<size_t>(c.b)] -= i;
                break;
            }
            case Kind::IdealDiode: {
                // Hybrid ideal diode, a -> b, parametrized by its algebraic state
                // s under a LATCHED mode. Off: v = Vknee + s, i = Goff v. On:
                // v = Vknee + Ron s, i = s + Goff Vknee. Both v(s) and i(s) are
                // continuous at s = 0. The mode is a discrete state of the layout
                // (flipped at a located knee event or by the settle after a
                // solve), never a function of the Newton iterate, so each solve
                // is linear and a stale Jacobian cannot bounce between modes.
                const double s = xat(c.stateIndex);
                const size_t ci = static_cast<size_t>(&c - L->comps.data());
                const bool off = (ci < L->diodeMode.size()) ? (L->diodeMode[ci] == 0) : (s < 0.0);
                const double vs = c.value3 + s * (off ? 1.0 : c.value);
                const double is = (off ? c.value2 * s : s) + c.value2 * c.value3;
                out[c.stateIndex] = (ePot(c.a) - ePot(c.b)) - vs;
                kcl[static_cast<size_t>(c.a)] += is;
                kcl[static_cast<size_t>(c.b)] -= is;
                break;
            }
            case Kind::Npn:
            case Kind::Pnp: {
                // Ebers-Moll BJT (a=C, b=E, c=B). s = +1 for NPN, -1 for PNP
                // flips the junction voltages and the terminal currents.
                const double s = (c.kind == Kind::Npn) ? 1.0 : -1.0;
                double abe = s * (ePot(c.c) - ePot(c.b)) / c.vt;
                double abc = s * (ePot(c.c) - ePot(c.a)) / c.vt;
                if (abe > 40.0) {
                    abe = 40.0;
                }
                if (abc > 40.0) {
                    abc = 40.0;
                }
                const double ef = std::exp(abe) - 1.0;
                const double er = std::exp(abc) - 1.0;
                const double icur = s * (c.value * (ef - er) - (c.value / c.value3) * er);
                const double ibase = s * ((c.value / c.value2) * ef + (c.value / c.value3) * er);
                // GMIN across both junctions (base-emitter, base-collector) so
                // the cold-start Jacobian is well conditioned.
                const double gBE = GMIN * (ePot(c.c) - ePot(c.b));
                const double gBC = GMIN * (ePot(c.c) - ePot(c.a));
                kcl[static_cast<size_t>(c.a)] += icur - gBC; // collector
                kcl[static_cast<size_t>(c.c)] += ibase + gBE + gBC; // base
                kcl[static_cast<size_t>(c.b)] -= (icur + ibase + gBE); // emitter
                break;
            }
            case Kind::Nmos:
            case Kind::Pmos: {
                // Square-law MOSFET (a=D, b=S, c=G). s = +1 NMOS / -1 PMOS
                // flips gate/drain overdrive and the drain current. The gate
                // draws no current; only D and S get a stamp.
                const double s = (c.kind == Kind::Nmos) ? 1.0 : -1.0;
                const double vgs = s * (ePot(c.c) - ePot(c.b));
                const double vds = s * (ePot(c.a) - ePot(c.b));
                const double vov = vgs - c.value2;
                double id = 0.0;
                if (vov > 0.0) {
                    id = (vds < vov) ? c.value * (vov * vds - 0.5 * vds * vds)
                                     : 0.5 * c.value * vov * vov;
                }
                // GMIN across drain-source keeps the channel Jacobian non-singular.
                const double gDS = GMIN * (ePot(c.a) - ePot(c.b));
                kcl[static_cast<size_t>(c.a)] += s * id + gDS; // drain
                kcl[static_cast<size_t>(c.b)] -= s * id + gDS; // source
                break;
            }
            case Kind::Radiation: {
                // Thermal radiation (Stefan-Boltzmann): Q = Gr (T_a^4 - T_b^4),
                // with the temperatures the node potentials. Nonlinear but
                // smooth; the DAE Newton handles it directly.
                const double ta = ePot(c.a);
                const double tb = ePot(c.b);
                const double q = c.value * (ta * ta * ta * ta - tb * tb * tb * tb);
                kcl[static_cast<size_t>(c.a)] += q;
                kcl[static_cast<size_t>(c.b)] -= q;
                break;
            }
            case Kind::Vcvs: {
                const double iBr = xat(c.stateIndex);
                // residual: (e_a - e_b) - gain*(e_c - e_d) = 0
                out[c.stateIndex] = (ePot(c.a) - ePot(c.b)) - c.value * (ePot(c.c) - ePot(c.d));
                kcl[static_cast<size_t>(c.a)] += iBr;
                kcl[static_cast<size_t>(c.b)] -= iBr;
                break;
            }
            case Kind::Ccvs: {
                // i_out (si), i_sense (si+1). Output v_ab = gain i_sense; the
                // sense branch (c,d) is a short (e_c = e_d) carrying i_sense.
                const double iOut = xat(c.stateIndex);
                const double iSense = xat(c.stateIndex + 1);
                out[c.stateIndex] = (ePot(c.a) - ePot(c.b)) - c.value * iSense;
                out[c.stateIndex + 1] = ePot(c.c) - ePot(c.d);
                kcl[static_cast<size_t>(c.a)] += iOut;
                kcl[static_cast<size_t>(c.b)] -= iOut;
                kcl[static_cast<size_t>(c.c)] += iSense;
                kcl[static_cast<size_t>(c.d)] -= iSense;
                break;
            }
            case Kind::Cccs: {
                // i_sense (si) through the shorted sense branch (c,d); the
                // output injects gain i_sense from a to b.
                const double iSense = xat(c.stateIndex);
                out[c.stateIndex] = ePot(c.c) - ePot(c.d);
                const double iOut = c.value * iSense;
                kcl[static_cast<size_t>(c.a)] += iOut;
                kcl[static_cast<size_t>(c.b)] -= iOut;
                kcl[static_cast<size_t>(c.c)] += iSense;
                kcl[static_cast<size_t>(c.d)] -= iSense;
                break;
            }
            case Kind::Transformer: {
                // Ideal transformer, primary (a,b) secondary (c,d), ratio n:
                //   v1 = n v2         residual (e_a-e_b) - n (e_c-e_d) = 0
                //   i2 = -n i1        (power conservation)
                // i1 is the algebraic state; secondary current i2 = -n i1.
                const double i1 = xat(c.stateIndex);
                const double i2 = -c.value * i1;
                out[c.stateIndex] = (ePot(c.a) - ePot(c.b)) - c.value * (ePot(c.c) - ePot(c.d));
                kcl[static_cast<size_t>(c.a)] += i1;
                kcl[static_cast<size_t>(c.b)] -= i1;
                kcl[static_cast<size_t>(c.c)] += i2;
                kcl[static_cast<size_t>(c.d)] -= i2;
                break;
            }
            case Kind::Capacitor: {
                const double vC = xat(c.stateIndex);
                const double iC = xat(c.stateIndex + 1);
                out[c.stateIndex] = iC / c.value;
                out[c.stateIndex + 1] = vC - (ePot(c.a) - ePot(c.b));
                kcl[static_cast<size_t>(c.a)] += iC;
                kcl[static_cast<size_t>(c.b)] -= iC;
                break;
            }
            case Kind::Inductor: {
                const double iL = xat(c.stateIndex);
                out[c.stateIndex] = (ePot(c.a) - ePot(c.b)) / c.value;
                kcl[static_cast<size_t>(c.a)] += iL;
                kcl[static_cast<size_t>(c.b)] -= iL;
                break;
            }
            case Kind::VSource:
            case Kind::CurrentSensor: {
                const double iBr = xat(c.stateIndex);
                double V = c.value;
                if (c.port >= 0) {
                    V = getInput(ctx, b.nid, c.port, 0.0);
                }
                out[c.stateIndex] = (ePot(c.a) - ePot(c.b)) - V;
                kcl[static_cast<size_t>(c.a)] += iBr;
                kcl[static_cast<size_t>(c.b)] -= iBr;
                break;
            }
            case Kind::ISource: {
                double I = c.value;
                if (c.port >= 0) {
                    I = getInput(ctx, b.nid, c.port, 0.0);
                }
                kcl[static_cast<size_t>(c.a)] += I;
                kcl[static_cast<size_t>(c.b)] -= I;
                break;
            }
            case Kind::VoltageSensor:
            case Kind::PotentialSensor:
                break;
            }
        }
        // Node KCL residuals (algebraic): out[e_n] = sum of currents leaving n.
        for (int n = 1; n < L->numNodes; ++n) {
            const int si = L->eIndex[static_cast<size_t>(n)];
            if (si >= 0) {
                out[si] = kcl[static_cast<size_t>(n)];
            }
        }
    }
    //=============================================================================
    static void
    islandImplicitAdvance(
        SimCtx& ctx, const Block& b, BlockState& st, IslandLayout* L, bool seedOnly)
    {
        const int N = L->numStates;
        if (N <= 0) {
            return;
        }
        const double dt = (ctx.dt > 0.0) ? ctx.dt : 1.0e-3;
        const bool firstPass = seedOnly || (L->lastAdvanceT <= -1.0e299);
        if (!seedOnly && !firstPass && (ctx.t - L->lastAdvanceT) < dt * (1.0 - 1e-6)) {
            // Repeated OUTPUT pass within one major step (an algebraic
            // re-evaluation, a located event mid-interval): hold the state.
            return;
        }
        if ((int)st.vec.size() < N) {
            st.vec.resize(static_cast<size_t>(N), 0.0);
        }
        // Current state: the solver loop's global slice when one is active
        // (kept in sync below), else the per-block vector (fixed-step loop).
        double* slice = blockX(ctx, b.nid);
        std::vector<double> xw(static_cast<size_t>(N), 0.0);
        for (int k = 0; k < N; ++k) {
            xw[static_cast<size_t>(k)] = slice ? slice[k] : st.vec[static_cast<size_t>(k)];
        }
        const std::vector<double> xprev = xw;
        std::vector<double> G(static_cast<size_t>(N), 0.0);
        std::vector<double> F(static_cast<size_t>(N), 0.0);
        std::vector<double> Fp(static_cast<size_t>(N), 0.0);
        std::vector<double> J(static_cast<size_t>(N) * N, 0.0);
        std::vector<double> rhs(static_cast<size_t>(N), 0.0);
        // Residual over the trial state xw. The first pass pins the
        // differential rows to their initial conditions and solves only the
        // algebraic components (IDACalcIC-style consistency, so the t = 0
        // sample of a branch current / node potential is physically correct);
        // later passes are one backward-Euler step over dt.
        auto evalResidual = [&](std::vector<double>& out) {
            islandStampRaw(ctx, b, L, xw.data(), st.vec, G.data());
            for (int i = 0; i < N; ++i) {
                const bool diff = (i < (int)L->id.size()) && (L->id[static_cast<size_t>(i)] > 0.5);
                if (diff) {
                    out[static_cast<size_t>(i)] = firstPass
                        ? (xw[static_cast<size_t>(i)] - xprev[static_cast<size_t>(i)])
                        : ((xw[static_cast<size_t>(i)] - xprev[static_cast<size_t>(i)]) / dt
                              - G[static_cast<size_t>(i)]);
                } else {
                    out[static_cast<size_t>(i)] = G[static_cast<size_t>(i)];
                }
            }
        };
        // Outer pass over the latched diode modes: each Newton solve runs in
        // fixed modes (linear for an ideal diode); when the solution disagrees
        // with a mode (off above its knee, on with reverse current) flip it and
        // solve again. Bounded; an island without latched modes runs once.
        for (int modePass = 0; modePass < 8; ++modePass) {
            for (int iter = 0; iter < 60; ++iter) {
                evalResidual(F);
                double nrm = 0.0;
                for (int i = 0; i < N; ++i) {
                    nrm += F[static_cast<size_t>(i)] * F[static_cast<size_t>(i)];
                }
                if (nrm < 1e-24) {
                    break;
                }
                for (int j = 0; j < N; ++j) {
                    const double xj = xw[static_cast<size_t>(j)];
                    const double h = 1e-7 * std::fabs(xj) + 1e-9;
                    xw[static_cast<size_t>(j)] = xj + h;
                    evalResidual(Fp);
                    xw[static_cast<size_t>(j)] = xj;
                    for (int i = 0; i < N; ++i) {
                        J[static_cast<size_t>(i) * N + j]
                            = (Fp[static_cast<size_t>(i)] - F[static_cast<size_t>(i)]) / h;
                    }
                }
                for (int i = 0; i < N; ++i) {
                    rhs[static_cast<size_t>(i)] = -F[static_cast<size_t>(i)];
                }
                islandGaussSolve(J, rhs, N);
                for (int i = 0; i < N; ++i) {
                    xw[static_cast<size_t>(i)] += rhs[static_cast<size_t>(i)];
                }
            }
            if (!L->hasIdealDiode || !islandSettleDiodeModes(L, xw.data(), st.vec)) {
                break;
            }
        }
        // Never scatter a non-finite iterate into the loop state: hold the
        // previous step instead (the run stays bounded and diagnosable).
        for (int i = 0; i < N; ++i) {
            if (!std::isfinite(xw[static_cast<size_t>(i)])) {
                xw = xprev;
                break;
            }
        }
        for (int k = 0; k < N; ++k) {
            st.vec[static_cast<size_t>(k)] = xw[static_cast<size_t>(k)];
            if (slice) {
                slice[k] = xw[static_cast<size_t>(k)];
            }
        }
        if (!seedOnly) {
            L->lastAdvanceT = ctx.t;
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
