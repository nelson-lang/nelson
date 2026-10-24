//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// mechanicalTranslationalIsland (acausal palette N4, tier A). 1-D translational
// mechanics as a single native block: nodes carry (position, velocity) and the
// through-variable is force. Curated index-1 by construction; every free node
// must carry a mass (Modelica's own guidance), so the system is a plain
// second-order ODE with no algebraic constraint:
//   mass node:  s' = v,  v' = F_net / m
//   spring a-b: F = k (s_a - s_b),  force -F on a, +F on b
//   damper a-b: F = d (v_a - v_b),  force -F on a, +F on b
//   force:      applied force f (constant or signal-driven) on a node
//   fixed:      pins a node to position s0 (reference, no state)
//   sensors:    position / speed on a node (output ports)
// Node 0 is the fixed ground reference (s = 0, v = 0). All states are
// differential, so the block runs under both the `dae` and the fixed-step RK
// solvers; it is registered with continuous states and no algebraic mask.
//
// Params JSON:
//   { "domain":"translational", "nodes":N,
//     "components":[ {"kind":"mass","node":1,"m":1,"s0":1,"v0":0},
//                    {"kind":"spring","a":1,"b":0,"k":4},
//                    {"kind":"damper","a":1,"b":0,"d":1},
//                    {"kind":"force","node":1,"F":10},        // or "port":0
//                    {"kind":"fixed","node":2,"s0":0.5},
//                    {"kind":"positionSensor","node":1,"outPort":0},
//                    {"kind":"speedSensor","node":1,"outPort":1} ] }
// A node that has a `mass` gets two differential states; a node named only by a
// `fixed` gets its prescribed position; node 0 is ground. A free node with no
// mass is rejected at INIT (it would be a massless algebraic junction, outside
// this curated tier).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "continuous_blocks.hpp"
#include <array>
#include <cmath>
#include <functional>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        enum class MKind
        {
            Mass,
            Spring,
            Damper,
            Force,
            Fixed,
            Gear, // ideal gear: pos_a = ratio * pos_b (structural node merge)
            PositionSensor,
            SpeedSensor,
            RelPositionSensor, // pos_a - pos_b between two flanges
            RelSpeedSensor, // vel_a - vel_b between two flanges
            Accelerate, // prescribed acceleration: v' = signal input
            PrescribedSpeed, // prescribed velocity: v = signal input (or constant)
            Friction, // regularised Coulomb friction: F = -Fc tanh(v/vEps)
            Clutch, // regularised clutch: friction on the relative slip a-b
            LinearSpeedForce, // speed-proportional resistance: F = -d v
            QuadraticSpeedForce, // quadratic (drag) resistance: F = -d v |v|
            Force2, // equal/opposite force between two flanges: +F on a, -F on b
            Backlash, // spring with a dead zone (play): F = c * deadzone(s_a - s_b, b/2)
            ElastoGap, // one-sided contact spring-damper across a gap (tanh-gated)
            Brake, // signal-actuated friction brake to ground: F = -f_brake tanh(v/vEps)
            Freewheel // one-way clutch: couples a->b only while slip = v_a - v_b > 0
        };
        //=============================================================================
        struct MComp
        {
            MKind kind;
            int a = 0;
            int b = 0;
            double value = 0.0; // m / k / d / F / s0 / ratio / Fc
            double s0 = 0.0; // mass initial position / friction vEps
            double v0 = 0.0; // mass initial velocity
            int port = -1;
            int outPort = -1;
            int stateIndex = -1; // mass: s at stateIndex, v at stateIndex+1
        };
        //=============================================================================
        // The layout groups gear-connected nodes: every node maps to a group
        // root and a ratio so pos_node = ratioToRoot[node] * pos_root. Only a
        // non-fixed root that carries mass (folded J_eff = sum m*ratio^2) gets a
        // (position, velocity) state pair; slaves read the root through the
        // ratio. This keeps a geared drivetrain a plain ODE (structural tier-B,
        // no algebraic constraint).
        struct MechLayout : public IBlockResource
        {
            int numNodes = 0;
            std::vector<MComp> comps;
            std::vector<int> rootOf; // [node] -> gear-group root node
            std::vector<double> ratioToRoot; // [node] -> pos_node / pos_root
            std::vector<int> sIndex; // [root node] -> state index (-1 if none)
            std::vector<double> jEff; // [root node] -> folded effective inertia
            std::vector<double> fixedPos; // [node] -> prescribed position
            std::vector<char> isFixed; // [node] (propagated through gear groups)
            std::vector<int> accelPort; // [root node] -> input port prescribing v'
                                        // (acceleration source); -1 = force balance
            std::vector<int> speedPort; // [root node] -> input port prescribing v
                                        // directly (velocity source); -1 = none
            std::vector<double> speedConst; // [root node] -> constant prescribed v
                                            // (ConstantSpeed) when speedPort < 0
            std::vector<char> isSpeedDriven; // [root node] -> velocity is prescribed
            std::vector<double> ic;
            int numStates = 0;
        };
        //=============================================================================
        MKind
        parseMKind(const std::string& s, bool& ok)
        {
            ok = true;
            if (s == "mass") {
                return MKind::Mass;
            }
            if (s == "spring") {
                return MKind::Spring;
            }
            if (s == "damper") {
                return MKind::Damper;
            }
            if (s == "force") {
                return MKind::Force;
            }
            if (s == "fixed") {
                return MKind::Fixed;
            }
            if (s == "gear") {
                return MKind::Gear;
            }
            if (s == "positionSensor") {
                return MKind::PositionSensor;
            }
            if (s == "speedSensor") {
                return MKind::SpeedSensor;
            }
            if (s == "relPositionSensor") {
                return MKind::RelPositionSensor;
            }
            if (s == "relSpeedSensor") {
                return MKind::RelSpeedSensor;
            }
            if (s == "accelerate") {
                return MKind::Accelerate;
            }
            if (s == "prescribedSpeed") {
                return MKind::PrescribedSpeed;
            }
            if (s == "backlash") {
                return MKind::Backlash;
            }
            if (s == "elastoGap") {
                return MKind::ElastoGap;
            }
            if (s == "brake") {
                return MKind::Brake;
            }
            if (s == "freewheel") {
                return MKind::Freewheel;
            }
            if (s == "friction") {
                return MKind::Friction;
            }
            if (s == "clutch") {
                return MKind::Clutch;
            }
            if (s == "linearSpeedForce") {
                return MKind::LinearSpeedForce;
            }
            if (s == "quadraticSpeedForce") {
                return MKind::QuadraticSpeedForce;
            }
            if (s == "force2") {
                return MKind::Force2;
            }
            ok = false;
            return MKind::Mass;
        }
        //=============================================================================
        int
        mjint(const json& o, const char* key, int dflt)
        {
            if (o.contains(key) && o[key].is_number()) {
                return o[key].get<int>();
            }
            return dflt;
        }
        //=============================================================================
        double
        mjdbl(const json& o, const char* key, double dflt)
        {
            if (o.contains(key) && o[key].is_number()) {
                return o[key].get<double>();
            }
            return dflt;
        }
        //=============================================================================
        MechLayout
        buildMechLayout(const json& params)
        {
            MechLayout L;
            L.numNodes = mjint(params, "nodes", 1);
            if (L.numNodes < 1) {
                L.numNodes = 1;
            }
            const int N = L.numNodes;
            L.rootOf.resize(static_cast<size_t>(N));
            L.ratioToRoot.assign(static_cast<size_t>(N), 1.0);
            L.sIndex.assign(static_cast<size_t>(N), -1);
            L.jEff.assign(static_cast<size_t>(N), 0.0);
            L.fixedPos.assign(static_cast<size_t>(N), 0.0);
            L.isFixed.assign(static_cast<size_t>(N), 0);
            L.accelPort.assign(static_cast<size_t>(N), -1);
            L.speedPort.assign(static_cast<size_t>(N), -1);
            L.speedConst.assign(static_cast<size_t>(N), 0.0);
            L.isSpeedDriven.assign(static_cast<size_t>(N), 0);
            for (int n = 0; n < N; ++n) {
                L.rootOf[static_cast<size_t>(n)] = n;
            }
            L.isFixed[0] = 1;

            // Pass 1: parse every component; collect gear edges and fixed nodes.
            std::vector<std::array<double, 3>> gearEdges; // {a, b, ratio}: pos_a = ratio*pos_b
            if (params.contains("components") && params["components"].is_array()) {
                for (const auto& c : params["components"]) {
                    if (!c.contains("kind") || !c["kind"].is_string()) {
                        continue;
                    }
                    bool ok = false;
                    MComp comp;
                    comp.kind = parseMKind(c["kind"].get<std::string>(), ok);
                    if (!ok) {
                        continue;
                    }
                    comp.a = mjint(c, "a", mjint(c, "node", 0));
                    comp.b = mjint(c, "b", 0);
                    comp.port = mjint(c, "port", -1);
                    comp.outPort = mjint(c, "outPort", -1);
                    switch (comp.kind) {
                    case MKind::Mass:
                        comp.value = mjdbl(c, "m", 1.0);
                        comp.s0 = mjdbl(c, "s0", 0.0);
                        comp.v0 = mjdbl(c, "v0", 0.0);
                        break;
                    case MKind::Spring:
                        comp.value = mjdbl(c, "k", 1.0);
                        break;
                    case MKind::Damper:
                    case MKind::LinearSpeedForce:
                    case MKind::QuadraticSpeedForce:
                        comp.value = mjdbl(c, "d", 1.0);
                        break;
                    case MKind::Force:
                    case MKind::Force2:
                        comp.value = mjdbl(c, "F", 0.0);
                        break;
                    case MKind::Backlash:
                        comp.value = mjdbl(c, "c", 1.0); // contact stiffness
                        // NB "play" (not "b"): the pin node field "b" would collide.
                        comp.s0 = mjdbl(c, "play", 0.0); // total backlash (play) width
                        break;
                    case MKind::ElastoGap:
                        comp.value = mjdbl(c, "c", 1.0); // contact stiffness
                        comp.v0 = mjdbl(c, "d", 0.0); // contact damping
                        comp.s0 = mjdbl(c, "s_rel0", 0.0); // undeformed gap length
                        break;
                    case MKind::Brake:
                        comp.value = mjdbl(c, "Fmax", 1.0); // peak friction if no signal
                        comp.s0 = mjdbl(c, "vEps", 1e-3); // regularisation velocity
                        if (comp.s0 <= 0.0) {
                            comp.s0 = 1e-3;
                        }
                        break;
                    case MKind::Freewheel:
                        comp.value = mjdbl(c, "d", 100.0); // one-way coupling strength
                        comp.s0 = mjdbl(c, "vEps", 1e-3); // slip regularisation
                        if (comp.s0 <= 0.0) {
                            comp.s0 = 1e-3;
                        }
                        break;
                    case MKind::Friction:
                        comp.value = mjdbl(c, "Fc", 1.0); // Coulomb force magnitude
                        comp.s0 = mjdbl(c, "vEps", 1e-3); // regularisation velocity
                        if (comp.s0 <= 0.0) {
                            comp.s0 = 1e-3;
                        }
                        break;
                    case MKind::Clutch:
                        comp.value = mjdbl(c, "Fc", 1.0); // max transmissible force
                        comp.s0 = mjdbl(c, "vEps", 1e-3); // regularisation velocity
                        if (comp.s0 <= 0.0) {
                            comp.s0 = 1e-3;
                        }
                        // Optional engagement signal (0..1) scales Fc; else fully
                        // engaged.
                        comp.port = mjint(c, "port", -1);
                        break;
                    case MKind::Fixed:
                        comp.value = mjdbl(c, "s0", 0.0);
                        if (comp.a >= 0 && comp.a < N) {
                            L.isFixed[comp.a] = 1;
                            L.fixedPos[comp.a] = comp.value;
                        }
                        break;
                    case MKind::Gear:
                        comp.value = mjdbl(c, "ratio", 1.0);
                        if (comp.a > 0 && comp.a < N && comp.b > 0 && comp.b < N
                            && comp.value != 0.0) {
                            gearEdges.push_back({ static_cast<double>(comp.a),
                                static_cast<double>(comp.b), comp.value });
                        }
                        break;
                    case MKind::Accelerate:
                        // Prescribes v' from the input port. Pass 3 allocates (s, v)
                        // even without mass.
                        if (comp.a > 0 && comp.a < N && comp.port >= 0) {
                            L.accelPort[static_cast<size_t>(comp.a)] = comp.port;
                        }
                        comp.s0 = mjdbl(c, "s0", 0.0);
                        comp.v0 = mjdbl(c, "v0", 0.0);
                        break;
                    case MKind::PrescribedSpeed:
                        // Prescribes v from the input port (Speed) or a constant
                        // (ConstantSpeed). Pass 3 allocates (s, v) even without mass;
                        // s' = v_prescribed and the v-state is unused.
                        comp.value = mjdbl(c, "v", 0.0); // constant speed (ConstantSpeed)
                        if (comp.a > 0 && comp.a < N) {
                            L.isSpeedDriven[static_cast<size_t>(comp.a)] = 1;
                            if (comp.port >= 0) {
                                L.speedPort[static_cast<size_t>(comp.a)] = comp.port;
                            } else {
                                L.speedConst[static_cast<size_t>(comp.a)] = comp.value;
                            }
                        }
                        comp.s0 = mjdbl(c, "s0", 0.0);
                        break;
                    case MKind::PositionSensor:
                    case MKind::SpeedSensor:
                    case MKind::RelPositionSensor:
                    case MKind::RelSpeedSensor:
                        break;
                    }
                    L.comps.push_back(comp);
                }
            }

            // Pass 2: merge gear-connected nodes. Root = lowest node index in the
            // group; ratioToRoot[node] = pos_node / pos_root. A gear edge
            // pos_a = ratio * pos_b relates the two ratios, found by repeated
            // relaxation (groups are tiny).
            auto findRoot = [&](int n) {
                while (L.rootOf[static_cast<size_t>(n)] != n) {
                    n = L.rootOf[static_cast<size_t>(n)];
                }
                return n;
            };
            for (const auto& e : gearEdges) {
                const int a = static_cast<int>(e[0]);
                const int b = static_cast<int>(e[1]);
                const int ra = findRoot(a);
                const int rb = findRoot(b);
                if (ra == rb) {
                    continue; // already merged (redundant gear); skip
                }
                // Merge the higher-index root under the lower-index one so the
                // root stays the minimum node index; recompute ratios by a BFS
                // relaxation over gear edges afterwards.
                if (ra < rb) {
                    L.rootOf[static_cast<size_t>(rb)] = ra;
                } else {
                    L.rootOf[static_cast<size_t>(ra)] = rb;
                }
            }
            // Path-compress and compute ratioToRoot by relaxing gear edges until
            // stable (each edge sets one endpoint's ratio from the other).
            for (int n = 0; n < N; ++n) {
                L.rootOf[static_cast<size_t>(n)] = findRoot(n);
            }
            L.ratioToRoot.assign(static_cast<size_t>(N), 0.0);
            for (int n = 0; n < N; ++n) {
                if (L.rootOf[static_cast<size_t>(n)] == n) {
                    L.ratioToRoot[static_cast<size_t>(n)] = 1.0;
                }
            }
            for (int iter = 0; iter < N + 2; ++iter) {
                for (const auto& e : gearEdges) {
                    const int a = static_cast<int>(e[0]);
                    const int b = static_cast<int>(e[1]);
                    const double ratio = e[2]; // pos_a = ratio * pos_b
                    // pos_a = ratioToRoot[a]*pos_root, pos_b = ratioToRoot[b]*pos_root
                    // => ratioToRoot[a] = ratio * ratioToRoot[b].
                    if (L.ratioToRoot[static_cast<size_t>(a)] == 0.0
                        && L.ratioToRoot[static_cast<size_t>(b)] != 0.0) {
                        L.ratioToRoot[static_cast<size_t>(a)]
                            = ratio * L.ratioToRoot[static_cast<size_t>(b)];
                    } else if (L.ratioToRoot[static_cast<size_t>(b)] == 0.0
                        && L.ratioToRoot[static_cast<size_t>(a)] != 0.0) {
                        L.ratioToRoot[static_cast<size_t>(b)]
                            = L.ratioToRoot[static_cast<size_t>(a)] / ratio;
                    }
                }
            }
            for (int n = 0; n < N; ++n) {
                if (L.ratioToRoot[static_cast<size_t>(n)] == 0.0) {
                    L.ratioToRoot[static_cast<size_t>(n)] = 1.0;
                }
            }

            // Propagate fixed: if any node in a gear group is fixed, the whole
            // group is pinned (rigid). fixedPos of a slave scales by its ratio.
            for (int n = 0; n < N; ++n) {
                const int r = L.rootOf[static_cast<size_t>(n)];
                if (L.isFixed[static_cast<size_t>(n)] && !L.isFixed[static_cast<size_t>(r)]) {
                    // move the pin to the root frame
                    L.isFixed[static_cast<size_t>(r)] = 1;
                    L.fixedPos[static_cast<size_t>(r)] = L.fixedPos[static_cast<size_t>(n)]
                        / L.ratioToRoot[static_cast<size_t>(n)];
                }
            }
            for (int n = 0; n < N; ++n) {
                if (L.isFixed[static_cast<size_t>(L.rootOf[static_cast<size_t>(n)])]) {
                    L.isFixed[static_cast<size_t>(n)] = 1;
                }
            }

            // Pass 3: fold mass into group roots (J_eff = sum m*ratio^2) and
            // allocate two states per non-fixed root that carries mass.
            std::vector<double> rootS0(static_cast<size_t>(N), 0.0);
            std::vector<double> rootV0(static_cast<size_t>(N), 0.0);
            std::vector<char> rootHasIc(static_cast<size_t>(N), 0);
            for (const MComp& c : L.comps) {
                if (c.kind != MKind::Mass || c.a <= 0 || c.a >= N) {
                    continue;
                }
                const int r = L.rootOf[static_cast<size_t>(c.a)];
                const double ratio = L.ratioToRoot[static_cast<size_t>(c.a)];
                L.jEff[static_cast<size_t>(r)] += c.value * ratio * ratio;
                if (!rootHasIc[static_cast<size_t>(r)]) {
                    rootS0[static_cast<size_t>(r)] = c.s0 / ratio;
                    rootV0[static_cast<size_t>(r)] = c.v0 / ratio;
                    rootHasIc[static_cast<size_t>(r)] = 1;
                }
            }
            // Acceleration-source nodes get (s, v) states too (their v' is the
            // signal, not a force balance), seeded from the source's s0/v0.
            for (const MComp& c : L.comps) {
                if (c.kind != MKind::Accelerate || c.a <= 0 || c.a >= N) {
                    continue;
                }
                const int r = L.rootOf[static_cast<size_t>(c.a)];
                if (!rootHasIc[static_cast<size_t>(r)]) {
                    rootS0[static_cast<size_t>(r)] = c.s0;
                    rootV0[static_cast<size_t>(r)] = c.v0;
                    rootHasIc[static_cast<size_t>(r)] = 1;
                }
            }
            // Prescribed-velocity nodes get (s, v) states too (position integrates
            // the prescribed speed; the v-state is unused), seeded from s0.
            for (const MComp& c : L.comps) {
                if (c.kind != MKind::PrescribedSpeed || c.a <= 0 || c.a >= N) {
                    continue;
                }
                const int r = L.rootOf[static_cast<size_t>(c.a)];
                if (!rootHasIc[static_cast<size_t>(r)]) {
                    rootS0[static_cast<size_t>(r)] = c.s0;
                    rootV0[static_cast<size_t>(r)] = 0.0;
                    rootHasIc[static_cast<size_t>(r)] = 1;
                }
            }
            int idx = 0;
            for (int r = 0; r < N; ++r) {
                const bool driven = (L.accelPort[static_cast<size_t>(r)] >= 0)
                    || (L.isSpeedDriven[static_cast<size_t>(r)] != 0);
                if (r == L.rootOf[static_cast<size_t>(r)] && !L.isFixed[static_cast<size_t>(r)]
                    && (L.jEff[static_cast<size_t>(r)] > 0.0 || driven)) {
                    L.sIndex[static_cast<size_t>(r)] = idx;
                    L.ic.push_back(rootS0[static_cast<size_t>(r)]);
                    L.ic.push_back(rootV0[static_cast<size_t>(r)]);
                    idx += 2;
                }
            }
            L.numStates = idx;
            return L;
        }
        //=============================================================================
        // RK4 codegen for the mechanical island. It is a plain ODE (no algebraic
        // constraint), so it lowers to the global rhs()+RK4 scheme with
        // continuousWidth = numStates. The C and Rust backends share this walk and
        // differ only in a few syntactic hooks (state access, tanh, decl).
        //=============================================================================
        struct MechEmitLang
        {
            bool rust = false;
            // state field access: s->mech_x_<id>_<i> (C) / s.mech_x_<id>_<i>
            std::string
            sref(const std::string& id, int i) const
            {
                return (rust ? "s.mech_x_" : "s->mech_x_") + id + "_" + std::to_string(i);
            }
            std::string
            tanh(const std::string& e) const
            {
                // Rust: route through the `libm` crate (like fabs below). The
                // generated crate is `#![no_std]`, where the `f64::tanh` inherent
                // method does not exist -- only `core` float ops do -- so a
                // method-form `(e).tanh()` fails to compile (E0599).
                return rust ? ("libm::tanh(" + e + ")") : ("tanh(" + e + ")");
            }
            std::string
            fabs(const std::string& e) const
            {
                return rust ? ("libm::fabs(" + e + ")") : ("fabs(" + e + ")");
            }
            std::string
            fmax(const std::string& x, const std::string& y) const
            {
                return rust ? ("(" + x + ").max(" + y + ")") : ("fmax(" + x + ", " + y + ")");
            }
            std::string
            fmin(const std::string& x, const std::string& y) const
            {
                return rust ? ("(" + x + ").min(" + y + ")") : ("fmin(" + x + ", " + y + ")");
            }
            std::string
            declLocal(const std::string& name, const std::string& init) const
            {
                return rust ? ("let mut " + name + " = " + init + ";")
                            : ("double " + name + " = " + init + ";");
            }
        };
        //=============================================================================
        // Output variable name for a block port: out_<id> for port 0, out_<id>_p<p>
        // otherwise. a.id is already sanitized.
        static std::string
        mechOutVar(const std::string& id, int port)
        {
            return port > 0 ? ("out_" + id + "_p" + std::to_string(port)) : ("out_" + id);
        }
        //=============================================================================
        static void
        emitMechOutputs(const BlockCodegenArgs& a, const MechLayout& L,
            const std::function<std::string(int, int)>& stateAt)
        {
            for (const MComp& c : L.comps) {
                if (c.outPort < 0) {
                    continue;
                }
                const std::string ov = mechOutVar(a.id, c.outPort);
                if (c.kind == MKind::PositionSensor) {
                    a.line(ov + " = " + stateAt(c.a, 0) + ";");
                } else if (c.kind == MKind::SpeedSensor) {
                    a.line(ov + " = " + stateAt(c.a, 1) + ";");
                } else if (c.kind == MKind::RelPositionSensor) {
                    a.line(ov + " = (" + stateAt(c.a, 0) + ") - (" + stateAt(c.b, 0) + ");");
                } else if (c.kind == MKind::RelSpeedSensor) {
                    a.line(ov + " = (" + stateAt(c.a, 1) + ") - (" + stateAt(c.b, 1) + ");");
                }
            }
        }
        //=============================================================================
        // Each state's derivative expression goes through emitDeriv(localIndex, expr),
        // so the RK4 seam (array write) and the forward-Euler step (temp + advance)
        // share the same dynamics.
        static void
        emitMechDeriv(const BlockCodegenArgs& a, const MechLayout& L, const MechEmitLang& lang,
            const std::function<std::string(int, int)>& stateAt,
            const std::function<std::string(int)>& inExpr,
            const std::function<void(int, const std::string&)>& emitDeriv)
        {
            const std::string id = a.id;
            auto num = [&](double v) { return a.fmt(v); };
            for (int r = 0; r < L.numNodes; ++r) {
                const int si = L.sIndex[static_cast<size_t>(r)];
                if (si < 0 || L.accelPort[static_cast<size_t>(r)] >= 0
                    || L.isSpeedDriven[static_cast<size_t>(r)]) {
                    continue;
                }
                a.line(lang.declLocal("__fm_" + id + "_" + std::to_string(si), "0.0"));
            }
            auto applyForce = [&](int node, const std::string& sign, const std::string& fvar) {
                if (node <= 0 || L.isFixed[static_cast<size_t>(node)]) {
                    return;
                }
                const int r = L.rootOf[static_cast<size_t>(node)];
                const int si = L.sIndex[static_cast<size_t>(r)];
                if (si < 0 || L.accelPort[static_cast<size_t>(r)] >= 0
                    || L.isSpeedDriven[static_cast<size_t>(r)]) {
                    return;
                }
                const double ratio = L.ratioToRoot[static_cast<size_t>(node)];
                std::string term = sign + fvar;
                if (ratio != 1.0) {
                    term += " * " + num(ratio);
                }
                a.line("__fm_" + id + "_" + std::to_string(si) + " += " + term + ";");
            };
            int tmp = 0;
            for (const MComp& c : L.comps) {
                const std::string fv = "__Fc_" + id + "_" + std::to_string(tmp++);
                switch (c.kind) {
                case MKind::Spring:
                    a.line(lang.declLocal(fv,
                        num(c.value) + " * ((" + stateAt(c.a, 0) + ") - (" + stateAt(c.b, 0)
                            + "))"));
                    applyForce(c.a, "-", fv);
                    applyForce(c.b, "", fv);
                    break;
                case MKind::Damper:
                    a.line(lang.declLocal(fv,
                        num(c.value) + " * ((" + stateAt(c.a, 1) + ") - (" + stateAt(c.b, 1)
                            + "))"));
                    applyForce(c.a, "-", fv);
                    applyForce(c.b, "", fv);
                    break;
                case MKind::Force:
                    a.line(lang.declLocal(fv, c.port >= 0 ? inExpr(c.port) : num(c.value)));
                    applyForce(c.a, "", fv);
                    break;
                case MKind::Force2:
                    a.line(lang.declLocal(fv, c.port >= 0 ? inExpr(c.port) : num(c.value)));
                    applyForce(c.a, "", fv);
                    applyForce(c.b, "-", fv);
                    break;
                case MKind::Friction:
                    a.line(lang.declLocal(fv,
                        num(c.value) + " * "
                            + lang.tanh("(" + stateAt(c.a, 1) + ") / " + num(c.s0))));
                    applyForce(c.a, "-", fv);
                    break;
                case MKind::LinearSpeedForce:
                    a.line(lang.declLocal(fv, num(c.value) + " * (" + stateAt(c.a, 1) + ")"));
                    applyForce(c.a, "-", fv);
                    break;
                case MKind::QuadraticSpeedForce:
                    a.line(lang.declLocal(fv,
                        num(c.value) + " * (" + stateAt(c.a, 1) + ") * "
                            + lang.fabs("(" + stateAt(c.a, 1) + ")")));
                    applyForce(c.a, "-", fv);
                    break;
                case MKind::Clutch: {
                    const std::string eng = c.port >= 0 ? inExpr(c.port) : std::string("1.0");
                    a.line(lang.declLocal(fv,
                        num(c.value) + " * (" + eng + ") * "
                            + lang.tanh("((" + stateAt(c.a, 1) + ") - (" + stateAt(c.b, 1) + ")) / "
                                + num(c.s0))));
                    applyForce(c.a, "-", fv);
                    applyForce(c.b, "", fv);
                    break;
                }
                case MKind::Backlash: {
                    const std::string srel
                        = "((" + stateAt(c.a, 0) + ") - (" + stateAt(c.b, 0) + "))";
                    const std::string half = num(0.5 * c.s0);
                    a.line(lang.declLocal(fv,
                        num(c.value) + " * (" + lang.fmax("0.0", srel + " - " + half) + " + "
                            + lang.fmin("0.0", srel + " + " + half) + ")"));
                    applyForce(c.a, "-", fv);
                    applyForce(c.b, "", fv);
                    break;
                }
                case MKind::ElastoGap: {
                    const std::string srel
                        = "((" + stateAt(c.a, 0) + ") - (" + stateAt(c.b, 0) + "))";
                    const std::string vrel
                        = "((" + stateAt(c.a, 1) + ") - (" + stateAt(c.b, 1) + "))";
                    const std::string pen = "(" + num(c.s0) + " - " + srel + ")";
                    const std::string gate = "(0.5 * (1.0 + " + lang.tanh(pen + " / 0.001") + "))";
                    a.line(lang.declLocal(fv,
                        "(" + num(c.value) + " * " + pen + " - " + num(c.v0) + " * " + vrel + ") * "
                            + gate));
                    applyForce(c.a, "", fv);
                    applyForce(c.b, "-", fv);
                    break;
                }
                case MKind::Brake: {
                    const std::string fmax = c.port >= 0 ? inExpr(c.port) : num(c.value);
                    a.line(lang.declLocal(fv,
                        "(" + fmax + ") * "
                            + lang.tanh("(" + stateAt(c.a, 1) + ") / " + num(c.s0))));
                    applyForce(c.a, "-", fv);
                    break;
                }
                case MKind::Freewheel: {
                    const std::string slip
                        = "((" + stateAt(c.a, 1) + ") - (" + stateAt(c.b, 1) + "))";
                    const std::string gate
                        = "(0.5 * (1.0 + " + lang.tanh(slip + " / " + num(c.s0)) + "))";
                    a.line(lang.declLocal(fv, num(c.value) + " * " + slip + " * " + gate));
                    applyForce(c.a, "-", fv);
                    applyForce(c.b, "", fv);
                    break;
                }
                default:
                    break;
                }
            }
            for (int r = 0; r < L.numNodes; ++r) {
                const int si = L.sIndex[static_cast<size_t>(r)];
                if (si < 0) {
                    continue;
                }
                if (L.isSpeedDriven[static_cast<size_t>(r)]) {
                    // Prescribed velocity: s' = v_prescribed; v-state frozen.
                    const int sp = L.speedPort[static_cast<size_t>(r)];
                    emitDeriv(si, sp >= 0 ? inExpr(sp) : num(L.speedConst[static_cast<size_t>(r)]));
                    emitDeriv(si + 1, "0.0");
                    continue;
                }
                emitDeriv(si, lang.sref(id, si + 1));
                const int ap = L.accelPort[static_cast<size_t>(r)];
                if (ap >= 0) {
                    emitDeriv(si + 1, inExpr(ap));
                } else {
                    emitDeriv(si + 1,
                        "__fm_" + id + "_" + std::to_string(si) + " / "
                            + num(L.jEff[static_cast<size_t>(r)]));
                }
            }
        }
        //=============================================================================
        static std::function<std::string(int, int)>
        mechStateAt(const BlockCodegenArgs& a, const MechLayout& L, const MechEmitLang& lang)
        {
            const std::string id = a.id;
            return [&a, &L, lang, id](int node, int which) -> std::string {
                if (node <= 0) {
                    return "0.0";
                }
                if (L.isFixed[static_cast<size_t>(node)]) {
                    return which == 0 ? a.fmt(L.fixedPos[static_cast<size_t>(node)])
                                      : std::string("0.0");
                }
                const int r = L.rootOf[static_cast<size_t>(node)];
                const int si = L.sIndex[static_cast<size_t>(r)];
                if (si < 0) {
                    return "0.0";
                }
                const double ratio = L.ratioToRoot[static_cast<size_t>(node)];
                // Prescribed-velocity node: the velocity read is the signal (or
                // constant), not the (unused) integrated v-state.
                if (which == 1 && L.isSpeedDriven[static_cast<size_t>(r)]) {
                    const int sp = L.speedPort[static_cast<size_t>(r)];
                    const std::string vexpr = (sp >= 0 && sp < static_cast<int>(a.in.size()))
                        ? a.in[static_cast<size_t>(sp)]
                        : a.fmt(L.speedConst[static_cast<size_t>(r)]);
                    return ratio == 1.0 ? vexpr : (a.fmt(ratio) + " * (" + vexpr + ")");
                }
                const std::string s = lang.sref(id, which == 0 ? si : si + 1);
                return ratio == 1.0 ? s : (a.fmt(ratio) + " * " + s);
            };
        }
        //=============================================================================
        static void
        emitMechRk4(const BlockCodegenArgs& a, const MechEmitLang& lang)
        {
            const MechLayout L = buildMechLayout(*a.params);
            const std::string id = a.id;
            const int off = a.stateOffset;
            auto stateAt = mechStateAt(a, L, lang);
            auto inExpr = [&a](int port) -> std::string {
                return (port >= 0 && port < static_cast<int>(a.in.size()))
                    ? a.in[static_cast<size_t>(port)]
                    : std::string("0.0");
            };
            switch (a.rk4Op) {
            case Rk4Gather:
                for (int i = 0; i < L.numStates; ++i) {
                    a.line(
                        a.rk4Arr + "[" + std::to_string(off + i) + "] = " + lang.sref(id, i) + ";");
                }
                break;
            case Rk4Scatter:
                for (int i = 0; i < L.numStates; ++i) {
                    a.line(
                        lang.sref(id, i) + " = " + a.rk4Arr + "[" + std::to_string(off + i) + "];");
                }
                break;
            case Rk4Output:
                emitMechOutputs(a, L, stateAt);
                break;
            case Rk4Deriv:
                emitMechDeriv(a, L, lang, stateAt, inExpr, [&](int i, const std::string& e) {
                    a.line(a.rk4Arr + "[" + std::to_string(off + i) + "] = " + e + ";");
                });
                break;
            case Rk4Clamp:
            default:
                break; // mechanical states are unbounded: nothing to clamp
            }
        }
        //=============================================================================
        // Forward-Euler emission for the non-ode4 fixed-step path.
        static void
        emitMechStep(const BlockCodegenArgs& a, const MechEmitLang& lang)
        {
            const MechLayout L = buildMechLayout(*a.params);
            if (L.numStates <= 0) {
                return;
            }
            const std::string id = a.id;
            auto stateAt = mechStateAt(a, L, lang);
            auto inExpr = [&a](int port) -> std::string {
                return (port >= 0 && port < static_cast<int>(a.in.size()))
                    ? a.in[static_cast<size_t>(port)]
                    : std::string("0.0");
            };
            emitMechOutputs(a, L, stateAt);
            // Derivatives into temps computed from the pre-step state, then the
            // Euler advance (all derivatives read the old state).
            emitMechDeriv(a, L, lang, stateAt, inExpr, [&](int i, const std::string& e) {
                a.line(lang.declLocal("__xd_" + id + "_" + std::to_string(i), e));
            });
            for (int i = 0; i < L.numStates; ++i) {
                a.line(lang.sref(id, i) + " = " + lang.sref(id, i) + " + dt * __xd_" + id + "_"
                    + std::to_string(i) + ";");
            }
        }
        //=============================================================================
        static BlockCodegenTemplate
        makeMechIslandCodegen(bool rust)
        {
            BlockCodegenTemplate t;
            t.emitState = [](const BlockCodegenStateArgs& a) {
                const MechLayout L = buildMechLayout(*a.params);
                for (int i = 0; i < L.numStates; ++i) {
                    a.addState("mech_x_" + a.id + "_" + std::to_string(i),
                        a.fmt(L.ic[static_cast<size_t>(i)]), "");
                }
            };
            t.continuousWidth = [](const BlockCodegenArgs& a) -> int {
                return buildMechLayout(*a.params).numStates;
            };
            const MechEmitLang lang { rust };
            t.emitRk4 = [lang](const BlockCodegenArgs& a) { emitMechRk4(a, lang); };
            t.emitStep = [lang](const BlockCodegenArgs& a) { emitMechStep(a, lang); };
            return t;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCMechanicalIsland()
    {
        return makeMechIslandCodegen(false);
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustMechanicalIsland()
    {
        return makeMechIslandCodegen(true);
    }
    //=============================================================================
    int
    mechanicalIslandStateCount(const Block& b, const ValMap&)
    {
        return buildMechLayout(b.params).numStates;
    }
    //=============================================================================
    bool
    handleMechanicalIsland(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);
        if (phase == Phase::INIT) {
            MechLayout L = buildMechLayout(b.params);
            st.vec.assign(L.ic.begin(), L.ic.end());
            st.external.reset(new MechLayout(std::move(L)));
            return false;
        }
        const MechLayout* L = dynamic_cast<const MechLayout*>(st.external.get());
        if (!L) {
            return false;
        }
        const double* x = blockX(ctx, b.nid);

        auto pos = [&](int node) -> double {
            if (node <= 0) {
                return 0.0;
            }
            if (L->isFixed[static_cast<size_t>(node)]) {
                return L->fixedPos[static_cast<size_t>(node)];
            }
            const int r = L->rootOf[static_cast<size_t>(node)];
            const int si = L->sIndex[static_cast<size_t>(r)];
            if (si < 0) {
                return 0.0;
            }
            const double ratio = L->ratioToRoot[static_cast<size_t>(node)];
            const double sRoot = x ? x[si] : (si < (int)st.vec.size() ? st.vec[si] : 0.0);
            return ratio * sRoot;
        };
        auto prescribedSpeed = [&](int r) -> double {
            return L->speedPort[static_cast<size_t>(r)] >= 0
                ? getInput(ctx, b.nid, L->speedPort[static_cast<size_t>(r)], 0.0)
                : L->speedConst[static_cast<size_t>(r)];
        };
        auto vel = [&](int node) -> double {
            if (node <= 0 || L->isFixed[static_cast<size_t>(node)]) {
                return 0.0;
            }
            const int r = L->rootOf[static_cast<size_t>(node)];
            const double ratio = L->ratioToRoot[static_cast<size_t>(node)];
            // Prescribed-velocity node: the speed is the signal (or constant),
            // not the (unused) integrated v-state.
            if (L->isSpeedDriven[static_cast<size_t>(r)]) {
                return ratio * prescribedSpeed(r);
            }
            const int si = L->sIndex[static_cast<size_t>(r)];
            if (si < 0) {
                return 0.0;
            }
            const double vRoot
                = x ? x[si + 1] : (si + 1 < (int)st.vec.size() ? st.vec[si + 1] : 0.0);
            return ratio * vRoot;
        };

        if (phase == Phase::OUTPUT) {
            for (const MComp& c : L->comps) {
                if (c.outPort < 0) {
                    continue;
                }
                if (c.kind == MKind::PositionSensor) {
                    setOutput(ctx, b.nid, pos(c.a), c.outPort);
                } else if (c.kind == MKind::SpeedSensor) {
                    setOutput(ctx, b.nid, vel(c.a), c.outPort);
                } else if (c.kind == MKind::RelPositionSensor) {
                    setOutput(ctx, b.nid, pos(c.a) - pos(c.b), c.outPort);
                } else if (c.kind == MKind::RelSpeedSensor) {
                    setOutput(ctx, b.nid, vel(c.a) - vel(c.b), c.outPort);
                }
            }
            return false;
        }

        if (phase == Phase::DERIVATIVE) {
            double* xdot = blockXdot(ctx, b.nid);
            if (!xdot) {
                return false;
            }
            // Forces accumulate at each node's GROUP ROOT, weighted by the
            // node's gear ratio (virtual work: a force f on a node does work
            // f*d(pos_node) = f*ratio*d(pos_root), so the equivalent force at
            // the root is f*ratio). A force on a fixed node is reacted by the
            // constraint and contributes nothing to a free state.
            std::vector<double> fRoot(static_cast<size_t>(L->numNodes), 0.0);
            auto applyForce = [&](int node, double f) {
                if (node <= 0 || L->isFixed[static_cast<size_t>(node)]) {
                    return;
                }
                const int r = L->rootOf[static_cast<size_t>(node)];
                fRoot[static_cast<size_t>(r)] += f * L->ratioToRoot[static_cast<size_t>(node)];
            };
            for (const MComp& c : L->comps) {
                switch (c.kind) {
                case MKind::Spring: {
                    const double F = c.value * (pos(c.a) - pos(c.b));
                    applyForce(c.a, -F);
                    applyForce(c.b, F);
                    break;
                }
                case MKind::Damper: {
                    const double F = c.value * (vel(c.a) - vel(c.b));
                    applyForce(c.a, -F);
                    applyForce(c.b, F);
                    break;
                }
                case MKind::Force: {
                    double f = c.value;
                    if (c.port >= 0) {
                        f = getInput(ctx, b.nid, c.port, 0.0);
                    }
                    applyForce(c.a, f);
                    break;
                }
                case MKind::Force2: {
                    double f = c.value;
                    if (c.port >= 0) {
                        f = getInput(ctx, b.nid, c.port, 0.0);
                    }
                    applyForce(c.a, f);
                    applyForce(c.b, -f);
                    break;
                }
                case MKind::Friction: {
                    // F = -Fc tanh(v / vEps): smooth, no chatter or events near v = 0.
                    const double v = vel(c.a);
                    applyForce(c.a, -c.value * std::tanh(v / c.s0));
                    break;
                }
                case MKind::LinearSpeedForce: {
                    // Speed-proportional resistance to absolute ground: F = -d v.
                    applyForce(c.a, -c.value * vel(c.a));
                    break;
                }
                case MKind::QuadraticSpeedForce: {
                    // Quadratic (aerodynamic-drag) resistance: F = -d v |v|.
                    const double v = vel(c.a);
                    applyForce(c.a, -c.value * v * std::abs(v));
                    break;
                }
                case MKind::Clutch: {
                    // Friction couple on the slip (v_a - v_b):
                    // T = Fc*eng*tanh((v_a-v_b)/vEps), reaction -T on a, +T on b.
                    // Optional engagement in [0,1] scales Fc.
                    double eng = 1.0;
                    if (c.port >= 0) {
                        eng = getInput(ctx, b.nid, c.port, 0.0);
                    }
                    const double slip = vel(c.a) - vel(c.b);
                    const double T = c.value * eng * std::tanh(slip / c.s0);
                    applyForce(c.a, -T);
                    applyForce(c.b, T);
                    break;
                }
                case MKind::Backlash: {
                    // Spring with a symmetric dead zone (play b): the force is zero
                    // while |s_rel| < b/2 and elastic beyond it. Branchless and
                    // continuous: F = c (max(0, s_rel - b/2) + min(0, s_rel + b/2)).
                    const double srel = pos(c.a) - pos(c.b);
                    const double half = 0.5 * c.s0;
                    const double F
                        = c.value * (std::max(0.0, srel - half) + std::min(0.0, srel + half));
                    applyForce(c.a, -F);
                    applyForce(c.b, F);
                    break;
                }
                case MKind::ElastoGap: {
                    // One-sided contact: a spring-damper that acts only while the
                    // gap is closed (s_rel < s_rel0). A tanh gate on the penetration
                    // switches it on smoothly (no event), pushing the flanges apart.
                    const double srel = pos(c.a) - pos(c.b);
                    const double pen = c.s0 - srel; // > 0 when compressed
                    const double gate = 0.5 * (1.0 + std::tanh(pen / 1e-3));
                    const double vrel = vel(c.a) - vel(c.b);
                    const double F = (c.value * pen - c.v0 * vrel) * gate;
                    applyForce(c.a, F);
                    applyForce(c.b, -F);
                    break;
                }
                case MKind::Brake: {
                    // Signal-actuated friction brake to ground: the input port gives
                    // the (non-negative) peak friction force; opposes the flange
                    // velocity, regularised by tanh (no stick-slip events).
                    const double fmax = (c.port >= 0) ? getInput(ctx, b.nid, c.port, 0.0) : c.value;
                    applyForce(c.a, -fmax * std::tanh(vel(c.a) / c.s0));
                    break;
                }
                case MKind::Freewheel: {
                    // One-way clutch: transmits a coupling torque only while a
                    // overruns b (slip = v_a - v_b > 0), driving them to a common
                    // speed; it freewheels (no torque) when a lags. The tanh gate
                    // makes the engagement smooth (no mode-switching events).
                    const double slip = vel(c.a) - vel(c.b);
                    const double gate = 0.5 * (1.0 + std::tanh(slip / c.s0));
                    const double T = c.value * slip * gate;
                    applyForce(c.a, -T);
                    applyForce(c.b, T);
                    break;
                }
                default:
                    break;
                }
            }
            // Root state derivatives: s' = v; v' = F_root / J_eff for a mass
            // node, or the prescribed acceleration signal for an Accelerate node.
            for (int r = 0; r < L->numNodes; ++r) {
                const int si = L->sIndex[static_cast<size_t>(r)];
                if (si < 0) {
                    continue;
                }
                if (L->isSpeedDriven[static_cast<size_t>(r)]) {
                    // Prescribed velocity: position integrates the prescribed speed;
                    // the v-state is unused (held constant).
                    xdot[si] = prescribedSpeed(r);
                    xdot[si + 1] = 0.0;
                    continue;
                }
                const double vRoot
                    = x ? x[si + 1] : (si + 1 < (int)st.vec.size() ? st.vec[si + 1] : 0.0);
                xdot[si] = vRoot;
                const int ap = L->accelPort[static_cast<size_t>(r)];
                if (ap >= 0) {
                    xdot[si + 1] = getInput(ctx, b.nid, ap, 0.0);
                } else {
                    xdot[si + 1] = fRoot[static_cast<size_t>(r)] / L->jEff[static_cast<size_t>(r)];
                }
            }
            return false;
        }
        return false;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
