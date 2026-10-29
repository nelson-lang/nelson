//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// planarMechanicalIsland (acausal palette N7, tier D). 2-D planar multibody as
// a single native DAE block, in absolute centre-of-mass coordinates. Each rigid
// body carries six differential states (xc, yc, phi, vx, vy, w); gravity acts
// at the COM (down = -y). Applied loads (forceX/forceY/torque, constant or
// signal-driven) and joints add to the equations of motion.
//
// Bodies alone are a plain second-order ODE (no algebraic state):
//   xc' = vx,  yc' = vy,  phi' = w
//   m vx' = Fx,  m vy' = Fy - m g,  I w' = tau
// so a free body traces an exact parabola and a constant torque an exact
// constant angular acceleration.
//
// A Revolute joint pins a body-fixed point P (offset a = (ax, ay) from the COM,
// so its world position is COM + R(phi) a) to a fixed world point (px, py). It
// introduces two ALGEBRAIC Lagrange multipliers lambda = (lx, ly) (the pin
// reaction force), marked id = 0. The constraint is stabilised at the
// acceleration level and lambda is eliminated analytically so the algebraic
// residual is a function of (x) only (never x'), matching the island residual
// convention r = id ? (x' - f) : f:
//   equations of motion : M v' = Q + J^T lambda
//   constraint (Baumgarte, omega): J v' = -Jdot v - 2 omega (J v) - omega^2 C
//   eliminate v' = M^-1 (Q + J^T lambda):
//       (J M^-1 J^T) lambda = [-Jdot v - 2 omega (J v) - omega^2 C] - J M^-1 Q
// with M = diag(m, m, I),
//      J = [[1, 0, -ry], [0, 1, rx]],  (rx, ry) = R(phi) a,
//      Jdot v = (-rx w^2, -ry w^2),  C = (xc + rx - px, yc + ry - py),
//      J v = (vx - ry w, vy + rx w),  J^T lambda = (lx, ly, rx ly - ry lx).
// The residual writes M^-1 (Q + J^T lambda) into the v' slots (differential)
// and the eliminated-multiplier residual into the lambda slots (algebraic);
// IDACalcIC then corrects lambda for consistent initial accelerations.
//
// Params JSON:
//   { "domain":"planar", "bodies":N, "gravity":9.81, "baumgarte":20,
//     "components":[
//        {"kind":"body","body":0,"m":1,"I":0.1,"x0":0,"y0":0,"phi0":0,
//                       "vx0":0,"vy0":0,"w0":0},
//        {"kind":"revolute","body":0,"ax":1,"ay":0,"px":0,"py":0},
//        {"kind":"forceX","body":0,"F":10},          // or "port":k
//        {"kind":"forceY","body":0,"port":0},
//        {"kind":"torque","body":0,"F":2},
//        {"kind":"positionSensorX","body":0,"outPort":0},   // xc / yc / phi
//        {"kind":"positionSensorY","body":0,"outPort":1},
//        {"kind":"positionSensorPhi","body":0,"outPort":2},
//        {"kind":"speedSensorX","body":0,"outPort":3},      // vx / vy / w
//        {"kind":"speedSensorY","body":0,"outPort":4},
//        {"kind":"speedSensorOmega","body":0,"outPort":5} ] }
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowCodegenLang.hpp"
#include "continuous_blocks.hpp"
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
        enum class PKind
        {
            Body,
            Revolute,
            Prismatic,
            RollingWheel, // rolls without slipping on a fixed surface line
            DistanceJoint, // rigid rod: |P_A - P_B| = L (one multiplier)
            SpringDamper, // 2D spring-damper between two body points (or a world point)
            ForceX,
            ForceY,
            Torque,
            RelativeTorque, // actuator: +tau on body, -tau on body2 (drives a joint)
            PositionSensorX,
            PositionSensorY,
            PositionSensorPhi,
            SpeedSensorX,
            SpeedSensorY,
            SpeedSensorOmega,
            FramePositionX, // world position of a body-fixed point (COM + R a)
            FramePositionY,
            FrameVelocityX, // world velocity of that point (v + w x R a)
            FrameVelocityY,
            RelPositionX, // point-A minus point-B (two bodies / world)
            RelPositionY,
            DistanceSensor, // scalar |point-A - point-B|
            AccelSensorX, // COM linear acceleration (cached from DERIVATIVE)
            AccelSensorY,
            AccelSensorAlpha // angular acceleration
        };
        //=============================================================================
        struct PComp
        {
            PKind kind;
            int body = 0;
            double m = 1.0; // body mass
            double inertia = 1.0; // central moment of inertia
            double ax = 0.0; // revolute: body-frame offset of the pinned point
            double ay = 0.0;
            double px = 0.0; // revolute: fixed world point (world-pin form)
            double py = 0.0;
            int body2 = -1; // revolute: second body (-1 = pin to world point)
            double ax2 = 0.0; // second body's body-frame offset of the joint point
            double ay2 = 0.0;
            double dx = 1.0; // prismatic: world slide-axis unit direction
            double dy = 0.0;
            double phiRef = 0.0; // prismatic: locked relative orientation
            int fOutA = -1; // CutForce: joint reaction output (revolute lx /
            int fOutB = -1; //   prismatic l1 / distance tension) and ly / l2
            double radius = 1.0; // rolling wheel radius
            double rollK = 0.0; // rolling: no-slip integration constant (INIT)
            double value = 0.0; // force / torque magnitude / spring stiffness c
            double value2 = 0.0; // spring-damper damping d
            int port = -1; // signal-driven load input port
            int outPort = -1; // sensor output port
            int multIndex = -1; // revolute: index of lambda_x (lambda_y = +1)
        };
        //=============================================================================
        struct BodyInfo
        {
            bool present = false;
            bool pointMass = false; // no orientation DOF (phi and w frozen)
            double m = 1.0;
            double inertia = 1.0;
            int stateIndex = -1; // xc at stateIndex .. w at stateIndex+5
            double x0 = 0.0, y0 = 0.0, phi0 = 0.0;
            double vx0 = 0.0, vy0 = 0.0, w0 = 0.0;
        };
        //=============================================================================
        struct PlanarLayout : public IBlockResource
        {
            int numBodies = 0;
            double gravity = 9.81;
            double omega = 20.0; // Baumgarte stabilisation frequency
            std::vector<BodyInfo> bodies;
            std::vector<PComp> comps;
            std::vector<double> ic;
            std::vector<double> id; // per-state 1 = differential, 0 = algebraic
            int numStates = 0;
        };
        //=============================================================================
        PKind
        parsePKind(const std::string& s, bool& ok)
        {
            ok = true;
            if (s == "body") {
                return PKind::Body;
            }
            if (s == "revolute") {
                return PKind::Revolute;
            }
            if (s == "prismatic") {
                return PKind::Prismatic;
            }
            if (s == "rollingWheel") {
                return PKind::RollingWheel;
            }
            if (s == "distanceJoint") {
                return PKind::DistanceJoint;
            }
            if (s == "springDamper") {
                return PKind::SpringDamper;
            }
            if (s == "forceX") {
                return PKind::ForceX;
            }
            if (s == "forceY") {
                return PKind::ForceY;
            }
            if (s == "torque") {
                return PKind::Torque;
            }
            if (s == "relativeTorque") {
                return PKind::RelativeTorque;
            }
            if (s == "positionSensorX") {
                return PKind::PositionSensorX;
            }
            if (s == "positionSensorY") {
                return PKind::PositionSensorY;
            }
            if (s == "positionSensorPhi") {
                return PKind::PositionSensorPhi;
            }
            if (s == "speedSensorX") {
                return PKind::SpeedSensorX;
            }
            if (s == "speedSensorY") {
                return PKind::SpeedSensorY;
            }
            if (s == "speedSensorOmega") {
                return PKind::SpeedSensorOmega;
            }
            if (s == "framePositionX") {
                return PKind::FramePositionX;
            }
            if (s == "framePositionY") {
                return PKind::FramePositionY;
            }
            if (s == "frameVelocityX") {
                return PKind::FrameVelocityX;
            }
            if (s == "frameVelocityY") {
                return PKind::FrameVelocityY;
            }
            if (s == "relPositionX") {
                return PKind::RelPositionX;
            }
            if (s == "relPositionY") {
                return PKind::RelPositionY;
            }
            if (s == "distanceSensor") {
                return PKind::DistanceSensor;
            }
            if (s == "accelSensorX") {
                return PKind::AccelSensorX;
            }
            if (s == "accelSensorY") {
                return PKind::AccelSensorY;
            }
            if (s == "accelSensorAlpha") {
                return PKind::AccelSensorAlpha;
            }
            ok = false;
            return PKind::Body;
        }
        //=============================================================================
        int
        pjint(const json& o, const char* key, int dflt)
        {
            if (o.contains(key) && o[key].is_number()) {
                return o[key].get<int>();
            }
            return dflt;
        }
        //=============================================================================
        double
        pjdbl(const json& o, const char* key, double dflt)
        {
            if (o.contains(key) && o[key].is_number()) {
                return o[key].get<double>();
            }
            return dflt;
        }
        //=============================================================================
        PlanarLayout
        buildPlanarLayout(const json& params)
        {
            PlanarLayout L;
            L.numBodies = pjint(params, "bodies", 1);
            if (L.numBodies < 1) {
                L.numBodies = 1;
            }
            L.gravity = pjdbl(params, "gravity", 9.81);
            L.omega = pjdbl(params, "baumgarte", 20.0);
            L.bodies.assign(static_cast<size_t>(L.numBodies), BodyInfo());

            // Pass 1: parse components, register bodies.
            if (params.contains("components") && params["components"].is_array()) {
                for (const auto& c : params["components"]) {
                    if (!c.contains("kind") || !c["kind"].is_string()) {
                        continue;
                    }
                    bool ok = false;
                    PComp comp;
                    comp.kind = parsePKind(c["kind"].get<std::string>(), ok);
                    if (!ok) {
                        continue;
                    }
                    comp.body = pjint(c, "body", 0);
                    comp.port = pjint(c, "port", -1);
                    comp.outPort = pjint(c, "outPort", -1);
                    if (comp.body < 0 || comp.body >= L.numBodies) {
                        continue;
                    }
                    switch (comp.kind) {
                    case PKind::Body: {
                        BodyInfo& bi = L.bodies[static_cast<size_t>(comp.body)];
                        bi.present = true;
                        bi.pointMass = (pjint(c, "pointMass", 0) != 0);
                        bi.m = pjdbl(c, "m", 1.0);
                        bi.inertia = pjdbl(c, "I", 1.0);
                        if (bi.pointMass && bi.inertia <= 0.0) {
                            bi.inertia = 1.0; // keep M invertible; phi is frozen anyway
                        }
                        bi.x0 = pjdbl(c, "x0", 0.0);
                        bi.y0 = pjdbl(c, "y0", 0.0);
                        bi.phi0 = pjdbl(c, "phi0", 0.0);
                        bi.vx0 = pjdbl(c, "vx0", 0.0);
                        bi.vy0 = pjdbl(c, "vy0", 0.0);
                        bi.w0 = pjdbl(c, "w0", 0.0);
                    } break;
                    case PKind::Revolute:
                        comp.ax = pjdbl(c, "ax", 0.0);
                        comp.ay = pjdbl(c, "ay", 0.0);
                        comp.px = pjdbl(c, "px", 0.0);
                        comp.py = pjdbl(c, "py", 0.0);
                        // Optional second body: body-to-body revolute (a chain
                        // link). Absent -> pin body's point to the world (px, py).
                        comp.body2 = pjint(c, "body2", -1);
                        comp.ax2 = pjdbl(c, "ax2", 0.0);
                        comp.ay2 = pjdbl(c, "ay2", 0.0);
                        if (comp.body2 < 0 || comp.body2 >= L.numBodies) {
                            comp.body2 = -1;
                        }
                        comp.fOutA = pjint(c, "outPortFx", -1); // pin force x
                        comp.fOutB = pjint(c, "outPortFy", -1); // pin force y
                        break;
                    case PKind::Prismatic: {
                        // Body slides along a fixed world axis (dx, dy) through
                        // anchor (px, py); relative orientation locked at phiRef.
                        double dxv = pjdbl(c, "dx", 1.0);
                        double dyv = pjdbl(c, "dy", 0.0);
                        const double nrm = std::sqrt(dxv * dxv + dyv * dyv);
                        if (nrm > 0.0) {
                            dxv /= nrm;
                            dyv /= nrm;
                        } else {
                            dxv = 1.0;
                            dyv = 0.0;
                        }
                        comp.dx = dxv;
                        comp.dy = dyv;
                        comp.px = pjdbl(c, "px", 0.0);
                        comp.py = pjdbl(c, "py", 0.0);
                        comp.phiRef
                            = pjdbl(c, "phi0", L.bodies[static_cast<size_t>(comp.body)].phi0);
                        comp.fOutA = pjint(c, "outPortF", -1); // off-axis force
                    } break;
                    case PKind::RollingWheel: {
                        // Wheel rolls without slipping on a fixed surface line
                        // through (px, py) with unit tangent (dx, dy); the centre
                        // sits at height `radius` on the +normal side. K (the
                        // no-slip integration constant) is set in pass 3.
                        double dxv = pjdbl(c, "dx", 1.0);
                        double dyv = pjdbl(c, "dy", 0.0);
                        const double nrm = std::sqrt(dxv * dxv + dyv * dyv);
                        if (nrm > 0.0) {
                            dxv /= nrm;
                            dyv /= nrm;
                        } else {
                            dxv = 1.0;
                            dyv = 0.0;
                        }
                        comp.dx = dxv;
                        comp.dy = dyv;
                        comp.px = pjdbl(c, "px", 0.0);
                        comp.py = pjdbl(c, "py", 0.0);
                        comp.radius = pjdbl(c, "radius", 1.0);
                        comp.fOutA = pjint(c, "outPortF", -1);
                    } break;
                    case PKind::DistanceJoint:
                        // Rigid rod between body point (ax, ay) and a second
                        // body point (body2) or a fixed world point (px, py).
                        comp.ax = pjdbl(c, "ax", 0.0);
                        comp.ay = pjdbl(c, "ay", 0.0);
                        comp.body2 = pjint(c, "body2", -1);
                        comp.ax2 = pjdbl(c, "ax2", 0.0);
                        comp.ay2 = pjdbl(c, "ay2", 0.0);
                        comp.px = pjdbl(c, "px", 0.0);
                        comp.py = pjdbl(c, "py", 0.0);
                        comp.value = pjdbl(c, "length", 1.0); // rod length L
                        if (comp.body2 < 0 || comp.body2 >= L.numBodies) {
                            comp.body2 = -1;
                        }
                        comp.fOutA = pjint(c, "outPortF", -1); // rod tension
                        break;
                    case PKind::SpringDamper:
                        // Linear 2D spring-damper between a body point (ax, ay) and
                        // a second body point (body2) or a fixed world point (px, py).
                        comp.ax = pjdbl(c, "ax", 0.0);
                        comp.ay = pjdbl(c, "ay", 0.0);
                        comp.body2 = pjint(c, "body2", -1);
                        comp.ax2 = pjdbl(c, "ax2", 0.0);
                        comp.ay2 = pjdbl(c, "ay2", 0.0);
                        comp.px = pjdbl(c, "px", 0.0);
                        comp.py = pjdbl(c, "py", 0.0);
                        comp.value = pjdbl(c, "c", 1.0); // stiffness
                        comp.value2 = pjdbl(c, "d", 0.0); // damping
                        if (comp.body2 < 0 || comp.body2 >= L.numBodies) {
                            comp.body2 = -1;
                        }
                        break;
                    case PKind::ForceX:
                    case PKind::ForceY:
                        comp.value = pjdbl(c, "F", 0.0);
                        // Optional application point (body-frame offset); nonzero
                        // -> the force also produces a torque (R(phi) a) x F.
                        comp.ax = pjdbl(c, "ax", 0.0);
                        comp.ay = pjdbl(c, "ay", 0.0);
                        break;
                    case PKind::Torque:
                        comp.value = pjdbl(c, "F", 0.0);
                        break;
                    case PKind::RelativeTorque:
                        comp.value = pjdbl(c, "F", 0.0);
                        comp.body2 = pjint(c, "body2", -1);
                        if (comp.body2 < 0 || comp.body2 >= L.numBodies) {
                            comp.body2 = -1;
                        }
                        break;
                    case PKind::FramePositionX:
                    case PKind::FramePositionY:
                    case PKind::FrameVelocityX:
                    case PKind::FrameVelocityY:
                    case PKind::RelPositionX:
                    case PKind::RelPositionY:
                    case PKind::DistanceSensor:
                        // Body-fixed point offset; relative/distance sensors add
                        // a second body point (body2 = -1 -> a fixed world point).
                        comp.ax = pjdbl(c, "ax", 0.0);
                        comp.ay = pjdbl(c, "ay", 0.0);
                        comp.body2 = pjint(c, "body2", -1);
                        comp.ax2 = pjdbl(c, "ax2", 0.0);
                        comp.ay2 = pjdbl(c, "ay2", 0.0);
                        comp.px = pjdbl(c, "px", 0.0);
                        comp.py = pjdbl(c, "py", 0.0);
                        if (comp.body2 < 0 || comp.body2 >= L.numBodies) {
                            comp.body2 = -1;
                        }
                        break;
                    default:
                        break;
                    }
                    L.comps.push_back(comp);
                }
            }

            // Pass 2: allocate six differential states per present body.
            int idx = 0;
            for (int bIdx = 0; bIdx < L.numBodies; ++bIdx) {
                BodyInfo& bi = L.bodies[static_cast<size_t>(bIdx)];
                if (!bi.present) {
                    continue;
                }
                bi.stateIndex = idx;
                const double s0[6] = { bi.x0, bi.y0, bi.phi0, bi.vx0, bi.vy0, bi.w0 };
                for (int k = 0; k < 6; ++k) {
                    L.ic.push_back(s0[k]);
                    L.id.push_back(1.0); // differential
                }
                idx += 6;
            }

            // Pass 3: allocate algebraic multipliers per joint. Revolute and
            // Prismatic each pin two scalar constraints (two multipliers); the
            // DistanceJoint is a single scalar constraint (one multiplier).
            for (PComp& c : L.comps) {
                int nMult = 0;
                if (c.kind == PKind::Revolute || c.kind == PKind::Prismatic
                    || c.kind == PKind::RollingWheel) {
                    nMult = 2;
                } else if (c.kind == PKind::DistanceJoint) {
                    nMult = 1;
                } else {
                    continue;
                }
                const BodyInfo& bi = L.bodies[static_cast<size_t>(c.body)];
                if (!bi.present) {
                    continue;
                }
                if (c.kind == PKind::RollingWheel) {
                    // K makes the no-slip constraint hold at t = 0:
                    //   C2 = d.(COM - anchor) + radius*phi - K = 0.
                    c.rollK = c.dx * (bi.x0 - c.px) + c.dy * (bi.y0 - c.py) + c.radius * bi.phi0;
                }
                c.multIndex = idx;
                for (int k = 0; k < nMult; ++k) {
                    L.ic.push_back(0.0); // lambda guess
                    L.id.push_back(0.0); // algebraic
                    idx += 1;
                }
            }

            L.numStates = idx;
            return L;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    int
    planarIslandStateCount(const Block& b, const ValMap&)
    {
        return buildPlanarLayout(asJson(b.params)).numStates;
    }
    //=============================================================================
    void
    planarIslandIdMask(const Block& b, const ValMap&, std::vector<double>& ids)
    {
        PlanarLayout L = buildPlanarLayout(asJson(b.params));
        ids.assign(L.id.begin(), L.id.end());
    }
    //=============================================================================
    bool
    handlePlanarIsland(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);
        if (phase == Phase::INIT) {
            PlanarLayout L = buildPlanarLayout(asJson(b.params));
            st.vec.assign(L.ic.begin(), L.ic.end());
            // vec2 caches each body's (ax, ay, alpha) from DERIVATIVE so
            // acceleration sensors can read them in OUTPUT.
            st.vec2.assign(static_cast<size_t>(3 * L.numBodies), 0.0);
            st.external.reset(new PlanarLayout(std::move(L)));
            return false;
        }
        const PlanarLayout* L = dynamic_cast<const PlanarLayout*>(st.external.get());
        if (!L) {
            return false;
        }
        const double* x = blockX(ctx, b.nid);
        auto sval = [&](int k) -> double {
            if (k < 0) {
                return 0.0;
            }
            if (x) {
                return x[k];
            }
            return k < static_cast<int>(st.vec.size()) ? st.vec[k] : 0.0;
        };

        if (phase == Phase::OUTPUT) {
            // World position of a body-fixed point P = COM + R(phi) a, and its
            // velocity vP = v_COM + w x (R(phi) a) = (vx - w ry, vy + w rx).
            // For a fixed world anchor (bodyIdx < 0) the point is (px, py), still.
            auto framePos = [&](int bodyIdx, double aax, double aay, double fpx, double fpy,
                                double& Px, double& Py) {
                if (bodyIdx < 0) {
                    Px = fpx;
                    Py = fpy;
                    return;
                }
                const BodyInfo& bb = L->bodies[static_cast<size_t>(bodyIdx)];
                const int s = bb.stateIndex;
                const double phi = sval(s + 2);
                const double rx = aax * std::cos(phi) - aay * std::sin(phi);
                const double ry = aax * std::sin(phi) + aay * std::cos(phi);
                Px = sval(s + 0) + rx;
                Py = sval(s + 1) + ry;
            };
            auto frameVel = [&](int bodyIdx, double aax, double aay, double& Vx, double& Vy) {
                const BodyInfo& bb = L->bodies[static_cast<size_t>(bodyIdx)];
                const int s = bb.stateIndex;
                const double phi = sval(s + 2);
                const double w = sval(s + 5);
                const double rx = aax * std::cos(phi) - aay * std::sin(phi);
                const double ry = aax * std::sin(phi) + aay * std::cos(phi);
                Vx = sval(s + 3) - w * ry;
                Vy = sval(s + 4) + w * rx;
            };
            for (const PComp& c : L->comps) {
                if (c.outPort < 0) {
                    continue;
                }
                const BodyInfo& bi = L->bodies[static_cast<size_t>(c.body)];
                if (!bi.present || bi.stateIndex < 0) {
                    continue;
                }
                const int s = bi.stateIndex;
                double out = 0.0;
                switch (c.kind) {
                case PKind::PositionSensorX:
                    out = sval(s + 0);
                    break;
                case PKind::PositionSensorY:
                    out = sval(s + 1);
                    break;
                case PKind::PositionSensorPhi:
                    out = sval(s + 2);
                    break;
                case PKind::SpeedSensorX:
                    out = sval(s + 3);
                    break;
                case PKind::SpeedSensorY:
                    out = sval(s + 4);
                    break;
                case PKind::SpeedSensorOmega:
                    out = sval(s + 5);
                    break;
                case PKind::FramePositionX: {
                    double Px, Py;
                    framePos(c.body, c.ax, c.ay, 0.0, 0.0, Px, Py);
                    out = Px;
                } break;
                case PKind::FramePositionY: {
                    double Px, Py;
                    framePos(c.body, c.ax, c.ay, 0.0, 0.0, Px, Py);
                    out = Py;
                } break;
                case PKind::FrameVelocityX: {
                    double Vx, Vy;
                    frameVel(c.body, c.ax, c.ay, Vx, Vy);
                    out = Vx;
                } break;
                case PKind::FrameVelocityY: {
                    double Vx, Vy;
                    frameVel(c.body, c.ax, c.ay, Vx, Vy);
                    out = Vy;
                } break;
                case PKind::RelPositionX:
                case PKind::RelPositionY:
                case PKind::DistanceSensor: {
                    double Pax, Pay, Pbx, Pby;
                    framePos(c.body, c.ax, c.ay, 0.0, 0.0, Pax, Pay);
                    framePos(c.body2, c.ax2, c.ay2, c.px, c.py, Pbx, Pby);
                    if (c.kind == PKind::RelPositionX) {
                        out = Pax - Pbx;
                    } else if (c.kind == PKind::RelPositionY) {
                        out = Pay - Pby;
                    } else {
                        out = std::sqrt((Pax - Pbx) * (Pax - Pbx) + (Pay - Pby) * (Pay - Pby));
                    }
                } break;
                case PKind::AccelSensorX:
                case PKind::AccelSensorY:
                case PKind::AccelSensorAlpha: {
                    // COM acceleration cached from the last DERIVATIVE residual.
                    const int off = 3 * c.body
                        + (c.kind == PKind::AccelSensorX ? 0
                                                         : (c.kind == PKind::AccelSensorY ? 1 : 2));
                    out = (off < static_cast<int>(st.vec2.size()))
                        ? st.vec2[static_cast<size_t>(off)]
                        : 0.0;
                } break;
                default:
                    continue;
                }
                setOutput(ctx, b.nid, out, c.outPort);
            }
            // CutForce: a joint's Lagrange multiplier is its reaction force.
            // Revolute -> pin force (lx, ly); Prismatic -> off-axis force l2;
            // DistanceJoint -> rod tension lambda. Emitted on request.
            for (const PComp& c : L->comps) {
                if (c.multIndex < 0 || c.fOutA < 0) {
                    continue;
                }
                if (c.kind == PKind::Revolute) {
                    setOutput(ctx, b.nid, sval(c.multIndex), c.fOutA);
                    if (c.fOutB >= 0) {
                        setOutput(ctx, b.nid, sval(c.multIndex + 1), c.fOutB);
                    }
                } else if (c.kind == PKind::Prismatic) {
                    setOutput(ctx, b.nid, sval(c.multIndex + 1), c.fOutA);
                } else if (c.kind == PKind::DistanceJoint) {
                    setOutput(ctx, b.nid, sval(c.multIndex), c.fOutA);
                }
            }
            return false;
        }

        if (phase == Phase::DERIVATIVE) {
            double* xdot = blockXdot(ctx, b.nid);
            if (!xdot) {
                return false;
            }
            const int nb = L->numBodies;
            // Accumulate generalised forces Q = (Qx, Qy, Qphi) per body.
            std::vector<double> Qx(static_cast<size_t>(nb), 0.0);
            std::vector<double> Qy(static_cast<size_t>(nb), 0.0);
            std::vector<double> Qp(static_cast<size_t>(nb), 0.0);
            for (int bIdx = 0; bIdx < nb; ++bIdx) {
                const BodyInfo& bi = L->bodies[static_cast<size_t>(bIdx)];
                if (bi.present) {
                    Qy[static_cast<size_t>(bIdx)] -= bi.m * L->gravity; // gravity at COM
                }
            }
            for (const PComp& c : L->comps) {
                const BodyInfo& bi = L->bodies[static_cast<size_t>(c.body)];
                if (!bi.present) {
                    continue;
                }
                double f = c.value;
                if (c.port >= 0) {
                    f = getInput(ctx, b.nid, c.port, 0.0);
                }
                if (c.kind == PKind::ForceX || c.kind == PKind::ForceY) {
                    // Force at a body-fixed point (offset a). r = R(phi) a; the
                    // torque about the COM is r x F. Fx form -> torque -ry f,
                    // Fy form -> torque rx f.
                    if (c.kind == PKind::ForceX) {
                        Qx[static_cast<size_t>(c.body)] += f;
                    } else {
                        Qy[static_cast<size_t>(c.body)] += f;
                    }
                    if (c.ax != 0.0 || c.ay != 0.0) {
                        const int s = bi.stateIndex;
                        const double phi = sval(s + 2);
                        const double rx = c.ax * std::cos(phi) - c.ay * std::sin(phi);
                        const double ry = c.ax * std::sin(phi) + c.ay * std::cos(phi);
                        Qp[static_cast<size_t>(c.body)]
                            += (c.kind == PKind::ForceX) ? (-ry * f) : (rx * f);
                    }
                } else if (c.kind == PKind::Torque) {
                    Qp[static_cast<size_t>(c.body)] += f;
                } else if (c.kind == PKind::RelativeTorque) {
                    // Actuator torque pair: +tau on body, -tau on body2 (an
                    // internal drive across a joint; reacts against the world
                    // when body2 is absent).
                    Qp[static_cast<size_t>(c.body)] += f;
                    if (c.body2 >= 0) {
                        Qp[static_cast<size_t>(c.body2)] -= f;
                    }
                }
            }
            auto rot = [&](int bodyIdx, double aax, double aay, double& rx, double& ry) {
                const BodyInfo& bb = L->bodies[static_cast<size_t>(bodyIdx)];
                const double phi = sval(bb.stateIndex + 2);
                const double cphi = std::cos(phi);
                const double sphi = std::sin(phi);
                rx = aax * cphi - aay * sphi;
                ry = aax * sphi + aay * cphi;
            };
            // 2D spring-dampers: an elastic force F = -(c dr + d dv) between a body
            // point A and a second body point B (or a fixed world point). The force
            // acts on A and its reaction -F on B, each with the torque r x F about
            // the respective COM.
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::SpringDamper) {
                    continue;
                }
                const BodyInfo& ba = L->bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                const int sa = ba.stateIndex;
                double rAx, rAy;
                rot(c.body, c.ax, c.ay, rAx, rAy);
                const double wa = sval(sa + 5);
                const double Ax = sval(sa + 0) + rAx;
                const double Ay = sval(sa + 1) + rAy;
                const double vAx = sval(sa + 3) - wa * rAy;
                const double vAy = sval(sa + 4) + wa * rAx;
                double Bx = c.px, By = c.py, vBx = 0.0, vBy = 0.0;
                double rBx = 0.0, rBy = 0.0;
                bool hasB = false;
                if (c.body2 >= 0) {
                    const BodyInfo& bb = L->bodies[static_cast<size_t>(c.body2)];
                    if (bb.present && bb.stateIndex >= 0) {
                        hasB = true;
                        const int sb = bb.stateIndex;
                        rot(c.body2, c.ax2, c.ay2, rBx, rBy);
                        const double wb = sval(sb + 5);
                        Bx = sval(sb + 0) + rBx;
                        By = sval(sb + 1) + rBy;
                        vBx = sval(sb + 3) - wb * rBy;
                        vBy = sval(sb + 4) + wb * rBx;
                    }
                }
                const double Fx = -(c.value * (Ax - Bx) + c.value2 * (vAx - vBx));
                const double Fy = -(c.value * (Ay - By) + c.value2 * (vAy - vBy));
                Qx[static_cast<size_t>(c.body)] += Fx;
                Qy[static_cast<size_t>(c.body)] += Fy;
                Qp[static_cast<size_t>(c.body)] += rAx * Fy - rAy * Fx;
                if (hasB) {
                    Qx[static_cast<size_t>(c.body2)] -= Fx;
                    Qy[static_cast<size_t>(c.body2)] -= Fy;
                    Qp[static_cast<size_t>(c.body2)] += rBy * Fx - rBx * Fy;
                }
            }
            // Revolute joints fold their reaction J^T lambda into Q (+lambda on
            // side A, -lambda on side B by Newton's third law); once Q is
            // complete below, they form the eliminated-multiplier residual.
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::Revolute || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& ba = L->bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                const double lx = sval(c.multIndex);
                const double ly = sval(c.multIndex + 1);
                double rxa, rya;
                rot(c.body, c.ax, c.ay, rxa, rya);
                Qx[static_cast<size_t>(c.body)] += lx;
                Qy[static_cast<size_t>(c.body)] += ly;
                Qp[static_cast<size_t>(c.body)] += rxa * ly - rya * lx;
                if (c.body2 >= 0) {
                    const BodyInfo& bb = L->bodies[static_cast<size_t>(c.body2)];
                    if (bb.present && bb.stateIndex >= 0) {
                        double rxb, ryb;
                        rot(c.body2, c.ax2, c.ay2, rxb, ryb);
                        Qx[static_cast<size_t>(c.body2)] -= lx;
                        Qy[static_cast<size_t>(c.body2)] -= ly;
                        Qp[static_cast<size_t>(c.body2)] -= rxb * ly - ryb * lx;
                    }
                }
            }
            // Prismatic joints (world form, COM reference): lambda_1 locks the
            // orientation, lambda_2 the off-axis position. Reaction J^T lambda =
            // (-dy lambda_2, dx lambda_2, lambda_1).
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::Prismatic || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& ba = L->bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                const double l1 = sval(c.multIndex);
                const double l2 = sval(c.multIndex + 1);
                Qx[static_cast<size_t>(c.body)] += -c.dy * l2;
                Qy[static_cast<size_t>(c.body)] += c.dx * l2;
                Qp[static_cast<size_t>(c.body)] += l1;
            }
            // Rolling wheel: lambda_1 = normal (surface) reaction, lambda_2 =
            // tangential (friction) that enforces no slip. Reaction J^T lambda:
            //   C1 normal  J1 = [-dy, dx, 0]  -> (-dy l1, dx l1, 0)
            //   C2 rolling J2 = [ dx, dy, R]  -> ( dx l2, dy l2, R l2)
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::RollingWheel || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& ba = L->bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                const double l1 = sval(c.multIndex);
                const double l2 = sval(c.multIndex + 1);
                Qx[static_cast<size_t>(c.body)] += -c.dy * l1 + c.dx * l2;
                Qy[static_cast<size_t>(c.body)] += c.dx * l1 + c.dy * l2;
                Qp[static_cast<size_t>(c.body)] += c.radius * l2;
            }
            // Also returns the rotated offset (rx, ry).
            auto pointPos = [&](int bodyIdx, double aax, double aay, double fpx, double fpy,
                                double& Px, double& Py, double& rx, double& ry) {
                if (bodyIdx < 0) {
                    Px = fpx;
                    Py = fpy;
                    rx = 0.0;
                    ry = 0.0;
                    return;
                }
                const BodyInfo& bb = L->bodies[static_cast<size_t>(bodyIdx)];
                const int s = bb.stateIndex;
                const double phi = sval(s + 2);
                rx = aax * std::cos(phi) - aay * std::sin(phi);
                ry = aax * std::sin(phi) + aay * std::cos(phi);
                Px = sval(s + 0) + rx;
                Py = sval(s + 1) + ry;
            };
            // Distance-joint reaction: a rod force lambda*u along the line, +u on
            // side A and -u on side B (tension/compression). u = (P_A-P_B)/|...|.
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::DistanceJoint || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& ba = L->bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                double Pax, Pay, rxa, rya, Pbx, Pby, rxb, ryb;
                pointPos(c.body, c.ax, c.ay, 0.0, 0.0, Pax, Pay, rxa, rya);
                pointPos(c.body2, c.ax2, c.ay2, c.px, c.py, Pbx, Pby, rxb, ryb);
                const double dxv = Pax - Pbx;
                const double dyv = Pay - Pby;
                const double r = std::sqrt(dxv * dxv + dyv * dyv);
                if (r < 1e-12) {
                    continue; // degenerate: no defined direction
                }
                const double ux = dxv / r;
                const double uy = dyv / r;
                const double lam = sval(c.multIndex);
                Qx[static_cast<size_t>(c.body)] += lam * ux;
                Qy[static_cast<size_t>(c.body)] += lam * uy;
                Qp[static_cast<size_t>(c.body)] += lam * (uy * rxa - ux * rya);
                if (c.body2 >= 0) {
                    const BodyInfo& bb = L->bodies[static_cast<size_t>(c.body2)];
                    if (bb.present && bb.stateIndex >= 0) {
                        Qx[static_cast<size_t>(c.body2)] -= lam * ux;
                        Qy[static_cast<size_t>(c.body2)] -= lam * uy;
                        Qp[static_cast<size_t>(c.body2)] -= lam * (uy * rxb - ux * ryb);
                    }
                }
            }
            // Body state derivatives: q' = v, v' = M^-1 Q (Q already carries the
            // pin reactions). For a free body this is the plain ODE.
            for (int bIdx = 0; bIdx < nb; ++bIdx) {
                const BodyInfo& bi = L->bodies[static_cast<size_t>(bIdx)];
                if (!bi.present || bi.stateIndex < 0) {
                    continue;
                }
                const int s = bi.stateIndex;
                xdot[s + 0] = sval(s + 3); // xc' = vx
                xdot[s + 1] = sval(s + 4); // yc' = vy
                xdot[s + 3] = Qx[static_cast<size_t>(bIdx)] / bi.m;
                xdot[s + 4] = Qy[static_cast<size_t>(bIdx)] / bi.m;
                if (bi.pointMass) {
                    // A point mass has no orientation: freeze phi and w (started
                    // at 0). Joint/force attachments are at its point (offset 0).
                    xdot[s + 2] = 0.0;
                    xdot[s + 5] = 0.0;
                } else {
                    xdot[s + 2] = sval(s + 5); // phi' = w
                    xdot[s + 5] = Qp[static_cast<size_t>(bIdx)] / bi.inertia;
                }
                // Cache (ax, ay, alpha) for the acceleration sensors' OUTPUT.
                if (3 * bIdx + 2 < static_cast<int>(st.vec2.size())) {
                    st.vec2[static_cast<size_t>(3 * bIdx + 0)] = xdot[s + 3];
                    st.vec2[static_cast<size_t>(3 * bIdx + 1)] = xdot[s + 4];
                    st.vec2[static_cast<size_t>(3 * bIdx + 2)] = xdot[s + 5];
                }
            }
            // Revolute algebraic residual: (J M^-1 J^T) lambda = gamma - J M^-1 Q0.
            // C = P_A - P_B with P = COM + R(phi) a (or a fixed world point for a
            // world-pin). Each real body contributes to the anchor position/
            // velocity, the constraint 2x2 matrix A = sum(J_side M^-1 J_side^T),
            // and J M^-1 Q0 (Q0 = load WITHOUT this joint's own reaction). Signs:
            // side A enters with +, side B with -.
            const double omega = L->omega;
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::Revolute || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& ba = L->bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                const double lx = sval(c.multIndex);
                const double ly = sval(c.multIndex + 1);
                double a11 = 0.0, a12 = 0.0, a22 = 0.0;
                double Px = 0.0, Py = 0.0; // anchor world position (A minus B)
                double Jvx = 0.0, Jvy = 0.0; // J v (A minus B)
                double Jdvx = 0.0, Jdvy = 0.0; // Jdot v (A minus B)
                double jmq0x = 0.0, jmq0y = 0.0; // J M^-1 Q0 (A minus B)
                // side accumulation: sign = +1 for A, -1 for B.
                auto addSide = [&](int bodyIdx, double aax, double aay, double sign) {
                    const BodyInfo& bb = L->bodies[static_cast<size_t>(bodyIdx)];
                    const int s = bb.stateIndex;
                    const double m = bb.m;
                    const double I = bb.inertia;
                    const double w = sval(s + 5);
                    const double vx = sval(s + 3);
                    const double vy = sval(s + 4);
                    double rx, ry;
                    rot(bodyIdx, aax, aay, rx, ry);
                    // reaction on this body from this joint = sign*(lx,ly, rx ly - ry lx)
                    const double reacx = sign * lx;
                    const double reacy = sign * ly;
                    const double reacp = sign * (rx * ly - ry * lx);
                    const double q0x = Qx[static_cast<size_t>(bodyIdx)] - reacx;
                    const double q0y = Qy[static_cast<size_t>(bodyIdx)] - reacy;
                    const double q0p = Qp[static_cast<size_t>(bodyIdx)] - reacp;
                    Px += sign * (sval(s + 0) + rx);
                    Py += sign * (sval(s + 1) + ry);
                    Jvx += sign * (vx - ry * w);
                    Jvy += sign * (vy + rx * w);
                    Jdvx += sign * (-rx * w * w);
                    Jdvy += sign * (-ry * w * w);
                    // J_side M^-1 J_side^T (same 2x2 for +/- since sign^2 = 1).
                    a11 += 1.0 / m + ry * ry / I;
                    a12 += -rx * ry / I;
                    a22 += 1.0 / m + rx * rx / I;
                    // J M^-1 Q0 carries the outer sign of J_side.
                    jmq0x += sign * (q0x / m - ry * q0p / I);
                    jmq0y += sign * (q0y / m + rx * q0p / I);
                };
                addSide(c.body, c.ax, c.ay, +1.0);
                if (c.body2 >= 0) {
                    const BodyInfo& bb = L->bodies[static_cast<size_t>(c.body2)];
                    if (bb.present && bb.stateIndex >= 0) {
                        addSide(c.body2, c.ax2, c.ay2, -1.0);
                    }
                } else {
                    Px -= c.px; // fixed world anchor on side B
                    Py -= c.py;
                }
                // gamma = -Jdot v - 2 omega (J v) - omega^2 C.
                const double gx = -Jdvx - 2.0 * omega * Jvx - omega * omega * Px;
                const double gy = -Jdvy - 2.0 * omega * Jvy - omega * omega * Py;
                // Residual = A lambda - (gamma - J M^-1 Q0) = 0 (algebraic).
                xdot[c.multIndex] = a11 * lx + a12 * ly - (gx - jmq0x);
                xdot[c.multIndex + 1] = a12 * lx + a22 * ly - (gy - jmq0y);
            }
            // Prismatic algebraic residual (world form, COM reference). With
            // J = [[0,0,1],[-dy,dx,0]] (constant, so Jdot = 0):
            //   C  = (phi - phiRef, -dy (xc-px) + dx (yc-py))
            //   Jv = (w, -dy vx + dx vy)
            //   A  = J M^-1 J^T = diag(1/I, 1/m)   (dx^2 + dy^2 = 1)
            //   J M^-1 Q0 = (q0p/I, (-dy q0x + dx q0y)/m)
            //   reaction on this body from lambda = (-dy l2, dx l2, l1).
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::Prismatic || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& bi = L->bodies[static_cast<size_t>(c.body)];
                if (!bi.present || bi.stateIndex < 0) {
                    continue;
                }
                const int s = bi.stateIndex;
                const double m = bi.m;
                const double I = bi.inertia;
                const double dx = c.dx;
                const double dy = c.dy;
                const double l1 = sval(c.multIndex);
                const double l2 = sval(c.multIndex + 1);
                const double q0x = Qx[static_cast<size_t>(c.body)] - (-dy * l2);
                const double q0y = Qy[static_cast<size_t>(c.body)] - (dx * l2);
                const double q0p = Qp[static_cast<size_t>(c.body)] - l1;
                const double C1 = sval(s + 2) - c.phiRef;
                const double C2 = -dy * (sval(s + 0) - c.px) + dx * (sval(s + 1) - c.py);
                const double Jv1 = sval(s + 5); // w
                const double Jv2 = -dy * sval(s + 3) + dx * sval(s + 4);
                const double g1 = -2.0 * omega * Jv1 - omega * omega * C1;
                const double g2 = -2.0 * omega * Jv2 - omega * omega * C2;
                const double jmq0_1 = q0p / I;
                const double jmq0_2 = (-dy * q0x + dx * q0y) / m;
                xdot[c.multIndex] = l1 / I - (g1 - jmq0_1);
                xdot[c.multIndex + 1] = l2 / m - (g2 - jmq0_2);
            }
            // Rolling-wheel algebraic residual (constant J, so Jdot = 0):
            //   C1 = -dy(xc-px) + dx(yc-py) - radius        (stay on surface)
            //   C2 =  dx(xc-px) + dy(yc-py) + radius*phi - K (no slip)
            //   J1 = [-dy,dx,0], J2 = [dx,dy,R]; A = diag(1/m, 1/m + R^2/I).
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::RollingWheel || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& bi = L->bodies[static_cast<size_t>(c.body)];
                if (!bi.present || bi.stateIndex < 0) {
                    continue;
                }
                const int s = bi.stateIndex;
                const double m = bi.m;
                const double I = bi.inertia;
                const double R = c.radius;
                const double dx = c.dx, dy = c.dy;
                const double l1 = sval(c.multIndex);
                const double l2 = sval(c.multIndex + 1);
                const double xr = sval(s + 0) - c.px;
                const double yr = sval(s + 1) - c.py;
                const double vx = sval(s + 3), vy = sval(s + 4), w = sval(s + 5);
                // Q0 = load without this wheel's own reaction.
                const double q0x = Qx[static_cast<size_t>(c.body)] - (-dy * l1 + dx * l2);
                const double q0y = Qy[static_cast<size_t>(c.body)] - (dx * l1 + dy * l2);
                const double q0p = Qp[static_cast<size_t>(c.body)] - (R * l2);
                const double C1 = -dy * xr + dx * yr - R;
                const double C2 = dx * xr + dy * yr + R * sval(s + 2) - c.rollK;
                const double Jv1 = -dy * vx + dx * vy;
                const double Jv2 = dx * vx + dy * vy + R * w;
                const double g1 = -2.0 * omega * Jv1 - omega * omega * C1;
                const double g2 = -2.0 * omega * Jv2 - omega * omega * C2;
                const double jmq0_1 = (-dy * q0x + dx * q0y) / m;
                const double jmq0_2 = (dx * q0x + dy * q0y) / m + R * q0p / I;
                // A = diag(1/m, 1/m + R^2/I) (off-diagonal is zero, d unit).
                xdot[c.multIndex] = l1 / m - (g1 - jmq0_1);
                xdot[c.multIndex + 1] = l2 * (1.0 / m + R * R / I) - (g2 - jmq0_2);
            }
            // Distance-joint algebraic residual. C = |P_A - P_B| - L; the
            // acceleration-level constraint C-ddot = J q-ddot + Gamma keeps the
            // centripetal + direction-rotation terms in Gamma so the eliminated-
            // multiplier residual stays a function of x only:
            //   Gamma = -wA^2 (u.rA) + wB^2 (u.rB) + (|ddot d|^2 - Cdot^2)/r
            //   gamma = -Gamma - 2 omega Cdot - omega^2 (r - L)
            //   A = sum_realside [1/m + (uy rx - ux ry)^2 / I]
            //   residual = A lambda - (gamma - J M^-1 Q0) = 0.
            for (const PComp& c : L->comps) {
                if (c.kind != PKind::DistanceJoint || c.multIndex < 0) {
                    continue;
                }
                const BodyInfo& ba = L->bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                const double lam = sval(c.multIndex);
                double Pax, Pay, rxa, rya, Pbx, Pby, rxb, ryb;
                pointPos(c.body, c.ax, c.ay, 0.0, 0.0, Pax, Pay, rxa, rya);
                pointPos(c.body2, c.ax2, c.ay2, c.px, c.py, Pbx, Pby, rxb, ryb);
                const double dxv = Pax - Pbx;
                const double dyv = Pay - Pby;
                const double r = std::sqrt(dxv * dxv + dyv * dyv);
                if (r < 1e-12) {
                    xdot[c.multIndex] = lam; // degenerate: drive lambda to 0
                    continue;
                }
                const double ux = dxv / r;
                const double uy = dyv / r;
                double A_mat = 0.0, jmq0 = 0.0;
                double dvx = 0.0, dvy = 0.0; // ddot = vP_A - vP_B
                double gammaCentri = 0.0; // -wA^2(u.rA) + wB^2(u.rB)
                auto accSide = [&](int bodyIdx, double rx, double ry, double sign) {
                    const BodyInfo& bb = L->bodies[static_cast<size_t>(bodyIdx)];
                    const int s = bb.stateIndex;
                    const double m = bb.m;
                    const double I = bb.inertia;
                    const double w = sval(s + 5);
                    const double vPx = sval(s + 3) - w * ry;
                    const double vPy = sval(s + 4) + w * rx;
                    dvx += sign * vPx;
                    dvy += sign * vPy;
                    // -w^2 (u.r), sign folds A(+)/B(-) into Gamma's convention:
                    //   Gamma has -wA^2(u.rA) + wB^2(u.rB) = sum -sign*w^2(u.r).
                    gammaCentri += -sign * w * w * (ux * rx + uy * ry);
                    // reaction on this side = sign*lambda*(ux,uy, uy rx - ux ry).
                    const double reacx = sign * lam * ux;
                    const double reacy = sign * lam * uy;
                    const double reacp = sign * lam * (uy * rx - ux * ry);
                    const double q0x = Qx[static_cast<size_t>(bodyIdx)] - reacx;
                    const double q0y = Qy[static_cast<size_t>(bodyIdx)] - reacy;
                    const double q0p = Qp[static_cast<size_t>(bodyIdx)] - reacp;
                    const double jrow = uy * rx - ux * ry; // J_side rotational entry
                    A_mat += 1.0 / m + jrow * jrow / I;
                    jmq0 += sign * ((ux * q0x + uy * q0y) / m + jrow * q0p / I);
                };
                accSide(c.body, rxa, rya, +1.0);
                if (c.body2 >= 0) {
                    const BodyInfo& bb = L->bodies[static_cast<size_t>(c.body2)];
                    if (bb.present && bb.stateIndex >= 0) {
                        accSide(c.body2, rxb, ryb, -1.0);
                    }
                }
                const double Cdot = ux * dvx + uy * dvy;
                const double dd2 = dvx * dvx + dvy * dvy;
                const double Gamma = gammaCentri + (dd2 - Cdot * Cdot) / r;
                const double C = r - c.value;
                const double gamma = -Gamma - 2.0 * omega * Cdot - omega * omega * C;
                xdot[c.multIndex] = A_mat * lam - (gamma - jmq0);
            }
            return false;
        }
        return false;
    }
    //=============================================================================
    // Code generation (joint-free subset). A joint-free island is a pure ODE: six
    // differential states per body, no multipliers. Islands using a component kind
    // outside that subset (joints, acceleration sensors) are rejected upstream by
    // planarIslandOdeGeneratable.
    //=============================================================================
    bool
    planarIslandOdeGeneratable(const json& params, std::string* whyKind)
    {
        if (params.contains("components") && params["components"].is_array()) {
            for (const auto& c : params["components"]) {
                if (!c.contains("kind") || !c["kind"].is_string()) {
                    continue;
                }
                const std::string k = c["kind"].get<std::string>();
                if (k == "revolute" || k == "prismatic" || k == "rollingWheel"
                    || k == "distanceJoint" || k == "accelSensorX" || k == "accelSensorY"
                    || k == "accelSensorAlpha") {
                    if (whyKind != nullptr) {
                        *whyKind = k;
                    }
                    return false;
                }
            }
        }
        return true;
    }
    //=============================================================================
    namespace {
        static std::string
        plOutVar(const std::string& id, int port)
        {
            return port > 0 ? ("out_" + id + "_p" + std::to_string(port)) : ("out_" + id);
        }
        //=============================================================================
        // State field access: pl_x_<id>_<k> holds state slot k of the island.
        static std::function<std::string(int)>
        plStateAt(const BlockCodegenArgs& a, const CodegenLang& lang)
        {
            const std::string id = a.id;
            return [lang, id](int k) { return lang.sref("pl_x_" + id + "_" + std::to_string(k)); };
        }
        //=============================================================================
        // Rotated body-frame offset (R(phi) a) as expressions; a zero offset
        // collapses to "0.0" (bit-exact: the runtime's 0*cos(phi) terms are
        // exactly 0.0 too).
        static void
        plRot(const BlockCodegenArgs& a, const CodegenLang& lang,
            const std::function<std::string(int)>& st, int s, double ax, double ay, std::string& rx,
            std::string& ry)
        {
            if (ax == 0.0 && ay == 0.0) {
                rx = "0.0";
                ry = "0.0";
                return;
            }
            const std::string phi = st(s + 2);
            rx = "(" + a.fmt(ax) + " * " + lang.call("cos", phi) + " - " + a.fmt(ay) + " * "
                + lang.call("sin", phi) + ")";
            ry = "(" + a.fmt(ax) + " * " + lang.call("sin", phi) + " + " + a.fmt(ay) + " * "
                + lang.call("cos", phi) + ")";
        }
        //=============================================================================
        static void
        emitPlanarOutputs(const BlockCodegenArgs& a, const PlanarLayout& L, const CodegenLang& lang)
        {
            auto st = plStateAt(a, lang);
            int tmp = 0;
            for (const PComp& c : L.comps) {
                if (c.outPort < 0) {
                    continue;
                }
                const BodyInfo& bi = L.bodies[static_cast<size_t>(c.body)];
                if (!bi.present || bi.stateIndex < 0) {
                    continue;
                }
                const int s = bi.stateIndex;
                const std::string ov = plOutVar(a.id, c.outPort);
                switch (c.kind) {
                case PKind::PositionSensorX:
                    a.line(ov + " = " + st(s + 0) + ";");
                    break;
                case PKind::PositionSensorY:
                    a.line(ov + " = " + st(s + 1) + ";");
                    break;
                case PKind::PositionSensorPhi:
                    a.line(ov + " = " + st(s + 2) + ";");
                    break;
                case PKind::SpeedSensorX:
                    a.line(ov + " = " + st(s + 3) + ";");
                    break;
                case PKind::SpeedSensorY:
                    a.line(ov + " = " + st(s + 4) + ";");
                    break;
                case PKind::SpeedSensorOmega:
                    a.line(ov + " = " + st(s + 5) + ";");
                    break;
                case PKind::FramePositionX:
                case PKind::FramePositionY: {
                    std::string rx, ry;
                    plRot(a, lang, st, s, c.ax, c.ay, rx, ry);
                    a.line(ov + " = "
                        + (c.kind == PKind::FramePositionX ? (st(s + 0) + " + " + rx)
                                                           : (st(s + 1) + " + " + ry))
                        + ";");
                } break;
                case PKind::FrameVelocityX:
                case PKind::FrameVelocityY: {
                    std::string rx, ry;
                    plRot(a, lang, st, s, c.ax, c.ay, rx, ry);
                    a.line(ov + " = "
                        + (c.kind == PKind::FrameVelocityX
                                ? (st(s + 3) + " - " + st(s + 5) + " * " + ry)
                                : (st(s + 4) + " + " + st(s + 5) + " * " + rx))
                        + ";");
                } break;
                case PKind::RelPositionX:
                case PKind::RelPositionY:
                case PKind::DistanceSensor: {
                    // Point A on the body; point B on body2 or a fixed world
                    // point. Locals keep the expressions readable.
                    const std::string n = a.id + "_" + std::to_string(tmp++);
                    std::string rAx, rAy;
                    plRot(a, lang, st, s, c.ax, c.ay, rAx, rAy);
                    a.line(lang.declLocal("__pax_" + n, st(s + 0) + " + " + rAx));
                    a.line(lang.declLocal("__pay_" + n, st(s + 1) + " + " + rAy));
                    std::string bx = a.fmt(c.px);
                    std::string by = a.fmt(c.py);
                    if (c.body2 >= 0) {
                        const BodyInfo& bb = L.bodies[static_cast<size_t>(c.body2)];
                        if (bb.present && bb.stateIndex >= 0) {
                            const int sb = bb.stateIndex;
                            std::string rBx, rBy;
                            plRot(a, lang, st, sb, c.ax2, c.ay2, rBx, rBy);
                            bx = st(sb + 0) + " + " + rBx;
                            by = st(sb + 1) + " + " + rBy;
                        }
                    }
                    a.line(lang.declLocal("__pbx_" + n, bx));
                    a.line(lang.declLocal("__pby_" + n, by));
                    if (c.kind == PKind::RelPositionX) {
                        a.line(ov + " = __pax_" + n + " - __pbx_" + n + ";");
                    } else if (c.kind == PKind::RelPositionY) {
                        a.line(ov + " = __pay_" + n + " - __pby_" + n + ";");
                    } else {
                        a.line(ov + " = "
                            + lang.call("sqrt",
                                "(__pax_" + n + " - __pbx_" + n + ") * (__pax_" + n + " - __pbx_"
                                    + n + ") + (__pay_" + n + " - __pby_" + n + ") * (__pay_" + n
                                    + " - __pby_" + n + ")")
                            + ";");
                    }
                } break;
                default:
                    break;
                }
            }
        }
        //=============================================================================
        // Emit the generalised-force accumulation and every state's derivative
        // expression through `emitDeriv(stateIndex, expr)` (shared by the RK4
        // seam and the forward-Euler step).
        static void
        emitPlanarDeriv(const BlockCodegenArgs& a, const PlanarLayout& L, const CodegenLang& lang,
            const std::function<void(int, const std::string&)>& emitDeriv)
        {
            auto st = plStateAt(a, lang);
            const std::string id = a.id;
            auto inExpr = [&a](int port) -> std::string {
                return (port >= 0 && port < static_cast<int>(a.in.size()))
                    ? a.in[static_cast<size_t>(port)]
                    : std::string("0.0");
            };
            // Q accumulators per present body; gravity pre-folded into Qy.
            for (int bIdx = 0; bIdx < L.numBodies; ++bIdx) {
                const BodyInfo& bi = L.bodies[static_cast<size_t>(bIdx)];
                if (!bi.present) {
                    continue;
                }
                const std::string b = std::to_string(bIdx);
                a.line(lang.declLocal("__qx_" + id + "_" + b, "0.0"));
                a.line(
                    lang.declLocal("__qy_" + id + "_" + b, "-(" + a.fmt(bi.m * L.gravity) + ")"));
                a.line(lang.declLocal("__qp_" + id + "_" + b, "0.0"));
            }
            int tmp = 0;
            for (const PComp& c : L.comps) {
                const BodyInfo& bi = L.bodies[static_cast<size_t>(c.body)];
                if (!bi.present) {
                    continue;
                }
                if (c.kind != PKind::ForceX && c.kind != PKind::ForceY && c.kind != PKind::Torque
                    && c.kind != PKind::RelativeTorque) {
                    continue;
                }
                const std::string b = std::to_string(c.body);
                const std::string fv = "__f_" + id + "_" + std::to_string(tmp++);
                a.line(lang.declLocal(fv, c.port >= 0 ? inExpr(c.port) : a.fmt(c.value)));
                if (c.kind == PKind::ForceX || c.kind == PKind::ForceY) {
                    a.line(std::string("__q") + (c.kind == PKind::ForceX ? "x" : "y") + "_" + id
                        + "_" + b + " += " + fv + ";");
                    if (c.ax != 0.0 || c.ay != 0.0) {
                        std::string rx, ry;
                        plRot(a, lang, st, bi.stateIndex, c.ax, c.ay, rx, ry);
                        a.line("__qp_" + id + "_" + b + " += "
                            + (c.kind == PKind::ForceX ? ("-" + ry) : rx) + " * " + fv + ";");
                    }
                } else if (c.kind == PKind::Torque) {
                    a.line("__qp_" + id + "_" + b + " += " + fv + ";");
                } else { // RelativeTorque: +tau on body, -tau on body2
                    a.line("__qp_" + id + "_" + b + " += " + fv + ";");
                    if (c.body2 >= 0 && L.bodies[static_cast<size_t>(c.body2)].present) {
                        a.line("__qp_" + id + "_" + std::to_string(c.body2) + " -= " + fv + ";");
                    }
                }
            }
            // 2D spring-dampers (body point A to body2 point B / world point).
            for (const PComp& c : L.comps) {
                if (c.kind != PKind::SpringDamper) {
                    continue;
                }
                const BodyInfo& ba = L.bodies[static_cast<size_t>(c.body)];
                if (!ba.present || ba.stateIndex < 0) {
                    continue;
                }
                const int sa = ba.stateIndex;
                const std::string n = id + "_" + std::to_string(tmp++);
                std::string rAx, rAy;
                plRot(a, lang, st, sa, c.ax, c.ay, rAx, rAy);
                a.line(lang.declLocal("__rax_" + n, rAx));
                a.line(lang.declLocal("__ray_" + n, rAy));
                a.line(lang.declLocal("__ax_" + n, st(sa + 0) + " + __rax_" + n));
                a.line(lang.declLocal("__ay_" + n, st(sa + 1) + " + __ray_" + n));
                a.line(lang.declLocal(
                    "__avx_" + n, st(sa + 3) + " - " + st(sa + 5) + " * __ray_" + n));
                a.line(lang.declLocal(
                    "__avy_" + n, st(sa + 4) + " + " + st(sa + 5) + " * __rax_" + n));
                bool hasB = false;
                int sb = -1;
                if (c.body2 >= 0) {
                    const BodyInfo& bb = L.bodies[static_cast<size_t>(c.body2)];
                    if (bb.present && bb.stateIndex >= 0) {
                        hasB = true;
                        sb = bb.stateIndex;
                    }
                }
                if (hasB) {
                    std::string rBx, rBy;
                    plRot(a, lang, st, sb, c.ax2, c.ay2, rBx, rBy);
                    a.line(lang.declLocal("__rbx_" + n, rBx));
                    a.line(lang.declLocal("__rby_" + n, rBy));
                    a.line(lang.declLocal("__bx_" + n, st(sb + 0) + " + __rbx_" + n));
                    a.line(lang.declLocal("__by_" + n, st(sb + 1) + " + __rby_" + n));
                    a.line(lang.declLocal(
                        "__bvx_" + n, st(sb + 3) + " - " + st(sb + 5) + " * __rby_" + n));
                    a.line(lang.declLocal(
                        "__bvy_" + n, st(sb + 4) + " + " + st(sb + 5) + " * __rbx_" + n));
                } else {
                    a.line(lang.declLocal("__bx_" + n, a.fmt(c.px)));
                    a.line(lang.declLocal("__by_" + n, a.fmt(c.py)));
                    a.line(lang.declLocal("__bvx_" + n, "0.0"));
                    a.line(lang.declLocal("__bvy_" + n, "0.0"));
                }
                a.line(lang.declLocal("__fx_" + n,
                    "-(" + a.fmt(c.value) + " * (__ax_" + n + " - __bx_" + n + ") + "
                        + a.fmt(c.value2) + " * (__avx_" + n + " - __bvx_" + n + "))"));
                a.line(lang.declLocal("__fy_" + n,
                    "-(" + a.fmt(c.value) + " * (__ay_" + n + " - __by_" + n + ") + "
                        + a.fmt(c.value2) + " * (__avy_" + n + " - __bvy_" + n + "))"));
                const std::string bA = std::to_string(c.body);
                a.line("__qx_" + id + "_" + bA + " += __fx_" + n + ";");
                a.line("__qy_" + id + "_" + bA + " += __fy_" + n + ";");
                a.line("__qp_" + id + "_" + bA + " += __rax_" + n + " * __fy_" + n + " - __ray_" + n
                    + " * __fx_" + n + ";");
                if (hasB) {
                    const std::string bB = std::to_string(c.body2);
                    a.line("__qx_" + id + "_" + bB + " -= __fx_" + n + ";");
                    a.line("__qy_" + id + "_" + bB + " -= __fy_" + n + ";");
                    a.line("__qp_" + id + "_" + bB + " += __rby_" + n + " * __fx_" + n + " - __rbx_"
                        + n + " * __fy_" + n + ";");
                }
            }
            // Kinematics + Newton (pointMass bodies freeze phi and w).
            for (int bIdx = 0; bIdx < L.numBodies; ++bIdx) {
                const BodyInfo& bi = L.bodies[static_cast<size_t>(bIdx)];
                if (!bi.present || bi.stateIndex < 0) {
                    continue;
                }
                const int s = bi.stateIndex;
                const std::string b = std::to_string(bIdx);
                emitDeriv(s + 0, st(s + 3));
                emitDeriv(s + 1, st(s + 4));
                emitDeriv(s + 3, "__qx_" + id + "_" + b + " / " + a.fmt(bi.m));
                emitDeriv(s + 4, "__qy_" + id + "_" + b + " / " + a.fmt(bi.m));
                if (bi.pointMass) {
                    emitDeriv(s + 2, "0.0");
                    emitDeriv(s + 5, "0.0");
                } else {
                    emitDeriv(s + 2, st(s + 5));
                    emitDeriv(s + 5, "__qp_" + id + "_" + b + " / " + a.fmt(bi.inertia));
                }
            }
        }
        //=============================================================================
        static BlockCodegenTemplate
        makePlanarIslandCodegen(CodegenLang lang)
        {
            BlockCodegenTemplate t;
            t.emitState = [](const BlockCodegenStateArgs& a) {
                const PlanarLayout L = buildPlanarLayout(*asJson(a.params));
                for (int i = 0; i < L.numStates; ++i) {
                    a.addState("pl_x_" + a.id + "_" + std::to_string(i),
                        a.fmt(L.ic[static_cast<size_t>(i)]), "");
                }
            };
            t.continuousWidth = [](const BlockCodegenArgs& a) -> int {
                return buildPlanarLayout(*asJson(a.params)).numStates;
            };
            t.emitRk4 = [lang](const BlockCodegenArgs& a) {
                const PlanarLayout L = buildPlanarLayout(*asJson(a.params));
                const std::string id = a.id;
                const int off = a.stateOffset;
                auto st = plStateAt(a, lang);
                switch (a.rk4Op) {
                case Rk4Gather:
                    for (int i = 0; i < L.numStates; ++i) {
                        a.line(a.rk4Arr + "[" + std::to_string(off + i) + "] = " + st(i) + ";");
                    }
                    break;
                case Rk4Scatter:
                    for (int i = 0; i < L.numStates; ++i) {
                        a.line(st(i) + " = " + a.rk4Arr + "[" + std::to_string(off + i) + "];");
                    }
                    break;
                case Rk4Output:
                    emitPlanarOutputs(a, L, lang);
                    break;
                case Rk4Deriv:
                    emitPlanarDeriv(a, L, lang, [&](int i, const std::string& e) {
                        a.line(a.rk4Arr + "[" + std::to_string(off + i) + "] = " + e + ";");
                    });
                    break;
                case Rk4Clamp:
                default:
                    break; // planar states are unbounded: nothing to clamp
                }
            };
            t.emitStep = [lang](const BlockCodegenArgs& a) {
                const PlanarLayout L = buildPlanarLayout(*asJson(a.params));
                if (L.numStates <= 0) {
                    return;
                }
                const std::string id = a.id;
                auto st = plStateAt(a, lang);
                // Outputs at the pre-step state, derivatives into temps from
                // that same state, then the forward-Euler advance.
                emitPlanarOutputs(a, L, lang);
                emitPlanarDeriv(a, L, lang, [&](int i, const std::string& e) {
                    a.line(lang.declLocal("__xd_" + id + "_" + std::to_string(i), e));
                });
                for (int i = 0; i < L.numStates; ++i) {
                    a.line(st(i) + " = " + st(i) + " + dt * __xd_" + id + "_" + std::to_string(i)
                        + ";");
                }
            };
            return t;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCPlanarIsland()
    {
        return makePlanarIslandCodegen({ false });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustPlanarIsland()
    {
        return makePlanarIslandCodegen({ true });
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
