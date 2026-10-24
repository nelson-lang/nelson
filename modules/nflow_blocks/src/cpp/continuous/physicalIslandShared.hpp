//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
// physicalIsland internals shared by its three translation units (layout
// assembly, runtime handler, code generators). The layout is a deterministic
// pure function of the block params, so every consumer (state count, id mask,
// residual stamping, emitters) agrees without sharing runtime state.
//=============================================================================
#include "SimEngineTypes.hpp"
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace physisland {
        //=============================================================================
        enum class Kind
        {
            Resistor,
            Conductor,
            VariableResistor, // resistance from a signal input
            VariableConductor, // conductance from a signal input
            VariableCapacitor, // capacitance from a signal input (charge state)
            VariableInductor, // inductance from a signal input (flux state)
            Capacitor,
            Inductor,
            VSource,
            ISource,
            Vcvs, // voltage-controlled voltage source
            Vccs, // voltage-controlled current source
            Ccvs, // current-controlled voltage source: v_ab = gain * i_cd
            Cccs, // current-controlled current source: i_ab = gain * i_cd
            OpAmp, // ideal op-amp (nullor): e_+ = e_-, output current free
            Short, // ideal short: e_a = e_b (branch current free)
            Idle, // ideal open: i = 0 (branch voltage free)
            Transformer, // ideal transformer (v1 = n v2, i2 = -n i1)
            Gyrator, // gyrator (i1 = G2 v2, i2 = -G1 v1)
            Switch, // ideal switch (control > 0.5 -> closed short, else open)
            Diode, // exponential (Shockley) diode
            ZDiode, // Zener diode: forward Shockley + reverse breakdown at -Vz
            IdealDiode, // hybrid ideal diode (state s): off s<0 leak Goff, on Ron + Vknee
            Npn, // NPN bipolar transistor (Ebers-Moll): a=C, b=E, c=B
            Pnp, // PNP bipolar transistor (Ebers-Moll): a=C, b=E, c=B
            Nmos, // N-channel MOSFET (square law): a=D, b=S, c=G
            Pmos, // P-channel MOSFET (square law): a=D, b=S, c=G
            Radiation, // thermal radiation: Q = Gr (T_a^4 - T_b^4)
            CurrentSensor,
            VoltageSensor,
            PotentialSensor
        };
        //=============================================================================
        struct IslandComp
        {
            Kind kind;
            int a = 0; // node index of pin p (0 = ground)
            int b = 0; // node index of pin n (0 = ground)
            int c = 0; // control node + (controlled sources)
            int d = 0; // control node - (controlled sources)
            double value = 0.0; // R / G / C / L / V / I / gain / n / G1 / Ron; ignored if port >= 0
            double value2 = 0.0; // gyrator G2 / ZDiode Vz / BJT forward beta / IdealDiode Goff
            double value3 = 0.0; // BJT reverse beta / IdealDiode Vknee
            double vt = 0.025; // diode thermal voltage
            double ic = 0.0; // capacitor / inductor initial condition
            int port = -1; // input port driving a source (-1 = constant)
            int outPort = -1; // output port for a sensor (-1 = none)
            int stateIndex = -1; // first own-state index in the block slice
        };
        //=============================================================================
        // Assembled layout of an island, a pure function of the block params.
        struct IslandLayout : public IBlockResource
        {
            int numNodes = 0;
            std::vector<IslandComp> comps;
            std::vector<int> eIndex; // [node] -> state index of e_node (-1 for ground)
            std::vector<double> id; // per-state 1 diff / 0 alg
            std::vector<double> ic; // per-state initial condition
            int numStates = 0;
            // Runtime scratch of the non-DAE loops (SimCtx::islandImplicitStep):
            // simulated time of the last embedded implicit advance, so repeated
            // OUTPUT passes over one major step advance the island exactly once.
            // -1e300 = never advanced (the t = 0 pass then runs the consistency
            // solve instead of a backward-Euler step).
            double lastAdvanceT = -1.0e300;
            // Mode-latched components (IdealDiode): the branch mode is a discrete
            // state flipped only at a located knee event or by the consistency
            // settle after a solve, never inside a Newton iteration, so every
            // residual solve is linear in the current modes.
            bool hasIdealDiode = false;
            std::vector<int> diodeMode; // [comp] 0 = off, 1 = on (IdealDiode only)
        };
        //=============================================================================
        // State indices are assigned in a fixed order: node potentials first
        // (algebraic), then per-component own states (capacitor v_C diff + i_C
        // alg, inductor i_L diff, vsource/ammeter i alg).
        IslandLayout
        buildLayout(const json& params);
        //=============================================================================
        // physicalIslandSolve.cpp: in-place Gaussian elimination with partial
        // pivoting (J: N x N row-major, bv: rhs in / solution out), the interpreter
        // twin of the emitted solver.
        void
        islandGaussSolve(std::vector<double>& J, std::vector<double>& bv, int N);
        // Knee band [V]: an ideal diode within this distance of its knee is left in
        // its latched mode by the settle (a located root sits inside the band).
        constexpr double kDiodeKneeTolV = 1e-6;
        // v_d - Vknee of an IdealDiode component over a state view (x when
        // non-null, else `fallback`).
        double
        islandDiodeKneeGap(const IslandLayout* L, const IslandComp& c, const double* x,
            const std::vector<double>& fallback);
        // Flip every ideal diode whose latched mode disagrees with its knee gap by
        // more than the band (off above the knee -> on, on below it -> off).
        // Returns true when a mode changed.
        bool
        islandSettleDiodeModes(
            IslandLayout* L, const double* x, const std::vector<double>& fallback);
        //=============================================================================
        // physicalIslandCodegenSupport.cpp: whether every component kind of the
        // island is covered by the emitted embedded-Newton solver, and the number
        // of distinct signal input ports the island reads.
        bool
        islandNewtonSupported(const IslandLayout& L);
        int
        islandPortCount(const IslandLayout& L);
        //=============================================================================
    } // namespace physisland
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
