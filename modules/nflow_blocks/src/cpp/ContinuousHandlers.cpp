//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "continuous/continuous_blocks.hpp"
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerContinuousHandlers(BlockRegistry& reg)
    {
        const std::string lib = "builtin.continuous";

        // integrator: one continuous state per signal element (x' = u).
        // A limited integrator exposes two zero-crossing surfaces per element
        // (x - min, x - max) so a variable-step solver stops where the state
        // engages or leaves a limit. An unbounded one allocates none.
        BlockMetadata integMeta;
        integMeta.numContinuousStates = 1;
        integMeta.numZeroCrossings = 2; // saturation limits: x-min, x-max
        integMeta.sampleTime = SampleTimeClass::Continuous;
        integMeta.directFeedthrough = false;
        integMeta.continuousStateCount
            = [](const Block& blk, const ValMap&) -> int { return std::max(1, blk.outWidth(0)); };
        integMeta.zeroCrossingCount = [](const Block& blk, const ValMap& vars) -> int {
            nflow::BlockDescriptor bd(blk, vars);
            const double mn = bd.paramDouble(nflow::kMin, -std::numeric_limits<double>::infinity());
            const double mx = bd.paramDouble(nflow::kMax, std::numeric_limits<double>::infinity());
            if (!std::isfinite(mn) && !std::isfinite(mx)) {
                return 0; // unbounded integrator: no saturation surfaces
            }
            return 2 * std::max(1, blk.outWidth(0));
        };
        // ALGEBRAIC carries the external reset only, the phase where the reset
        // input is settled. The output is the state, so no direct feedthrough.
        reg.registerHandler("integrator", handleIntegrator,
            { Phase::INIT, Phase::OUTPUT, Phase::ALGEBRAIC, Phase::UPDATE, Phase::DERIVATIVE,
                Phase::ZERO_CROSSING },
            lib, integMeta);

        // tf: continuous-state count is the denominator order (0 for a pure gain,
        // which feeds through in the ALGEBRAIC phase).
        BlockMetadata tfMeta;
        tfMeta.sampleTime = SampleTimeClass::Continuous;
        tfMeta.directFeedthrough = true;
        // Vector tf: n independent SISO functions -> nx = n * signal width.
        tfMeta.acceptsVectorInputs = true;
        tfMeta.continuousStateCount = [](const Block& blk, const ValMap& vars) -> int {
            std::vector<double> aLast, cVec, initS;
            double d = 0.0;
            int n = 0;
            buildTfState(asJson(blk.params).value(nflow::kNum, json::array()),
                asJson(blk.params).value(nflow::kDen, json::array()), aLast, cVec, d, initS, n,
                vars);
            return n * std::max(1, blk.outWidth(0));
        };
        reg.registerHandler("tf", handleTf,
            { Phase::INIT, Phase::OUTPUT, Phase::ALGEBRAIC, Phase::UPDATE, Phase::DERIVATIVE }, lib,
            tfMeta);

        // delay: ring buffer sized from dt at INIT; no continuous state, so
        // variable-step solvers must reject it.
        BlockMetadata delayMeta;
        delayMeta.sampleTime = SampleTimeClass::Discrete;
        delayMeta.acceptsVectorInputs = true;
        reg.registerHandler(
            "delay", handleDelay, { Phase::INIT, Phase::OUTPUT, Phase::UPDATE }, lib, delayMeta);

        // stateSpace: MIMO. State count = nx (A is nx x nx), output width = ny
        // (rows of C). D makes it feed through, but the fixed-step engine latches
        // the state, so directFeedthrough stays false for scheduling.
        BlockMetadata ssMeta;
        ssMeta.numContinuousStates = 1;
        ssMeta.sampleTime = SampleTimeClass::Continuous;
        ssMeta.directFeedthrough = false;
        ssMeta.continuousStateCount
            = [](const Block& blk, const ValMap& vars) { return stateSpaceStateCount(blk, vars); };
        ssMeta.resolveDims = [](const Block& blk, const ValMap& vars, const std::vector<PortSig>&,
                                 std::vector<PortSig>& outSigs, std::string&) {
            if (blk.outputs > 1) {
                // Expanded form (code generation only): one scalar port per row.
                for (auto& o : outSigs) {
                    o.setVector(1);
                    o.type = SigType::Double;
                }
                return true;
            }
            outSigs[0].setVector(stateSpaceOutputWidth(blk, vars));
            outSigs[0].type = SigType::Double;
            return true;
        };
        ssMeta.acceptsVectorInputs = true; // MIMO: input is a nu-vector
        ssMeta.requiresDoubleInputs = true;
        // ALGEBRAIC re-emits y with the CURRENT inputs for the D != 0 feedthrough
        // (no-op when D is zero).
        reg.registerHandler("stateSpace", handleStateSpace,
            { Phase::INIT, Phase::OUTPUT, Phase::ALGEBRAIC, Phase::UPDATE, Phase::DERIVATIVE }, lib,
            ssMeta);

        auto widthStateCount
            = [](const Block& blk, const ValMap&) { return std::max(1, blk.outWidth(0)); };

        BlockMetadata lpfMeta;
        lpfMeta.numContinuousStates = 1;
        lpfMeta.sampleTime = SampleTimeClass::Continuous;
        lpfMeta.directFeedthrough = false;
        lpfMeta.acceptsVectorInputs = true;
        lpfMeta.continuousStateCount = widthStateCount;
        reg.registerHandler("lpf", handleLpf,
            { Phase::INIT, Phase::OUTPUT, Phase::UPDATE, Phase::DERIVATIVE }, lib, lpfMeta);

        BlockMetadata hpfMeta;
        hpfMeta.numContinuousStates = 1;
        hpfMeta.sampleTime = SampleTimeClass::Continuous;
        hpfMeta.directFeedthrough = false;
        hpfMeta.acceptsVectorInputs = true;
        hpfMeta.continuousStateCount = widthStateCount;
        reg.registerHandler("hpf", handleHpf,
            { Phase::INIT, Phase::OUTPUT, Phase::UPDATE, Phase::DERIVATIVE }, lib, hpfMeta);

        // derivative: discrete backward difference, no continuous state. Its
        // output reads the current input, so it runs in the topologically ordered
        // ALGEBRAIC phase: in the declaration-ordered OUTPUT phase it could run
        // before its source and sample an input not yet computed.
        BlockMetadata derivMeta;
        derivMeta.numDiscreteStates = 1;
        derivMeta.sampleTime = SampleTimeClass::Discrete;
        derivMeta.directFeedthrough = true;
        derivMeta.acceptsVectorInputs = true;
        reg.registerHandler("derivative", handleDerivative,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, lib, derivMeta);

        // pid: one continuous integral state per element plus a discrete
        // derivative term.
        BlockMetadata pidMeta;
        pidMeta.numContinuousStates = 1;
        pidMeta.numDiscreteStates = 1;
        pidMeta.sampleTime = SampleTimeClass::Continuous;
        // The output answers the CURRENT input (kp*u), so it is produced in the
        // ALGEBRAIC phase; emitting what UPDATE latched would lag by one step.
        pidMeta.directFeedthrough = true;
        pidMeta.acceptsVectorInputs = true;
        // A filtered derivative (N > 0) adds a second state per element, after
        // the integral.
        pidMeta.continuousStateCount = [](const Block& blk, const ValMap& vars) {
            const int w = std::max(1, blk.outWidth(0));
            nflow::BlockDescriptor bd(blk, vars);
            const double n = bd.paramDouble(nflow::kPidN, 0.0);
            return ((n > 0.0) && std::isfinite(n)) ? 2 * w : w;
        };
        reg.registerHandler("pid", handlePid,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE, Phase::DERIVATIVE }, lib, pidMeta);

        // constraint: one algebraic state whose residual g(x)=0 is the block
        // input; only meaningful under the `dae` solver.
        BlockMetadata constraintMeta;
        constraintMeta.numContinuousStates = 1;
        constraintMeta.algebraicState = true;
        constraintMeta.sampleTime = SampleTimeClass::Continuous;
        constraintMeta.directFeedthrough = false;
        reg.registerHandler("constraint", handleConstraint,
            { Phase::INIT, Phase::OUTPUT, Phase::DERIVATIVE }, lib, constraintMeta);

        // physicalIsland (acausal): an assembled electrical netlist. Its states
        // mix differential (capacitor voltages, inductor currents) and algebraic
        // (node potentials, branch currents), so it uses the per-state
        // algebraicStateMask rather than the whole-slice algebraicState flag.
        // Only meaningful under the `dae` solver.
        BlockMetadata islandMeta;
        islandMeta.sampleTime = SampleTimeClass::Continuous;
        islandMeta.directFeedthrough = false;
        islandMeta.acceptsVectorInputs = false;
        islandMeta.continuousStateCount = [](const Block& blk, const ValMap& vars) {
            return physicalIslandStateCount(blk, vars);
        };
        islandMeta.algebraicStateMask
            = [](const Block& blk, const ValMap& vars, std::vector<double>& ids) {
                  physicalIslandIdMask(blk, vars, ids);
              };
        // An ideal switch flips its branch when its control crosses 0.5; a
        // zero-crossing lets the DAE stop and reinitialise instead of failing on
        // the topology jump.
        islandMeta.zeroCrossingCount = [](const Block& blk, const ValMap& vars) {
            return physicalIslandZeroCrossingCount(blk, vars);
        };
        // EVENT: a mode-latched component (ideal diode) flips at its located knee
        // crossing and the island re-solves its algebraic state.
        reg.registerHandler("physicalIsland", handlePhysicalIsland,
            { Phase::INIT, Phase::OUTPUT, Phase::DERIVATIVE, Phase::ZERO_CROSSING, Phase::EVENT },
            lib, islandMeta);

        // mechanicalTranslationalIsland (acausal): 1-D translational mechanics.
        // All states are differential (s, v per mass node), so it runs under
        // `dae` and the fixed-step RK solvers alike.
        BlockMetadata mechMeta;
        mechMeta.sampleTime = SampleTimeClass::Continuous;
        mechMeta.directFeedthrough = false;
        mechMeta.acceptsVectorInputs = false;
        mechMeta.continuousStateCount = [](const Block& blk, const ValMap& vars) {
            return mechanicalIslandStateCount(blk, vars);
        };
        reg.registerHandler("mechanicalTranslationalIsland", handleMechanicalIsland,
            { Phase::INIT, Phase::OUTPUT, Phase::DERIVATIVE }, lib, mechMeta);

        // planarMechanicalIsland (acausal): 2-D planar multibody. Six differential
        // states per body (COM pose + velocity); revolute joints add two
        // algebraic Baumgarte multipliers (per-state algebraicStateMask), so it
        // is meaningful only under the `dae` solver.
        BlockMetadata planarMeta;
        planarMeta.sampleTime = SampleTimeClass::Continuous;
        planarMeta.directFeedthrough = false;
        planarMeta.acceptsVectorInputs = false;
        planarMeta.continuousStateCount = [](const Block& blk, const ValMap& vars) {
            return planarIslandStateCount(blk, vars);
        };
        planarMeta.algebraicStateMask
            = [](const Block& blk, const ValMap& vars, std::vector<double>& ids) {
                  planarIslandIdMask(blk, vars, ids);
              };
        reg.registerHandler("planarMechanicalIsland", handlePlanarIsland,
            { Phase::INIT, Phase::OUTPUT, Phase::DERIVATIVE }, lib, planarMeta);
        // Joint-free islands are a pure ODE and lower through the RK4 seam;
        // jointed islands are rejected by planarIslandOdeGeneratable.
        reg.registerCodegen("planarMechanicalIsland",
            BlockRegistry::BlockCodegenPair {
                getCodeGenCPlanarIsland(), getCodeGenRustPlanarIsland() });

        reg.registerCodegen("derivative", "c", getCodeGenCDerivative());
        reg.registerCodegen("derivative", "rust", getCodeGenRustDerivative());

        reg.registerCodegen("integrator", "c", getCodeGenCIntegrator());
        reg.registerCodegen("integrator", "rust", getCodeGenRustIntegrator());
        reg.registerCodegen("tf", "c", getCodeGenCTf());
        reg.registerCodegen("tf", "rust", getCodeGenRustTf());
        reg.registerCodegen("lpf", "c", getCodeGenCLpf());
        reg.registerCodegen("lpf", "rust", getCodeGenRustLpf());
        reg.registerCodegen("hpf", "c", getCodeGenCHpf());
        reg.registerCodegen("hpf", "rust", getCodeGenRustHpf());
        reg.registerCodegen("pid", "c", getCodeGenCPid());
        reg.registerCodegen("pid", "rust", getCodeGenRustPid());
        reg.registerCodegen("delay", "c", getCodeGenCDelay());
        reg.registerCodegen("delay", "rust", getCodeGenRustDelay());
        reg.registerCodegen("stateSpace", "c", getCodeGenCStateSpace());
        reg.registerCodegen("stateSpace", "rust", getCodeGenRustStateSpace());
        // mechanicalTranslationalIsland: a pure ODE, so it joins the RK4 seam
        // (ode4) and emits forward Euler otherwise.
        reg.registerCodegen("mechanicalTranslationalIsland", "c", getCodeGenCMechanicalIsland());
        reg.registerCodegen(
            "mechanicalTranslationalIsland", "rust", getCodeGenRustMechanicalIsland());
        // physicalIsland: a non-linear island (diode rectifier, RLC, radiation)
        // lowers to an embedded Newton solve with an implicit backward-Euler step
        // in C and Rust; an unsupported kind emits a typed marker.
        reg.registerCodegen("physicalIsland", "c", getCodeGenCPhysicalIsland());
        reg.registerCodegen("physicalIsland", "rust", getCodeGenRustPhysicalIsland());
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
