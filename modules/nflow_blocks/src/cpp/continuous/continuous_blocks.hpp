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
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleIntegrator(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleTf(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDelay(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleStateSpace(SimCtx& ctx, const Block& b, Phase phase);
    // MIMO state-space dimension helpers (A square nx x nx; ny = rows(C)).
    int
    stateSpaceStateCount(const Block& b, const ValMap& vars);
    int
    stateSpaceOutputWidth(const Block& b, const ValMap& vars);
    bool
    handleLpf(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleHpf(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDerivative(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handlePid(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleConstraint(SimCtx& ctx, const Block& b, Phase phase);
    // physicalIsland (acausal): assembled electrical netlist -> MNA DAE
    // residual over the block's own mixed differential/algebraic state slice.
    bool
    handlePhysicalIsland(SimCtx& ctx, const Block& b, Phase phase);
    int
    physicalIslandStateCount(const Block& b, const ValMap& vars);
    void
    physicalIslandIdMask(const Block& b, const ValMap& vars, std::vector<double>& ids);
    int
    physicalIslandZeroCrossingCount(const Block& b, const ValMap& vars);
    // mechanicalTranslationalIsland (acausal): 1-D translational mechanics,
    // (position, velocity) states per mass node, force through-variable. All
    // states differential (no algebraic mask); runs under dae and rk4.
    bool
    handleMechanicalIsland(SimCtx& ctx, const Block& b, Phase phase);
    int
    mechanicalIslandStateCount(const Block& b, const ValMap& vars);
    // planarMechanicalIsland (acausal): 2-D planar multibody in absolute COM
    // coordinates. Six differential states per body (xc, yc, phi, vx, vy, w);
    // Revolute joints add two algebraic Lagrange multipliers (Baumgarte-
    // stabilised), so it uses the per-state algebraicStateMask under `dae`.
    bool
    handlePlanarIsland(SimCtx& ctx, const Block& b, Phase phase);
    int
    planarIslandStateCount(const Block& b, const ValMap& vars);
    void
    planarIslandIdMask(const Block& b, const ValMap& vars, std::vector<double>& ids);
    // True when the island is a pure ODE the code generators can lower (no
    // joints / acceleration sensors); otherwise *whyKind names the component.
    bool
    planarIslandOdeGeneratable(const json& params, std::string* whyKind);
    BlockCodegenTemplate
    getCodeGenCPlanarIsland();
    BlockCodegenTemplate
    getCodeGenRustPlanarIsland();
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCDerivative();
    BlockCodegenTemplate
    getCodeGenRustDerivative();
    BlockCodegenTemplate
    getCodeGenCIntegrator();
    BlockCodegenTemplate
    getCodeGenRustIntegrator();
    BlockCodegenTemplate
    getCodeGenCTf();
    BlockCodegenTemplate
    getCodeGenRustTf();
    BlockCodegenTemplate
    getCodeGenCLpf();
    BlockCodegenTemplate
    getCodeGenRustLpf();
    BlockCodegenTemplate
    getCodeGenCHpf();
    BlockCodegenTemplate
    getCodeGenRustHpf();
    BlockCodegenTemplate
    getCodeGenCPid();
    BlockCodegenTemplate
    getCodeGenRustPid();
    BlockCodegenTemplate
    getCodeGenCDelay();
    BlockCodegenTemplate
    getCodeGenRustDelay();
    BlockCodegenTemplate
    getCodeGenCStateSpace();
    BlockCodegenTemplate
    getCodeGenRustStateSpace();
    BlockCodegenTemplate
    getCodeGenCMechanicalIsland();
    BlockCodegenTemplate
    getCodeGenRustMechanicalIsland();
    BlockCodegenTemplate
    getCodeGenCPhysicalIsland();
    BlockCodegenTemplate
    getCodeGenRustPhysicalIsland();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
