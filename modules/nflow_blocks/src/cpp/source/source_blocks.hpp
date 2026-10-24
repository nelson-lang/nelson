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
    handleConstant(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleEnumeratedConstant(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleStep(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleRamp(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleImpulse(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSine(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleChirp(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleNoise(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleClock(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleFileSource(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleFromWorkspace(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleLabelSource(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleToggleSwitch(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handlePulse(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSawTooth(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleExpSine(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleTrapezoid(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleExponentials(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleCounterFreeRunning(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleCounterLimited(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleRepeatingSequenceStair(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleRepeatingSequenceInterpolated(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSignalGenerator(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCPulse();
    BlockCodegenTemplate
    getCodeGenRustPulse();
    BlockCodegenTemplate
    getCodeGenCSawTooth();
    BlockCodegenTemplate
    getCodeGenRustSawTooth();
    BlockCodegenTemplate
    getCodeGenCExpSine();
    BlockCodegenTemplate
    getCodeGenRustExpSine();
    BlockCodegenTemplate
    getCodeGenCTrapezoid();
    BlockCodegenTemplate
    getCodeGenRustTrapezoid();
    BlockCodegenTemplate
    getCodeGenCExponentials();
    BlockCodegenTemplate
    getCodeGenRustExponentials();
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCChirp();
    BlockCodegenTemplate
    getCodeGenRustChirp();
    BlockCodegenTemplate
    getCodeGenCEnumeratedConstant();
    BlockCodegenTemplate
    getCodeGenRustEnumeratedConstant();
    BlockCodegenTemplate
    getCodeGenCConstant();
    BlockCodegenTemplate
    getCodeGenRustConstant();
    BlockCodegenTemplate
    getCodeGenCClock();
    BlockCodegenTemplate
    getCodeGenRustClock();
    BlockCodegenTemplate
    getCodeGenCStep();
    BlockCodegenTemplate
    getCodeGenRustStep();
    BlockCodegenTemplate
    getCodeGenCRamp();
    BlockCodegenTemplate
    getCodeGenRustRamp();
    BlockCodegenTemplate
    getCodeGenCImpulse();
    BlockCodegenTemplate
    getCodeGenRustImpulse();
    BlockCodegenTemplate
    getCodeGenCSine();
    BlockCodegenTemplate
    getCodeGenRustSine();
    BlockCodegenTemplate
    getCodeGenCToggleSwitch();
    BlockCodegenTemplate
    getCodeGenRustToggleSwitch();
    BlockCodegenTemplate
    getCodeGenCFileSource();
    BlockCodegenTemplate
    getCodeGenRustFileSource();
    BlockCodegenTemplate
    getCodeGenCFromWorkspace();
    BlockCodegenTemplate
    getCodeGenRustFromWorkspace();
    BlockCodegenTemplate
    getCodeGenCNoise();
    BlockCodegenTemplate
    getCodeGenRustNoise();
    BlockCodegenTemplate
    getCodeGenCLabelSource();
    BlockCodegenTemplate
    getCodeGenRustLabelSource();
    BlockCodegenTemplate
    getCodeGenCCounterFreeRunning();
    BlockCodegenTemplate
    getCodeGenRustCounterFreeRunning();
    BlockCodegenTemplate
    getCodeGenCCounterLimited();
    BlockCodegenTemplate
    getCodeGenRustCounterLimited();
    BlockCodegenTemplate
    getCodeGenCRepeatingSequenceStair();
    BlockCodegenTemplate
    getCodeGenRustRepeatingSequenceStair();
    BlockCodegenTemplate
    getCodeGenCRepeatingSequenceInterpolated();
    BlockCodegenTemplate
    getCodeGenRustRepeatingSequenceInterpolated();
    BlockCodegenTemplate
    getCodeGenCSignalGenerator();
    BlockCodegenTemplate
    getCodeGenRustSignalGenerator();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
