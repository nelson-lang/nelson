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
    handleSaturation(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDeadZone(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleQuantizer(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSwitch(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleRate(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleBacklash(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleHysteresis(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleHitCrossing(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleCoulombViscousFriction(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCCoulombViscousFriction();
    BlockCodegenTemplate
    getCodeGenRustCoulombViscousFriction();
    BlockCodegenTemplate
    getCodeGenCHysteresis();
    BlockCodegenTemplate
    getCodeGenRustHysteresis();
    BlockCodegenTemplate
    getCodeGenCHitCrossing();
    BlockCodegenTemplate
    getCodeGenRustHitCrossing();
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCQuantizer();
    BlockCodegenTemplate
    getCodeGenRustQuantizer();
    BlockCodegenTemplate
    getCodeGenCSwitch();
    BlockCodegenTemplate
    getCodeGenRustSwitch();
    BlockCodegenTemplate
    getCodeGenCSaturation();
    BlockCodegenTemplate
    getCodeGenRustSaturation();
    BlockCodegenTemplate
    getCodeGenCDeadZone();
    BlockCodegenTemplate
    getCodeGenRustDeadZone();
    BlockCodegenTemplate
    getCodeGenCRate();
    BlockCodegenTemplate
    getCodeGenRustRate();
    BlockCodegenTemplate
    getCodeGenCBacklash();
    BlockCodegenTemplate
    getCodeGenRustBacklash();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
