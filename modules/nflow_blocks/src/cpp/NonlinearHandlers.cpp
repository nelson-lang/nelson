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
#include "nonlinear/nonlinear_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerNonlinearHandlers(BlockRegistry& reg)
    {
        {
            BlockMetadata satMeta;
            satMeta.numZeroCrossings = 2; // clamp-engage surfaces: u-lower, u-upper
            satMeta.directFeedthrough = true;
            satMeta.zeroCrossingCount
                = [](const Block& blk, const ValMap&) { return 2 * std::max(1, blk.outWidth(0)); };
            reg.registerHandler("saturation", handleSaturation,
                { Phase::ALGEBRAIC, Phase::ZERO_CROSSING }, "builtin.nonlinear", satMeta);
        }
        {
            BlockMetadata dzMeta;
            dzMeta.numZeroCrossings = 2; // dead-band edges: u-lower, u-upper
            dzMeta.directFeedthrough = true;
            dzMeta.zeroCrossingCount
                = [](const Block& blk, const ValMap&) { return 2 * std::max(1, blk.outWidth(0)); };
            reg.registerHandler("deadZone", handleDeadZone,
                { Phase::ALGEBRAIC, Phase::ZERO_CROSSING }, "builtin.nonlinear", dzMeta);
        }
        reg.registerHandler(
            "quantizer", handleQuantizer, { Phase::ALGEBRAIC }, "builtin.nonlinear");
        {
            BlockMetadata swMeta;
            swMeta.numZeroCrossings = 1; // control crossing the threshold
            swMeta.directFeedthrough = true;
            swMeta.zeroCrossingCount
                = [](const Block& blk, const ValMap&) { return std::max(1, blk.outWidth(0)); };
            reg.registerHandler("switch", handleSwitch, { Phase::ALGEBRAIC, Phase::ZERO_CROSSING },
                "builtin.nonlinear", swMeta);
        }
        {
            // rate/backlash answer the CURRENT input (ALGEBRAIC) and commit their
            // state in UPDATE, as the emitted C/Rust step does.
            BlockMetadata rbMeta;
            rbMeta.acceptsVectorInputs = true;
            rbMeta.directFeedthrough = true;
            reg.registerHandler("rate", handleRate,
                { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.nonlinear", rbMeta);
            reg.registerHandler("backlash", handleBacklash,
                { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.nonlinear", rbMeta);
        }
        {
            BlockMetadata hyMeta;
            hyMeta.directFeedthrough = true;
            reg.registerHandler("hysteresis", handleHysteresis,
                { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.nonlinear", hyMeta);
        }
        reg.registerHandler("hitCrossing", handleHitCrossing,
            { Phase::INIT, Phase::OUTPUT, Phase::UPDATE }, "builtin.nonlinear");
        reg.registerHandler("coulombViscousFriction", handleCoulombViscousFriction,
            { Phase::ALGEBRAIC }, "builtin.nonlinear");

        // Code-generation templates (other blocks keep native emission paths).
        reg.registerCodegen("quantizer", "c", getCodeGenCQuantizer());
        reg.registerCodegen("quantizer", "rust", getCodeGenRustQuantizer());
        reg.registerCodegen("switch", "c", getCodeGenCSwitch());
        reg.registerCodegen("switch", "rust", getCodeGenRustSwitch());
        reg.registerCodegen("saturation", "c", getCodeGenCSaturation());
        reg.registerCodegen("saturation", "rust", getCodeGenRustSaturation());
        reg.registerCodegen("deadZone", "c", getCodeGenCDeadZone());
        reg.registerCodegen("deadZone", "rust", getCodeGenRustDeadZone());

        reg.registerCodegen("rate", "c", getCodeGenCRate());
        reg.registerCodegen("rate", "rust", getCodeGenRustRate());
        reg.registerCodegen("backlash", "c", getCodeGenCBacklash());
        reg.registerCodegen("backlash", "rust", getCodeGenRustBacklash());
        reg.registerCodegen("hysteresis", "c", getCodeGenCHysteresis());
        reg.registerCodegen("hysteresis", "rust", getCodeGenRustHysteresis());
        reg.registerCodegen("hitCrossing", "c", getCodeGenCHitCrossing());
        reg.registerCodegen("hitCrossing", "rust", getCodeGenRustHitCrossing());
        reg.registerCodegen("coulombViscousFriction", "c", getCodeGenCCoulombViscousFriction());
        reg.registerCodegen(
            "coulombViscousFriction", "rust", getCodeGenRustCoulombViscousFriction());
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
