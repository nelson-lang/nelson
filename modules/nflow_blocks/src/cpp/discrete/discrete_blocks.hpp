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
    // Multi-rate sample rate for the code generators: round(SampleTime / base
    // step), >= 1 (1 = every step / single-rate). Shared by state-holding block
    // emitters that gate their state advance on the generated __nflow_step index.
    // Kept in sync with runSimulation's per-block rate.
    inline int
    codegenSampleRate(const nlohmann::json* params, double dt)
    {
        if (params == nullptr || dt <= 0.0 || !params->contains("SampleTime")
            || !(*params)["SampleTime"].is_number()) {
            return 1;
        }
        const double ts = (*params)["SampleTime"].get<double>();
        if (ts <= dt * (1.0 + 1e-9)) {
            return 1;
        }
        const int rate = static_cast<int>(ts / dt + 0.5);
        return (rate < 1) ? 1 : rate;
    }
    //=============================================================================
    bool
    handleZoh(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleFoh(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDtf(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDdelay(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDstateSpace(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleUnitDelay(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleRateTransition(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDifference(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDetectChange(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleRisingEdge(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleFallingEdge(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDetectIncrease(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDetectDecrease(SimCtx& ctx, const Block& b, Phase phase);
    //=============================================================================
    // Code-generation templates (see BlockCodegenTemplate).
    BlockCodegenTemplate
    getCodeGenCRateTransition();
    BlockCodegenTemplate
    getCodeGenRustRateTransition();
    BlockCodegenTemplate
    getCodeGenCZoh();
    BlockCodegenTemplate
    getCodeGenRustZoh();
    BlockCodegenTemplate
    getCodeGenCFoh();
    BlockCodegenTemplate
    getCodeGenRustFoh();
    BlockCodegenTemplate
    getCodeGenCDtf();
    BlockCodegenTemplate
    getCodeGenRustDtf();
    BlockCodegenTemplate
    getCodeGenCDdelay();
    BlockCodegenTemplate
    getCodeGenRustDdelay();
    BlockCodegenTemplate
    getCodeGenCDstateSpace();
    BlockCodegenTemplate
    getCodeGenRustDstateSpace();
    BlockCodegenTemplate
    getCodeGenCUnitDelay();
    BlockCodegenTemplate
    getCodeGenRustUnitDelay();
    BlockCodegenTemplate
    getCodeGenCDifference();
    BlockCodegenTemplate
    getCodeGenRustDifference();
    BlockCodegenTemplate
    getCodeGenCDetectChange();
    BlockCodegenTemplate
    getCodeGenRustDetectChange();
    BlockCodegenTemplate
    getCodeGenCRisingEdge();
    BlockCodegenTemplate
    getCodeGenRustRisingEdge();
    BlockCodegenTemplate
    getCodeGenCFallingEdge();
    BlockCodegenTemplate
    getCodeGenRustFallingEdge();
    BlockCodegenTemplate
    getCodeGenCDetectIncrease();
    BlockCodegenTemplate
    getCodeGenRustDetectIncrease();
    BlockCodegenTemplate
    getCodeGenCDetectDecrease();
    BlockCodegenTemplate
    getCodeGenRustDetectDecrease();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
