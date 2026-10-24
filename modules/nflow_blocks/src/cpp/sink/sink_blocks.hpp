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
    handleScope(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleXyScope(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleXyzScope(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleLabelSink(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleTerminator(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDisplay(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleToWorkspace(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleStopSimulation(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleFileSink(SimCtx& ctx, const Block& b, Phase phase);
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCScope();
    BlockCodegenTemplate
    getCodeGenRustScope();
    BlockCodegenTemplate
    getCodeGenCXyScope();
    BlockCodegenTemplate
    getCodeGenRustXyScope();
    BlockCodegenTemplate
    getCodeGenCXyzScope();
    BlockCodegenTemplate
    getCodeGenRustXyzScope();
    BlockCodegenTemplate
    getCodeGenCDisplay();
    BlockCodegenTemplate
    getCodeGenRustDisplay();
    BlockCodegenTemplate
    getCodeGenCToWorkspace();
    BlockCodegenTemplate
    getCodeGenRustToWorkspace();
    BlockCodegenTemplate
    getCodeGenCLabelSink();
    BlockCodegenTemplate
    getCodeGenRustLabelSink();
    BlockCodegenTemplate
    getCodeGenCTerminator();
    BlockCodegenTemplate
    getCodeGenRustTerminator();
    BlockCodegenTemplate
    getCodeGenCStopSimulation();
    BlockCodegenTemplate
    getCodeGenRustStopSimulation();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
