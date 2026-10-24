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
#include "logic_helpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleAnd(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleOr(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleXor(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleNot(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleCompareToConstant(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleCompareToZero(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleIntervalTest(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleIntervalTestDynamic(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleBitwiseOperator(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleBitSet(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleBitClear(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleExtractBits(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleCombinatorialLogic(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleShiftArithmetic(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleRelationalOperator(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleLogicalOperator(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSwitchCase(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleIf(SimCtx& ctx, const Block& b, Phase phase);
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCCombinatorialLogic();
    BlockCodegenTemplate
    getCodeGenRustCombinatorialLogic();
    BlockCodegenTemplate
    getCodeGenCCompareToConstant();
    BlockCodegenTemplate
    getCodeGenRustCompareToConstant();
    BlockCodegenTemplate
    getCodeGenCCompareToZero();
    BlockCodegenTemplate
    getCodeGenRustCompareToZero();
    BlockCodegenTemplate
    getCodeGenCIntervalTest();
    BlockCodegenTemplate
    getCodeGenRustIntervalTest();
    BlockCodegenTemplate
    getCodeGenCIntervalTestDynamic();
    BlockCodegenTemplate
    getCodeGenRustIntervalTestDynamic();
    BlockCodegenTemplate
    getCodeGenCBitwiseOperator();
    BlockCodegenTemplate
    getCodeGenRustBitwiseOperator();
    BlockCodegenTemplate
    getCodeGenCBitSet();
    BlockCodegenTemplate
    getCodeGenRustBitSet();
    BlockCodegenTemplate
    getCodeGenCBitClear();
    BlockCodegenTemplate
    getCodeGenRustBitClear();
    BlockCodegenTemplate
    getCodeGenCExtractBits();
    BlockCodegenTemplate
    getCodeGenRustExtractBits();
    BlockCodegenTemplate
    getCodeGenCShiftArithmetic();
    BlockCodegenTemplate
    getCodeGenRustShiftArithmetic();
    BlockCodegenTemplate
    getCodeGenCSwitchCase();
    BlockCodegenTemplate
    getCodeGenRustSwitchCase();
    BlockCodegenTemplate
    getCodeGenCIf();
    BlockCodegenTemplate
    getCodeGenRustIf();
    BlockCodegenTemplate
    getCodeGenCRelationalOperator();
    BlockCodegenTemplate
    getCodeGenRustRelationalOperator();
    BlockCodegenTemplate
    getCodeGenCLogicalOperator();
    BlockCodegenTemplate
    getCodeGenRustLogicalOperator();
    BlockCodegenTemplate
    getCodeGenCAnd();
    BlockCodegenTemplate
    getCodeGenRustAnd();
    BlockCodegenTemplate
    getCodeGenCOr();
    BlockCodegenTemplate
    getCodeGenRustOr();
    BlockCodegenTemplate
    getCodeGenCXor();
    BlockCodegenTemplate
    getCodeGenRustXor();
    BlockCodegenTemplate
    getCodeGenCNot();
    BlockCodegenTemplate
    getCodeGenRustNot();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
