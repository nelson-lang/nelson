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
    handleGain(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSum(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleMult(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleAbs(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDivide(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleNegate(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleBias(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleWrapToZero(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handlePolynomial(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleMin(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleMax(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleAtan2(SimCtx& ctx, const Block& b, Phase phase);
    // mathFunction: elementwise sin/cos/.../sqrt/sign via a Function param.
    bool
    handleMathFunction(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCMathFunction();
    BlockCodegenTemplate
    getCodeGenRustMathFunction();
    // roundingFunction: elementwise floor/ceil/round/fix via an Operator param.
    bool
    handleRoundingFunction(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCRoundingFunction();
    BlockCodegenTemplate
    getCodeGenRustRoundingFunction();
    // sign: elementwise signum (-1/0/+1). No params.
    bool
    handleSign(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCSign();
    BlockCodegenTemplate
    getCodeGenRustSign();
    // sqrt: elementwise sqrt/signedSqrt/rSqrt via a Function param.
    bool
    handleSqrt(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCSqrt();
    BlockCodegenTemplate
    getCodeGenRustSqrt();
    // trigFunction: elementwise sin/cos/.../atanh via a Function param.
    bool
    handleTrigFunction(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCTrigFunction();
    BlockCodegenTemplate
    getCodeGenRustTrigFunction();
    //=============================================================================
    // Code-generation templates (C / Rust); sum uses a native emitStep callback
    // (variable-arity emission with per-input signs).
    BlockCodegenTemplate
    getCodeGenCSum();
    BlockCodegenTemplate
    getCodeGenRustSum();
    BlockCodegenTemplate
    getCodeGenCMult();
    BlockCodegenTemplate
    getCodeGenRustMult();
    BlockCodegenTemplate
    getCodeGenCGain();
    BlockCodegenTemplate
    getCodeGenRustGain();
    BlockCodegenTemplate
    getCodeGenCAbs();
    BlockCodegenTemplate
    getCodeGenRustAbs();
    BlockCodegenTemplate
    getCodeGenCDivide();
    BlockCodegenTemplate
    getCodeGenRustDivide();
    BlockCodegenTemplate
    getCodeGenCNegate();
    BlockCodegenTemplate
    getCodeGenRustNegate();
    BlockCodegenTemplate
    getCodeGenCBias();
    BlockCodegenTemplate
    getCodeGenCWrapToZero();
    BlockCodegenTemplate
    getCodeGenRustWrapToZero();
    BlockCodegenTemplate
    getCodeGenCPolynomial();
    BlockCodegenTemplate
    getCodeGenRustPolynomial();
    BlockCodegenTemplate
    getCodeGenRustBias();
    BlockCodegenTemplate
    getCodeGenCMin();
    BlockCodegenTemplate
    getCodeGenRustMin();
    BlockCodegenTemplate
    getCodeGenCMax();
    BlockCodegenTemplate
    getCodeGenRustMax();
    BlockCodegenTemplate
    getCodeGenCAtan2();
    BlockCodegenTemplate
    getCodeGenRustAtan2();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
