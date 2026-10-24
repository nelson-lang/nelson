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
#include "math/math_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerMathHandlers(BlockRegistry& reg)
    {
        reg.registerHandler("gain", handleGain, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("sum", handleSum, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("mult", handleMult, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("abs", handleAbs, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("divide", handleDivide, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("negate", handleNegate, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("bias", handleBias, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("wrapToZero", handleWrapToZero, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("polynomial", handlePolynomial, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("min", handleMin, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("max", handleMax, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("atan2", handleAtan2, { Phase::ALGEBRAIC }, "builtin.math");
        // INIT validates the Function name. An unknown name must fail here, not turn
        // the block into a wire.
        reg.registerHandler(
            "mathFunction", handleMathFunction, { Phase::INIT, Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler(
            "roundingFunction", handleRoundingFunction, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("sign", handleSign, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler("sqrt", handleSqrt, { Phase::ALGEBRAIC }, "builtin.math");
        reg.registerHandler(
            "trigFunction", handleTrigFunction, { Phase::ALGEBRAIC }, "builtin.math");

        // Code-generation templates (sum uses a native emitter, see sum.cpp).
        reg.registerCodegen("sum", "c", getCodeGenCSum());
        reg.registerCodegen("sum", "rust", getCodeGenRustSum());
        reg.registerCodegen("mult", "c", getCodeGenCMult());
        reg.registerCodegen("mult", "rust", getCodeGenRustMult());
        reg.registerCodegen("gain", "c", getCodeGenCGain());
        reg.registerCodegen("gain", "rust", getCodeGenRustGain());
        reg.registerCodegen("abs", "c", getCodeGenCAbs());
        reg.registerCodegen("abs", "rust", getCodeGenRustAbs());
        reg.registerCodegen("divide", "c", getCodeGenCDivide());
        reg.registerCodegen("divide", "rust", getCodeGenRustDivide());
        reg.registerCodegen("negate", "c", getCodeGenCNegate());
        reg.registerCodegen("negate", "rust", getCodeGenRustNegate());
        reg.registerCodegen("bias", "c", getCodeGenCBias());
        reg.registerCodegen("bias", "rust", getCodeGenRustBias());
        reg.registerCodegen("wrapToZero", "c", getCodeGenCWrapToZero());
        reg.registerCodegen("wrapToZero", "rust", getCodeGenRustWrapToZero());
        reg.registerCodegen("polynomial", "c", getCodeGenCPolynomial());
        reg.registerCodegen("polynomial", "rust", getCodeGenRustPolynomial());
        reg.registerCodegen("min", "c", getCodeGenCMin());
        reg.registerCodegen("min", "rust", getCodeGenRustMin());
        reg.registerCodegen("max", "c", getCodeGenCMax());
        reg.registerCodegen("max", "rust", getCodeGenRustMax());
        reg.registerCodegen("atan2", "c", getCodeGenCAtan2());
        reg.registerCodegen("atan2", "rust", getCodeGenRustAtan2());
        reg.registerCodegen("mathFunction", "c", getCodeGenCMathFunction());
        reg.registerCodegen("mathFunction", "rust", getCodeGenRustMathFunction());
        reg.registerCodegen("roundingFunction", "c", getCodeGenCRoundingFunction());
        reg.registerCodegen("roundingFunction", "rust", getCodeGenRustRoundingFunction());
        reg.registerCodegen("sign", "c", getCodeGenCSign());
        reg.registerCodegen("sign", "rust", getCodeGenRustSign());
        reg.registerCodegen("sqrt", "c", getCodeGenCSqrt());
        reg.registerCodegen("sqrt", "rust", getCodeGenRustSqrt());
        reg.registerCodegen("trigFunction", "c", getCodeGenCTrigFunction());
        reg.registerCodegen("trigFunction", "rust", getCodeGenRustTrigFunction());
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
