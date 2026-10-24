//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Complex conversion blocks: realImagToComplex, complexToRealImag,
// magnitudeAngleToComplex, complexToMagnitudeAngle, conjugate. Element-wise;
// complexity of the outputs is fixed by forcesComplexOut metadata (the
// two-output extractors write both ports directly, demux-style).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "math_blocks.hpp"
#include <cmath>
#include <complex>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    static bool
    handleRealImagToComplex(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView re = getInputSig(ctx, b.nid, 0);
        SigView im = getInputSig(ctx, b.nid, 1);
        return emitElementwiseComplex(
            ctx, b.nid, [&](int i) { return std::complex<double> { sigAt(re, i), sigAt(im, i) }; });
    }
    //=============================================================================
    // Two-output extractor helper: writes elemFn(i).first to port 0 and
    // .second to port 1 (both real), demux-style change detection.
    template <typename PairFn>
    static bool
    emitTwoRealOutputs(SimCtx& ctx, BlockId nid, int w, PairFn pairFn)
    {
        double* y0 = outputSlice(ctx, nid, 0);
        double* y1 = outputSlice(ctx, nid, 1);
        bool changed = false;
        for (int i = 0; i < w; ++i) {
            const auto pv = pairFn(i);
            if (y0[i] != pv.first) {
                y0[i] = pv.first;
                changed = true;
            }
            if (y1[i] != pv.second) {
                y1[i] = pv.second;
                changed = true;
            }
        }
        return changed;
    }
    //=============================================================================
    static bool
    handleComplexToRealImag(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        const int w = std::max(1, outputWidth(ctx, b.nid, 0));
        return emitTwoRealOutputs(ctx, b.nid, w, [&](int i) {
            const std::complex<double> z = sigAtC(u, i);
            return std::pair<double, double> { z.real(), z.imag() };
        });
    }
    //=============================================================================
    static bool
    handleMagnitudeAngleToComplex(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView mag = getInputSig(ctx, b.nid, 0);
        SigView ang = getInputSig(ctx, b.nid, 1);
        return emitElementwiseComplex(ctx, b.nid, [&](int i) {
            const double m = sigAt(mag, i);
            const double a = sigAt(ang, i);
            return std::complex<double> { m * std::cos(a), m * std::sin(a) };
        });
    }
    //=============================================================================
    static bool
    handleComplexToMagnitudeAngle(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        const int w = std::max(1, outputWidth(ctx, b.nid, 0));
        return emitTwoRealOutputs(ctx, b.nid, w, [&](int i) {
            const std::complex<double> z = sigAtC(u, i);
            return std::pair<double, double> { std::abs(z), std::arg(z) };
        });
    }
    //=============================================================================
    static bool
    handleConjugate(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        if (complexOutPort(ctx, b.nid)) {
            return emitElementwiseComplex(
                ctx, b.nid, [&](int i) { return std::conj(sigAtC(u, i)); });
        }
        // Real input: conj is the identity.
        return emitElementwise(ctx, b.nid, [&](int i) { return sigAt(u, i); });
    }
    //=============================================================================
    void
    registerComplexHandlers(BlockRegistry& reg)
    {
        BlockMetadata feedthrough;
        feedthrough.directFeedthrough = true;
        reg.registerHandler("realImagToComplex", handleRealImagToComplex, { Phase::ALGEBRAIC },
            "builtin.math", feedthrough);
        reg.registerHandler("complexToRealImag", handleComplexToRealImag, { Phase::ALGEBRAIC },
            "builtin.math", feedthrough);
        reg.registerHandler("magnitudeAngleToComplex", handleMagnitudeAngleToComplex,
            { Phase::ALGEBRAIC }, "builtin.math", feedthrough);
        reg.registerHandler("complexToMagnitudeAngle", handleComplexToMagnitudeAngle,
            { Phase::ALGEBRAIC }, "builtin.math", feedthrough);
        reg.registerHandler(
            "conjugate", handleConjugate, { Phase::ALGEBRAIC }, "builtin.math", feedthrough);

        // Width-generalized, default dims rule (outputs adopt the input
        // shape; the two-output extractors get it on both ports).
        for (const char* t : { "realImagToComplex", "complexToRealImag", "magnitudeAngleToComplex",
                 "complexToMagnitudeAngle", "conjugate" }) {
            reg.setDimsMetadata(t, true);
        }
        // Complexity contracts:
        // - builders emit complex from real parts;
        // - extractors consume complex, emit real;
        // - conjugate passes complexity through (real conj == identity).
        reg.setComplexSupport("realImagToComplex", false, 1);
        reg.setComplexSupport("magnitudeAngleToComplex", false, 1);
        reg.setComplexSupport("complexToRealImag", true, 0);
        reg.setComplexSupport("complexToMagnitudeAngle", true, 0);
        reg.setComplexSupport("conjugate", true, -1);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
