//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Vector math blocks (Coselica Blocks.Math.Vectors parity):
//   sumElements:       y = sum_i u[i]         (one vector -> scalar)
//   productOfElements: y = prod_i u[i]        (one vector -> scalar)
//   dotProduct:        y = sum_i a[i] b[i]    (two vectors -> scalar)
//   crossProduct:      y = a x b              (two 3-vectors -> 3-vector)
// All feedthrough (ALGEBRAIC), real double; dims set via resolveDims.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    static bool
    handleSumElements(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        double acc = 0.0;
        for (int i = 0; i < u.width; ++i) {
            acc += sigAt(u, i);
        }
        setOutput(ctx, b.nid, acc);
        return false;
    }
    //=============================================================================
    static bool
    handleProductOfElements(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        double acc = 1.0;
        for (int i = 0; i < u.width; ++i) {
            acc *= sigAt(u, i);
        }
        setOutput(ctx, b.nid, acc);
        return false;
    }
    //=============================================================================
    static bool
    handleDotProduct(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView a = getInputSig(ctx, b.nid, 0);
        SigView bb = getInputSig(ctx, b.nid, 1);
        const int n = std::min(a.width, bb.width);
        double acc = 0.0;
        for (int i = 0; i < n; ++i) {
            acc += sigAt(a, i) * sigAt(bb, i);
        }
        setOutput(ctx, b.nid, acc);
        return false;
    }
    //=============================================================================
    static bool
    handleCrossProduct(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        SigView a = getInputSig(ctx, b.nid, 0);
        SigView bb = getInputSig(ctx, b.nid, 1);
        double* y = outputSlice(ctx, b.nid, 0);
        const double a0 = sigAt(a, 0), a1 = sigAt(a, 1), a2 = sigAt(a, 2);
        const double b0 = sigAt(bb, 0), b1 = sigAt(bb, 1), b2 = sigAt(bb, 2);
        if (outputWidth(ctx, b.nid, 0) >= 3) {
            y[0] = a1 * b2 - a2 * b1;
            y[1] = a2 * b0 - a0 * b2;
            y[2] = a0 * b1 - a1 * b0;
        }
        return false;
    }
    //=============================================================================
    void
    registerVectorMathHandlers(BlockRegistry& reg)
    {
        BlockMetadata ft;
        ft.directFeedthrough = true;
        ft.acceptsVectorInputs = true;
        ft.requiresDoubleInputs = true;

        reg.registerHandler(
            "sumElements", handleSumElements, { Phase::ALGEBRAIC }, "builtin.math", ft);
        reg.setDimsMetadata("sumElements", true,
            [](const Block&, const ValMap&, const std::vector<PortSig>&,
                std::vector<PortSig>& outSigs, std::string&) {
                outSigs[0].setScalar();
                outSigs[0].type = SigType::Double;
                return true;
            });

        reg.registerHandler(
            "productOfElements", handleProductOfElements, { Phase::ALGEBRAIC }, "builtin.math", ft);
        reg.setDimsMetadata("productOfElements", true,
            [](const Block&, const ValMap&, const std::vector<PortSig>&,
                std::vector<PortSig>& outSigs, std::string&) {
                outSigs[0].setScalar();
                outSigs[0].type = SigType::Double;
                return true;
            });

        reg.registerHandler(
            "dotProduct", handleDotProduct, { Phase::ALGEBRAIC }, "builtin.math", ft);
        reg.setDimsMetadata("dotProduct", true,
            [](const Block&, const ValMap&, const std::vector<PortSig>&,
                std::vector<PortSig>& outSigs, std::string&) {
                outSigs[0].setScalar();
                outSigs[0].type = SigType::Double;
                return true;
            });

        reg.registerHandler(
            "crossProduct", handleCrossProduct, { Phase::ALGEBRAIC }, "builtin.math", ft);
        reg.setDimsMetadata("crossProduct", true,
            [](const Block&, const ValMap&, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                for (size_t k = 0; k < inSigs.size() && k < 2; ++k) {
                    if (inSigs[k].width() > 1 && inSigs[k].width() != 3) {
                        err = "crossProduct requires 3-element vector inputs";
                        return false;
                    }
                }
                outSigs[0].setVector(3);
                outSigs[0].type = SigType::Double;
                return true;
            });
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
