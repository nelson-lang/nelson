//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// matmul: two-input matrix product / element-wise product over matrix signals.
//   MultiplicationRule = "matrix"      -> Y = A * B  ([m,k]x[k,n] -> [m,n])
//                        "elementwise" -> Y = A .* B (same dims)
// Column-major element layout (element (r,c) at index c*rows + r), matching the
// engine's PortSig convention. The accumulation happens in double and the
// result is quantized to the unified input signal type.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <algorithm>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Defined in vectorMath.cpp (same builtin.math library).
    void
    registerVectorMathHandlers(BlockRegistry& reg);
    //=============================================================================
    static bool
    matmulIsElementwise(const Block& b)
    {
        auto it = asJson(b.params).find(nflow::kMulRule);
        if (it != asJson(b.params).end() && it->is_string()) {
            const std::string r = it->get<std::string>();
            return r == "elementwise" || r == ".*" || r == "element-wise";
        }
        return false;
    }
    //=============================================================================
    static bool
    handleMatmul(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        const PortSig* ps = portSigOf(ctx, b.nid, 0);
        const SigType ty = ps ? ps->type : SigType::Double;
        const bool typed = (ty != SigType::Double);
        const bool sat = typed ? blockSaturates(ctx, b.nid) : true;
        const int wOut = ps ? ps->width() : 1;
        double* y = outputSlice(ctx, b.nid, 0);
        SigView A = getInputSig(ctx, b.nid, 0);
        SigView B = getInputSig(ctx, b.nid, 1);

        std::vector<double> tmp(std::max(1, wOut), 0.0);

        if (matmulIsElementwise(b)) {
            for (int i = 0; i < wOut; ++i) {
                tmp[i] = sigAt(A, i) * sigAt(B, i);
            }
        } else {
            // Matrix product: A[m x k] * B[k x n] -> Y[m x n]. Scalars expand.
            const int m = (A.width > 1) ? A.rows : (ps ? ps->rows() : 1);
            const int k = (A.width > 1) ? A.cols : ((B.width > 1) ? B.rows : 1);
            const int n = (B.width > 1) ? B.cols : (ps ? ps->cols() : 1);
            auto aAt = [&](int r, int c) {
                return (A.width > 1) ? A.data[c * A.rows + r] : sigAt(A, 0);
            };
            auto bAt = [&](int r, int c) {
                return (B.width > 1) ? B.data[c * B.rows + r] : sigAt(B, 0);
            };
            for (int c = 0; c < n; ++c) {
                for (int r = 0; r < m; ++r) {
                    double acc = 0.0;
                    for (int t = 0; t < k; ++t) {
                        acc += aAt(r, t) * bAt(t, c);
                    }
                    const int idx = c * m + r;
                    if (idx < wOut) {
                        tmp[idx] = acc;
                    }
                }
            }
        }

        bool changed = false;
        BlockState& st = ctx.blockState[b.nid];
        if ((int)st.outLatch.size() != wOut) {
            st.outLatch.assign(wOut, 0.0);
        }
        for (int i = 0; i < wOut; ++i) {
            double v = typed ? quantizeToType(tmp[i], ty, sat) : tmp[i];
            if (y[i] != v) {
                y[i] = v;
                changed = true;
            }
            st.outLatch[i] = v;
        }
        return changed;
    }
    //=============================================================================
    void
    registerMatrixHandlers(BlockRegistry& reg)
    {
        BlockMetadata feedthrough;
        feedthrough.directFeedthrough = true;
        reg.registerHandler(
            "matmul", handleMatmul, { Phase::ALGEBRAIC }, "builtin.math", feedthrough);

        reg.setDimsMetadata("matmul", true,
            [](const Block& b, const ValMap&, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                PortSig A = inSigs.size() > 0 ? inSigs[0] : PortSig {};
                PortSig B = inSigs.size() > 1 ? inSigs[1] : PortSig {};
                // Unified type (both inputs must agree; scalar/unconnected = A's).
                const SigType ty = A.type;
                if (B.width() > 0 && inSigs.size() > 1 && B.type != ty) {
                    err = std::string("matmul inputs mix signal types (") + sigTypeName(ty) + " vs "
                        + sigTypeName(B.type) + "); insert a convert block";
                    return false;
                }
                outSigs[0].type = ty;
                if (matmulIsElementwise(b)) {
                    // Element-wise: dims are the element-wise max (scalar
                    // expand); non-scalar shapes must match (padded with
                    // trailing 1s, so [n] and [n x 1] stay interchangeable).
                    const PortSig& big = (A.width() >= B.width()) ? A : B;
                    outSigs[0].copyShape(big);
                    if (A.width() > 1 && B.width() > 1) {
                        const size_t n = std::max(A.dims.size(), B.dims.size());
                        for (size_t k = 0; k < n; ++k) {
                            const int da = (k < A.dims.size()) ? A.dims[k] : 1;
                            const int db = (k < B.dims.size()) ? B.dims[k] : 1;
                            if (da != db) {
                                err = "elementwise matmul requires equal input dimensions";
                                return false;
                            }
                        }
                    }
                    return true;
                }
                // Matrix product: 2-D only, inner dims must agree (scalars
                // pass through).
                if ((A.width() > 1 && A.numDims() > 2) || (B.width() > 1 && B.numDims() > 2)) {
                    err = "matrix product requires 2-D inputs";
                    return false;
                }
                if (A.width() > 1 && B.width() > 1 && A.cols() != B.rows()) {
                    err = "matrix product dimension mismatch: [" + std::to_string(A.rows()) + "x"
                        + std::to_string(A.cols()) + "] * [" + std::to_string(B.rows()) + "x"
                        + std::to_string(B.cols()) + "]";
                    return false;
                }
                if (A.width() <= 1 && B.width() <= 1) {
                    outSigs[0].setScalar();
                } else if (A.width() <= 1) {
                    outSigs[0].copyShape(B); // scalar * matrix
                } else if (B.width() <= 1) {
                    outSigs[0].copyShape(A); // matrix * scalar
                } else {
                    outSigs[0].setShape2D(A.rows(), B.cols());
                }
                return true;
            });

        registerVectorMathHandlers(reg);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
