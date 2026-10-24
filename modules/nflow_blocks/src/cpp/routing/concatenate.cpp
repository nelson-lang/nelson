//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "routing_blocks.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <algorithm>
//=============================================================================
// concatenate: N-D concatenation along ConcatenateDimension (1 = rows /
// vertical, 2 = cols / horizontal, >= 3 stacks along a higher dimension).
// Column-major layout on both sides: with P = product of the output dims
// before the concat dim and O = product of the dims after it, each outer
// slice o interleaves one contiguous chunk of P * extent_p(dim) elements per
// input. Scalar inputs broadcast via sigAt.
//=============================================================================
bool
Nelson::NFlow::handleConcatenate(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    int dim = (int)std::llround(bd.paramDouble(nflow::kConcatDim, 1.0));
    if (dim < 1) {
        dim = 1;
    }
    const PortSig* ps = portSigOf(ctx, b.nid, 0);
    const int nIn = numInputs(ctx, b.nid);
    std::vector<SigView> ins(nIn);
    for (int p = 0; p < nIn; ++p) {
        ins[p] = getInputSig(ctx, b.nid, p);
    }
    const int wOut = outputWidth(ctx, b.nid, 0);
    double* y = outputSlice(ctx, b.nid, 0);
    bool changed = false;
    // Prefix product of the output shape before the concat dim, and the
    // output extent along it (dims beyond the resolved rank count as 1).
    int prefix = 1;
    int extentOut = wOut;
    if (ps) {
        const int nd = ps->numDims();
        for (int k = 1; k < dim && k <= nd; ++k) {
            prefix *= ps->dims[(size_t)k - 1];
        }
        extentOut = (dim <= nd) ? ps->dims[(size_t)dim - 1] : 1;
    }
    const int slice = std::max(1, prefix * extentOut);
    const int nOuter = std::max(1, wOut / slice);
    const PortSig* pex = exact64OutPort(ctx, b.nid);
    long long* yi = pex ? outputSliceI64(ctx, b.nid, 0) : nullptr;
    const bool cx = complexOutPort(ctx, b.nid) != nullptr;
    double* yim = cx ? outputSliceImag(ctx, b.nid, 0) : nullptr;
    for (int o = 0; o < nOuter; ++o) {
        const int dstBase = o * slice;
        int along = 0;
        for (int p = 0; p < nIn; ++p) {
            int ep = 1;
            if (ins[p].width > 1 && ins[p].dims) {
                ep = (dim <= ins[p].numDims) ? ins[p].dims[dim - 1] : 1;
            }
            const int chunk = prefix * ep;
            for (int k = 0; k < chunk; ++k) {
                const int dstIdx = dstBase + prefix * along + k;
                int srcIdx = o * chunk + k;
                if (ins[p].width > 1 && srcIdx >= ins[p].width) {
                    srcIdx = ins[p].width - 1; // clamp to the last element
                }
                if (dstIdx < wOut) {
                    if (yi) {
                        const long long iv = sigAtI64(ins[p], srcIdx, pex->type);
                        if (yi[dstIdx] != iv) {
                            yi[dstIdx] = iv;
                            changed = true;
                        }
                        y[dstIdx] = i64ToDouble(iv, pex->type);
                    } else if (yim) {
                        const std::complex<double> z = sigAtC(ins[p], srcIdx);
                        if (y[dstIdx] != z.real()) {
                            y[dstIdx] = z.real();
                            changed = true;
                        }
                        if (yim[dstIdx] != z.imag()) {
                            yim[dstIdx] = z.imag();
                            changed = true;
                        }
                    } else {
                        const double val = sigAt(ins[p], srcIdx);
                        if (y[dstIdx] != val) {
                            y[dstIdx] = val;
                            changed = true;
                        }
                    }
                }
            }
            along += ep;
        }
    }
    return changed;
}
//=============================================================================
