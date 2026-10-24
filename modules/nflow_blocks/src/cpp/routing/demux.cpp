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
//=============================================================================
// demux: split the input vector evenly across the output ports (the split
// widths were resolved by the dimension-propagation pass).
//=============================================================================
bool
Nelson::NFlow::handleDemux(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    SigView v = getInputSig(ctx, b.nid, 0);
    const int nOut = std::max(1, b.outputs);
    bool changed = false;
    int at = 0;
    if (v.isComplex) {
        for (int p = 0; p < nOut; ++p) {
            const int wo = outputWidth(ctx, b.nid, p);
            double* y = outputSlice(ctx, b.nid, p);
            double* yim = outputSliceImag(ctx, b.nid, p);
            for (int k = 0; k < wo; ++k, ++at) {
                const std::complex<double> z = sigAtC(v, at);
                if (y[k] != z.real()) {
                    y[k] = z.real();
                    changed = true;
                }
                if (yim && yim[k] != z.imag()) {
                    yim[k] = z.imag();
                    changed = true;
                }
            }
        }
        return changed;
    }
    if (v.idata) {
        for (int p = 0; p < nOut; ++p) {
            const int wo = outputWidth(ctx, b.nid, p);
            double* y = outputSlice(ctx, b.nid, p);
            long long* yi = outputSliceI64(ctx, b.nid, p);
            for (int k = 0; k < wo; ++k, ++at) {
                const long long iv = sigAtI64(v, at, v.type);
                if (yi && yi[k] != iv) {
                    yi[k] = iv;
                    changed = true;
                }
                y[k] = i64ToDouble(iv, v.type);
            }
        }
        return changed;
    }
    for (int p = 0; p < nOut; ++p) {
        const int wo = outputWidth(ctx, b.nid, p);
        double* y = outputSlice(ctx, b.nid, p);
        for (int k = 0; k < wo; ++k, ++at) {
            const double val = sigAt(v, at);
            if (y[k] != val) {
                y[k] = val;
                changed = true;
            }
        }
    }
    return changed;
}
//=============================================================================
