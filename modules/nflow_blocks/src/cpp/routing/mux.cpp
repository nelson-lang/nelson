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
// mux: concatenate the input signals (scalar expansion NOT applied; each
// input contributes its own width) into one 1-D output vector.
//=============================================================================
bool
Nelson::NFlow::handleMux(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    const int nIn = numInputs(ctx, b.nid);
    const int wOut = outputWidth(ctx, b.nid, 0);
    double* y = outputSlice(ctx, b.nid, 0);
    bool changed = false;
    int at = 0;
    if (complexOutPort(ctx, b.nid)) {
        double* yim = outputSliceImag(ctx, b.nid, 0);
        for (int p = 0; p < nIn && at < wOut; ++p) {
            SigView v = getInputSig(ctx, b.nid, p);
            const int w = std::max(1, v.width);
            for (int k = 0; k < w && at < wOut; ++k, ++at) {
                const std::complex<double> z = sigAtC(v, k);
                if (y[at] != z.real()) {
                    y[at] = z.real();
                    changed = true;
                }
                if (yim && yim[at] != z.imag()) {
                    yim[at] = z.imag();
                    changed = true;
                }
            }
        }
        return changed;
    }
    if (const PortSig* pex = exact64OutPort(ctx, b.nid)) {
        long long* yi = outputSliceI64(ctx, b.nid, 0);
        for (int p = 0; p < nIn && at < wOut; ++p) {
            SigView v = getInputSig(ctx, b.nid, p);
            const int w = std::max(1, v.width);
            for (int k = 0; k < w && at < wOut; ++k, ++at) {
                const long long iv = sigAtI64(v, k, pex->type);
                if (yi && yi[at] != iv) {
                    yi[at] = iv;
                    changed = true;
                }
                y[at] = i64ToDouble(iv, pex->type);
            }
        }
        return changed;
    }
    for (int p = 0; p < nIn && at < wOut; ++p) {
        SigView v = getInputSig(ctx, b.nid, p);
        const int w = std::max(1, v.width);
        for (int k = 0; k < w && at < wOut; ++k, ++at) {
            const double val = sigAt(v, k);
            if (y[at] != val) {
                y[at] = val;
                changed = true;
            }
        }
    }
    return changed;
}
//=============================================================================
