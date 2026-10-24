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
// reshape: element passthrough; only the port dims differ (validated at
// INIT: the propagation pass may not have known the input width yet).
//=============================================================================
bool
Nelson::NFlow::handleReshape(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase == Phase::INIT) {
        const int wOut = outputWidth(ctx, b.nid, 0);
        SigView v = getInputSig(ctx, b.nid, 0);
        if (v.width > 1 && v.width != wOut && ctx.diag) {
            SimDiagnostic d;
            d.code = "reshape_mismatch";
            d.blockId = b.id;
            d.message = "reshape: input width " + std::to_string(v.width)
                + " does not match the declared output element count " + std::to_string(wOut);
            ctx.diag->fail(std::move(d));
        }
        return false;
    }
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    SigView v = getInputSig(ctx, b.nid, 0);
    if (v.idata && exact64OutPort(ctx, b.nid)) {
        return emitElementwiseI64(ctx, b.nid, [&](int i) { return sigAtI64(v, i, v.type); });
    }
    if (v.isComplex && complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int i) { return sigAtC(v, i); });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return sigAt(v, i); });
}
//=============================================================================
