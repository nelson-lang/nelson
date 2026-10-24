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
#include <cmath>
//=============================================================================
// selector: extract elements of the input vector by 1-based indices (the
// Indices parameter), clamped into the input range.
//=============================================================================
bool
Nelson::NFlow::handleSelector(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    std::vector<double> idx = bd.paramList(nflow::kIndices);
    SigView v = getInputSig(ctx, b.nid, 0);
    const int inW = std::max(1, v.width);
    auto pick = [&](int i) {
        int k = (i < (int)idx.size()) ? (int)std::llround(idx[i]) : (i + 1);
        return std::min(std::max(k, 1), inW) - 1;
    };
    if (v.idata && exact64OutPort(ctx, b.nid)) {
        return emitElementwiseI64(ctx, b.nid, [&](int i) { return sigAtI64(v, pick(i), v.type); });
    }
    if (v.isComplex && complexOutPort(ctx, b.nid)) {
        return emitElementwiseComplex(ctx, b.nid, [&](int i) { return sigAtC(v, pick(i)); });
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return sigAt(v, pick(i)); });
}
//=============================================================================
