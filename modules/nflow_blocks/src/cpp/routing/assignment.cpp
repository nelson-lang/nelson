//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// assignment: writes elements into a signal. Port 0 is the base signal (its
// width sets the output width); port 1 carries the replacement values. Indices
// (1-based) selects which base elements are overwritten by the successive values
// i.e. out = base, then out[Indices[k]] = values[k]. Elements not listed pass
// through unchanged. Simulation-only (vector-shape routing, like reshape /
// selector): no scalar code-generation path.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include "routing_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleAssignment(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const std::vector<double> indices = bd.paramList("Indices");
    SigView base = getInputSig(ctx, b.nid, 0);
    SigView vals = getInputSig(ctx, b.nid, 1);
    return emitElementwise(ctx, b.nid, [&](int i) -> double {
        for (size_t k = 0; k < indices.size(); ++k) {
            if (static_cast<int>(std::lround(indices[k])) - 1 == i) {
                return sigAt(vals, static_cast<int>(k));
            }
        }
        return sigAt(base, i);
    });
}
//=============================================================================
