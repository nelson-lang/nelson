//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <algorithm>
#include "sink_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleScope(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        // One series per signal ELEMENT: a vector wire of width w on one port
        // contributes w series (the port order is preserved).
        int n = numInputs(ctx, b.nid);
        int total = 0;
        for (int i = 0; i < n; ++i) {
            SigView v = getInputSig(ctx, b.nid, i);
            total += std::max(1, v.width);
        }
        st.scopeSeries.assign(total, std::vector<double> {});
        return false;
    }
    if (phase == Phase::AFTER_STEP) {
        int n = numInputs(ctx, b.nid);
        int s = 0;
        for (int i = 0; i < n && s < (int)st.scopeSeries.size(); ++i) {
            SigView v = getInputSig(ctx, b.nid, i);
            const int w = std::max(1, v.width);
            const bool conn = hasInput(ctx, b.nid, i);
            for (int k = 0; k < w && s < (int)st.scopeSeries.size(); ++k, ++s) {
                double val = conn ? sigAt(v, k) : std::numeric_limits<double>::quiet_NaN();
                st.scopeSeries[s].push_back(val);
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCScope()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0};";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustScope()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0};";
    return t;
}
//=============================================================================
