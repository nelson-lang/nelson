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
Nelson::NFlow::handleXyzScope(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        st.xSeries.clear();
        st.ySeries.clear();
        st.zSeries.clear();
        return false;
    }
    if (phase == Phase::AFTER_STEP) {
        static constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
        st.xSeries.push_back(hasInput(ctx, b.nid, 0) ? getInput(ctx, b.nid, 0, 0.0) : kNaN);
        st.ySeries.push_back(hasInput(ctx, b.nid, 1) ? getInput(ctx, b.nid, 1, 0.0) : kNaN);
        st.zSeries.push_back(hasInput(ctx, b.nid, 2) ? getInput(ctx, b.nid, 2, 0.0) : kNaN);
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCXyzScope()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0};";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustXyzScope()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0};";
    return t;
}
//=============================================================================
