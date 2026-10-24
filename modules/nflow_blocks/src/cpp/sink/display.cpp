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
Nelson::NFlow::handleDisplay(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        st.scalar = std::numeric_limits<double>::quiet_NaN();
        st.scalar2 = 0.0; // decimation step counter
        return false;
    }
    if (phase == Phase::AFTER_STEP) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        int decimation = static_cast<int>(bd.paramDouble(nflow::kDecimation, 1.0));
        if (decimation < 1) {
            decimation = 1;
        }
        st.scalar2 += 1.0;
        if (static_cast<int>(st.scalar2) % decimation == 0) {
            SigView v = getInputSig(ctx, b.nid, 0);
            if (v.width > 1) {
                st.displayValues.assign(v.data, v.data + v.width);
                st.scalar = v.data[0];
            } else {
                st.displayValues.clear();
                st.scalar = hasInput(ctx, b.nid, 0) ? getInput(ctx, b.nid, 0, 0.0)
                                                    : std::numeric_limits<double>::quiet_NaN();
            }
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCDisplay()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0};";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustDisplay()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0};";
    return t;
}
//=============================================================================
