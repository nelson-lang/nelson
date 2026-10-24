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
#include <cstdint>
#include <cmath>
#include "logic_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleAnd(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    int n = numInputs(ctx, b.nid);
    std::vector<SigView> ins(n);
    for (int i = 0; i < n; ++i) {
        ins[i] = getInputSig(ctx, b.nid, i);
    }
    return emitElementwise(ctx, b.nid, [&](int k) {
        for (int i = 0; i < n; ++i) {
            double v = sigAt(ins[i], k);
            if (std::isnan(v) || v == 0.0) {
                return 0.0;
            }
        }
        return 1.0;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCAnd()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = (({in0} != 0.0) && ({in1} != 0.0)) ? 1.0 : 0.0;";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustAnd()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = if {in0} != 0.0_f64 && {in1} != 0.0_f64 { 1.0_f64 } else { 0.0_f64 };";
    return t;
}
//=============================================================================
