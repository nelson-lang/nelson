//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// sign: element-wise signum. out = -1 (u<0), 0 (u==0), +1 (u>0). No params.
// Feedthrough (ALGEBRAIC), real double signals, vector element-wise; has
// C / Rust code generation (branchless (u>0)-(u<0)).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "math_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleSign(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) {
            const double x = sigAt(u, i);
            return (x > 0.0) ? 1.0 : ((x < 0.0) ? -1.0 : 0.0);
        });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCSign()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = (double)(({in0} > 0.0) - ({in0} < 0.0));";
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustSign()
    {
        BlockCodegenTemplate t;
        t.step = "out_{id} = (({in0} > 0.0) as i32 - ({in0} < 0.0) as i32) as f64;";
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
