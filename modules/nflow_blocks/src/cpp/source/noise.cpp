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
#include "source_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleNoise(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    double amp = bd.paramDouble(nflow::kAmp, 0.0);
    auto& state = getState(ctx, b.nid);
    state.rngState = 1664525u * state.rngState + 1013904223u;
    setOutput(ctx, b.nid, amp * ((state.rngState / 4294967295.0) * 2.0 - 1.0));
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCNoise()
{
    BlockCodegenTemplate t;
    t.sharedPriority = 6;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.declState("unsigned int rng_state;");
        a.addInit("  s->rng_state = 1u;");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.line("s->rng_state = 1664525u * s->rng_state + 1013904223u;");
        a.line("out_" + a.id + " = " + a.fmt(bd.paramDouble(nflow::kAmp, 0.0))
            + " * ((s->rng_state / 4294967295.0) * 2.0 - 1.0);");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustNoise()
{
    BlockCodegenTemplate t;
    t.sharedPriority = 6;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        a.declState("    pub rng_state: u32,");
        a.addInit("    s.rng_state = 1_u32;");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        // Same linear congruential generator and [-1, 1] mapping as the
        // interpreter (handleNoise) and the C backend, so the Rust program
        // reproduces the noise stream bit-for-bit. u32 arithmetic wraps modulo
        // 2^32 like C's unsigned int; wrapping_mul/wrapping_add keep that and
        // avoid a debug-build overflow panic.
        a.line("s.rng_state = 1664525_u32.wrapping_mul(s.rng_state)"
               ".wrapping_add(1013904223_u32);");
        a.line("out_" + a.id + " = " + a.fmt(bd.paramDouble(nflow::kAmp, 0.0))
            + " * ((s.rng_state as f64 / 4294967295.0_f64) * 2.0_f64 - 1.0_f64);");
    };
    return t;
}
//=============================================================================
