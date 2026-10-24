//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// width: outputs the number of elements of its input signal as a scalar. A
// modeling / introspection aid (e.g. to drive a gain by the signal width). The
// output is always scalar regardless of the input width. Interpreter-only: the
// code generator flattens vector signals to scalar wires before per-block
// emission, so the width is not recoverable in generated code.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "routing_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleWidth(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    SigView u = getInputSig(ctx, b.nid, 0);
    const int w = (u.width > 0) ? u.width : 0;
    setOutput(ctx, b.nid, static_cast<double>(w));
    return false;
}
//=============================================================================
