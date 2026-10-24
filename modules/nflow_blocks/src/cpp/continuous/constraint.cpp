//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Algebraic constraint block. Declares one ALGEBRAIC state z
// (its output); the DAE solver drives the block's input g -> 0, so z is defined
// implicitly by the equation g(z, x) = 0 the surrounding diagram builds. OUTPUT
// emits z = x[xOffset]; DERIVATIVE writes the residual g (the block input) into
// the xdot slot, which the solver-loop residual assembly reads as the algebraic
// residual because this state's id = 0 (BlockMetadata::algebraicState). There is
// no fixed-step / discrete meaning; the block only works under the `dae` solver.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "continuous_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleConstraint(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);
        if (phase == Phase::INIT) {
            nflow::BlockDescriptor bd(b, ctx.variables);
            st.scalar = bd.paramDouble(nflow::kInitial, 0.0); // initial guess for z
            st.output = st.scalar;
            return false;
        }
        if (phase == Phase::OUTPUT) {
            const double* xs = blockX(ctx, b.nid);
            setOutput(ctx, b.nid, xs ? xs[0] : st.scalar);
            return false;
        }
        if (phase == Phase::DERIVATIVE) {
            // The algebraic residual IS the block's input g; store it in the xdot
            // slot so the DAE residual assembly picks it up (id = 0 -> r = g).
            double* xdot = blockXdot(ctx, b.nid);
            if (xdot) {
                xdot[0] = getInput(ctx, b.nid, 0, 0.0);
            }
            return false;
        }
        return false;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
