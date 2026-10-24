//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "nlsNflow_blocks_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Result of a global continuous-state / zero-crossing layout pass.
    struct StateLayout
    {
        int totalContinuous = 0; // length of the global x / xdot vectors
        int totalZeroCrossings = 0; // length of the global g vector
    };
    //=============================================================================
    // Assign each block a contiguous (offset, length) slice of the global
    // continuous-state vector and of the zero-crossing-surface vector, writing
    // the offsets into BlockState (xOffset/xLength, gOffset/gLength). Nested
    // subsystems are flattened by recursing into BlockState::subsystem, so one
    // flat x vector spans every level.
    //
    // Per-block counts come from BlockRegistry metadata: the parameter-dependent
    // continuousStateCount / zeroCrossingCount callbacks when present, otherwise
    // the static numContinuousStates / numZeroCrossings. Blocks with no
    // continuous state get xOffset = -1.
    //
    // Must run AFTER the INIT phase, because parameter-sized blocks (tf order,
    // stateSpace) only know their state count once initialised.
    NLSNFLOW_BLOCKS_IMPEXP StateLayout
    buildStateLayout(const std::vector<Block>& blocks, std::vector<BlockState>& blockState,
        const BlockRegistry& reg, const ValMap& vars);
    //=============================================================================
    // Fill the DAE id vector (1.0 = differential, 0.0 = algebraic) for every
    // block's continuous-state slice, recursing into nested subsystems and
    // for-each replicas like buildStateLayout (so a `constraint` block inside a
    // subsystem is marked algebraic too). Per state,
    // BlockMetadata::algebraicStateMask (a block mixing differential and
    // algebraic states) takes precedence over the whole-slice
    // ::algebraicState flag. `id` must already be sized to the layout's
    // totalContinuous with entries defaulted to 1.0; only algebraic entries
    // are overwritten. Must run AFTER buildStateLayout.
    NLSNFLOW_BLOCKS_IMPEXP void
    fillDaeIds(const std::vector<Block>& blocks, const std::vector<BlockState>& blockState,
        const BlockRegistry& reg, const ValMap& vars, std::vector<double>& id);
    //=============================================================================
    // Fill the per-surface root-direction vector (+1 rising / -1 falling /
    // 0 both) for every block's zero-crossing slice, recursing into nested
    // subsystems and for-each replicas like buildStateLayout. Blocks without a
    // BlockMetadata::zeroCrossingDirections hook keep the 0 default (both
    // directions). `dirs` must already be
    // sized to the layout's totalZeroCrossings, zero-filled. Must run AFTER
    // buildStateLayout.
    NLSNFLOW_BLOCKS_IMPEXP void
    fillRootDirections(const std::vector<Block>& blocks, const std::vector<BlockState>& blockState,
        const BlockRegistry& reg, const ValMap& vars, std::vector<int>& dirs);
    //=============================================================================
    // Copy the continuous state held by the block states into their slices of
    // the global x vector, recursing into nested subsystems and for-each
    // replicas like buildStateLayout. Used to seed x at t0 from the
    // post-INIT block states, and to publish a resettable subsystem's re-seeded
    // states so the solver integrates on from them instead of from the values
    // it still holds. `x` must be sized to the layout's totalContinuous. Must
    // run AFTER buildStateLayout.
    NLSNFLOW_BLOCKS_IMPEXP void
    gatherContinuousState(const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, double* x, int nx);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
