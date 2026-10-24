//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StateLayout.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Recurse one level, advancing the shared continuous-state and
    // zero-crossing cursors so nested subsystems share the flat global vectors.
    static void
    layoutLevel(const std::vector<Block>& blocks, std::vector<BlockState>& states,
        const BlockRegistry& reg, const ValMap& vars, int& xCursor, int& gCursor)
    {
        for (size_t i = 0; i < blocks.size(); ++i) {
            const Block& b = blocks[i];
            const int nid = (b.nid >= 0) ? b.nid : static_cast<int>(i);
            if (nid < 0 || nid >= static_cast<int>(states.size())) {
                continue;
            }
            BlockState& st = states[nid];

            const BlockMetadata meta = reg.metadataOf(b.type);
            const int nx = meta.continuousStateCount ? meta.continuousStateCount(b, vars)
                                                     : meta.numContinuousStates;
            const int ng
                = meta.zeroCrossingCount ? meta.zeroCrossingCount(b, vars) : meta.numZeroCrossings;

            if (nx > 0) {
                st.xOffset = xCursor;
                st.xLength = nx;
                xCursor += nx;
            } else {
                st.xOffset = -1;
                st.xLength = 0;
            }
            if (ng > 0) {
                st.gOffset = gCursor;
                st.gLength = ng;
                gCursor += ng;
            } else {
                st.gOffset = -1;
                st.gLength = 0;
            }

            if (st.subsystem) {
                layoutLevel(
                    st.subsystem->blocks, st.subsystem->blockStateVec, reg, vars, xCursor, gCursor);
                // For-each per-iteration state: each iteration replica
                // gets its own global continuous-state / zero-crossing slice, so a
                // per-channel integrator bank integrates independently.
                for (auto& rep : st.subsystem->feReplicas) {
                    if (rep) {
                        layoutLevel(rep->blocks, rep->blockStateVec, reg, vars, xCursor, gCursor);
                    }
                }
            }
        }
    }
    //=============================================================================
    StateLayout
    buildStateLayout(const std::vector<Block>& blocks, std::vector<BlockState>& blockState,
        const BlockRegistry& reg, const ValMap& vars)
    {
        int xCursor = 0;
        int gCursor = 0;
        layoutLevel(blocks, blockState, reg, vars, xCursor, gCursor);
        StateLayout layout;
        layout.totalContinuous = xCursor;
        layout.totalZeroCrossings = gCursor;
        return layout;
    }
    //=============================================================================
    // Recursive body of fillDaeIds, mirroring layoutLevel's traversal so every
    // slice allocated by the layout (including subsystem-inner and for-each
    // replica states) gets its id entries.
    static void
    idsLevel(const std::vector<Block>& blocks, const std::vector<BlockState>& states,
        const BlockRegistry& reg, const ValMap& vars, std::vector<double>& id)
    {
        for (size_t i = 0; i < blocks.size(); ++i) {
            const Block& b = blocks[i];
            const int nid = (b.nid >= 0) ? b.nid : static_cast<int>(i);
            if (nid < 0 || nid >= static_cast<int>(states.size())) {
                continue;
            }
            const BlockState& st = states[nid];
            if (st.xOffset >= 0 && st.xLength > 0) {
                const BlockMetadata meta = reg.metadataOf(b.type);
                if (meta.algebraicStateMask) {
                    std::vector<double> ids(static_cast<size_t>(st.xLength), 1.0);
                    meta.algebraicStateMask(b, vars, ids);
                    for (int k = 0; k < st.xLength; ++k) {
                        const size_t gi = static_cast<size_t>(st.xOffset + k);
                        if (gi < id.size()) {
                            id[gi] = (k < static_cast<int>(ids.size()) && ids[k] < 0.5) ? 0.0 : 1.0;
                        }
                    }
                } else if (meta.algebraicState) {
                    for (int k = 0; k < st.xLength; ++k) {
                        const size_t gi = static_cast<size_t>(st.xOffset + k);
                        if (gi < id.size()) {
                            id[gi] = 0.0;
                        }
                    }
                }
            }
            if (st.subsystem) {
                idsLevel(st.subsystem->blocks, st.subsystem->blockStateVec, reg, vars, id);
                for (const auto& rep : st.subsystem->feReplicas) {
                    if (rep) {
                        idsLevel(rep->blocks, rep->blockStateVec, reg, vars, id);
                    }
                }
            }
        }
    }
    //=============================================================================
    void
    fillDaeIds(const std::vector<Block>& blocks, const std::vector<BlockState>& blockState,
        const BlockRegistry& reg, const ValMap& vars, std::vector<double>& id)
    {
        idsLevel(blocks, blockState, reg, vars, id);
    }
    //=============================================================================
    // Recursive body of fillRootDirections, mirroring layoutLevel's traversal.
    static void
    dirsLevel(const std::vector<Block>& blocks, const std::vector<BlockState>& states,
        const BlockRegistry& reg, const ValMap& vars, std::vector<int>& dirs)
    {
        for (size_t i = 0; i < blocks.size(); ++i) {
            const Block& b = blocks[i];
            const int nid = (b.nid >= 0) ? b.nid : static_cast<int>(i);
            if (nid < 0 || nid >= static_cast<int>(states.size())) {
                continue;
            }
            const BlockState& st = states[nid];
            if (st.gOffset >= 0 && st.gLength > 0) {
                const BlockMetadata meta = reg.metadataOf(b.type);
                if (meta.zeroCrossingDirections) {
                    std::vector<int> d(static_cast<size_t>(st.gLength), 0);
                    meta.zeroCrossingDirections(b, vars, d);
                    for (int k = 0; k < st.gLength; ++k) {
                        const size_t gi = static_cast<size_t>(st.gOffset + k);
                        if (gi < dirs.size() && k < static_cast<int>(d.size())) {
                            dirs[gi] = d[static_cast<size_t>(k)];
                        }
                    }
                }
            }
            if (st.subsystem) {
                dirsLevel(st.subsystem->blocks, st.subsystem->blockStateVec, reg, vars, dirs);
                for (const auto& rep : st.subsystem->feReplicas) {
                    if (rep) {
                        dirsLevel(rep->blocks, rep->blockStateVec, reg, vars, dirs);
                    }
                }
            }
        }
    }
    //=============================================================================
    void
    fillRootDirections(const std::vector<Block>& blocks, const std::vector<BlockState>& blockState,
        const BlockRegistry& reg, const ValMap& vars, std::vector<int>& dirs)
    {
        dirsLevel(blocks, blockState, reg, vars, dirs);
    }
    //=============================================================================
    void
    gatherContinuousState(const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, double* x, int nx)
    {
        if (x == nullptr || nx <= 0) {
            return;
        }
        for (size_t i = 0; i < blocks.size(); ++i) {
            const int nid = (blocks[i].nid >= 0) ? blocks[i].nid : static_cast<int>(i);
            if (nid < 0 || nid >= static_cast<int>(blockState.size())) {
                continue;
            }
            const BlockState& st = blockState[static_cast<size_t>(nid)];
            auto put = [&](int k, double v) {
                const int gi = st.xOffset + k;
                if (gi >= 0 && gi < nx) {
                    x[gi] = v;
                }
            };
            if (st.xOffset >= 0) {
                if (blocks[i].type == "tf") {
                    for (int k = 0; k < st.xLength && k < (int)st.tfState.size(); ++k) {
                        put(k, st.tfState[k]);
                    }
                } else if (blocks[i].type == "pid") {
                    put(0, st.pidIntegral);
                } else if (!st.vec.empty()) {
                    for (int k = 0; k < st.xLength && k < (int)st.vec.size(); ++k) {
                        put(k, st.vec[k]);
                    }
                } else {
                    put(0, st.scalar);
                }
            }
            if (st.subsystem) {
                gatherContinuousState(st.subsystem->blocks, st.subsystem->blockStateVec, x, nx);
                for (const auto& rep : st.subsystem->feReplicas) {
                    if (rep) {
                        gatherContinuousState(rep->blocks, rep->blockStateVec, x, nx);
                    }
                }
            }
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
