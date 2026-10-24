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
#include "RouterTypes.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    struct NudgeRect
    {
        double left = 0, right = 0, top = 0, bottom = 0;
    };
    //=============================================================================
    /// One wire handed to the nudge pass. `points` is mutated in place (pixel
    /// space, post-stub). Pinned wires are never moved.
    struct NudgeConn
    {
        std::vector<RPoint>* points = nullptr;
        std::string from;
        std::string to;
        int fromIndex = 0;
        int toIndex = 0;
        bool pinned = false;
    };
    //=============================================================================
    /// Post-route pass that separates overlapping channels and centers lone
    /// segments. Returns the number of applied moves.
    int
    nudgeConnections(std::vector<NudgeConn>& connections, const std::vector<NudgeRect>& rects,
        int spacing = ROUTER_GRID_SIZE);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
