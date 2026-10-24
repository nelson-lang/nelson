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
#include <unordered_map>
#include <unordered_set>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    /// Parameters for computeWireRoutes.
    struct ComputeWireRoutesParams
    {
        std::vector<RouterNode> nodes;
        std::vector<RouterConnInput> connections;
        std::vector<RObstacle> obstacles;
        std::unordered_map<std::string, std::vector<RPoint>> prevWires; ///< keyed by conn key
        CostOpts opts;
    };
    //=============================================================================
    struct RouteWiresResult
    {
        std::unordered_map<std::string, WireRouteResult> wires; ///< keyed by conn key
        WireCostStats costs;
        NearBreakdown nearBreakdown;
        std::vector<std::string> failedKeys;
    };
    //=============================================================================
    /// Routes all dirty connections using A*.
    RouteWiresResult
    computeWireRoutes(ComputeWireRoutesParams params);
    //=============================================================================
    /// Encodes a search state into one integer: (cell * 4 + dirIdx) * 2 + hopLock.
    inline int
    stateIndex(int cell, int dirIdx, int hopLock)
    {
        return (cell * 4 + dirIdx) * 2 + hopLock;
    }
    //=============================================================================
    /// Decodes a search state index into {x, y, dir, hopLock}.
    struct SearchState
    {
        int x = 0;
        int y = 0;
        int dir = 0;
        int hopLock = 0;
    };
    //=============================================================================
    inline SearchState
    decodeState(int idx, int minX, int minY, int gridW)
    {
        int cell = idx / 8;
        int rem = idx - cell * 8;
        int dir = rem / 2;
        int hopLock = rem % 2;
        int x = (cell % gridW) + minX;
        int y = (cell / gridW) + minY;
        return { x, y, dir, hopLock };
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
