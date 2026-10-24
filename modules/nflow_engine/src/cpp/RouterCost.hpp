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
#include "RouterGrid.hpp"
#include <unordered_map>
#include <unordered_set>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    /// Penalty for turns occurring too close to source/destination ports.
    double
    nearPortTurnPenalty(
        const RPoint& point, const RPoint& start, const RPoint& end, const CostOpts& opts);
    //=============================================================================
    /// Penalty for breaking a shared junction owned by the current connection.
    double
    sharedJunctionPenalty(const RPoint& statePoint, int prevDirIdx, int nextDirIdx,
        const Occupancy& occupancy, const WireMeta* meta, const CostOpts& opts);
    //=============================================================================
    struct ProximityResult
    {
        double cost = 0.0;
        NearBreakdown breakdown;
    };
    //=============================================================================
    /// Compute wire- and obstacle-proximity penalties for a candidate point.
    ProximityResult
    proximityPenalty(const RPoint& point, const std::unordered_set<int32_t>* obstacleGrid,
        const std::vector<uint8_t>* obstacleNearMap,
        const std::unordered_map<int32_t, std::string>* obstacleOwners,
        const std::vector<uint8_t>* wireNearMap, const Occupancy& occupancy, const RBounds* bounds,
        const CostOpts& opts, const WireMeta* meta, const WireCostStats& currentStats);
    //=============================================================================
    /// Accumulate cost components into a new WireCostStats.
    WireCostStats
    buildCost(const WireCostStats& currentStats, const CostOpts& opts, double turnCost,
        double hopCost, double nearCost, const NearBreakdown& nearBreakdown,
        int lengthIncrement = 1);
    //=============================================================================
    struct PathEvalResult
    {
        bool feasible = false;
        WireCostStats cost;
    };
    //=============================================================================
    /// Re-cost a densified, orthogonal, grid-space path by replaying the exact
    /// per-step cost model of the A* search. Used by route hysteresis.
    PathEvalResult
    evaluatePathCost(const std::vector<RPoint>& points, const std::vector<RObstacle>& obstacles,
        const std::unordered_set<int32_t>* obstacleGrid,
        const std::vector<uint8_t>* obstacleNearMap,
        const std::unordered_map<int32_t, std::string>* obstacleOwners,
        const std::vector<uint8_t>* wireNearMap, const Occupancy& occupancy, const RBounds* bounds,
        const CostOpts& opts, const WireMeta* meta);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
