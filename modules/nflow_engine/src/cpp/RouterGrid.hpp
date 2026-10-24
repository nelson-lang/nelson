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
#include <cstdint>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    /// Pack (x,y) into a 32-bit signed integer key.
    int32_t
    pointKeyInt(int x, int y);
    std::string
    pointKey(const RPoint& p);
    /// Canonical edge key between two orthogonal adjacent points.
    std::string
    edgeKeyFor(const RPoint& a, const RPoint& b);
    int
    cellIndex(int x, int y, int minX, int minY, int gridW);
    //=============================================================================
    /// Build a set of packed integer keys for all cells covered by obstacles.
    /// Returns an empty set when obstacles is empty.
    std::unordered_set<int32_t>
    buildBlockOccupancyMap(const std::vector<RObstacle>& obstacles);
    /// Map each obstacle cell to its owner id (for friendly-obstacle checks).
    std::unordered_map<int32_t, std::string>
    buildObstacleOwners(const std::vector<RObstacle>& obstacles);
    //=============================================================================
    /// Near-obstacle flags: obs1 = adjacent (distance 1), obs2 = distance 2 or diagonal
    struct NearFlags
    {
        int obs1 = 0;
        int obs2 = 0;
    };
    //=============================================================================
    NearFlags
    obstacleNear(const RPoint& point, const std::unordered_set<int32_t>& grid);
    /// Check obstacle proximity, treating friendly-owner cells as absent.
    NearFlags
    obstacleNearUnfriendly(const RPoint& point, const std::unordered_set<int32_t>& grid,
        const std::unordered_map<int32_t, std::string>& owners,
        const std::unordered_set<std::string>& friendly);
    /// Build a compact Uint8-equivalent proximity map for obstacles within bounds.
    /// Values: 0 = none, 1 = adjacent, 2 = distance-2/diagonal.
    /// Returns empty vector when grid is null/empty or bounds is null.
    std::vector<uint8_t>
    buildObstacleNearMap(const std::unordered_set<int32_t>* grid, const RBounds* bounds);
    /// Build a compact proximity map for routed wires within bounds.
    /// Values: 0 = none, 1 = adjacent, 2 = distance-2/diagonal.
    std::vector<uint8_t>
    buildWireNearMap(const Occupancy& occupancy, const RBounds* bounds);
    /// Incrementally mark the wire-near map for freshly occupied points
    /// (exact per-point logic of buildWireNearMap; codes only strengthen).
    void
    wireNearMapMarkPoints(
        std::vector<uint8_t>& map, const RBounds& bounds, const std::vector<RPoint>& points);
    //=============================================================================
    /// Return true when point is blocked by obstacles (respecting allowedSet).
    bool
    isBlocked(const RPoint& point, const std::vector<RObstacle>* obstacles,
        const std::unordered_set<int32_t>* allowedSet,
        const std::unordered_set<int32_t>* obstacleGrid);
    //=============================================================================
    Occupancy
    buildOccupancy(const std::vector<std::pair<std::vector<RPoint>, WireMeta>>& wires);
    void
    occupancyAddWire(Occupancy& occupancy, const std::vector<RPoint>& points, const WireMeta* meta);
    bool
    occupancyEdgeBlocked(const RPoint& from, const RPoint& to, const Occupancy& occupancy,
        const std::unordered_set<int32_t>& allowedSet, const WireMeta* meta);
    /// Return occupancy info for a point, filtering out friendly-owned data.
    OccupancyPointInfo
    occupancyPointInfo(const RPoint& point, const Occupancy& occupancy,
        const std::unordered_set<int32_t>& allowedSet, const WireMeta* meta);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
