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
#include <string>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <cstdint>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    /// Pixels per grid cell.
    static constexpr int ROUTER_GRID_SIZE = 10;
    //=============================================================================
    struct RPoint
    {
        int x = 0;
        int y = 0;
    };
    //=============================================================================
    struct RObstacle
    {
        int x0 = 0;
        int y0 = 0;
        int x1 = 0;
        int y1 = 0;
        std::string owner;
    };
    //=============================================================================
    struct RBounds
    {
        int minX = 0;
        int minY = 0;
        int maxX = 0;
        int maxY = 0;
    };
    //=============================================================================
    struct OccupancyPointInfo
    {
        bool horizontal = false; ///< point is interior of a horizontal run
        bool vertical = false; ///< point is interior of a vertical run
        bool edgeH = false; ///< has at least one horizontal edge
        bool edgeV = false; ///< has at least one vertical edge
        std::unordered_set<std::string> hOwners; ///< horizontal-edge owners
        std::unordered_set<std::string> vOwners; ///< vertical-edge owners
        bool occupied = false; ///< any wire passes through
        std::unordered_set<std::string> owners; ///< endpoint ids of owning wires
        std::unordered_set<std::string> wireIds; ///< wire-key ids occupying
    };
    //=============================================================================
    struct Occupancy
    {
        std::unordered_map<std::string, OccupancyPointInfo> points; ///< keyed by "x,y"
        std::unordered_map<std::string, std::unordered_set<std::string>>
            edges; ///< keyed by edgeKey
    };
    //=============================================================================
    struct WireMeta
    {
        std::string from; ///< source endpoint id
        std::string to; ///< target endpoint id
        std::string key;
        std::string fromBlock;
        std::string toBlock;
        bool valid = false; ///< false when no meta was provided
    };
    //=============================================================================
    struct RouterNode
    {
        std::string id;
        int x = 0;
        int y = 0;
        std::string dir; ///< "right", "left", "up", "down"
    };
    //=============================================================================
    struct RouterConnInput
    {
        std::string from; ///< source node id
        std::string to; ///< target node id
        int fromIndex = 0;
        int toIndex = 0;
        std::string key;
        std::vector<RPoint> prevPoints; ///< previous route for incremental routing
        std::vector<uint8_t> prevPins; ///< per-point pin flags (parallel to prevPoints)
        bool pinned = false; ///< has user-anchored points (see prevPins)
    };
    //=============================================================================
    struct NearBreakdown
    {
        int wire1 = 0;
        int wire2 = 0;
        int obs1 = 0;
        int obs2 = 0;
    };
    //=============================================================================
    struct WireCostStats
    {
        double total = 0.0;
        int length = 0;
        int turns = 0;
        int hops = 0;
        double near = 0.0;
        NearBreakdown nearBreakdown;
    };
    //=============================================================================
    struct WireRouteResult
    {
        std::vector<RPoint> points; ///< grid-space points (empty = failed)
        WireCostStats cost;
        std::string reason; ///< failure reason when points is empty
        std::string key;
        /// Grid "x,y" keys of pinned (user-anchored) points, kept through
        /// stitching so the JSON output can re-flag them.
        std::unordered_set<std::string> pinnedPointKeys;
    };
    //=============================================================================
    struct CostOpts
    {
        int maxTimeMs = 200;
        double lengthCost = 1.0;
        double turnCost = 6.0;
        double hopCost = 20.0;
        double nearWirePenalty1 = 0.0;
        double nearWirePenalty2 = 0.0;
        double nearObstaclePenalty1 = 0.0;
        double nearObstaclePenalty2 = 0.0;
        double junctionPenalty = 4.0;
        double nearPortTurnPenalty = 40.0;
        int preferredPortStubCells = 2;
        bool incremental = false;
        bool fullOptimize = true;
        int searchPadding = 20;
        /// When true, wall-clock time caps are replaced by maxExpansions so
        /// results are machine-independent (parity/golden tests).
        bool deterministic = false;
        /// Per-connection A* expansion cap used in deterministic mode.
        int maxExpansions = 400000;
        /// Route hysteresis: per-edge discount for reusing the previous route
        /// during the search. 0 disables.
        double reuseBonus = 0.2;
        /// Route hysteresis: keep the previous route unless the new one
        /// improves its true cost by at least this fraction. 0 disables.
        double acceptThreshold = 0.15;
        /// Rip-up-and-reroute refinement iterations after the main pass.
        /// Negative selects 2 for full routes, 0 for incremental ones.
        int refineIterations = -1;
        std::unordered_set<std::string> changedConnections;
        std::unordered_set<std::string> changedNodes;
    };
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
