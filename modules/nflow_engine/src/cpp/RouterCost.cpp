//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "RouterCost.hpp"
#include <algorithm>
#include <cmath>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    double
    nearPortTurnPenalty(
        const RPoint& point, const RPoint& start, const RPoint& end, const CostOpts& opts)
    {
        double p = opts.nearPortTurnPenalty;
        if (p <= 0.0) {
            return 0.0;
        }
        int want = std::max(1, opts.preferredPortStubCells);
        int ds = std::abs(point.x - start.x) + std::abs(point.y - start.y);
        int de = std::abs(point.x - end.x) + std::abs(point.y - end.y);
        int deficit = std::max(0, want - ds) + std::max(0, want - de);
        return deficit > 0 ? p * deficit * deficit * 8.0 : 0.0;
    }
    //=============================================================================
    // 'H' for directions 0/1 (right/left), 'V' for 2/3 (down/up).
    static char
    dirOrientation(int dirIdx)
    {
        return (dirIdx == 0 || dirIdx == 1) ? 'H' : 'V';
    }
    //=============================================================================
    double
    sharedJunctionPenalty(const RPoint& statePoint, int prevDirIdx, int nextDirIdx,
        const Occupancy& occupancy, const WireMeta* meta, const CostOpts& opts)
    {
        if (!meta) {
            return 0.0;
        }
        char a = dirOrientation(prevDirIdx);
        char b = dirOrientation(nextDirIdx);
        if (a == b) {
            return 0.0;
        }

        auto it = occupancy.points.find(pointKey(statePoint));
        if (it == occupancy.points.end()) {
            return 0.0;
        }
        const auto& info = it->second;

        bool owns = (!meta->from.empty() && info.owners.count(meta->from))
            || (!meta->to.empty() && info.owners.count(meta->to));
        if (!owns) {
            return 0.0;
        }

        auto edgeHas = [&](char orient) -> bool {
            if (orient == 'H') {
                return (!meta->from.empty() && info.hOwners.count(meta->from))
                    || (!meta->to.empty() && info.hOwners.count(meta->to));
            } else {
                return (!meta->from.empty() && info.vOwners.count(meta->from))
                    || (!meta->to.empty() && info.vOwners.count(meta->to));
            }
        };

        bool prevHas = edgeHas(a);
        bool nextHas = edgeHas(b);
        return (prevHas && !nextHas) ? opts.junctionPenalty : 0.0;
    }
    //=============================================================================
    ProximityResult
    proximityPenalty(const RPoint& point, const std::unordered_set<int32_t>* obstacleGrid,
        const std::vector<uint8_t>* obstacleNearMap,
        const std::unordered_map<int32_t, std::string>* obstacleOwners,
        const std::vector<uint8_t>* wireNearMap, const Occupancy& occupancy, const RBounds* bounds,
        const CostOpts& opts, const WireMeta* meta, const WireCostStats& /*currentStats*/)
    {
        int w1 = 0, w2 = 0, o1 = 0, o2 = 0;

        int wireCode = 0;
        if (wireNearMap && bounds && !wireNearMap->empty()) {
            int w = bounds->maxX - bounds->minX + 1;
            int idx = cellIndex(point.x, point.y, bounds->minX, bounds->minY, w);
            if (idx >= 0 && idx < static_cast<int>(wireNearMap->size())) {
                wireCode = (*wireNearMap)[idx];
            }
        }

        auto checkNear = [&](const int dxdy[][2], int count) -> bool {
            for (int k = 0; k < count; ++k) {
                RPoint cand { point.x + dxdy[k][0], point.y + dxdy[k][1] };
                auto it = occupancy.points.find(pointKey(cand));
                if (it == occupancy.points.end()) {
                    continue;
                }
                const auto& info = it->second;
                if (info.wireIds.empty()) {
                    continue;
                }
                if (meta) {
                    bool ownedByMeta = (!meta->from.empty() && info.owners.count(meta->from))
                        || (!meta->to.empty() && info.owners.count(meta->to));
                    if (ownedByMeta) {
                        continue;
                    }
                }
                return true;
            }
            return false;
        };

        if (wireCode != 0) {
            static const int d1[4][2] = { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } };
            static const int d2[8][2] = { { 2, 0 }, { -2, 0 }, { 0, 2 }, { 0, -2 }, { 1, 1 },
                { 1, -1 }, { -1, 1 }, { -1, -1 } };
            if (wireCode == 1) {
                if (checkNear(d1, 4)) {
                    w1 = 1;
                }
            }
            if (w1 == 0 && wireCode != 0) {
                if (checkNear(d2, 8)) {
                    w2 = 1;
                }
            }
        }

        if (obstacleGrid && !obstacleGrid->empty()) {
            bool hasFriendly
                = meta && (!meta->fromBlock.empty() || !meta->toBlock.empty()) && obstacleOwners;

            if (hasFriendly) {
                std::unordered_set<std::string> friendly;
                if (!meta->fromBlock.empty()) {
                    friendly.insert(meta->fromBlock);
                }
                if (!meta->toBlock.empty()) {
                    friendly.insert(meta->toBlock);
                }
                if (!friendly.empty()) {
                    auto near
                        = obstacleNearUnfriendly(point, *obstacleGrid, *obstacleOwners, friendly);
                    o1 = near.obs1;
                    o2 = near.obs2;
                } else {
                    auto near = obstacleNear(point, *obstacleGrid);
                    o1 = near.obs1;
                    o2 = near.obs2;
                }
            } else if (obstacleNearMap && bounds && !obstacleNearMap->empty()) {
                int w = bounds->maxX - bounds->minX + 1;
                int idx = cellIndex(point.x, point.y, bounds->minX, bounds->minY, w);
                if (idx >= 0 && idx < static_cast<int>(obstacleNearMap->size())) {
                    uint8_t code = (*obstacleNearMap)[idx];
                    o1 = code == 1 ? 1 : 0;
                    o2 = code == 2 ? 1 : 0;
                }
            } else {
                auto near = obstacleNear(point, *obstacleGrid);
                o1 = near.obs1;
                o2 = near.obs2;
            }
        }

        double totalCost = w1 * opts.nearWirePenalty1 + w2 * opts.nearWirePenalty2
            + o1 * opts.nearObstaclePenalty1 + o2 * opts.nearObstaclePenalty2;

        return { totalCost, { w1, w2, o1, o2 } };
    }
    //=============================================================================
    WireCostStats
    buildCost(const WireCostStats& currentStats, const CostOpts& opts, double turnCost,
        double hopCost, double nearCost, const NearBreakdown& nearBreakdown, int lengthIncrement)
    {
        WireCostStats s;
        s.length = currentStats.length + lengthIncrement;
        s.turns = currentStats.turns + (turnCost > 0.0 ? 1 : 0);
        s.hops = currentStats.hops + (hopCost > 0.0 ? 1 : 0);
        s.near = currentStats.near + nearCost;
        s.nearBreakdown.wire1 = currentStats.nearBreakdown.wire1 + nearBreakdown.wire1;
        s.nearBreakdown.wire2 = currentStats.nearBreakdown.wire2 + nearBreakdown.wire2;
        s.nearBreakdown.obs1 = currentStats.nearBreakdown.obs1 + nearBreakdown.obs1;
        s.nearBreakdown.obs2 = currentStats.nearBreakdown.obs2 + nearBreakdown.obs2;
        s.total
            = s.length * opts.lengthCost + s.turns * opts.turnCost + s.hops * opts.hopCost + s.near;
        return s;
    }
    //=============================================================================
    PathEvalResult
    evaluatePathCost(const std::vector<RPoint>& points, const std::vector<RObstacle>& obstacles,
        const std::unordered_set<int32_t>* obstacleGrid,
        const std::vector<uint8_t>* obstacleNearMap,
        const std::unordered_map<int32_t, std::string>* obstacleOwners,
        const std::vector<uint8_t>* wireNearMap, const Occupancy& occupancy, const RBounds* bounds,
        const CostOpts& opts, const WireMeta* meta)
    {
        PathEvalResult infeasible;
        if (points.size() < 2) {
            return infeasible;
        }

        const RPoint& start = points.front();
        const RPoint& end = points.back();
        std::unordered_set<int32_t> allowedSet;
        allowedSet.insert(pointKeyInt(start.x, start.y));
        allowedSet.insert(pointKeyInt(end.x, end.y));
        allowedSet.insert(pointKeyInt(points[1].x, points[1].y));
        allowedSet.insert(pointKeyInt(points[points.size() - 2].x, points[points.size() - 2].y));

        auto dirOf = [](const RPoint& a, const RPoint& b) -> int {
            if (b.x == a.x + 1 && b.y == a.y) {
                return 0;
            }
            if (b.x == a.x - 1 && b.y == a.y) {
                return 1;
            }
            if (b.x == a.x && b.y == a.y + 1) {
                return 2;
            }
            if (b.x == a.x && b.y == a.y - 1) {
                return 3;
            }
            return -1;
        };

        WireCostStats stats;
        int prevDir = -1;
        for (size_t i = 1; i < points.size(); ++i) {
            const RPoint& from = points[i - 1];
            const RPoint& to = points[i];
            int dir = dirOf(from, to);
            if (dir < 0) {
                return infeasible; // not densified/orthogonal
            }

            if (isBlocked(to, &obstacles, &allowedSet, obstacleGrid)) {
                return infeasible;
            }
            if (occupancyEdgeBlocked(from, to, occupancy, allowedSet, meta)) {
                return infeasible;
            }

            bool eOcc = occupancy.edges.count(edgeKeyFor(from, to)) > 0;
            auto occInfo = occupancyPointInfo(to, occupancy, allowedSet, meta);
            double hopCost = 0.0;
            if (occInfo.occupied) {
                bool crossesVertical = occInfo.vertical && (dir == 0 || dir == 1);
                bool crossesHorizontal = occInfo.horizontal && (dir == 2 || dir == 3);
                if (crossesVertical || crossesHorizontal) {
                    hopCost = opts.hopCost;
                } else {
                    return infeasible; // blocked by wire
                }
            }

            double turnCost = (prevDir >= 0 && prevDir != dir) ? opts.turnCost : 0.0;
            double portTurnCost
                = (turnCost > 0.0) ? nearPortTurnPenalty(from, start, end, opts) : 0.0;
            double junctionCost = (turnCost > 0.0 && prevDir >= 0)
                ? sharedJunctionPenalty(from, prevDir, dir, occupancy, meta, opts)
                : 0.0;
            auto nearData = proximityPenalty(to, obstacleGrid, obstacleNearMap, obstacleOwners,
                wireNearMap, occupancy, bounds, opts, meta, stats);

            stats = buildCost(stats, opts, turnCost, hopCost,
                nearData.cost + junctionCost + portTurnCost, nearData.breakdown, eOcc ? 0 : 1);
            prevDir = dir;
        }

        PathEvalResult result;
        result.feasible = true;
        result.cost = stats;
        return result;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
