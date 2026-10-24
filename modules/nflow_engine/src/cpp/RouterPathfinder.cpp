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
#include "RouterPathfinder.hpp"
#include "RouterCost.hpp"
#include "RouterGrid.hpp"
#include "RouterMinHeap.hpp"
#include <algorithm>
#include <chrono>
#include <cmath>
#include <limits>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Direction vectors: 0=right, 1=left, 2=down, 3=up
    struct DirVec
    {
        int x, y, idx;
    };
    //=============================================================================
    static const DirVec s_dirs[4] = { { 1, 0, 0 }, { -1, 0, 1 }, { 0, 1, 2 }, { 0, -1, 3 } };
    //=============================================================================
    static DirVec
    dirToVector(const std::string& dir)
    {
        if (dir == "right" || dir == "0") {
            return { 1, 0, 0 };
        }
        if (dir == "left" || dir == "180") {
            return { -1, 0, 1 };
        }
        if (dir == "down" || dir == "90") {
            return { 0, 1, 2 };
        }
        if (dir == "up" || dir == "270") {
            return { 0, -1, 3 };
        }
        return { 0, 0, -1 };
    }
    //=============================================================================
    static inline int
    heuristic(const RPoint& a, const RPoint& b)
    {
        return std::abs(a.x - b.x) + std::abs(a.y - b.y);
    }
    //=============================================================================
    static std::string
    connectionKey(const RouterConnInput& conn, int idx)
    {
        if (!conn.key.empty()) {
            return conn.key;
        }
        return conn.from + ':' + std::to_string(conn.fromIndex) + "->" + conn.to + ':'
            + std::to_string(conn.toIndex) + ':' + std::to_string(idx);
    }
    //=============================================================================
    static std::string
    blockIdFromNodeId(const std::string& nodeId)
    {
        auto colon = nodeId.find(':');
        return colon != std::string::npos ? nodeId.substr(0, colon) : nodeId;
    }
    //=============================================================================
    static RBounds
    computeBounds(
        const std::vector<RouterNode>& nodes, const std::vector<RObstacle>& obstacles, int padding)
    {
        int minX = std::numeric_limits<int>::max();
        int minY = std::numeric_limits<int>::max();
        int maxX = std::numeric_limits<int>::min();
        int maxY = std::numeric_limits<int>::min();
        for (const auto& n : nodes) {
            minX = std::min(minX, n.x);
            minY = std::min(minY, n.y);
            maxX = std::max(maxX, n.x);
            maxY = std::max(maxY, n.y);
        }
        for (const auto& o : obstacles) {
            minX = std::min(minX, o.x0);
            minY = std::min(minY, o.y0);
            maxX = std::max(maxX, o.x1);
            maxY = std::max(maxY, o.y1);
        }
        if (minX == std::numeric_limits<int>::max()) {
            return { 0, 0, 0, 0 };
        }
        return { minX - padding, minY - padding, maxX + padding, maxY + padding };
    }
    //=============================================================================
    // Restricted A* window for one connection: endpoints' bounding box plus a fixed
    // margin, clamped to the global bounds. State buffers scale with the window area.
    static constexpr int SEARCH_WINDOW_MARGIN_CELLS = 40;
    static RBounds
    computeSearchBounds(const RouterNode& from, const RouterNode& to, const RBounds& bounds,
        int margin = SEARCH_WINDOW_MARGIN_CELLS)
    {
        RBounds sb;
        sb.minX = (std::max)(bounds.minX, (std::min)(from.x, to.x) - margin);
        sb.minY = (std::max)(bounds.minY, (std::min)(from.y, to.y) - margin);
        sb.maxX = (std::min)(bounds.maxX, (std::max)(from.x, to.x) + margin);
        sb.maxY = (std::min)(bounds.maxY, (std::max)(from.y, to.y) + margin);
        return sb;
    }
    //=============================================================================
    struct StateArrays
    {
        std::vector<double> gScore;
        std::vector<int> cameFrom;
        std::vector<int> lenArr;
        std::vector<int16_t> turnsArr;
        std::vector<int16_t> hopsArr;
        std::vector<int32_t> nearArr;
        std::vector<int32_t> wire1Arr;
        std::vector<int32_t> wire2Arr;
        std::vector<int32_t> obs1Arr;
        std::vector<int32_t> obs2Arr;
        std::vector<double> bonusScratch; ///< hysteresis reuse-bonus per state
    };
    //=============================================================================
    // assign() reuses the vector capacity, so a shared StateArrays instance avoids
    // reallocating ten stateCount-sized arrays per connection, the dominant cost on
    // large diagrams.
    static void
    resetStateArrays(StateArrays& arr, int stateCount)
    {
        arr.gScore.assign(stateCount, std::numeric_limits<double>::infinity());
        arr.cameFrom.assign(stateCount, -1);
        arr.lenArr.assign(stateCount, 0);
        arr.turnsArr.assign(stateCount, 0);
        arr.hopsArr.assign(stateCount, 0);
        arr.nearArr.assign(stateCount, 0);
        arr.wire1Arr.assign(stateCount, 0);
        arr.wire2Arr.assign(stateCount, 0);
        arr.obs1Arr.assign(stateCount, 0);
        arr.obs2Arr.assign(stateCount, 0);
    }
    //=============================================================================
    static WireRouteResult
    buildResultFromArrays(
        int endIdx, const StateArrays& arr, int minX, int minY, int gridW, const CostOpts& opts)
    {
        std::vector<RPoint> path;
        int currentIdx = endIdx;
        while (currentIdx >= 0) {
            auto state = decodeState(currentIdx, minX, minY, gridW);
            path.push_back({ state.x, state.y });
            currentIdx = arr.cameFrom[currentIdx];
        }
        std::reverse(path.begin(), path.end());

        WireCostStats cost;
        cost.length = arr.lenArr[endIdx];
        cost.turns = arr.turnsArr[endIdx];
        cost.hops = arr.hopsArr[endIdx];
        cost.near = arr.nearArr[endIdx];
        cost.nearBreakdown.wire1 = arr.wire1Arr[endIdx];
        cost.nearBreakdown.wire2 = arr.wire2Arr[endIdx];
        cost.nearBreakdown.obs1 = arr.obs1Arr[endIdx];
        cost.nearBreakdown.obs2 = arr.obs2Arr[endIdx];
        // True total from clean components; gScore may hold a discounted value when
        // route hysteresis was active.
        cost.total = cost.length * opts.lengthCost + cost.turns * opts.turnCost
            + cost.hops * opts.hopCost + cost.near;

        WireRouteResult res;
        res.points = std::move(path);
        res.cost = cost;
        return res;
    }
    //=============================================================================
    static WireRouteResult
    makeEmptyResult(const std::string& reason)
    {
        WireRouteResult res;
        res.reason = reason;
        return res;
    }
    //=============================================================================
    static WireRouteResult
    planSingleConnection(const RouterNode& from, const RouterNode& to,
        const std::vector<RObstacle>& obstacles, const std::unordered_set<int32_t>* obstacleGrid,
        const std::vector<uint8_t>* obstacleNearMap,
        const std::unordered_map<int32_t, std::string>* obstacleOwners,
        const std::vector<uint8_t>* wireNearMap, const Occupancy& occupancy, const RBounds& bounds,
        const CostOpts& opts, const WireMeta& meta, const std::vector<RPoint>* prevPoints = nullptr,
        StateArrays* scratch = nullptr, const RBounds* searchBounds = nullptr)
    {
        auto startDirVec = dirToVector(from.dir);
        auto endDirVec = dirToVector(to.dir);
        if (startDirVec.idx < 0 || endDirVec.idx < 0) {
            return makeEmptyResult("bad_direction");
        }

        DirVec endIncoming { -endDirVec.x, -endDirVec.y, -1 };
        for (auto& d : s_dirs) {
            if (d.x == endIncoming.x && d.y == endIncoming.y) {
                endIncoming.idx = d.idx;
                break;
            }
        }
        if (endIncoming.idx < 0) {
            return makeEmptyResult("bad_direction");
        }

        RPoint start { from.x, from.y };
        RPoint end { to.x, to.y };
        RPoint startStep { start.x + startDirVec.x, start.y + startDirVec.y };
        RPoint endStep { end.x - endIncoming.x, end.y - endIncoming.y };

        std::unordered_set<int32_t> allowedSet;
        allowedSet.insert(pointKeyInt(start.x, start.y));
        allowedSet.insert(pointKeyInt(end.x, end.y));
        allowedSet.insert(pointKeyInt(startStep.x, startStep.y));
        allowedSet.insert(pointKeyInt(endStep.x, endStep.y));

        if (isBlocked(startStep, &obstacles, &allowedSet, obstacleGrid)) {
            return makeEmptyResult("blocked_start_step");
        }
        if (isBlocked(endStep, &obstacles, &allowedSet, obstacleGrid)) {
            return makeEmptyResult("blocked_end_step");
        }

        // State space runs on the (possibly restricted) search window; near-maps stay
        // indexed on the global bounds they were built with.
        const RBounds sB = searchBounds ? *searchBounds : bounds;
        int gridW = sB.maxX - sB.minX + 1;
        int gridH = sB.maxY - sB.minY + 1;
        int cellCount = gridW * gridH;
        int stateCount = cellCount * 8;

        StateArrays localArr;
        StateArrays& arr = scratch ? *scratch : localArr;
        resetStateArrays(arr, stateCount);

        int startDirIdx = startDirVec.idx;
        int endDirIdx = endIncoming.idx;
        int startCell = cellIndex(start.x, start.y, sB.minX, sB.minY, gridW);
        int startIdx = stateIndex(startCell, startDirIdx, 0);

        arr.gScore[startIdx] = 0.0;

        // Route hysteresis: edges of the previous route earn a small discount so
        // equal-quality alternatives resolve toward the old shape. The heuristic is
        // scaled down to stay admissible.
        double reuseBonus = (prevPoints && prevPoints->size() > 1) ? opts.reuseBonus : 0.0;
        std::unordered_set<std::string> prevEdgeSet;
        if (reuseBonus > 0.0) {
            for (size_t i = 1; i < prevPoints->size(); ++i) {
                prevEdgeSet.insert(edgeKeyFor((*prevPoints)[i - 1], (*prevPoints)[i]));
            }
        }
        std::vector<double>& bonusArr = arr.bonusScratch;
        if (reuseBonus > 0.0) {
            bonusArr.assign(stateCount, 0.0);
        }
        double hFactor = reuseBonus > 0.0 ? (std::max)(0.0, 1.0 - reuseBonus) : 1.0;

        RouterMinHeap open;
        open.push(startIdx, static_cast<double>(heuristic(start, end)) * hFactor);

        auto startTime = std::chrono::steady_clock::now();
        // Deterministic mode replaces the wall-clock cap with an expansion cap so results
        // are machine-independent.
        long long expansions = 0;
        bool expansionCapHit = false;

        while (!open.empty()) {
            if (opts.deterministic) {
                if (++expansions > opts.maxExpansions) {
                    expansionCapHit = true;
                    break;
                }
            } else {
                auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
                    std::chrono::steady_clock::now() - startTime)
                                   .count();
                if (elapsed > opts.maxTimeMs) {
                    break;
                }
            }

            auto entry = open.pop();
            int curIdx = entry.idx;
            auto state = decodeState(curIdx, sB.minX, sB.minY, gridW);

            if (state.x == end.x && state.y == end.y && state.dir == endDirIdx) {
                return buildResultFromArrays(curIdx, arr, sB.minX, sB.minY, gridW, opts);
            }

            WireCostStats currentStats;
            currentStats.length = arr.lenArr[curIdx];
            currentStats.turns = arr.turnsArr[curIdx];
            currentStats.hops = arr.hopsArr[curIdx];
            currentStats.near = arr.nearArr[curIdx];
            currentStats.nearBreakdown.wire1 = arr.wire1Arr[curIdx];
            currentStats.nearBreakdown.wire2 = arr.wire2Arr[curIdx];
            currentStats.nearBreakdown.obs1 = arr.obs1Arr[curIdx];
            currentStats.nearBreakdown.obs2 = arr.obs2Arr[curIdx];
            currentStats.total = arr.gScore[curIdx];

            for (const auto& dv : s_dirs) {
                if (state.hopLock > 0 && dv.idx != state.dir) {
                    continue;
                }
                if (state.x == start.x && state.y == start.y && dv.idx != state.dir) {
                    continue;
                }

                int nx = state.x + dv.x;
                int ny = state.y + dv.y;
                if (nx < sB.minX || nx > sB.maxX || ny < sB.minY || ny > sB.maxY) {
                    continue;
                }

                RPoint pNext { nx, ny };

                if (nx == end.x && ny == end.y && dv.idx != endDirIdx) {
                    continue;
                }
                if (nx == end.x && ny == end.y && (state.x != endStep.x || state.y != endStep.y)) {
                    continue;
                }

                if (isBlocked(pNext, &obstacles, &allowedSet, obstacleGrid)) {
                    continue;
                }

                RPoint statePoint { state.x, state.y };
                if (occupancyEdgeBlocked(statePoint, pNext, occupancy, allowedSet, &meta)) {
                    continue;
                }

                // Edge already carried by another wire: traversing it adds no length.
                std::string eKey = edgeKeyFor(statePoint, pNext);
                bool edgeOccupied = occupancy.edges.count(eKey) > 0;

                auto occInfo = occupancyPointInfo(pNext, occupancy, allowedSet, &meta);

                int nextHopLock = std::max(state.hopLock - 1, 0);
                double hopCost = 0.0;
                if (occInfo.occupied) {
                    bool crossesV = occInfo.vertical && dv.x != 0;
                    bool crossesH = occInfo.horizontal && dv.y != 0;
                    if (crossesV || crossesH) {
                        nextHopLock = std::max(nextHopLock, 1);
                        hopCost = opts.hopCost;
                    } else {
                        continue; // blocked by wire
                    }
                }

                double turnCost = (state.dir != dv.idx) ? opts.turnCost : 0.0;
                double portTurnCost
                    = (turnCost > 0.0) ? nearPortTurnPenalty(statePoint, start, end, opts) : 0.0;
                double junctionCost = (turnCost > 0.0)
                    ? sharedJunctionPenalty(statePoint, state.dir, dv.idx, occupancy, &meta, opts)
                    : 0.0;

                auto nearData = proximityPenalty(pNext, obstacleGrid, obstacleNearMap,
                    obstacleOwners, wireNearMap, occupancy, &bounds, opts, &meta, currentStats);

                // Port-turn and junction penalties are additive costs carried via the near
                // component: nearCost = nearData.cost + junctionCost + portTurnCost. The turn
                // parameter itself is only a flag.
                auto newCost = buildCost(currentStats, opts, turnCost, hopCost,
                    nearData.cost + junctionCost + portTurnCost, nearData.breakdown,
                    edgeOccupied ? 0 : 1);

                int nextCell = cellIndex(nx, ny, sB.minX, sB.minY, gridW);
                int nextIdx = stateIndex(nextCell, dv.idx, nextHopLock);
                // gScore holds the discounted total (true total minus accumulated reuse bonuses);
                // component stats stay clean.
                double edgeBonus = (reuseBonus > 0.0 && prevEdgeSet.count(eKey)) ? reuseBonus : 0.0;
                double bonus = (reuseBonus > 0.0 ? bonusArr[curIdx] : 0.0) + edgeBonus;
                double tentative = newCost.total - bonus;

                if (tentative < arr.gScore[nextIdx]) {
                    arr.cameFrom[nextIdx] = curIdx;
                    arr.gScore[nextIdx] = tentative;
                    if (reuseBonus > 0.0) {
                        bonusArr[nextIdx] = bonus;
                    }
                    arr.lenArr[nextIdx] = newCost.length;
                    arr.turnsArr[nextIdx] = static_cast<int16_t>(newCost.turns);
                    arr.hopsArr[nextIdx] = static_cast<int16_t>(newCost.hops);
                    arr.nearArr[nextIdx] = static_cast<int32_t>(newCost.near);
                    arr.wire1Arr[nextIdx] = static_cast<int32_t>(newCost.nearBreakdown.wire1);
                    arr.wire2Arr[nextIdx] = static_cast<int32_t>(newCost.nearBreakdown.wire2);
                    arr.obs1Arr[nextIdx] = static_cast<int32_t>(newCost.nearBreakdown.obs1);
                    arr.obs2Arr[nextIdx] = static_cast<int32_t>(newCost.nearBreakdown.obs2);
                    open.push(nextIdx, tentative + heuristic(pNext, end) * hFactor);
                }
            }
        }

        if (opts.deterministic) {
            return makeEmptyResult(expansionCapHit ? "timeout" : "no_path");
        }
        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - startTime)
                           .count();
        return makeEmptyResult(elapsed > opts.maxTimeMs ? "timeout" : "no_path");
    }
    //=============================================================================
    // Variant of densifyGridPath that carries per-point pin flags along.
    static void
    densifyGridPathWithPins(const std::vector<RPoint>& points, const std::vector<uint8_t>& pins,
        std::vector<RPoint>& outPoints, std::vector<uint8_t>& outPins)
    {
        outPoints.clear();
        outPins.clear();
        if (points.size() < 2) {
            outPoints = points;
            outPins.assign(points.size(), 0);
            for (size_t i = 0; i < points.size() && i < pins.size(); ++i) {
                outPins[i] = pins[i];
            }
            return;
        }
        auto pinAt = [&](size_t i) -> uint8_t { return i < pins.size() ? pins[i] : 0; };
        auto markLast = [&](uint8_t pin) {
            if (pin && !outPins.empty()) {
                outPins.back() = 1;
            }
        };
        for (size_t i = 0; i + 1 < points.size(); ++i) {
            const RPoint& a = points[i];
            const RPoint& b = points[i + 1];
            if (outPoints.empty()) {
                outPoints.push_back(a);
                outPins.push_back(0);
                markLast(pinAt(i));
            }
            int sx = (b.x > a.x) - (b.x < a.x);
            int sy = (b.y > a.y) - (b.y < a.y);
            int steps = (std::max)(std::abs(b.x - a.x), std::abs(b.y - a.y));
            if (sx != 0 && sy != 0) {
                const RPoint& last = outPoints.back();
                if (last.x != b.x || last.y != b.y) {
                    outPoints.push_back(b);
                    outPins.push_back(0);
                }
                markLast(pinAt(i + 1));
                continue;
            }
            int cx = a.x;
            int cy = a.y;
            for (int s = 0; s < steps; ++s) {
                cx += sx;
                cy += sy;
                outPoints.push_back({ cx, cy });
                outPins.push_back(0);
            }
            markLast(pinAt(i + 1));
        }
    }
    //=============================================================================
    // Expand a polyline so consecutive points are one grid cell apart. Diagonal pairs
    // are passed through unexpanded.
    static std::vector<RPoint>
    densifyGridPath(const std::vector<RPoint>& points)
    {
        std::vector<RPoint> out;
        if (points.size() < 2) {
            return points;
        }
        for (size_t i = 0; i + 1 < points.size(); ++i) {
            const RPoint& a = points[i];
            const RPoint& b = points[i + 1];
            if (out.empty()) {
                out.push_back(a);
            }
            int sx = (b.x > a.x) - (b.x < a.x);
            int sy = (b.y > a.y) - (b.y < a.y);
            int steps = (std::max)(std::abs(b.x - a.x), std::abs(b.y - a.y));
            if (sx != 0 && sy != 0) {
                const RPoint& last = out.back();
                if (last.x != b.x || last.y != b.y) {
                    out.push_back(b);
                }
                continue;
            }
            int cx = a.x;
            int cy = a.y;
            for (int s = 0; s < steps; ++s) {
                cx += sx;
                cy += sy;
                out.push_back({ cx, cy });
            }
        }
        return out;
    }
    //=============================================================================
    RouteWiresResult
    computeWireRoutes(ComputeWireRoutesParams params)
    {
        auto startTime = std::chrono::steady_clock::now();
        const auto& opts = params.opts;

        std::unordered_map<std::string, RouterNode> nodeMap;
        for (const auto& n : params.nodes) {
            nodeMap[n.id] = n;
        }

        std::vector<std::string> connKeys;
        connKeys.reserve(params.connections.size());
        for (int i = 0; i < static_cast<int>(params.connections.size()); ++i) {
            connKeys.push_back(connectionKey(params.connections[i], i));
        }

        std::unordered_set<std::string> dirtySet;
        for (int i = 0; i < static_cast<int>(params.connections.size()); ++i) {
            const auto& conn = params.connections[i];
            const auto& key = connKeys[i];
            bool dirty = !opts.incremental || opts.fullOptimize
                || opts.changedConnections.count(key) || opts.changedNodes.count(conn.from)
                || opts.changedNodes.count(conn.to) || !params.prevWires.count(key);
            if (dirty) {
                dirtySet.insert(key);
            }
        }

        auto bounds = computeBounds(params.nodes, params.obstacles, opts.searchPadding);

        auto obstacleGrid = buildBlockOccupancyMap(params.obstacles);
        const std::unordered_set<int32_t>* obstacleGridPtr
            = obstacleGrid.empty() ? nullptr : &obstacleGrid;
        auto obstacleNearMap = buildObstacleNearMap(obstacleGridPtr, &bounds);
        const std::vector<uint8_t>* obstacleNearMapPtr
            = obstacleNearMap.empty() ? nullptr : &obstacleNearMap;
        auto obstacleOwners = buildObstacleOwners(params.obstacles);
        const std::unordered_map<int32_t, std::string>* obstacleOwnersPtr
            = obstacleOwners.empty() ? nullptr : &obstacleOwners;

        RouteWiresResult result;
        std::vector<std::pair<std::vector<RPoint>, WireMeta>> staticWires;
        for (int i = 0; i < static_cast<int>(params.connections.size()); ++i) {
            const auto& conn = params.connections[i];
            const auto& key = connKeys[i];
            if (!dirtySet.count(key)) {
                auto it = params.prevWires.find(key);
                if (it != params.prevWires.end() && !it->second.empty()) {
                    WireRouteResult wr;
                    wr.points = it->second;
                    wr.key = key;
                    result.wires[key] = wr;
                    WireMeta meta;
                    meta.from = conn.from;
                    meta.to = conn.to;
                    meta.key = key;
                    meta.fromBlock = blockIdFromNodeId(conn.from);
                    meta.toBlock = blockIdFromNodeId(conn.to);
                    meta.valid = true;
                    staticWires.push_back({ it->second, meta });
                }
            }
        }

        Occupancy occupancy = buildOccupancy(staticWires);
        // Shared A* scratch buffers, reused by every planSingleConnection call of this
        // pass (main loop, pinned legs, refine).
        StateArrays scratch;

        // Bounded-region search: try the endpoints' window first; fall back to the full
        // bounds when no path fits the window.
        auto planWithBoundedSearch = [&](const RouterNode& pFrom, const RouterNode& pTo,
                                         const std::vector<uint8_t>* nearPtr, const WireMeta& pMeta,
                                         const std::vector<RPoint>* pPrev) {
            RBounds windowBounds = computeSearchBounds(pFrom, pTo, bounds);
            auto boundedResult = planSingleConnection(pFrom, pTo, params.obstacles, obstacleGridPtr,
                obstacleNearMapPtr, obstacleOwnersPtr, nearPtr, occupancy, bounds, opts, pMeta,
                pPrev, &scratch, &windowBounds);
            if (!boundedResult.points.empty()) {
                return boundedResult;
            }
            return planSingleConnection(pFrom, pTo, params.obstacles, obstacleGridPtr,
                obstacleNearMapPtr, obstacleOwnersPtr, nearPtr, occupancy, bounds, opts, pMeta,
                pPrev, &scratch, nullptr);
        };

        // Shared wire-near map, updated incrementally as wires land; rip-up rebuilds it.
        int mapWidth = bounds.maxX - bounds.minX + 1;
        int mapHeight = bounds.maxY - bounds.minY + 1;
        std::vector<uint8_t> sharedWireNearMap = buildWireNearMap(occupancy, &bounds);
        if (sharedWireNearMap.empty()) {
            sharedWireNearMap.assign(static_cast<size_t>(mapWidth) * mapHeight, 0);
        }
        auto rebuildSharedWireNearMap = [&]() {
            sharedWireNearMap = buildWireNearMap(occupancy, &bounds);
            if (sharedWireNearMap.empty()) {
                sharedWireNearMap.assign(static_cast<size_t>(mapWidth) * mapHeight, 0);
            }
        };
        const std::vector<uint8_t>* sharedNearPtr = &sharedWireNearMap;

        // Sort dirty connections: feedback wires first, then by distance (longest first)
        struct RouteOrder
        {
            int idx;
            int score;
        };
        std::vector<RouteOrder> order;
        order.reserve(params.connections.size());
        for (int i = 0; i < static_cast<int>(params.connections.size()); ++i) {
            const auto& conn = params.connections[i];
            auto itFrom = nodeMap.find(conn.from);
            auto itTo = nodeMap.find(conn.to);
            int dist = 0, backward = 0;
            if (itFrom != nodeMap.end() && itTo != nodeMap.end()) {
                const auto& f = itFrom->second;
                const auto& t = itTo->second;
                dist = std::abs(f.x - t.x) + std::abs(f.y - t.y);
                backward = (f.x > t.x) ? (f.x - t.x) * 2 : 0;
            }
            order.push_back({ i, dist + backward });
        }
        // Deterministic order: score descending, then connection key ascending. The key is
        // content-based, so equal-score connections route in the same order whatever the
        // input array order.
        std::stable_sort(
            order.begin(), order.end(), [&connKeys](const RouteOrder& a, const RouteOrder& b) {
                if (a.score != b.score) {
                    return a.score > b.score;
                }
                const auto& ka = connKeys[a.idx];
                const auto& kb = connKeys[b.idx];
                if (ka != kb) {
                    return ka < kb;
                }
                return a.idx < b.idx;
            });

        for (const auto& ro : order) {
            int i = ro.idx;
            const auto& conn = params.connections[i];
            const auto& key = connKeys[i];
            if (!dirtySet.count(key)) {
                continue;
            }

            if (!opts.deterministic) {
                auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
                    std::chrono::steady_clock::now() - startTime)
                                   .count();
                if (elapsed > opts.maxTimeMs) {
                    result.costs.total += 0;
                    result.failedKeys.push_back(key);
                    continue;
                }
            }

            auto itFrom = nodeMap.find(conn.from);
            auto itTo = nodeMap.find(conn.to);
            if (itFrom == nodeMap.end() || itTo == nodeMap.end()) {
                result.failedKeys.push_back(key);
                continue;
            }

            const std::vector<uint8_t>* wireNearMapPtr = sharedNearPtr;

            WireMeta meta;
            meta.from = conn.from;
            meta.to = conn.to;
            meta.key = key;
            meta.fromBlock = blockIdFromNodeId(conn.from);
            meta.toBlock = blockIdFromNodeId(conn.to);
            meta.valid = true;

            // Pinned wire: keep the user-anchored chain verbatim and route only the free
            // sub-paths port-->firstPin and lastPin-->port. Fall back to the whole previous
            // shape when a free end cannot be routed.
            if (conn.pinned && conn.prevPoints.size() > 1) {
                std::vector<RPoint> pts;
                std::vector<uint8_t> pinFlags;
                densifyGridPathWithPins(conn.prevPoints, conn.prevPins, pts, pinFlags);
                int firstPin = -1;
                int lastPin = -1;
                for (int pi = 0; pi < static_cast<int>(pinFlags.size()); ++pi) {
                    if (pinFlags[pi]) {
                        if (firstPin < 0) {
                            firstPin = pi;
                        }
                        lastPin = pi;
                    }
                }

                auto dirNameOf = [](int dx, int dy) -> std::string {
                    if (dx == 1) {
                        return "right";
                    }
                    if (dx == -1) {
                        return "left";
                    }
                    if (dy == 1) {
                        return "down";
                    }
                    return "up";
                };
                auto oppositeOf = [](const std::string& d) -> std::string {
                    if (d == "right") {
                        return "left";
                    }
                    if (d == "left") {
                        return "right";
                    }
                    if (d == "down") {
                        return "up";
                    }
                    return "down";
                };
                auto sgn = [](int v) { return (v > 0) - (v < 0); };

                WireRouteResult finalWire;
                bool constrained = false;
                if (firstPin > 0 && lastPin < static_cast<int>(pts.size()) - 1) {
                    int inDx = sgn(pts[firstPin].x - pts[firstPin - 1].x);
                    int inDy = sgn(pts[firstPin].y - pts[firstPin - 1].y);
                    int outDx = sgn(pts[lastPin + 1].x - pts[lastPin].x);
                    int outDy = sgn(pts[lastPin + 1].y - pts[lastPin].y);
                    if ((inDx != 0 || inDy != 0) && (outDx != 0 || outDy != 0)) {
                        auto routeLeg = [&](const RouterNode& legFrom, const RouterNode& legTo) {
                            return planWithBoundedSearch(
                                legFrom, legTo, sharedNearPtr, meta, nullptr);
                        };
                        RouterNode headTo;
                        headTo.id = key + ":pinhead";
                        headTo.x = pts[firstPin].x;
                        headTo.y = pts[firstPin].y;
                        headTo.dir = oppositeOf(dirNameOf(inDx, inDy));
                        auto head = routeLeg(itFrom->second, headTo);
                        if (!head.points.empty()) {
                            RouterNode tailFrom;
                            tailFrom.id = key + ":pintail";
                            tailFrom.x = pts[lastPin].x;
                            tailFrom.y = pts[lastPin].y;
                            tailFrom.dir = dirNameOf(outDx, outDy);
                            auto tail = routeLeg(tailFrom, itTo->second);
                            if (!tail.points.empty()) {
                                // Stitch head + pinned chain + tail; anchors keep
                                // their pin flags via pinnedPointKeys.
                                finalWire.points.assign(head.points.begin(), head.points.end() - 1);
                                for (int pi = firstPin; pi <= lastPin; ++pi) {
                                    finalWire.points.push_back(pts[pi]);
                                    if (pinFlags[pi]) {
                                        finalWire.pinnedPointKeys.insert(pointKey(pts[pi]));
                                    }
                                }
                                finalWire.points.insert(finalWire.points.end(),
                                    tail.points.begin() + 1, tail.points.end());
                                finalWire.cost.total = head.cost.total + tail.cost.total;
                                finalWire.cost.length = head.cost.length + tail.cost.length;
                                finalWire.cost.turns = head.cost.turns + tail.cost.turns;
                                finalWire.cost.hops = head.cost.hops + tail.cost.hops;
                                finalWire.cost.near = head.cost.near + tail.cost.near;
                                finalWire.cost.nearBreakdown.wire1
                                    = head.cost.nearBreakdown.wire1 + tail.cost.nearBreakdown.wire1;
                                finalWire.cost.nearBreakdown.wire2
                                    = head.cost.nearBreakdown.wire2 + tail.cost.nearBreakdown.wire2;
                                finalWire.cost.nearBreakdown.obs1
                                    = head.cost.nearBreakdown.obs1 + tail.cost.nearBreakdown.obs1;
                                finalWire.cost.nearBreakdown.obs2
                                    = head.cost.nearBreakdown.obs2 + tail.cost.nearBreakdown.obs2;
                                constrained = true;
                            }
                        }
                    }
                }
                if (!constrained) {
                    // Fallback: whole previous shape verbatim (Stage A).
                    finalWire = WireRouteResult {};
                    finalWire.points = pts;
                    for (int pi = 0; pi < static_cast<int>(pinFlags.size()); ++pi) {
                        if (pinFlags[pi]) {
                            finalWire.pinnedPointKeys.insert(pointKey(pts[pi]));
                        }
                    }
                }
                finalWire.key = key;
                result.wires[key] = finalWire;
                occupancyAddWire(occupancy, result.wires[key].points, &meta);
                wireNearMapMarkPoints(sharedWireNearMap, bounds, result.wires[key].points);
                result.costs.total += finalWire.cost.total;
                result.costs.length += finalWire.cost.length;
                result.costs.turns += finalWire.cost.turns;
                result.costs.hops += finalWire.cost.hops;
                result.costs.near += finalWire.cost.near;
                result.nearBreakdown.wire1 += finalWire.cost.nearBreakdown.wire1;
                result.nearBreakdown.wire2 += finalWire.cost.nearBreakdown.wire2;
                result.nearBreakdown.obs1 += finalWire.cost.nearBreakdown.obs1;
                result.nearBreakdown.obs2 += finalWire.cost.nearBreakdown.obs2;
                continue;
            }

            // Route hysteresis: the previous route of the connection (densified, endpoints
            // unchanged) biases the search toward its own shape and competes against the
            // fresh result below.
            const std::vector<RPoint>* hysteresisPrev = nullptr;
            std::vector<RPoint> densifiedPrev;
            if (opts.reuseBonus > 0.0 || opts.acceptThreshold > 0.0) {
                auto prevIt = params.prevWires.find(key);
                if (prevIt != params.prevWires.end() && prevIt->second.size() > 1) {
                    densifiedPrev = densifyGridPath(prevIt->second);
                    if (densifiedPrev.size() > 1 && densifiedPrev.front().x == itFrom->second.x
                        && densifiedPrev.front().y == itFrom->second.y
                        && densifiedPrev.back().x == itTo->second.x
                        && densifiedPrev.back().y == itTo->second.y) {
                        hysteresisPrev = &densifiedPrev;
                    }
                }
            }

            auto wr = planWithBoundedSearch(
                itFrom->second, itTo->second, wireNearMapPtr, meta, hysteresisPrev);

            if (wr.points.empty()) {
                result.failedKeys.push_back(key);
                continue;
            }

            // Accept-only-if-better: keep the previous shape unless the fresh
            // route improves its true cost by at least acceptThreshold (and
            // the previous route is still feasible).
            if (hysteresisPrev && opts.acceptThreshold > 0.0) {
                auto prevEval = evaluatePathCost(*hysteresisPrev, params.obstacles, obstacleGridPtr,
                    obstacleNearMapPtr, obstacleOwnersPtr, wireNearMapPtr, occupancy, &bounds, opts,
                    &meta);
                if (prevEval.feasible
                    && wr.cost.total > (1.0 - opts.acceptThreshold) * prevEval.cost.total) {
                    wr.points = *hysteresisPrev;
                    wr.cost = prevEval.cost;
                }
            }

            wr.key = key;
            result.wires[key] = wr;

            occupancyAddWire(occupancy, wr.points, &meta);
            wireNearMapMarkPoints(sharedWireNearMap, bounds, wr.points);

            result.costs.total += wr.cost.total;
            result.costs.length += wr.cost.length;
            result.costs.turns += wr.cost.turns;
            result.costs.hops += wr.cost.hops;
            result.costs.near += wr.cost.near;
            result.nearBreakdown.wire1 += wr.cost.nearBreakdown.wire1;
            result.nearBreakdown.wire2 += wr.cost.nearBreakdown.wire2;
            result.nearBreakdown.obs1 += wr.cost.nearBreakdown.obs1;
            result.nearBreakdown.obs2 += wr.cost.nearBreakdown.obs2;
        }

        // Rip-up-and-reroute refinement: the worst wires (hops / wire proximity) are
        // removed from occupancy and rerouted with full knowledge of the others. The
        // batch is rolled back unless it strictly improves the lexicographic aggregate
        // (hops, near, turns, length).
        int refineIterations
            = opts.refineIterations >= 0 ? opts.refineIterations : (opts.fullOptimize ? 2 : 0);

        auto metaFor = [&](const std::string& key, int idx) {
            WireMeta meta;
            const auto& conn = params.connections[idx];
            meta.from = conn.from;
            meta.to = conn.to;
            meta.key = key;
            meta.fromBlock = blockIdFromNodeId(conn.from);
            meta.toBlock = blockIdFromNodeId(conn.to);
            meta.valid = true;
            return meta;
        };
        std::unordered_map<std::string, int> keyToIdx;
        for (int i = 0; i < static_cast<int>(connKeys.size()); ++i) {
            keyToIdx[connKeys[i]] = i;
        }

        auto rebuildOccupancy = [&](const std::unordered_set<std::string>* excluded) {
            Occupancy occ = buildOccupancy(staticWires);
            std::vector<std::string> keys;
            keys.reserve(result.wires.size());
            for (const auto& [key, _wr] : result.wires) {
                keys.push_back(key);
            }
            std::sort(keys.begin(), keys.end());
            for (const auto& key : keys) {
                if (excluded && excluded->count(key)) {
                    continue;
                }
                if (!dirtySet.count(key)) {
                    continue; // statics already included
                }
                const auto& wr = result.wires.at(key);
                if (!wr.points.empty()) {
                    auto meta = metaFor(key, keyToIdx[key]);
                    occupancyAddWire(occ, wr.points, &meta);
                }
            }
            return occ;
        };

        struct Aggregate
        {
            int hops = 0;
            double near = 0.0;
            int turns = 0;
            int length = 0;
        };
        auto aggregateOf = [&](const std::vector<std::string>& keys) {
            Aggregate agg;
            for (const auto& key : keys) {
                auto it = result.wires.find(key);
                if (it == result.wires.end()) {
                    continue;
                }
                agg.hops += it->second.cost.hops;
                agg.near += it->second.cost.near;
                agg.turns += it->second.cost.turns;
                agg.length += it->second.cost.length;
            }
            return agg;
        };
        auto lexBetter = [](const Aggregate& a, const Aggregate& b) {
            if (a.hops != b.hops) {
                return a.hops < b.hops;
            }
            if (a.near != b.near) {
                return a.near < b.near;
            }
            if (a.turns != b.turns) {
                return a.turns < b.turns;
            }
            return a.length < b.length;
        };

        for (int iter = 0; iter < refineIterations; ++iter) {
            if (!opts.deterministic) {
                auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
                    std::chrono::steady_clock::now() - startTime)
                                   .count();
                if (elapsed > 0.7 * opts.maxTimeMs) {
                    break;
                }
            }

            // Score badness a posteriori: re-cost each route under the final occupancy
            // (evaluatePathCost's friendly-owner filtering ignores the wire's own cells), so
            // early-routed wires crowded by later ones become candidates too.
            const std::vector<uint8_t>* refineNearPtr = sharedNearPtr;
            struct Candidate
            {
                std::string key;
                double badness;
                WireCostStats evalCost;
            };
            std::vector<Candidate> candidates;
            for (const auto& [key, wr] : result.wires) {
                if (!dirtySet.count(key)) {
                    continue;
                }
                // Pinned wires are never ripped up; their shape is
                // user-anchored.
                if (!wr.pinnedPointKeys.empty()) {
                    continue;
                }
                if (wr.points.empty()) {
                    continue;
                }
                auto meta = metaFor(key, keyToIdx[key]);
                auto ev = evaluatePathCost(wr.points, params.obstacles, obstacleGridPtr,
                    obstacleNearMapPtr, obstacleOwnersPtr, refineNearPtr, occupancy, &bounds, opts,
                    &meta);
                const WireCostStats& c = ev.feasible ? ev.cost : wr.cost;
                double badness
                    = c.hops * opts.hopCost + c.nearBreakdown.wire1 * opts.nearWirePenalty1;
                if (badness > 0.0) {
                    candidates.push_back({ key, badness, c });
                }
            }
            if (candidates.empty()) {
                break;
            }
            std::sort(candidates.begin(), candidates.end(), [](const auto& a, const auto& b) {
                if (a.badness != b.badness) {
                    return a.badness > b.badness;
                }
                return a.key < b.key;
            });
            // Rip one wire per iteration: ripping an interacting pair together reproduces the
            // original mutual environment (deterministic search) and rolls back. Single
            // rips let each wire adapt to the others.
            std::vector<std::string> ripped = { candidates[0].key };
            std::unordered_set<std::string> rippedSet(ripped.begin(), ripped.end());

            // "Before" uses the a-posteriori cost so the comparison with the rerouted cost
            // is like for like.
            Aggregate before;
            before.hops = candidates[0].evalCost.hops;
            before.near = candidates[0].evalCost.near;
            before.turns = candidates[0].evalCost.turns;
            before.length = candidates[0].evalCost.length;
            std::unordered_map<std::string, WireRouteResult> saved;
            for (const auto& key : ripped) {
                saved[key] = result.wires.at(key);
            }

            occupancy = rebuildOccupancy(&rippedSet);
            rebuildSharedWireNearMap();
            bool failedRip = false;
            // Reroute in the global routing order restricted to the ripped set.
            for (const auto& ro : order) {
                const auto& key = connKeys[ro.idx];
                if (!rippedSet.count(key)) {
                    continue;
                }
                const auto& conn = params.connections[ro.idx];
                auto itFrom = nodeMap.find(conn.from);
                auto itTo = nodeMap.find(conn.to);
                if (itFrom == nodeMap.end() || itTo == nodeMap.end()) {
                    failedRip = true;
                    break;
                }
                const std::vector<uint8_t>* wireNearMapPtr = sharedNearPtr;
                auto meta = metaFor(key, ro.idx);
                // Hysteresis is off so these wires can move.
                auto rr = planWithBoundedSearch(
                    itFrom->second, itTo->second, wireNearMapPtr, meta, nullptr);
                if (rr.points.empty()) {
                    failedRip = true;
                    break;
                }
                rr.key = key;
                result.wires[key] = rr;
                occupancyAddWire(occupancy, rr.points, &meta);
                wireNearMapMarkPoints(sharedWireNearMap, bounds, rr.points);
            }

            bool keep = false;
            if (!failedRip) {
                auto after = aggregateOf(ripped);
                keep = lexBetter(after, before);
            }
            if (!keep) {
                for (auto& [key, wr] : saved) {
                    result.wires[key] = wr;
                }
                occupancy = rebuildOccupancy(nullptr);
                rebuildSharedWireNearMap();
                break;
            }
        }

        if (refineIterations > 0) {
            result.costs = WireCostStats {};
            result.nearBreakdown = NearBreakdown {};
            for (const auto& [key, wr] : result.wires) {
                if (!dirtySet.count(key)) {
                    continue;
                }
                result.costs.total += wr.cost.total;
                result.costs.length += wr.cost.length;
                result.costs.turns += wr.cost.turns;
                result.costs.hops += wr.cost.hops;
                result.costs.near += wr.cost.near;
                result.nearBreakdown.wire1 += wr.cost.nearBreakdown.wire1;
                result.nearBreakdown.wire2 += wr.cost.nearBreakdown.wire2;
                result.nearBreakdown.obs1 += wr.cost.nearBreakdown.obs1;
                result.nearBreakdown.obs2 += wr.cost.nearBreakdown.obs2;
            }
        }

        return result;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
