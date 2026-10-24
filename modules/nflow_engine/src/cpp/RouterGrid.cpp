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
#include "RouterGrid.hpp"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <sstream>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Key layout: ((x & 0xffff) << 16) | (y & 0xffff), as a signed 32-bit int.
    int32_t
    pointKeyInt(int x, int y)
    {
        auto ux = static_cast<uint32_t>(x & 0xffff);
        auto uy = static_cast<uint32_t>(y & 0xffff);
        return static_cast<int32_t>((ux << 16) | uy);
    }
    //=============================================================================
    std::string
    pointKey(const RPoint& p)
    {
        return std::to_string(p.x) + ',' + std::to_string(p.y);
    }
    //=============================================================================
    std::string
    edgeKeyFor(const RPoint& a, const RPoint& b)
    {
        // Vertical edge: x same, canonicalize y order
        if (a.x == b.x) {
            int y0 = a.y < b.y ? a.y : b.y;
            int y1 = a.y < b.y ? b.y : a.y;
            return 'v' + std::string(":") + std::to_string(a.x) + ':' + std::to_string(y0) + ':'
                + std::to_string(y1);
        }
        // Horizontal edge: y same, canonicalize x order
        int x0 = a.x < b.x ? a.x : b.x;
        int x1 = a.x < b.x ? b.x : a.x;
        return 'h' + std::string(":") + std::to_string(x0) + ':' + std::to_string(x1) + ':'
            + std::to_string(a.y);
    }
    //=============================================================================
    int
    cellIndex(int x, int y, int minX, int minY, int gridW)
    {
        return (y - minY) * gridW + (x - minX);
    }
    //=============================================================================
    std::unordered_set<int32_t>
    buildBlockOccupancyMap(const std::vector<RObstacle>& obstacles)
    {
        std::unordered_set<int32_t> cells;
        for (const auto& rect : obstacles) {
            int startX = static_cast<int>(std::floor(static_cast<float>(rect.x0)));
            int endX = static_cast<int>(std::floor(static_cast<float>(rect.x1)));
            int startY = static_cast<int>(std::floor(static_cast<float>(rect.y0)));
            int endY = static_cast<int>(std::floor(static_cast<float>(rect.y1)));
            for (int cx = startX; cx <= endX; ++cx) {
                for (int cy = startY; cy <= endY; ++cy) {
                    cells.insert(pointKeyInt(cx, cy));
                }
            }
        }
        return cells;
    }
    //=============================================================================
    std::unordered_map<int32_t, std::string>
    buildObstacleOwners(const std::vector<RObstacle>& obstacles)
    {
        std::unordered_map<int32_t, std::string> ownerMap;
        for (const auto& rect : obstacles) {
            const std::string& owner = rect.owner;
            int x0 = static_cast<int>(std::floor(static_cast<float>(rect.x0)));
            int x1 = static_cast<int>(std::floor(static_cast<float>(rect.x1)));
            int y0 = static_cast<int>(std::floor(static_cast<float>(rect.y0)));
            int y1 = static_cast<int>(std::floor(static_cast<float>(rect.y1)));
            for (int xx = x0; xx <= x1; ++xx) {
                for (int yy = y0; yy <= y1; ++yy) {
                    ownerMap[pointKeyInt(xx, yy)] = owner;
                }
            }
        }
        return ownerMap;
    }
    //=============================================================================
    NearFlags
    obstacleNear(const RPoint& point, const std::unordered_set<int32_t>& grid)
    {
        if (grid.count(pointKeyInt(point.x, point.y))) {
            return { 1, 0 };
        }
        static const int d1[][2] = { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } };
        for (auto& d : d1) {
            if (grid.count(pointKeyInt(point.x + d[0], point.y + d[1]))) {
                return { 1, 0 };
            }
        }
        // Distance-2 orthogonal + diagonals
        static const int d2[][2] = { { 2, 0 }, { -2, 0 }, { 0, 2 }, { 0, -2 }, { 1, 1 }, { 1, -1 },
            { -1, 1 }, { -1, -1 } };
        for (auto& d : d2) {
            if (grid.count(pointKeyInt(point.x + d[0], point.y + d[1]))) {
                return { 0, 1 };
            }
        }
        return { 0, 0 };
    }
    //=============================================================================
    NearFlags
    obstacleNearUnfriendly(const RPoint& point, const std::unordered_set<int32_t>& grid,
        const std::unordered_map<int32_t, std::string>& owners,
        const std::unordered_set<std::string>& friendly)
    {
        auto friendlyCheck = [&](int tx, int ty) -> bool {
            int32_t k = pointKeyInt(tx, ty);
            if (!grid.count(k)) {
                return true;
            }
            auto it = owners.find(k);
            if (it == owners.end()) {
                return false;
            }
            return !it->second.empty() && friendly.count(it->second) > 0;
        };

        static const int d1[][2] = { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } };
        for (auto& d : d1) {
            int nx = point.x + d[0], ny = point.y + d[1];
            if (grid.count(pointKeyInt(nx, ny)) && !friendlyCheck(nx, ny)) {
                return { 1, 0 };
            }
        }
        // Distance-2 + diagonals
        static const int d2[][2] = { { 2, 0 }, { -2, 0 }, { 0, 2 }, { 0, -2 }, { 1, 1 }, { 1, -1 },
            { -1, 1 }, { -1, -1 } };
        for (auto& d : d2) {
            int nx = point.x + d[0], ny = point.y + d[1];
            if (grid.count(pointKeyInt(nx, ny)) && !friendlyCheck(nx, ny)) {
                return { 0, 1 };
            }
        }
        return { 0, 0 };
    }
    //=============================================================================
    std::vector<uint8_t>
    buildObstacleNearMap(const std::unordered_set<int32_t>* grid, const RBounds* bounds)
    {
        if (!grid || grid->empty() || !bounds) {
            return {};
        }
        int width = bounds->maxX - bounds->minX + 1;
        int height = bounds->maxY - bounds->minY + 1;
        std::vector<uint8_t> out(static_cast<size_t>(width * height), 0);
        for (int row = bounds->minY; row <= bounds->maxY; ++row) {
            for (int col = bounds->minX; col <= bounds->maxX; ++col) {
                int index = cellIndex(col, row, bounds->minX, bounds->minY, width);
                auto near = obstacleNear({ col, row }, *grid);
                out[index] = near.obs1 ? 1 : near.obs2 ? 2 : 0;
            }
        }
        return out;
    }
    //=============================================================================
    std::vector<uint8_t>
    buildWireNearMap(const Occupancy& occupancy, const RBounds* bounds)
    {
        if (!bounds || occupancy.points.empty()) {
            return {};
        }
        int width = bounds->maxX - bounds->minX + 1;
        int height = bounds->maxY - bounds->minY + 1;
        std::vector<uint8_t> out(static_cast<size_t>(width * height), 0);

        auto safeMark = [&](int x, int y, uint8_t val) {
            if (x < bounds->minX || x > bounds->maxX || y < bounds->minY || y > bounds->maxY) {
                return;
            }
            int idx = cellIndex(x, y, bounds->minX, bounds->minY, width);
            if (val == 1 && out[idx] != 1) {
                out[idx] = 1;
            }
            if (val == 2 && out[idx] == 0) {
                out[idx] = 2;
            }
        };

        for (const auto& [key, _info] : occupancy.points) {
            auto comma = key.find(',');
            if (comma == std::string::npos) {
                continue;
            }
            int cx = std::stoi(key.substr(0, comma));
            int cy = std::stoi(key.substr(comma + 1));

            safeMark(cx + 1, cy, 1);
            safeMark(cx - 1, cy, 1);
            safeMark(cx, cy + 1, 1);
            safeMark(cx, cy - 1, 1);

            safeMark(cx + 2, cy, 2);
            safeMark(cx - 2, cy, 2);
            safeMark(cx, cy + 2, 2);
            safeMark(cx, cy - 2, 2);
            safeMark(cx + 1, cy + 1, 2);
            safeMark(cx + 1, cy - 1, 2);
            safeMark(cx - 1, cy + 1, 2);
            safeMark(cx - 1, cy - 1, 2);
        }
        return out;
    }
    //=============================================================================
    void
    wireNearMapMarkPoints(
        std::vector<uint8_t>& map, const RBounds& bounds, const std::vector<RPoint>& points)
    {
        // Codes only strengthen on adds; only removals (rip-up) need a rebuild
        // of the whole map.
        if (map.empty()) {
            return;
        }
        int width = bounds.maxX - bounds.minX + 1;
        auto safeMark = [&](int x, int y, uint8_t val) {
            if (x < bounds.minX || x > bounds.maxX || y < bounds.minY || y > bounds.maxY) {
                return;
            }
            int idx = cellIndex(x, y, bounds.minX, bounds.minY, width);
            if (val == 1 && map[idx] != 1) {
                map[idx] = 1;
            }
            if (val == 2 && map[idx] == 0) {
                map[idx] = 2;
            }
        };
        for (const auto& p : points) {
            int cx = p.x;
            int cy = p.y;
            safeMark(cx + 1, cy, 1);
            safeMark(cx - 1, cy, 1);
            safeMark(cx, cy + 1, 1);
            safeMark(cx, cy - 1, 1);

            safeMark(cx + 2, cy, 2);
            safeMark(cx - 2, cy, 2);
            safeMark(cx, cy + 2, 2);
            safeMark(cx, cy - 2, 2);
            safeMark(cx + 1, cy + 1, 2);
            safeMark(cx + 1, cy - 1, 2);
            safeMark(cx - 1, cy + 1, 2);
            safeMark(cx - 1, cy - 1, 2);
        }
    }
    //=============================================================================
    bool
    isBlocked(const RPoint& point, const std::vector<RObstacle>* obstacles,
        const std::unordered_set<int32_t>* allowedSet,
        const std::unordered_set<int32_t>* obstacleGrid)
    {
        int32_t packed = pointKeyInt(point.x, point.y);
        if (allowedSet && allowedSet->count(packed)) {
            return false;
        }
        if (obstacleGrid) {
            return obstacleGrid->count(packed) > 0;
        }
        if (!obstacles || obstacles->empty()) {
            return false;
        }
        for (const auto& r : *obstacles) {
            if (point.x >= r.x0 && point.x <= r.x1 && point.y >= r.y0 && point.y <= r.y1) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    Occupancy
    buildOccupancy(const std::vector<std::pair<std::vector<RPoint>, WireMeta>>& wires)
    {
        Occupancy occ;
        for (const auto& [pts, meta] : wires) {
            occupancyAddWire(occ, pts, meta.valid ? &meta : nullptr);
        }
        return occ;
    }
    //=============================================================================
    void
    occupancyAddWire(Occupancy& occupancy, const std::vector<RPoint>& points, const WireMeta* meta)
    {
        std::unordered_set<std::string> owners;
        std::unordered_set<std::string> wireIds;
        if (meta) {
            if (!meta->from.empty()) {
                owners.insert(meta->from);
            }
            if (!meta->to.empty()) {
                owners.insert(meta->to);
            }
            if (!meta->key.empty()) {
                wireIds.insert(meta->key);
            }
        }

        auto getOrCreate = [&](const RPoint& p) -> OccupancyPointInfo& {
            std::string k = pointKey(p);
            auto it = occupancy.points.find(k);
            if (it != occupancy.points.end()) {
                return it->second;
            }
            OccupancyPointInfo blank;
            occupancy.points[k] = blank;
            return occupancy.points[k];
        };

        auto flagEdge = [&](OccupancyPointInfo& info, char orient) {
            if (orient == 'V') {
                info.edgeV = true;
                for (const auto& id : owners) {
                    info.vOwners.insert(id);
                }
            } else {
                info.edgeH = true;
                for (const auto& id : owners) {
                    info.hOwners.insert(id);
                }
            }
        };

        for (int i = 0; i < static_cast<int>(points.size()); ++i) {
            const auto& curr = points[i];
            auto& info = getOrCreate(curr);
            info.occupied = true;
            for (const auto& id : owners) {
                info.owners.insert(id);
            }
            for (const auto& wid : wireIds) {
                info.wireIds.insert(wid);
            }

            if (i > 0) {
                const auto& prev = points[i - 1];
                auto& prevInfo = getOrCreate(prev);
                bool sameX = (prev.x == curr.x);
                bool sameY = (prev.y == curr.y);
                char orient = sameX ? 'V' : (sameY ? 'H' : 0);
                if (orient) {
                    flagEdge(prevInfo, orient);
                    flagEdge(info, orient);
                }
                std::string eKey = edgeKeyFor(prev, curr);
                auto& edgeSet = occupancy.edges[eKey];
                for (const auto& id : owners) {
                    edgeSet.insert(id);
                }
                if (meta && !meta->key.empty()) {
                    edgeSet.insert(meta->key);
                }

                // Flag horizontal/vertical through-runs for middle points
                if (i < static_cast<int>(points.size()) - 1) {
                    const auto& next = points[i + 1];
                    if (prev.x == curr.x && curr.x == next.x) {
                        info.vertical = true;
                    }
                    if (prev.y == curr.y && curr.y == next.y) {
                        info.horizontal = true;
                    }
                }
            }
        }
    }
    //=============================================================================
    bool
    occupancyEdgeBlocked(const RPoint& from, const RPoint& to, const Occupancy& occupancy,
        const std::unordered_set<int32_t>& allowedSet, const WireMeta* meta)
    {
        int32_t a = pointKeyInt(from.x, from.y);
        int32_t b = pointKeyInt(to.x, to.y);
        if (allowedSet.count(a) || allowedSet.count(b)) {
            return false;
        }
        std::string eKey = edgeKeyFor(from, to);
        auto it = occupancy.edges.find(eKey);
        if (it == occupancy.edges.end()) {
            return false;
        }
        const auto& edgeOwners = it->second;
        if (!meta) {
            return true;
        }
        if (!meta->from.empty() && edgeOwners.count(meta->from)) {
            return false;
        }
        if (!meta->to.empty() && edgeOwners.count(meta->to)) {
            return false;
        }
        if (!meta->key.empty() && edgeOwners.count(meta->key)) {
            return false;
        }
        return true;
    }
    //=============================================================================
    OccupancyPointInfo
    occupancyPointInfo(const RPoint& point, const Occupancy& occupancy,
        const std::unordered_set<int32_t>& allowedSet, const WireMeta* meta)
    {
        static const OccupancyPointInfo empty;
        int32_t packed = pointKeyInt(point.x, point.y);
        if (allowedSet.count(packed)) {
            return empty;
        }
        auto it = occupancy.points.find(pointKey(point));
        if (it == occupancy.points.end()) {
            return empty;
        }
        const auto& info = it->second;
        if (!meta) {
            return info;
        }

        bool metaMatches = (!meta->from.empty() && info.owners.count(meta->from))
            || (!meta->to.empty() && info.owners.count(meta->to))
            || (!meta->key.empty() && info.wireIds.count(meta->key));

        if (metaMatches) {
            OccupancyPointInfo filtered;
            filtered.hOwners = info.hOwners;
            filtered.vOwners = info.vOwners;
            filtered.owners = info.owners;
            filtered.wireIds = info.wireIds;
            filtered.occupied = false; // key: don't block self
            return filtered;
        }
        return info;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
