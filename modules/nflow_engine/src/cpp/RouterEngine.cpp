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
#define _USE_MATH_DEFINES
#include "RouterEngine.hpp"
#include "RouterNudge.hpp"
#include "RouterPathfinder.hpp"
#include "RouterGrid.hpp"
#include "RouterTypes.hpp"
#include <algorithm>
#include <cmath>
#include <nlohmann/json.hpp>
#include <string>
#include <unordered_map>
#include <vector>
//=============================================================================
using json = nlohmann::json;
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    struct BlockDesc
    {
        std::string id;
        double x = 0, y = 0;
        double width = 0, height = 0;
        double rotation = 0;
        struct Port
        {
            std::string type; // "in" or "out"
            int index = 0;
            double x = 0, y = 0;
        };
        std::vector<Port> ports;
    };
    //=============================================================================
    static double
    snapToGrid(double value)
    {
        double ratio = value / ROUTER_GRID_SIZE;
        double rounded = (ratio >= 0) ? std::floor(ratio + 0.5) : std::ceil(ratio - 0.5);
        return rounded * ROUTER_GRID_SIZE;
    }
    //=============================================================================
    static std::pair<double, double>
    alignToGrid(double px, double py)
    {
        return { snapToGrid(px), snapToGrid(py) };
    }
    //=============================================================================
    /// Rotate a point around the block center; quarter turns only (0/90/180/270).
    static std::pair<double, double>
    rotatePoint(double px, double py, const BlockDesc& block)
    {
        int angle = static_cast<int>(block.rotation) % 360;
        if (angle < 0) {
            angle += 360;
        }

        double cx = block.x + block.width / 2.0;
        double cy = block.y + block.height / 2.0;
        double dx = px - cx;
        double dy = py - cy;

        double rx, ry;
        switch (angle) {
        case 90:
            rx = cx + dy;
            ry = cy - dx;
            break;
        case 180:
            rx = cx - dx;
            ry = cy - dy;
            break;
        case 270:
            rx = cx - dy;
            ry = cy + dx;
            break;
        default: // 0
            rx = px;
            ry = py;
            break;
        }
        return { rx, ry };
    }
    //=============================================================================
    static std::string
    getPortSide(const BlockDesc& block, double rotX, double rotY)
    {
        double cx = block.x + block.width / 2.0;
        double cy = block.y + block.height / 2.0;
        double dx = rotX - cx;
        double dy = rotY - cy;
        double absDx = std::abs(dx);
        double absDy = std::abs(dy);
        if (absDx >= absDy) {
            return dx < 0 ? "left" : "right";
        }
        return dy < 0 ? "top" : "bottom";
    }
    //=============================================================================
    static std::string
    sideToDir(const std::string& side)
    {
        if (side == "left") {
            return "left";
        }
        if (side == "right") {
            return "right";
        }
        if (side == "top") {
            return "up";
        }
        return "down";
    }
    //=============================================================================
    struct RotatedBounds
    {
        double left, right, top, bottom;
    };
    //=============================================================================
    static RotatedBounds
    getRotatedBounds(const BlockDesc& block)
    {
        double pts[4][2] = {
            { block.x, block.y },
            { block.x + block.width, block.y },
            { block.x + block.width, block.y + block.height },
            { block.x, block.y + block.height },
        };
        double left = 1e18, right = -1e18, top = 1e18, bottom = -1e18;
        for (auto& pt : pts) {
            auto [rx, ry] = rotatePoint(pt[0], pt[1], block);
            left = std::min(left, rx);
            right = std::max(right, rx);
            top = std::min(top, ry);
            bottom = std::max(bottom, ry);
        }
        return { left, right, top, bottom };
    }
    //=============================================================================
    static std::pair<int, int>
    buildStubPoint(int px, int py, const std::string& side)
    {
        if (side == "left") {
            return { px - ROUTER_GRID_SIZE, py };
        }
        if (side == "right") {
            return { px + ROUTER_GRID_SIZE, py };
        }
        if (side == "top") {
            return { px, py - ROUTER_GRID_SIZE };
        }
        return { px, py + ROUTER_GRID_SIZE }; // bottom / down
    }
    //=============================================================================
    static bool
    isValidStub(int portX, int portY, int otherX, int otherY, const std::string& side)
    {
        if (side == "left") {
            return otherY == portY && otherX <= portX - ROUTER_GRID_SIZE;
        }
        if (side == "right") {
            return otherY == portY && otherX >= portX + ROUTER_GRID_SIZE;
        }
        if (side == "top") {
            return otherX == portX && otherY <= portY - ROUTER_GRID_SIZE;
        }
        return otherX == portX && otherY >= portY + ROUTER_GRID_SIZE; // bottom
    }
    //=============================================================================
    static std::vector<RPoint>
    dedupePoints(const std::vector<RPoint>& pts)
    {
        std::vector<RPoint> res;
        for (const auto& p : pts) {
            if (res.empty() || res.back().x != p.x || res.back().y != p.y) {
                res.push_back(p);
            }
        }
        return res;
    }
    //=============================================================================
    static std::vector<RPoint>
    removeColinearPoints(const std::vector<RPoint>& pts)
    {
        if (pts.size() < 3) {
            return pts;
        }
        std::vector<RPoint> out;
        out.push_back(pts[0]);
        for (int i = 1; i + 1 < static_cast<int>(pts.size()); ++i) {
            const auto& prev = out.back();
            const auto& curr = pts[i];
            const auto& next = pts[i + 1];
            bool colinX = (prev.x == curr.x && curr.x == next.x);
            bool colinY = (prev.y == curr.y && curr.y == next.y);
            if (!colinX && !colinY) {
                out.push_back(curr);
            }
        }
        out.push_back(pts.back());
        return out;
    }
    //=============================================================================
    static std::vector<RPoint>
    simplifyOrthogonalPath(const std::vector<RPoint>& pts)
    {
        if (pts.empty()) {
            return pts;
        }
        auto seq = removeColinearPoints(dedupePoints(pts));
        // two diagonal points get a turn point
        if (seq.size() == 2 && seq[0].x != seq[1].x && seq[0].y != seq[1].y) {
            seq = { seq[0], { seq[1].x, seq[0].y }, seq[1] };
        }
        return seq;
    }
    //=============================================================================
    static std::vector<RObstacle>
    blockToObstacles(const BlockDesc& block)
    {
        const double PORT_RADIUS = 6.0;
        auto b = getRotatedBounds(block);
        double l = b.left, r = b.right, t = b.top, bt = b.bottom;
        for (const auto& p : block.ports) {
            auto [rx, ry] = rotatePoint(block.x + p.x, block.y + p.y, block);
            l = std::min(l, rx - PORT_RADIUS);
            r = std::max(r, rx + PORT_RADIUS);
            t = std::min(t, ry - PORT_RADIUS);
            bt = std::max(bt, ry + PORT_RADIUS);
        }
        return { { static_cast<int>(std::floor(l / ROUTER_GRID_SIZE)),
            static_cast<int>(std::floor(t / ROUTER_GRID_SIZE)),
            static_cast<int>(std::floor(r / ROUTER_GRID_SIZE)),
            static_cast<int>(std::floor(bt / ROUTER_GRID_SIZE)), block.id } };
    }
    //=============================================================================
    // Pixel-space rectangle of a block (rotated bounds + port radius) for the nudge pass.
    static NudgeRect
    blockToPixelRect(const BlockDesc& block)
    {
        const double PORT_RADIUS = 6.0;
        auto b = getRotatedBounds(block);
        NudgeRect r { b.left, b.right, b.top, b.bottom };
        for (const auto& p : block.ports) {
            auto [rx, ry] = rotatePoint(block.x + p.x, block.y + p.y, block);
            r.left = std::min(r.left, rx - PORT_RADIUS);
            r.right = std::max(r.right, rx + PORT_RADIUS);
            r.top = std::min(r.top, ry - PORT_RADIUS);
            r.bottom = std::max(r.bottom, ry + PORT_RADIUS);
        }
        return r;
    }
    //=============================================================================
    struct RouterInput
    {
        std::vector<RouterNode> nodes;
        std::vector<RouterConnInput> connections;
        std::vector<RObstacle> obstacles;
    };
    //=============================================================================
    /// Build (or fetch from nodeMap) the RouterNode of a port. Returns false if the port is
    /// not found.
    static bool
    ensurePortNode(const std::unordered_map<std::string, BlockDesc>& blockMap,
        const std::string& blockId, const std::string& type, int index,
        std::unordered_map<std::string, RouterNode>& nodeMap, std::vector<RouterNode>& nodes,
        RouterNode& outNode)
    {
        auto it = blockMap.find(blockId);
        if (it == blockMap.end()) {
            return false;
        }
        const auto& block = it->second;

        const BlockDesc::Port* port = nullptr;
        for (const auto& p : block.ports) {
            if (p.type == type && p.index == index) {
                port = &p;
                break;
            }
        }
        if (!port) {
            return false;
        }

        std::string key = blockId + ':' + type + ':' + std::to_string(index);
        auto nit = nodeMap.find(key);
        if (nit != nodeMap.end()) {
            outNode = nit->second;
            return true;
        }

        auto [rx, ry] = rotatePoint(block.x + port->x, block.y + port->y, block);
        auto [ax, ay] = alignToGrid(rx, ry);
        std::string side = getPortSide(block, rx, ry);
        std::string dir = sideToDir(side);

        RouterNode node;
        node.id = key;
        node.x = static_cast<int>(std::round(ax / ROUTER_GRID_SIZE));
        node.y = static_cast<int>(std::round(ay / ROUTER_GRID_SIZE));
        node.dir = dir;

        nodeMap[key] = node;
        nodes.push_back(node);
        outNode = node;
        return true;
    }
    //=============================================================================
    static RouterInput
    buildRouterInput(const std::unordered_map<std::string, BlockDesc>& blockMap,
        const std::vector<json>& rawConns)
    {
        RouterInput input;
        std::unordered_map<std::string, RouterNode> nodeMap;

        for (int i = 0; i < static_cast<int>(rawConns.size()); ++i) {
            const auto& rc = rawConns[i];
            std::string fromId = rc.value("from", std::string());
            std::string toId = rc.value("to", std::string());
            int fromIndex = rc.value("fromIndex", 0);
            int toIndex = rc.value("toIndex", 0);
            if (fromId.empty() || toId.empty()) {
                continue;
            }
            if (!blockMap.count(fromId) || !blockMap.count(toId)) {
                continue;
            }

            RouterNode fromNode, toNode;
            bool gotFrom = ensurePortNode(
                blockMap, fromId, "out", fromIndex, nodeMap, input.nodes, fromNode);
            bool gotTo
                = ensurePortNode(blockMap, toId, "in", toIndex, nodeMap, input.nodes, toNode);
            if (!gotFrom || !gotTo) {
                continue;
            }

            std::string key = fromId + ':' + std::to_string(fromIndex) + "->" + toId + ':'
                + std::to_string(toIndex);

            RouterConnInput conn;
            conn.from = fromNode.id;
            conn.to = toNode.id;
            conn.fromIndex = fromIndex;
            conn.toIndex = toIndex;
            conn.key = key;

            // Optional previous route (pixel space, [[x,y],...] or [{x,y},...]),
            // converted to grid coordinates. It feeds route hysteresis; a third
            // element `1` marks a pinned (user-anchored) point and makes the wire
            // STATIC for the router.
            if (rc.contains("points") && rc["points"].is_array()) {
                for (const auto& pj : rc["points"]) {
                    double px = 0.0, py = 0.0;
                    bool pin = false;
                    bool ok = false;
                    if (pj.is_array() && pj.size() >= 2 && pj[0].is_number() && pj[1].is_number()) {
                        px = pj[0].get<double>();
                        py = pj[1].get<double>();
                        pin = pj.size() >= 3 && pj[2].is_number() && pj[2].get<int>() == 1;
                        ok = true;
                    } else if (pj.is_object() && pj.contains("x") && pj.contains("y")) {
                        px = pj.value("x", 0.0);
                        py = pj.value("y", 0.0);
                        pin = pj.value("pin", false);
                        ok = true;
                    }
                    if (!ok) {
                        conn.prevPoints.clear();
                        conn.prevPins.clear();
                        conn.pinned = false;
                        break;
                    }
                    conn.prevPoints.push_back(
                        { static_cast<int>(std::llround(px / ROUTER_GRID_SIZE)),
                            static_cast<int>(std::llround(py / ROUTER_GRID_SIZE)) });
                    conn.prevPins.push_back(pin ? 1 : 0);
                    if (pin) {
                        conn.pinned = true;
                    }
                }
                if (conn.prevPoints.size() < 2) {
                    conn.prevPoints.clear();
                    conn.prevPins.clear();
                    conn.pinned = false;
                }
            }
            input.connections.push_back(conn);
        }

        // Sort blocks by id: obstacle order (and so obstacle-owner overwrites) must be
        // deterministic and equal to the TS router.
        std::vector<const BlockDesc*> sortedBlocks;
        sortedBlocks.reserve(blockMap.size());
        for (const auto& [_id, block] : blockMap) {
            sortedBlocks.push_back(&block);
        }
        std::sort(sortedBlocks.begin(), sortedBlocks.end(),
            [](const BlockDesc* a, const BlockDesc* b) { return a->id < b->id; });
        for (const auto* block : sortedBlocks) {
            auto obs = blockToObstacles(*block);
            for (auto& o : obs) {
                input.obstacles.push_back(o);
            }
        }

        return input;
    }
    //=============================================================================
    static std::vector<RPoint>
    enforcePortStubs(const std::unordered_map<std::string, BlockDesc>& blockMap, const json& conn,
        std::vector<RPoint> pts)
    {
        if (pts.size() < 2) {
            return pts;
        }

        std::string fromId = conn.value("from", std::string());
        std::string toId = conn.value("to", std::string());
        int fromIndex = conn.value("fromIndex", 0);
        int toIndex = conn.value("toIndex", 0);

        auto fromIt = blockMap.find(fromId);
        auto toIt = blockMap.find(toId);
        if (fromIt == blockMap.end() || toIt == blockMap.end()) {
            return pts;
        }
        const auto& fromBlk = fromIt->second;
        const auto& toBlk = toIt->second;

        const BlockDesc::Port* fromPort = nullptr;
        for (const auto& p : fromBlk.ports) {
            if (p.type == "out" && p.index == fromIndex) {
                fromPort = &p;
                break;
            }
        }
        const BlockDesc::Port* toPort = nullptr;
        for (const auto& p : toBlk.ports) {
            if (p.type == "in" && p.index == toIndex) {
                toPort = &p;
                break;
            }
        }
        if (!fromPort || !toPort) {
            return pts;
        }

        auto [frx, fry] = rotatePoint(fromBlk.x + fromPort->x, fromBlk.y + fromPort->y, fromBlk);
        auto [trx, try_] = rotatePoint(toBlk.x + toPort->x, toBlk.y + toPort->y, toBlk);
        auto [fax, fay] = alignToGrid(frx, fry);
        auto [tax, tay] = alignToGrid(trx, try_);
        int fromGx = static_cast<int>(fax), fromGy = static_cast<int>(fay);
        int toGx = static_cast<int>(tax), toGy = static_cast<int>(tay);

        std::string fromSide = getPortSide(fromBlk, frx, fry);
        std::string toSide = getPortSide(toBlk, trx, try_);

        if (pts[0].x != fromGx || pts[0].y != fromGy) {
            pts.insert(pts.begin(), { fromGx, fromGy });
        }

        if (pts.size() >= 2) {
            const auto& second = pts[1];
            if (!isValidStub(fromGx, fromGy, second.x, second.y, fromSide)) {
                auto [sx, sy] = buildStubPoint(fromGx, fromGy, fromSide);
                pts.insert(pts.begin() + 1, { sx, static_cast<int>(sy) });
                if (pts.size() >= 3) {
                    const auto& after = pts[2];
                    if (after.x != sx || after.y != static_cast<int>(sy)) {
                        if (fromSide == "left" || fromSide == "right") {
                            pts.insert(pts.begin() + 2, { static_cast<int>(sx), after.y });
                        } else {
                            pts.insert(pts.begin() + 2, { after.x, static_cast<int>(sy) });
                        }
                    }
                }
            }
        }

        if (pts.back().x != toGx || pts.back().y != toGy) {
            pts.push_back({ toGx, toGy });
        }

        if (pts.size() >= 2) {
            const auto& prev = pts[pts.size() - 2];
            if (!isValidStub(toGx, toGy, prev.x, prev.y, toSide)) {
                auto [ex, ey] = buildStubPoint(toGx, toGy, toSide);
                if (prev.x != ex || prev.y != static_cast<int>(ey)) {
                    int insertPos = static_cast<int>(pts.size()) - 1;
                    if (toSide == "left" || toSide == "right") {
                        pts.insert(pts.begin() + insertPos, { static_cast<int>(ex), prev.y });
                    } else {
                        pts.insert(pts.begin() + insertPos, { prev.x, static_cast<int>(ey) });
                    }
                }
                pts.insert(pts.end() - 1, { static_cast<int>(ex), static_cast<int>(ey) });
            }
        }

        return simplifyOrthogonalPath(dedupePoints(pts));
    }
    //=============================================================================
    std::string
    routeWires(const std::string& snapshotJson)
    {
        json input;
        try {
            input = json::parse(snapshotJson);
        } catch (...) {
            return R"({"ok":false,"error":"JSON parse error"})";
        }

        // Accept both { blocks, connections, ... } and { snapshot: { blocks, connections }, ... }
        const json* blocksArr = nullptr;
        const json* connsArr = nullptr;
        if (input.contains("snapshot") && input["snapshot"].is_object()) {
            const auto& snap = input["snapshot"];
            if (snap.contains("blocks")) {
                blocksArr = &snap["blocks"];
            }
            if (snap.contains("connections")) {
                connsArr = &snap["connections"];
            }
        }
        if (!blocksArr && input.contains("blocks")) {
            blocksArr = &input["blocks"];
        }
        if (!connsArr && input.contains("connections")) {
            connsArr = &input["connections"];
        }

        if (!blocksArr || !connsArr || !blocksArr->is_array() || !connsArr->is_array()) {
            return R"({"ok":false,"error":"missing blocks or connections"})";
        }

        int timeLimitMs = input.value("timeLimitMs", 4000);
        int width = input.value("width", 1);
        int height = input.value("height", 1);

        std::unordered_map<std::string, BlockDesc> blockMap;
        for (const auto& bj : *blocksArr) {
            BlockDesc b;
            b.id = bj.value("id", std::string());
            if (b.id.empty()) {
                continue;
            }
            b.x = bj.value("x", 0.0);
            b.y = bj.value("y", 0.0);
            b.width = bj.value("width", 0.0);
            b.height = bj.value("height", 0.0);
            b.rotation = bj.value("rotation", 0.0);
            if (bj.contains("ports") && bj["ports"].is_array()) {
                for (const auto& pj : bj["ports"]) {
                    BlockDesc::Port p;
                    p.type = pj.value("type", std::string("out"));
                    p.index = pj.value("index", 0);
                    p.x = pj.value("x", 0.0);
                    p.y = pj.value("y", 0.0);
                    b.ports.push_back(p);
                }
            }
            blockMap[b.id] = b;
        }

        std::vector<json> rawConns;
        for (const auto& rc : *connsArr) {
            rawConns.push_back(rc);
        }

        auto routerInput = buildRouterInput(blockMap, rawConns);

        CostOpts opts;
        opts.maxTimeMs = timeLimitMs;
        opts.incremental = false;
        opts.fullOptimize = true;
        // Search padding around the diagram bounds, in grid cells. Optimal detours stay
        // near the bounding box, so the padding is capped: a canvas-proportional value
        // inflated the A* state space on large diagrams.
        opts.searchPadding = (std::max)(20,
            (std::min)(40,
                static_cast<int>(std::ceil(std::max(width, height) / ROUTER_GRID_SIZE)) + 5));
        opts.nearObstaclePenalty1 = 2.0;
        opts.nearObstaclePenalty2 = 0.0;
        opts.nearWirePenalty1 = 3.0;
        opts.nearWirePenalty2 = 1.0;
        // Deterministic mode (parity/golden tests): expansion caps instead of
        // wall-clock limits, so results do not depend on the machine.
        opts.deterministic = input.value("deterministic", false);
        opts.maxExpansions = input.value("maxExpansions", 400000);

        ComputeWireRoutesParams params;
        params.nodes = routerInput.nodes;
        params.connections = routerInput.connections;
        params.obstacles = routerInput.obstacles;
        params.opts = opts;
        // Previous routes feed route hysteresis: fullOptimize keeps every connection
        // dirty, prevWires only biases and anchors the search.
        for (const auto& conn : routerInput.connections) {
            if (conn.prevPoints.size() > 1) {
                params.prevWires[conn.key] = conn.prevPoints;
            }
        }

        auto routeResult = computeWireRoutes(std::move(params));

        // Output routes array aligns with rawConns order.
        json routesJson = json::array();

        // Pinned pixel keys let the JSON output re-flag user anchors.
        std::unordered_map<std::string, std::vector<RPoint>> pixelRoutes;
        std::unordered_map<std::string, std::unordered_set<std::string>> pixelPins;
        for (auto& [key, wr] : routeResult.wires) {
            if (wr.points.empty()) {
                continue;
            }
            std::vector<RPoint> pixPts;
            pixPts.reserve(wr.points.size());
            for (const auto& gp : wr.points) {
                RPoint px { gp.x * ROUTER_GRID_SIZE, gp.y * ROUTER_GRID_SIZE };
                if (!wr.pinnedPointKeys.empty()
                    && wr.pinnedPointKeys.count(
                        std::to_string(gp.x) + ',' + std::to_string(gp.y))) {
                    pixelPins[key].insert(std::to_string(px.x) + ',' + std::to_string(px.y));
                }
                pixPts.push_back(px);
            }
            pixelRoutes[key] = std::move(pixPts);
        }

        // Stubbed routes first (aligned with rawConns) so the nudge pass sees final geometry.
        std::vector<std::vector<RPoint>> stubbedRoutes(rawConns.size());
        std::vector<std::string> connKeysOut(rawConns.size());
        for (size_t ci = 0; ci < rawConns.size(); ++ci) {
            const auto& rc = rawConns[ci];
            std::string fromId = rc.value("from", std::string());
            std::string toId = rc.value("to", std::string());
            int fromIndex = rc.value("fromIndex", 0);
            int toIndex = rc.value("toIndex", 0);
            std::string key = fromId + ':' + std::to_string(fromIndex) + "->" + toId + ':'
                + std::to_string(toIndex);
            connKeysOut[ci] = key;
            auto pit = pixelRoutes.find(key);
            if (pit == pixelRoutes.end() || pit->second.size() < 2) {
                continue;
            }
            auto pts = enforcePortStubs(blockMap, rc, pit->second);
            if (pts.size() >= 2) {
                stubbedRoutes[ci] = std::move(pts);
            }
        }

        // Separation/centering pass for headless consumers. The GUI runs the TS nudge
        // itself; the pass is idempotent, so applying it twice is harmless.
        if (input.value("nudge", false)) {
            std::vector<NudgeConn> nudgeConns;
            for (size_t ci = 0; ci < rawConns.size(); ++ci) {
                if (stubbedRoutes[ci].size() < 2) {
                    continue;
                }
                NudgeConn nc;
                nc.points = &stubbedRoutes[ci];
                nc.from = rawConns[ci].value("from", std::string());
                nc.to = rawConns[ci].value("to", std::string());
                nc.fromIndex = rawConns[ci].value("fromIndex", 0);
                nc.toIndex = rawConns[ci].value("toIndex", 0);
                nc.pinned = pixelPins.count(connKeysOut[ci]) > 0;
                nudgeConns.push_back(nc);
            }
            std::vector<NudgeRect> nudgeRects;
            std::vector<const BlockDesc*> sortedForNudge;
            for (const auto& [bid, block] : blockMap) {
                (void)bid;
                sortedForNudge.push_back(&block);
            }
            std::sort(sortedForNudge.begin(), sortedForNudge.end(),
                [](const BlockDesc* a, const BlockDesc* b) { return a->id < b->id; });
            for (const auto* block : sortedForNudge) {
                nudgeRects.push_back(blockToPixelRect(*block));
            }
            nudgeConnections(nudgeConns, nudgeRects);
        }

        for (size_t ci = 0; ci < rawConns.size(); ++ci) {
            std::string key = connKeysOut[ci];
            const auto& pts = stubbedRoutes[ci];
            if (pts.size() < 2) {
                routesJson.push_back(nullptr);
                continue;
            }

            json ptArray = json::array();
            auto pinsIt = pixelPins.find(key);
            for (const auto& p : pts) {
                json pt;
                pt["x"] = p.x;
                pt["y"] = p.y;
                if (pinsIt != pixelPins.end()
                    && pinsIt->second.count(std::to_string(p.x) + ',' + std::to_string(p.y))) {
                    pt["pin"] = true;
                }
                ptArray.push_back(pt);
            }
            routesJson.push_back(ptArray);
        }

        json out;
        out["ok"] = true;
        out["routes"] = routesJson;

        // Raw grid routes (pre-stub), aligned with the connections array, for parity
        // tests against the TS router at the computeWireRoutes level.
        if (input.value("emitGrid", false)) {
            json gridJson = json::array();
            for (const auto& rc : rawConns) {
                std::string fromId = rc.value("from", std::string());
                std::string toId = rc.value("to", std::string());
                int fromIndex = rc.value("fromIndex", 0);
                int toIndex = rc.value("toIndex", 0);
                std::string key = fromId + ':' + std::to_string(fromIndex) + "->" + toId + ':'
                    + std::to_string(toIndex);
                auto wit = routeResult.wires.find(key);
                if (wit == routeResult.wires.end() || wit->second.points.empty()) {
                    gridJson.push_back(nullptr);
                    continue;
                }
                json ptArray = json::array();
                const auto& pinnedKeys = wit->second.pinnedPointKeys;
                for (const auto& gp : wit->second.points) {
                    bool pin = !pinnedKeys.empty()
                        && pinnedKeys.count(std::to_string(gp.x) + ',' + std::to_string(gp.y)) > 0;
                    if (pin) {
                        ptArray.push_back(json::array({ gp.x, gp.y, 1 }));
                    } else {
                        ptArray.push_back(json::array({ gp.x, gp.y }));
                    }
                }
                gridJson.push_back(ptArray);
            }
            out["gridRoutes"] = gridJson;
        }
        return out.dump();
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
