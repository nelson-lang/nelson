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
#include <algorithm>
#include <array>
#include <cmath>
#include <utility>
#include <vector>
#include "ColoredPoint.hpp"
//=============================================================================
// Axis-aligned clip box in the axes "mapped" coordinate system produced by GOAxis::reMap
// (data coordinates after the log and reverse transforms, before the model/view
// projection). Primitives handed to a renderer are already in that space. When an axes
// has Clipping == 'on', its children are clipped to this cuboid.
//=============================================================================
struct RenderClipBox
{
    bool enabled = false;
    double xmin = 0.0;
    double xmax = 0.0;
    double ymin = 0.0;
    double ymax = 0.0;
    double zmin = 0.0;
    double zmax = 0.0;
};
//=============================================================================
// Slack that keeps a vertex sitting exactly on a limit (data range equal to the axes
// limits) from being discarded by floating point round-off.
inline double
renderClipEpsilon(const RenderClipBox& box)
{
    // (std::max) defeats the windows.h max() macro: this header is included by
    // translation units that pull in <windows.h> without NOMINMAX.
    double span = (std::max)({ std::abs(box.xmax - box.xmin), std::abs(box.ymax - box.ymin),
        std::abs(box.zmax - box.zmin), 1.0 });
    return span * 1e-9;
}
//=============================================================================
inline bool
renderClipPointInside(const RenderClipBox& box, double x, double y, double z, double eps)
{
    return x >= box.xmin - eps && x <= box.xmax + eps && y >= box.ymin - eps && y <= box.ymax + eps
        && z >= box.zmin - eps && z <= box.zmax + eps;
}
//=============================================================================
// True when at least one finite vertex of the parallel xs/ys/zs arrays lies outside the
// box. Lets callers keep the unclipped path and geometry caches when everything is inside.
inline bool
renderClipNeeded(const RenderClipBox& box, const std::vector<double>& xs,
    const std::vector<double>& ys, const std::vector<double>& zs)
{
    if (!box.enabled) {
        return false;
    }
    const double eps = renderClipEpsilon(box);
    const size_t n = (std::min)(xs.size(), ys.size());
    for (size_t i = 0; i < n; i++) {
        const double z = i < zs.size() ? zs[i] : 0.0;
        if (!std::isfinite(xs[i]) || !std::isfinite(ys[i]) || !std::isfinite(z)) {
            continue;
        }
        if (!renderClipPointInside(box, xs[i], ys[i], z, eps)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Same test for the face/edge strips of a surface.
inline bool
renderClipNeededStrips(
    const RenderClipBox& box, const std::vector<std::vector<coloredPoint>>& strips)
{
    if (!box.enabled) {
        return false;
    }
    const double eps = renderClipEpsilon(box);
    for (const auto& strip : strips) {
        for (const coloredPoint& p : strip) {
            if (!std::isfinite(p.x) || !std::isfinite(p.y) || !std::isfinite(p.z)) {
                continue;
            }
            if (!renderClipPointInside(box, p.x, p.y, p.z, eps)) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
// Liang-Barsky clip of the 3D segment (x0,y0,z0)-(x1,y1,z1) against the box.
// Returns false when the whole segment is outside; otherwise writes the clipped
// endpoints. Endpoints already inside are returned unchanged.
inline bool
renderClipSegment(const RenderClipBox& box, double x0, double y0, double z0, double x1, double y1,
    double z1, double& ox0, double& oy0, double& oz0, double& ox1, double& oy1, double& oz1)
{
    const double eps = renderClipEpsilon(box);
    const double dx = x1 - x0;
    const double dy = y1 - y0;
    const double dz = z1 - z0;
    double t0 = 0.0;
    double t1 = 1.0;
    // For each boundary: p * t <= q keeps the segment. p < 0 -> entering,
    // p > 0 -> leaving, p == 0 -> parallel (rejected only when already outside).
    const std::array<std::pair<double, double>, 6> edges = { {
        { -dx, x0 - (box.xmin - eps) },
        { dx, (box.xmax + eps) - x0 },
        { -dy, y0 - (box.ymin - eps) },
        { dy, (box.ymax + eps) - y0 },
        { -dz, z0 - (box.zmin - eps) },
        { dz, (box.zmax + eps) - z0 },
    } };
    for (const auto& e : edges) {
        const double p = e.first;
        const double q = e.second;
        if (p == 0.0) {
            if (q < 0.0) {
                return false;
            }
            continue;
        }
        const double r = q / p;
        if (p < 0.0) {
            if (r > t1) {
                return false;
            }
            if (r > t0) {
                t0 = r;
            }
        } else {
            if (r < t0) {
                return false;
            }
            if (r < t1) {
                t1 = r;
            }
        }
    }
    ox0 = x0 + t0 * dx;
    oy0 = y0 + t0 * dy;
    oz0 = z0 + t0 * dz;
    ox1 = x0 + t1 * dx;
    oy1 = y0 + t1 * dy;
    oz1 = z0 + t1 * dz;
    return true;
}
//=============================================================================
inline coloredPoint
renderClipLerp(const coloredPoint& a, const coloredPoint& b, double t)
{
    return coloredPoint(a.x + t * (b.x - a.x), a.y + t * (b.y - a.y), a.z + t * (b.z - a.z),
        a.r + t * (b.r - a.r), a.g + t * (b.g - a.g), a.b + t * (b.b - a.b), a.a + t * (b.a - a.a),
        a.nx + t * (b.nx - a.nx), a.ny + t * (b.ny - a.ny), a.nz + t * (b.nz - a.nz),
        a.u + t * (b.u - a.u), a.v + t * (b.v - a.v));
}
//=============================================================================
// Sutherland-Hodgman clip of a convex polygon against a single half-space,
// interpolating every vertex attribute at the crossings.
inline void
renderClipPolygonPlane(const std::vector<coloredPoint>& in, std::vector<coloredPoint>& out,
    int axis, double limit, bool keepBelow, double eps)
{
    out.clear();
    const size_t n = in.size();
    if (n == 0) {
        return;
    }
    auto coord = [axis](const coloredPoint& p) -> double {
        return axis == 0 ? p.x : (axis == 1 ? p.y : p.z);
    };
    auto dist = [&](const coloredPoint& p) -> double {
        double c = coord(p);
        return keepBelow ? (limit + eps - c) : (c - (limit - eps));
    };
    for (size_t i = 0; i < n; i++) {
        const coloredPoint& cur = in[i];
        const coloredPoint& nxt = in[(i + 1) % n];
        const double dCur = dist(cur);
        const double dNxt = dist(nxt);
        if (dCur >= 0.0) {
            out.push_back(cur);
            if (dNxt < 0.0) {
                out.push_back(renderClipLerp(cur, nxt, dCur / (dCur - dNxt)));
            }
        } else if (dNxt >= 0.0) {
            out.push_back(renderClipLerp(cur, nxt, dCur / (dCur - dNxt)));
        }
    }
}
//=============================================================================
// The result can be empty, or have more vertices than the input.
inline std::vector<coloredPoint>
renderClipPolygon(const RenderClipBox& box, const std::vector<coloredPoint>& poly)
{
    const double eps = renderClipEpsilon(box);
    // Fast path: a polygon fully inside skips the six clipping passes and their allocations.
    bool allInside = true;
    for (const coloredPoint& p : poly) {
        if (!renderClipPointInside(box, p.x, p.y, p.z, eps)) {
            allInside = false;
            break;
        }
    }
    if (allInside) {
        return poly;
    }
    std::vector<coloredPoint> a(poly);
    std::vector<coloredPoint> b;
    renderClipPolygonPlane(a, b, 0, box.xmin, false, eps);
    renderClipPolygonPlane(b, a, 0, box.xmax, true, eps);
    renderClipPolygonPlane(a, b, 1, box.ymin, false, eps);
    renderClipPolygonPlane(b, a, 1, box.ymax, true, eps);
    renderClipPolygonPlane(a, b, 2, box.zmin, false, eps);
    renderClipPolygonPlane(b, a, 2, box.zmax, true, eps);
    return a;
}
//=============================================================================
