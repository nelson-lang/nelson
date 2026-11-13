//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <limits>
#include "GOLimitsInRange.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
hasNonFinite(const std::array<double, 3>& p);
static bool
clipSegment(const LimitsRange& range, std::array<double, 3>& a, std::array<double, 3>& b);
static std::vector<std::array<double, 3>>
clipPolygonPlane(const std::vector<std::array<double, 3>>& polygon, int dim, double bound,
    bool keepAbove);
//=============================================================================
LimitsRange::LimitsRange(const std::vector<double>& box, const std::array<bool, 3>& logScale)
{
    for (int k = 0; k < 3; ++k) {
        log[k] = logScale[k];
        double lo = (box.size() > 2 * k) ? box[2 * k] : -std::numeric_limits<double>::infinity();
        double hi = (box.size() > 2 * k + 1) ? box[2 * k + 1]
                                             : std::numeric_limits<double>::infinity();
        if (std::isnan(lo)) {
            lo = -std::numeric_limits<double>::infinity();
        }
        if (std::isnan(hi)) {
            hi = std::numeric_limits<double>::infinity();
        }
        if (log[k]) {
            lo = (lo > 0.) ? std::log10(lo) : -std::numeric_limits<double>::infinity();
            hi = (hi > 0.) ? std::log10(hi) : -std::numeric_limits<double>::infinity();
        }
        lower[k] = lo;
        upper[k] = hi;
    }
}
//=============================================================================
bool
LimitsRange::contains(const std::array<double, 3>& p) const
{
    for (int k = 0; k < 3; ++k) {
        if (!(p[k] >= lower[k] && p[k] <= upper[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
std::array<double, 3>
LimitsRange::toClip(double x, double y, double z) const
{
    std::array<double, 3> p = { x, y, z };
    for (int k = 0; k < 3; ++k) {
        if (log[k]) {
            p[k] = (p[k] > 0.) ? std::log10(p[k]) : std::nan("");
        }
    }
    return p;
}
//=============================================================================
std::array<double, 3>
LimitsRange::fromClip(const std::array<double, 3>& p) const
{
    std::array<double, 3> q = p;
    for (int k = 0; k < 3; ++k) {
        if (log[k]) {
            q[k] = std::pow(10., q[k]);
        }
    }
    return q;
}
//=============================================================================
LimitsInRangeAccumulator::LimitsInRangeAccumulator(const LimitsRange& range) : range(range)
{
    for (int k = 0; k < 3; ++k) {
        mins[k] = std::numeric_limits<double>::infinity();
        maxs[k] = -std::numeric_limits<double>::infinity();
    }
}
//=============================================================================
void
LimitsInRangeAccumulator::addPoints(
    const std::vector<double>& xs, const std::vector<double>& ys, const std::vector<double>& zs)
{
    size_t n = std::min(xs.size(), ys.size());
    for (size_t i = 0; i < n; ++i) {
        std::array<double, 3> p = range.toClip(xs[i], ys[i], i < zs.size() ? zs[i] : 0.);
        if (!hasNonFinite(p) && range.contains(p)) {
            addClipPoint(p);
        }
    }
}
//=============================================================================
void
LimitsInRangeAccumulator::addPolyline(
    const std::vector<double>& xs, const std::vector<double>& ys, const std::vector<double>& zs)
{
    // NaN breaks are bridged, as the reference does for its limits.
    size_t n = std::min(xs.size(), ys.size());
    std::vector<std::array<double, 3>> points;
    points.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        std::array<double, 3> p = range.toClip(xs[i], ys[i], i < zs.size() ? zs[i] : 0.);
        if (!hasNonFinite(p)) {
            points.push_back(p);
        }
    }
    if (points.size() == 1) {
        if (range.contains(points[0])) {
            addClipPoint(points[0]);
        }
        return;
    }
    for (size_t i = 0; i + 1 < points.size(); ++i) {
        std::array<double, 3> a = points[i];
        std::array<double, 3> b = points[i + 1];
        if (clipSegment(range, a, b)) {
            addClipPoint(a);
            addClipPoint(b);
        }
    }
}
//=============================================================================
void
LimitsInRangeAccumulator::addPolygon(const std::vector<std::array<double, 3>>& polygon)
{
    std::vector<std::array<double, 3>> clipped;
    clipped.reserve(polygon.size());
    for (const auto& v : polygon) {
        std::array<double, 3> p = range.toClip(v[0], v[1], v[2]);
        if (!hasNonFinite(p)) {
            clipped.push_back(p);
        }
    }
    for (int k = 0; k < 3 && !clipped.empty(); ++k) {
        if (std::isfinite(range.lower[k])) {
            clipped = clipPolygonPlane(clipped, k, range.lower[k], true);
        }
        if (!clipped.empty() && std::isfinite(range.upper[k])) {
            clipped = clipPolygonPlane(clipped, k, range.upper[k], false);
        }
    }
    for (const auto& p : clipped) {
        addClipPoint(p);
    }
}
//=============================================================================
bool
LimitsInRangeAccumulator::empty() const
{
    return !hasData;
}
//=============================================================================
std::vector<double>
LimitsInRangeAccumulator::result(const std::vector<double>& fullLimits) const
{
    if (!hasData) {
        return {};
    }
    std::array<double, 3> lo = range.fromClip({ mins[0], mins[1], mins[2] });
    std::array<double, 3> hi = range.fromClip({ maxs[0], maxs[1], maxs[2] });
    std::vector<double> limits = { lo[0], hi[0], lo[1], hi[1], lo[2], hi[2] };
    for (size_t k = 6; k < 10; ++k) {
        limits.push_back(k < fullLimits.size() ? fullLimits[k] : 0.);
    }
    return limits;
}
//=============================================================================
void
LimitsInRangeAccumulator::addClipPoint(const std::array<double, 3>& p)
{
    for (int k = 0; k < 3; ++k) {
        mins[k] = std::min(mins[k], p[k]);
        maxs[k] = std::max(maxs[k], p[k]);
    }
    hasData = true;
}
//=============================================================================
bool
hasNonFinite(const std::array<double, 3>& p)
{
    return !std::isfinite(p[0]) || !std::isfinite(p[1]) || !std::isfinite(p[2]);
}
//=============================================================================
// Liang-Barsky clipping of segment [a b] to the range; false when nothing is left.
bool
clipSegment(const LimitsRange& range, std::array<double, 3>& a, std::array<double, 3>& b)
{
    double t0 = 0.;
    double t1 = 1.;
    auto clipEdge = [&](double p, double q) -> bool {
        if (p == 0.) {
            return q >= 0.;
        }
        double r = q / p;
        if (p < 0.) {
            if (r > t1) {
                return false;
            }
            t0 = std::max(t0, r);
        } else {
            if (r < t0) {
                return false;
            }
            t1 = std::min(t1, r);
        }
        return true;
    };
    for (int k = 0; k < 3; ++k) {
        double d = b[k] - a[k];
        if (std::isfinite(range.lower[k]) && !clipEdge(-d, a[k] - range.lower[k])) {
            return false;
        }
        if (std::isfinite(range.upper[k]) && !clipEdge(d, range.upper[k] - a[k])) {
            return false;
        }
    }
    std::array<double, 3> start = a;
    for (int k = 0; k < 3; ++k) {
        double d = b[k] - start[k];
        a[k] = (t0 == 0.) ? start[k] : start[k] + t0 * d;
        b[k] = (t1 == 1.) ? b[k] : start[k] + t1 * d;
        // Rounding must not push a clipped end outside the range.
        a[k] = std::min(std::max(a[k], range.lower[k]), range.upper[k]);
        b[k] = std::min(std::max(b[k], range.lower[k]), range.upper[k]);
    }
    return true;
}
//=============================================================================
// One Sutherland-Hodgman pass against the plane p[dim] = bound.
std::vector<std::array<double, 3>>
clipPolygonPlane(
    const std::vector<std::array<double, 3>>& polygon, int dim, double bound, bool keepAbove)
{
    auto inside = [&](const std::array<double, 3>& p) {
        return keepAbove ? p[dim] >= bound : p[dim] <= bound;
    };
    std::vector<std::array<double, 3>> out;
    size_t n = polygon.size();
    if (n == 1) {
        if (inside(polygon[0])) {
            out.push_back(polygon[0]);
        }
        return out;
    }
    for (size_t i = 0; i < n; ++i) {
        const std::array<double, 3>& cur = polygon[i];
        const std::array<double, 3>& prev = polygon[(i + n - 1) % n];
        bool curIn = inside(cur);
        bool prevIn = inside(prev);
        if (curIn != prevIn) {
            double t = (bound - prev[dim]) / (cur[dim] - prev[dim]);
            std::array<double, 3> cut;
            for (int k = 0; k < 3; ++k) {
                cut[k] = (k == dim) ? bound : prev[k] + t * (cur[k] - prev[k]);
            }
            out.push_back(cut);
        }
        if (curIn) {
            out.push_back(cur);
        }
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
