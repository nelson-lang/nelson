//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RenderWebMarkerGeometry.hpp"
#include "RenderInterface.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
MarkerGeom
markerShapeGeometry(int shape, double cx, double cy, double r,
    std::vector<std::pair<double, double>>& poly, std::vector<std::array<double, 4>>& segs)
{
    constexpr double PI = 3.14159265358979323846;
    // Glyph proportions, as multiples of the circle radius r, chosen so a filled
    // marker's area and bounding box match for a given SizeData. The square is
    // smaller than its bounding box, the diamond is a tall rhombus, and the
    // triangles are equilateral and anchored at the data point (centroid,
    // circumradius 4/3 r). Kept in step with DrawSymbol in the graphics module and
    // figureMarkers.ts in the frontend.
    constexpr double squareHalf = 0.7778;
    constexpr double diamondHalfW = 1.1021;
    constexpr double diamondHalfH = 1.4685;
    constexpr double triApex = 4.0 / 3.0;
    constexpr double triBase = 2.0 / 3.0;
    constexpr double triHalf = 1.1547005; // 2/sqrt(3)
    poly.clear();
    segs.clear();
    switch (shape) {
    case RenderInterface::Square: {
        const double s = squareHalf * r;
        poly = { { cx - s, cy - s }, { cx + s, cy - s }, { cx + s, cy + s }, { cx - s, cy + s } };
        return MarkerGeom::Polygon;
    }
    case RenderInterface::Diamond: {
        const double dw = diamondHalfW * r, dh = diamondHalfH * r;
        poly = { { cx, cy - dh }, { cx + dw, cy }, { cx, cy + dh }, { cx - dw, cy } };
        return MarkerGeom::Polygon;
    }
    case RenderInterface::Up: {
        const double ax = triHalf * r, ap = triApex * r, bs = triBase * r;
        poly = { { cx, cy - ap }, { cx + ax, cy + bs }, { cx - ax, cy + bs } };
        return MarkerGeom::Polygon;
    }
    case RenderInterface::Down: {
        const double ax = triHalf * r, ap = triApex * r, bs = triBase * r;
        poly = { { cx, cy + ap }, { cx + ax, cy - bs }, { cx - ax, cy - bs } };
        return MarkerGeom::Polygon;
    }
    case RenderInterface::Left: {
        const double ay = triHalf * r, ap = triApex * r, bs = triBase * r;
        poly = { { cx - ap, cy }, { cx + bs, cy - ay }, { cx + bs, cy + ay } };
        return MarkerGeom::Polygon;
    }
    case RenderInterface::Right: {
        const double ay = triHalf * r, ap = triApex * r, bs = triBase * r;
        poly = { { cx + ap, cy }, { cx - bs, cy - ay }, { cx - bs, cy + ay } };
        return MarkerGeom::Polygon;
    }
    case RenderInterface::Pentagram:
    case RenderInterface::Hexagram: {
        const int spikes = (shape == RenderInterface::Pentagram) ? 5 : 6;
        const double inner = r * ((shape == RenderInterface::Pentagram) ? 0.38 : 0.5773503);
        for (int i = 0; i < spikes * 2; ++i) {
            const double ang = -PI / 2.0 + i * PI / spikes;
            const double rad = (i % 2 == 0) ? r : inner;
            poly.emplace_back(cx + rad * std::cos(ang), cy + rad * std::sin(ang));
        }
        return MarkerGeom::Polygon;
    }
    case RenderInterface::Plus:
        segs = { { cx - r, cy, cx + r, cy }, { cx, cy - r, cx, cy + r } };
        return MarkerGeom::Strokes;
    case RenderInterface::Times: {
        segs = { { cx - r, cy - r, cx + r, cy + r }, { cx + r, cy - r, cx - r, cy + r } };
        return MarkerGeom::Strokes;
    }
    case RenderInterface::Star: {
        const double d = r / std::sqrt(2.0);
        segs = { { cx - r, cy, cx + r, cy }, { cx, cy - r, cx, cy + r },
            { cx - d, cy - d, cx + d, cy + d }, { cx - d, cy + d, cx + d, cy - d } };
        return MarkerGeom::Strokes;
    }
    case RenderInterface::Circle:
    case RenderInterface::Dot:
    default:
        return MarkerGeom::Disc;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
