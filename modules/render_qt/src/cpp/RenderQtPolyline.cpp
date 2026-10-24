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
#include <QtCore/QLineF>
#include "RenderQtPolyline.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Segments per stroked chunk. Measured on 2000 and 20000 random points with
// 1.67 and 5 px pens: 8..64 are within noise at 20000 points, and 16 stays
// fastest at 2000 (the chunk cost grows with its length, 128 is ~6x slower).
static constexpr int CHUNK_SEGMENTS = 16;
// At or below this pixel width, a solid pen is drawn segment by segment
// (Qt's line rasterizer, ~4x faster than any path stroke): the bevel wedge a
// join would add is then under a pixel, and the OpenGL canvas (a line strip)
// draws no join either.
static constexpr double THIN_PEN_PIXELS = 2.0;
//=============================================================================
static double
segmentLength(const QPointF& a, const QPointF& b);
static void
extendEndpoint(QVector<QPointF>& points, int end, int neighbor, double distance);
//=============================================================================
void
strokePolyline(QPainter* painter, QVector<QPointF> points)
{
    const int pointCount = static_cast<int>(points.size());
    if (pointCount <= CHUNK_SEGMENTS + 1) {
        painter->drawPolyline(points);
        return;
    }
    const QPen originalPen = painter->pen();
    if (originalPen.style() == Qt::NoPen) {
        return;
    }
    // Qt states pen-relative lengths in pen widths (a cosmetic 0 counts as 1).
    const double penUnit = std::max(1.0, originalPen.widthF());
    const bool solid = originalPen.style() == Qt::SolidLine;
    QPen pen(originalPen);
    if (solid && originalPen.capStyle() != Qt::RoundCap) {
        // Chunk ends meet inside the stroke: they must not be capped. A square
        // cap is a flat cap on an endpoint pushed out by half the width, so
        // the polyline's two true ends keep theirs that way.
        if (originalPen.capStyle() == Qt::SquareCap) {
            extendEndpoint(points, 0, 1, penUnit / 2);
            extendEndpoint(points, pointCount - 1, pointCount - 2, penUnit / 2);
        }
        pen.setCapStyle(Qt::FlatCap);
        painter->setPen(pen);
    }
    if (solid && originalPen.widthF() <= THIN_PEN_PIXELS) {
        QVector<QLineF> segments;
        segments.reserve(pointCount - 1);
        for (int k = 1; k < pointCount; ++k) {
            segments.append(QLineF(points[k - 1], points[k]));
        }
        painter->drawLines(segments);
        painter->setPen(originalPen);
        return;
    }
    // Consecutive chunks share one segment: every interior vertex is then a
    // join of some chunk, and each chunk end lies inside the shared segment.
    double travelled = 0.0;
    int start = 0;
    while (true) {
        const int end = std::min(start + CHUNK_SEGMENTS, pointCount - 1);
        if (!solid) {
            // Keep the dash phase running across chunks. Dashes keep the
            // pen's cap: each dash has its own caps anyway.
            pen.setDashOffset(originalPen.dashOffset() + travelled / penUnit);
            painter->setPen(pen);
        }
        painter->drawPolyline(points.constData() + start, end - start + 1);
        if (end == pointCount - 1) {
            break;
        }
        for (int k = start; k < end - 1; ++k) {
            travelled += segmentLength(points[k], points[k + 1]);
        }
        start = end - 1;
    }
    painter->setPen(originalPen);
}
//=============================================================================
double
segmentLength(const QPointF& a, const QPointF& b)
{
    return std::hypot(b.x() - a.x(), b.y() - a.y());
}
//=============================================================================
void
extendEndpoint(QVector<QPointF>& points, int end, int neighbor, double distance)
{
    const double length = segmentLength(points[neighbor], points[end]);
    if (length > 0) {
        points[end] += (points[end] - points[neighbor]) * (distance / length);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
