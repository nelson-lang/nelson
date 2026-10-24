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
#include <array>
#include <utility>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
// Marker geometry in screen pixels (y-down, apex up), kept in step with the
// frontend's figureMarkers.ts so the software raster and the SVG/PDF writers draw
// what the canvas draws. A shape is either a disc, a polygon to fill/outline, or a
// set of strokes (segments x1,y1,x2,y2).
enum class MarkerGeom
{
    Disc,
    Polygon,
    Strokes
};
//=============================================================================
// shape is a RenderInterface::SymbolType; (cx, cy) the marker centre and r its
// outer radius. poly / segs receive the geometry for Polygon / Strokes.
MarkerGeom
markerShapeGeometry(int shape, double cx, double cy, double r,
    std::vector<std::pair<double, double>>& poly, std::vector<std::array<double, 4>>& segs);
//=============================================================================
} // namespace Nelson
//=============================================================================
