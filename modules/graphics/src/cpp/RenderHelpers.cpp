//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include <math.h>
#ifndef M_PI
#define M_PI 3.141592653589793
#endif
#include "RenderHelpers.hpp"
#include "GraphicsEnvironment.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
double
pointsToPixels(double points)
{
    return points * (Nelson::GraphicsEnvironment::primaryScreenDpi() / 72.0);
}
//=============================================================================
double
fontPointsToQtPoints(double points)
{
    return points * 1.25;
}
//=============================================================================
void
DrawSymbol(RenderInterface& gc, RenderInterface::SymbolType symb, double x, double y, double z,
    double sze, std::vector<double> edgecolor, std::vector<double> fillcolor, double width)
{
    double _sze = gc.pointsToPixels(sze) / 2.0; // _sze now means "half-size"
    bool stroke = edgecolor[0] != -1;
    bool fill = fillcolor[0] != -1;

    // Glyph proportions as multiples of the circle radius _sze, so a filled
    // marker's area and bounding box match for a given scatter SizeData. Circle and
    // star keep radius _sze. The square is smaller than its bounding box, the
    // diamond is a tall rhombus, and the triangles are equilateral and anchored at
    // their centroid (circumradius 4/3 _sze).
    constexpr double squareHalf = 0.7778; // half side / _sze
    constexpr double diamondHalfW = 1.1021;
    constexpr double diamondHalfH = 1.4685;
    constexpr double triApex = 4.0 / 3.0; // centroid -> apex vertex
    constexpr double triBase = 2.0 / 3.0; // centroid -> base edge
    constexpr double triHalf = 1.1547005; // 2/sqrt(3): centroid -> base corner
    constexpr double hexagramInner = 0.5773503; // 1/sqrt(3): inner vertex radius

    gc.lineWidth(width);
    gc.setLineStyle(L"-");

    switch (symb) {
    case RenderInterface::Plus:
        if (stroke) {
            gc.color(edgecolor);

            gc.color(edgecolor);

            // Half-pixel offset to centre odd line widths
            double offset = (int(std::round(width)) % 2) ? 0.5 : 0.0;

            double cx = x + offset;
            double cy = y + offset;

            gc.line(cx, cy, z, cx, cy - _sze, z);

            gc.line(cx, cy, z, cx + _sze, cy, z);

            gc.line(cx, cy, z, cx, cy + _sze, z);

            gc.line(cx, cy, z, cx - _sze, cy, z);
        }
        break;

    case RenderInterface::Circle:
        if (fill) {
            gc.color(fillcolor);
            gc.circleFill(x, y, _sze);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.circle(x, y, _sze);
        }
        break;
    case RenderInterface::Star:
        if (stroke) {
            gc.color(edgecolor);
            double arm = _sze;

            gc.line(x - arm, y, z, x + arm, y, z);

            gc.line(x, y - arm, z, x, y + arm, z);

            double diag = arm / sqrt(2.0);
            gc.line(x - diag, y - diag, z, x + diag, y + diag, z);
            gc.line(x - diag, y + diag, z, x + diag, y - diag, z);
        }
        break;
    case RenderInterface::Dot:
        if (stroke) {
            gc.color(edgecolor);
            // The point marker '.' has a diameter of MarkerSize/3 points (_sze is the
            // half-size, so the radius is _sze/3).
            gc.circleFill(x, y, _sze / 3.0);
        }
        break;

    case RenderInterface::Times:
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x - _sze, y - _sze, z, x + _sze, y + _sze, z);
            gc.line(x + _sze, y - _sze, z, x - _sze, y + _sze, z);
        }
        break;

    case RenderInterface::Square: {
        double sq = squareHalf * _sze;
        if (fill) {
            gc.color(fillcolor);
            gc.rectFill(x - sq, y - sq, x + sq, y + sq);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.rect(x - sq, y - sq, x + sq, y + sq);
        }
    } break;

    case RenderInterface::Diamond: {
        double dw = diamondHalfW * _sze;
        double dh = diamondHalfH * _sze;
        if (fill) {
            gc.color(fillcolor);
            gc.quad(x, y - dh, 0, x + dw, y, 0, x, y + dh, 0, x - dw, y, 0);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.quadline(x, y - dh, 0, x + dw, y, 0, x, y + dh, 0, x - dw, y, 0);
        }
    } break;

    case RenderInterface::Up: {
        double ax = triHalf * _sze, ap = triApex * _sze, bs = triBase * _sze;
        if (fill) {
            gc.color(fillcolor);
            // A filled face, not three strokes: outlining in the face color left
            // MarkerFaceColor indistinguishable from MarkerEdgeColor.
            gc.tri(x, y + ap, z, x + ax, y - bs, z, x - ax, y - bs, z);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x, y + ap, z, x + ax, y - bs, z);
            gc.line(x + ax, y - bs, z, x - ax, y - bs, z);
            gc.line(x - ax, y - bs, z, x, y + ap, z);
        }
    } break;

    case RenderInterface::Down: {
        double ax = triHalf * _sze, ap = triApex * _sze, bs = triBase * _sze;
        if (fill) {
            gc.color(fillcolor);
            gc.tri(x, y - ap, z, x + ax, y + bs, z, x - ax, y + bs, z);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x, y - ap, z, x + ax, y + bs, z);
            gc.line(x + ax, y + bs, z, x - ax, y + bs, z);
            gc.line(x - ax, y + bs, z, x, y - ap, z);
        }
    } break;

    case RenderInterface::Left: {
        double ay = triHalf * _sze, ap = triApex * _sze, bs = triBase * _sze;
        if (fill) {
            gc.color(fillcolor);
            gc.tri(x - ap, y, z, x + bs, y + ay, z, x + bs, y - ay, z);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x + bs, y + ay, z, x - ap, y, z);
            gc.line(x - ap, y, z, x + bs, y - ay, z);
            gc.line(x + bs, y - ay, z, x + bs, y + ay, z);
        }
    } break;

    case RenderInterface::Right: {
        double ay = triHalf * _sze, ap = triApex * _sze, bs = triBase * _sze;
        if (fill) {
            gc.color(fillcolor);
            gc.tri(x + ap, y, z, x - bs, y + ay, z, x - bs, y - ay, z);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x - bs, y + ay, z, x + ap, y, z);
            gc.line(x + ap, y, z, x - bs, y - ay, z);
            gc.line(x - bs, y - ay, z, x - bs, y + ay, z);
        }
    } break;
    case RenderInterface::Pentagram: {
        const double pi = M_PI;
        std::vector<double> px(10), py(10);

        double innerRadius = _sze * 0.38;

        for (int i = 0; i < 10; i++) {
            double angle = pi / 2 + i * pi / 5; // start at top, 36 degree steps
            double radius = (i % 2 == 0) ? _sze : innerRadius;
            px[i] = x + radius * cos(angle);
            py[i] = y + radius * sin(angle);
        }

        if (fill) {
            gc.color(fillcolor);
            // A star is star-shaped about its center, so a triangle fan covers it.
            for (int i = 0; i < 10; i++) {
                int next = (i + 1) % 10;
                gc.tri(x, y, z, px[i], py[i], z, px[next], py[next], z);
            }
        }

        if (stroke) {
            gc.color(edgecolor);
            for (int i = 0; i < 10; i++) {
                int next = (i + 1) % 10;
                gc.line(px[i], py[i], z, px[next], py[next], z);
            }
        }
    } break;

    case RenderInterface::Hexagram: {
        const double pi = 3.14159265358979323846;
        std::vector<double> px(12), py(12);

        double innerRadius
            = _sze * hexagramInner; // 1/sqrt(3) of the outer radius: a regular hexagram

        for (int i = 0; i < 12; i++) {
            double angle = pi / 2 + i * pi / 6; // start at top, 30 degree steps
            double radius = (i % 2 == 0) ? _sze : innerRadius;
            px[i] = x + radius * cos(angle);
            py[i] = y + radius * sin(angle);
        }

        if (fill) {
            gc.color(fillcolor);
            for (int i = 0; i < 12; i++) {
                int next = (i + 1) % 12;
                gc.tri(x, y, z, px[i], py[i], z, px[next], py[next], z);
            }
        }

        if (stroke) {
            gc.color(edgecolor);

            for (int i = 0; i < 12; i++) {
                int next = (i + 1) % 12;
                gc.line(px[i], py[i], z, px[next], py[next], z);
            }
        }
    } break;
    default:
    case RenderInterface::None:
        break;
    }
}
//=============================================================================
bool
DrawSymbolBatch(RenderInterface& gc, RenderInterface::SymbolType symb,
    const std::vector<double>& xs, const std::vector<double>& ys, const std::vector<double>& sizes,
    const std::vector<double>& edgeColorsFlat, const std::vector<double>& faceColorsFlat,
    double width, const std::vector<double>& depths)
{
    if (xs.empty()) {
        return true;
    }
    const bool stroke = !edgeColorsFlat.empty();
    const bool fill = !faceColorsFlat.empty();
    const std::vector<double> none;
    switch (symb) {
    case RenderInterface::Plus:
    case RenderInterface::Times:
    case RenderInterface::Star:
        // Stroke-only glyphs, drawn in the edge colour (see DrawSymbol).
        if (!stroke) {
            return true; // nothing to draw, as DrawSymbol would also draw nothing
        }
        return gc.markerBatch(symb, xs, ys, sizes, none, edgeColorsFlat, width, depths);
    case RenderInterface::Dot:
        // A small filled disc drawn in the edge colour (see DrawSymbol::Dot).
        if (!stroke) {
            return true;
        }
        return gc.markerBatch(
            RenderInterface::Dot, xs, ys, sizes, edgeColorsFlat, none, width, depths);
    case RenderInterface::Circle:
    case RenderInterface::Square:
    case RenderInterface::Diamond:
    case RenderInterface::Up:
    case RenderInterface::Down:
    case RenderInterface::Left:
    case RenderInterface::Right:
    case RenderInterface::Pentagram:
    case RenderInterface::Hexagram:
        if (!fill && !stroke) {
            return true;
        }
        return gc.markerBatch(symb, xs, ys, sizes, fill ? faceColorsFlat : none,
            stroke ? edgeColorsFlat : none, width, depths);
    default:
    case RenderInterface::None:
        return true;
    }
}
//=============================================================================
bool
DrawSymbolBatchUniform(RenderInterface& gc, RenderInterface::SymbolType symb,
    const std::vector<double>& xs, const std::vector<double>& ys, double sze,
    const std::vector<double>& edgeColor, const std::vector<double>& faceColor, double width,
    const std::vector<double>& depths)
{
    if (xs.empty()) {
        return true;
    }
    const size_t n = xs.size();
    const bool edgeDrawable = edgeColor.size() >= 3 && edgeColor[0] != -1;
    const bool faceDrawable = faceColor.size() >= 3 && faceColor[0] != -1;
    std::vector<double> sizes(n, sze);
    std::vector<double> edgeFlat;
    std::vector<double> faceFlat;
    if (edgeDrawable) {
        edgeFlat.reserve(n * 4);
        for (size_t i = 0; i < n; i++) {
            edgeFlat.push_back(edgeColor[0]);
            edgeFlat.push_back(edgeColor[1]);
            edgeFlat.push_back(edgeColor[2]);
            edgeFlat.push_back(edgeColor.size() > 3 ? edgeColor[3] : 1.0);
        }
    }
    if (faceDrawable) {
        faceFlat.reserve(n * 4);
        for (size_t i = 0; i < n; i++) {
            faceFlat.push_back(faceColor[0]);
            faceFlat.push_back(faceColor[1]);
            faceFlat.push_back(faceColor[2]);
            faceFlat.push_back(faceColor.size() > 3 ? faceColor[3] : 1.0);
        }
    }
    return DrawSymbolBatch(gc, symb, xs, ys, sizes, edgeFlat, faceFlat, width, depths);
}
//=============================================================================
RenderInterface::SymbolType
StringToSymbol(const std::wstring& symbolName)
{
    if (symbolName == L"+") {
        return RenderInterface::Plus;
    }
    if (symbolName == L"o") {
        return RenderInterface::Circle;
    }
    if (symbolName == L"*") {
        return RenderInterface::Star;
    }
    if (symbolName == L".") {
        return RenderInterface::Dot;
    }
    if (symbolName == L"x") {
        return RenderInterface::Times;
    }
    if ((symbolName == GO_PROPERTY_VALUE_SQUARE_STR) || (symbolName == L"s")) {
        return RenderInterface::Square;
    }
    if ((symbolName == GO_PROPERTY_VALUE_DIAMOND_STR) || (symbolName == L"d")) {
        return RenderInterface::Diamond;
    }
    if (symbolName == L"^") {
        return RenderInterface::Up;
    }
    if (symbolName == L"v") {
        return RenderInterface::Down;
    }
    if (symbolName == L">") {
        return RenderInterface::Right;
    }
    if (symbolName == L"<") {
        return RenderInterface::Left;
    }
    if ((symbolName == GO_PROPERTY_VALUE_HEXAGRAM_STR) || (symbolName == L"h")) {
        return RenderInterface::Hexagram;
    }
    if ((symbolName == GO_PROPERTY_VALUE_PENTAGRAM_STR) || (symbolName == L"p")) {
        return RenderInterface::Pentagram;
    }

    return RenderInterface::None;
}
//=============================================================================
