//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QGuiApplication>
#include <QtGui/QScreen>
#include <cstdlib>
#include <math.h>
#ifndef M_PI
#define M_PI 3.141592653589793
#endif
#include "RenderHelpers.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
static double
pointsToPixels(double points)
{
    QScreen* screen = QGuiApplication::primaryScreen();
    double dpi = screen->logicalDotsPerInch();
    return points * (dpi / 72.0);
}
//=============================================================================
void
DrawSymbol(RenderInterface& gc, RenderInterface::SymbolType symb, double x, double y, double z,
    double sze, std::vector<double> edgecolor, std::vector<double> fillcolor, double width)
{
    double _sze = pointsToPixels(sze) / 2.0; // _sze now means "half-size"
    bool stroke = edgecolor[0] != -1;
    bool fill = fillcolor[0] != -1;

    gc.lineWidth(width);
    gc.setLineStyle(L"-");

    switch (symb) {
    case RenderInterface::Plus:
        if (stroke) {
            gc.color(edgecolor);

            gc.color(edgecolor);

            // Small adjustment to ensure perfect centering for odd line widths
            double offset = (int(std::round(width)) % 2) ? 0.5 : 0.0;

            // Center point coordinates with offset adjustment
            double cx = x + offset;
            double cy = y + offset;

            // Draw the plus sign with four equal segments from center
            // Top segment
            gc.line(cx, cy, z, cx, cy - _sze, z);

            // Right segment
            gc.line(cx, cy, z, cx + _sze, cy, z);

            // Bottom segment
            gc.line(cx, cy, z, cx, cy + _sze, z);

            // Left segment
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

            // Horizontal line
            gc.line(x - arm, y, z, x + arm, y, z);

            // Vertical line
            gc.line(x, y - arm, z, x, y + arm, z);

            // Diagonal lines (45 degrees)
            double diag = arm / sqrt(2.0);
            gc.line(x - diag, y - diag, z, x + diag, y + diag, z);
            gc.line(x - diag, y + diag, z, x + diag, y - diag, z);
        }
        break;
    case RenderInterface::Dot:
        if (stroke) {
            gc.color(edgecolor);
            gc.circleFill(x, y, _sze * 0.6);
        }
        break;

    case RenderInterface::Times:
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x - _sze, y - _sze, z, x + _sze, y + _sze, z);
            gc.line(x + _sze, y - _sze, z, x - _sze, y + _sze, z);
        }
        break;

    case RenderInterface::Square:
        if (fill) {
            gc.color(fillcolor);
            gc.rectFill(x - _sze, y - _sze, x + _sze, y + _sze);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.rect(x - _sze, y - _sze, x + _sze, y + _sze);
        }
        break;

    case RenderInterface::Diamond:
        if (fill) {
            gc.color(fillcolor);
            gc.quad(x, y - _sze, 0, x + _sze, y, 0, x, y + _sze, 0, x - _sze, y, 0);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.quadline(x, y - _sze, 0, x + _sze, y, 0, x, y + _sze, 0, x - _sze, y, 0);
        }
        break;

    case RenderInterface::Up:
        if (fill) {
            gc.color(fillcolor);
            // Fill the upward triangle by drawing lines between the three points
            gc.line(x, y + _sze, z, x + _sze, y - _sze, z);
            gc.line(x + _sze, y - _sze, z, x - _sze, y - _sze, z);
            gc.line(x - _sze, y - _sze, z, x, y + _sze,
                z); // Close the triangle by connecting back to the start
        }
        if (stroke) {
            gc.color(edgecolor);
            // Outline of the upward triangle
            gc.line(x, y + _sze, z, x + _sze, y - _sze, z);
            gc.line(x + _sze, y - _sze, z, x - _sze, y - _sze, z);
            gc.line(x - _sze, y - _sze, z, x, y + _sze,
                z); // Close the triangle by connecting back to the start
        }
        break;

    case RenderInterface::Down:
        if (fill) {
            gc.color(fillcolor);
            // Fill the downward triangle by drawing lines between the three points
            gc.line(x, y - _sze, z, x + _sze, y + _sze, z);
            gc.line(x + _sze, y + _sze, z, x - _sze, y + _sze, z);
            gc.line(x - _sze, y + _sze, z, x, y - _sze,
                z); // Close the triangle by connecting back to the start
        }
        if (stroke) {
            gc.color(edgecolor);
            // Outline of the downward triangle
            gc.line(x, y - _sze, z, x + _sze, y + _sze, z);
            gc.line(x + _sze, y + _sze, z, x - _sze, y + _sze, z);
            gc.line(x - _sze, y + _sze, z, x, y - _sze,
                z); // Close the triangle by connecting back to the start
        }
        break;

    case RenderInterface::Left:
        if (fill) {
            gc.color(fillcolor);
            // Fill the leftward triangle by drawing lines between the three points
            gc.line(x - _sze, y + _sze, z, x + _sze, y, z);
            gc.line(x + _sze, y, z, x - _sze, y - _sze, z);
            gc.line(x - _sze, y - _sze, z, x - _sze, y + _sze,
                z); // Close the triangle by connecting back to the start
        }
        if (stroke) {
            gc.color(edgecolor);
            // Outline of the leftward triangle
            gc.line(x - _sze, y + _sze, z, x + _sze, y, z);
            gc.line(x + _sze, y, z, x - _sze, y - _sze, z);
            gc.line(x - _sze, y - _sze, z, x - _sze, y + _sze,
                z); // Close the triangle by connecting back to the start
        }
        break;

    case RenderInterface::Right:
        if (fill) {
            gc.color(fillcolor);
            // Fill the rightward triangle by drawing lines between the three points
            gc.line(x + _sze, y + _sze, z, x - _sze, y, z);
            gc.line(x - _sze, y, z, x + _sze, y - _sze, z);
            gc.line(x + _sze, y - _sze, z, x + _sze, y + _sze,
                z); // Close the triangle by connecting back to the start
        }
        if (stroke) {
            gc.color(edgecolor);
            // Outline of the rightward triangle
            gc.line(x + _sze, y + _sze, z, x - _sze, y, z);
            gc.line(x - _sze, y, z, x + _sze, y - _sze, z);
            gc.line(x + _sze, y - _sze, z, x + _sze, y + _sze,
                z); // Close the triangle by connecting back to the start
        }
        break;
    case RenderInterface::Pentagram: {
        // A regular pentagram (5-pointed star) formed by a single path
        // similar to the hexagram implementation
        const double pi = M_PI;
        std::vector<double> px(10), py(10); // 10 points for a proper pentagram

        double innerRadius = _sze * 0.38; // Inner radius adjustment for standard star shape

        // Generate 10 points alternating between outer and inner circle
        for (int i = 0; i < 10; i++) {
            double angle = pi / 2 + i * pi / 5; // Start at top (90d) and go in 36d increments
            // Alternate between outer and inner radius
            double radius = (i % 2 == 0) ? _sze : innerRadius;
            px[i] = x + radius * cos(angle);
            py[i] = y + radius * sin(angle);
        }

        if (fill) {
            gc.color(fillcolor);
            // For filling, draw triangular segments
            for (int i = 0; i < 10; i++) {
                int next = (i + 1) % 10;
                // Line from center to current point
                gc.line(x, y, z, px[i], py[i], z);
                // Line from current point to next point
                gc.line(px[i], py[i], z, px[next], py[next], z);
            }
        }

        if (stroke) {
            gc.color(edgecolor);
            // Draw the continuous star path connecting all points
            for (int i = 0; i < 10; i++) {
                int next = (i + 1) % 10;
                gc.line(px[i], py[i], z, px[next], py[next], z);
            }
        }
    } break;

    case RenderInterface::Hexagram: {
        // A regular hexagram (6-pointed star) formed by a single path
        // The points alternate between an outer circle and an inner circle
        const double pi = 3.14159265358979323846;
        std::vector<double> px(12), py(12); // 12 points for a proper hexagram

        double innerRadius
            = _sze * 0.5; // Inner radius is half the outer radius for standard star shape

        // Generate 12 points alternating between outer and inner circle
        for (int i = 0; i < 12; i++) {
            double angle = pi / 2 + i * pi / 6; // Start at top (90s) and go in 30s increments
            // Alternate between outer and inner radius
            double radius = (i % 2 == 0) ? _sze : innerRadius;
            px[i] = x + radius * cos(angle);
            py[i] = y + radius * sin(angle);
        }

        if (fill) {

            // For filling, we'll draw lines from the center to each point
            // and between consecutive points to form triangular segments
            for (int i = 0; i < 12; i++) {
                int next = (i + 1) % 12;
                // Line from center to current point
                gc.line(x, y, z, px[i], py[i], z);
                // Line from current point to next point
                gc.line(px[i], py[i], z, px[next], py[next], z);
            }
        }

        if (stroke) {
            gc.color(edgecolor);

            // Draw the continuous star path connecting all points
            for (int i = 0; i < 12; i++) {
                int next = (i + 1) % 12;
                gc.line(px[i], py[i], z, px[next], py[next], z);
            }
        }
    } break;
    default:
    case RenderInterface::None:
        // Do nothing
        break;
    }
}
//=============================================================================
RenderInterface::SymbolType
StringToSymbol(const std::wstring& symbolName)
{
    if (symbolName == L"+")
        return RenderInterface::Plus;
    if (symbolName == L"o")
        return RenderInterface::Circle;
    if (symbolName == L"*")
        return RenderInterface::Star;
    if (symbolName == L".")
        return RenderInterface::Dot;
    if (symbolName == L"x")
        return RenderInterface::Times;
    if ((symbolName == GO_PROPERTY_VALUE_SQUARE_STR) || (symbolName == L"s"))
        return RenderInterface::Square;
    if ((symbolName == GO_PROPERTY_VALUE_DIAMOND_STR) || (symbolName == L"d"))
        return RenderInterface::Diamond;
    if (symbolName == L"^")
        return RenderInterface::Up;
    if (symbolName == L"v")
        return RenderInterface::Down;
    if (symbolName == L">")
        return RenderInterface::Right;
    if (symbolName == L"<")
        return RenderInterface::Left;
    if ((symbolName == GO_PROPERTY_VALUE_HEXAGRAM_STR) || (symbolName == L"h"))
        return RenderInterface::Hexagram;
    if ((symbolName == GO_PROPERTY_VALUE_PENTAGRAM_STR) || (symbolName == L"p"))
        return RenderInterface::Pentagram;

    return RenderInterface::None;
}
//=============================================================================
