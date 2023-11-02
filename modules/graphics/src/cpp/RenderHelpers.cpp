//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "GOPropertyValues.hpp"
//=============================================================================
void
DrawSymbol(RenderInterface& gc, RenderInterface::SymbolType symb, double x, double y, double z,
    double sze, std::vector<double> edgecolor, std::vector<double> fillcolor, double width)
{
    bool stroke = edgecolor[0] != -1;
    bool fill = fillcolor[0] != -1;
    gc.lineWidth(width);
    gc.setLineStyle(L"-");
    switch (symb) {
    case RenderInterface::Plus:
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x - sze, y, z, x + sze, y, z);
            gc.line(x, y - sze, z, x, y + sze, z);
        }
        break;
    case RenderInterface::Circle:
        if (fill) {
            gc.color(fillcolor);
            gc.circleFill(x, y, sze);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.circle(x, y, sze);
        }
        break;
    case RenderInterface::Star:
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x - sze, y - sze, z, x + sze, y + sze, z);
            gc.line(x + sze, y - sze, z, x - sze, y + sze, z);
            gc.line(x - sze, y, z, x + sze, y, z);
            gc.line(x, y - sze, z, x, y + sze, z);
        }
        break;
    case RenderInterface::Dot:
        if (stroke) {
            gc.color(edgecolor);
            gc.circleFill(x, y, sze / 3.0);
        }
        break;
    case RenderInterface::Times:
        if (stroke) {
            gc.color(edgecolor);
            gc.line(x - sze, y - sze, z, x + sze, y + sze, z);
            gc.line(x + sze, y - sze, z, x - sze, y + sze, z);
        }
        break;
    case RenderInterface::Square:
        if (fill) {
            gc.color(fillcolor);
            gc.rectFill(x - sze, y - sze, x + sze, y + sze);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.rect(x - sze, y - sze, x + sze, y + sze);
        }
        break;
    case RenderInterface::Diamond:
        if (fill) {
            gc.color(fillcolor);
            gc.quad(x, y - sze, 0, x + sze, y, 0, x, y + sze, 0, x - sze, y, 0);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.quadline(x, y - sze, 0, x + sze, y, 0, x, y + sze, 0, x - sze, y, 0);
        }
        break;
    case RenderInterface::Up:
        if (fill) {
            gc.color(fillcolor);
            gc.tri(x, y + sze, 0, x + sze, y - sze, 0, x - sze, y - sze, 0);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.triLine(x, y + sze, 0, x + sze, y - sze, 0, x - sze, y - sze, 0);
        }
        break;
    case RenderInterface::Down:
        if (fill) {
            gc.color(fillcolor);
            gc.tri(x, y - sze, 0, x + sze, y + sze, 0, x - sze, y + sze, 0);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.triLine(x, y - sze, 0, x + sze, y + sze, 0, x - sze, y + sze, 0);
        }
        break;
    case RenderInterface::Right:
        if (fill) {
            gc.color(fillcolor);
            gc.tri(x - sze, y + sze, 0, x + sze, y, 0, x - sze, y - sze, 0);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.triLine(x - sze, y + sze, 0, x + sze, y, 0, x - sze, y - sze, 0);
        }
        break;
    case RenderInterface::Left:
        if (fill) {
            gc.color(fillcolor);
            gc.tri(x + sze, y + sze, 0, x - sze, y, 0, x + sze, y - sze, 0);
        }
        if (stroke) {
            gc.color(edgecolor);
            gc.triLine(x + sze, y + sze, 0, x - sze, y, 0, x + sze, y - sze, 0);
        }
        break;
    case RenderInterface::Pentagram: {
        gc.color(edgecolor);

        // Calculate the outer and inner points of the star
        double outerRadius = sze;
        double innerRadius = sze / 2.5; // You can adjust the factor to control the star's shape

        for (int i = 0; i < 5; i++) {
            double angleOuter = i * 2 * M_PI / 5; // Calculate the angle for outer points
            double angleInner = (i + 0.5) * 2 * M_PI / 5; // Calculate the angle for inner points

            // Calculate the coordinates for the outer and inner points
            double outerX = x + outerRadius * cos(angleOuter);
            double outerY = y + outerRadius * sin(angleOuter);
            double innerX = x + innerRadius * cos(angleInner);
            double innerY = y + innerRadius * sin(angleInner);

            // Draw lines connecting the outer and inner points to form the star
            gc.line(outerX, outerY, z, innerX, innerY, z);
        }
    } break;
    case RenderInterface::Hexagram: {
        gc.color(edgecolor);

        // Calculate the outer and inner points of the star
        double outerRadius = sze;
        double innerRadius = sze / 2.5; // You can adjust the factor to control the star's shape

        for (int i = 0; i < 8; i++) {
            double angleOuter = i * M_PI / 4; // Calculate the angle for outer points
            double angleInner = (i + 0.5) * M_PI / 4; // Calculate the angle for inner points

            // Calculate the coordinates for the outer and inner points
            double outerX = x + outerRadius * cos(angleOuter);
            double outerY = y + outerRadius * sin(angleOuter);
            double innerX = x + innerRadius * cos(angleInner);
            double innerY = y + innerRadius * sin(angleInner);

            // Draw lines connecting the outer and inner points to form the star
            gc.line(outerX, outerY, z, innerX, innerY, z);
        }
    } break;
    case RenderInterface::None:
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
