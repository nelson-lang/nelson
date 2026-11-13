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
#include <array>
#include <cmath>
#include "GOArrayOfProperty.hpp"
#include "GOColorBarGeometry.hpp"
#include "GOAxesLayoutManager.hpp"
#include "GOAxis.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isHorizontalLocation(const std::wstring& loc)
{
    return loc == GO_PROPERTY_VALUE_NORTH_STR || loc == GO_PROPERTY_VALUE_SOUTH_STR
        || loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR || loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR;
}
//=============================================================================
bool
isDefaultColorBarAxesPosition(const std::vector<double>& pos)
{
    if (pos.size() < 4) {
        return false;
    }
    const std::vector<double> ref = { 0.13, 0.11, 0.775, 0.815 };
    for (size_t k = 0; k < 4; ++k) {
        if (std::fabs(pos[k] - ref[k]) > 1e-8) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
isColorBarPeer3D(GraphicsObject* peer)
{
    GOAxis* axis = dynamic_cast<GOAxis*>(peer);
    return axis && !axis->is2D();
}
//=============================================================================
bool
isColorBarPeerInTiledLayout(GraphicsObject* peer)
{
    if (!peer) {
        return false;
    }
    GOGObjectsProperty* layoutProp
        = dynamic_cast<GOGObjectsProperty*>(peer->findProperty(GO_LAYOUT_PROPERTY_NAME_STR, false));
    if (!layoutProp || layoutProp->data().empty()) {
        return false;
    }
    int64 layoutOptionsHandle = layoutProp->data()[0];
    if (layoutOptionsHandle < HANDLE_OFFSET_OBJECT
        || isDeletedGraphicsObject(layoutOptionsHandle)) {
        return false;
    }
    GraphicsObject* layoutOptions = findGraphicsObject(layoutOptionsHandle, false);
    if (!layoutOptions || !layoutOptions->isType(L"layoutoptions")) {
        return false;
    }
    GOArrayOfProperty* tileProp = dynamic_cast<GOArrayOfProperty*>(
        layoutOptions->findProperty(GO_TILE_PROPERTY_NAME_STR, false));
    if (!tileProp) {
        return false;
    }
    ArrayOf tile = tileProp->get();
    if (!tile.isNumeric() || tile.isComplex() || tile.elementCount() != 1) {
        return false;
    }
    std::vector<double> value = tile.asDoubleVector();
    return !value.empty() && value[0] > 0.0;
}
//=============================================================================
std::vector<double>
tiledColorBarPeerPosition(GraphicsObject* peer, const std::vector<double>& base)
{
    std::vector<double> result = base;
    if (result.size() < 4 || !isColorBarPeerInTiledLayout(peer)) {
        return result;
    }
    double reserve = 0.0292292314737081;
    if (result[3] > 0.33 && result[3] < 0.38) {
        double top = result[1] + result[3];
        result[3] -= reserve;
        if (result[1] > 0.5) {
            // Upper-row tile: keep the top edge fixed, shrink from the bottom.
            result[1] = top - result[3];
        }
        // Lower-row tile: keep the bottom edge fixed, shrink from the top.
    }
    return result;
}
//=============================================================================
std::vector<double>
tiledColorBarPosition(const std::vector<double>& base, const std::wstring& loc, bool peer3D)
{
    double x = base[0];
    double y = base[1];
    double w = base[2];
    double h = base[3];
    double thickV = 0.0186219739292365;
    double thickH = 0.0309597523219815;
    double gapX = peer3D ? 0.040968501886028 : 0.0124146492861577;
    double gapY = 0.0557275539335207;
    double inset = 0.015;

    if (loc == GO_PROPERTY_VALUE_EAST_STR) {
        return { x + w - thickV - inset * w, y + inset * h, thickV, h * (1 - 2 * inset) };
    }
    if (loc == GO_PROPERTY_VALUE_WEST_STR) {
        return { x + inset * w, y + inset * h, thickV, h * (1 - 2 * inset) };
    }
    if (loc == GO_PROPERTY_VALUE_NORTH_STR) {
        return { x + inset * w, y + h - thickH - inset * h, w * (1 - 2 * inset), thickH };
    }
    if (loc == GO_PROPERTY_VALUE_SOUTH_STR) {
        return { x + inset * w, y + inset * h, w * (1 - 2 * inset), thickH };
    }
    if (loc == GO_PROPERTY_VALUE_EASTOUTSIDE_STR) {
        return { x + w + gapX, y, thickV, h };
    }
    if (loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR) {
        return { std::max(0.0, x - gapX - thickV), y, thickV, h };
    }
    if (loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR) {
        return { x, y + h + gapY, w, thickH };
    }
    if (loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR) {
        return { x, std::max(0.0, y - gapY - thickH), w, thickH };
    }
    return { x + w + gapX, y, thickV, h };
}
//=============================================================================
static bool
isEastLocation(const std::wstring& loc)
{
    return loc == GO_PROPERTY_VALUE_EAST_STR || loc == GO_PROPERTY_VALUE_EASTOUTSIDE_STR;
}
//=============================================================================
std::vector<double>
constrainedColorBarPosition(GraphicsObject* peer, const std::wstring& loc,
    const std::vector<double>& colorBarPosition, const std::vector<double>& peerBox,
    double figureWidth, double figureHeight)
{
    std::vector<double> result = colorBarPosition;
    if (result.size() < 4 || peerBox.size() < 4) {
        return result;
    }
    // The colorbar hugs the box the peer really draws when its aspect is locked.
    std::vector<double> drawn = axesDrawnBox(peer, peerBox, figureWidth, figureHeight);
    if (drawn.size() < 4 || drawn == peerBox) {
        return result;
    }
    const double px = peerBox[0];
    const double py = peerBox[1];
    const double pw = peerBox[2];
    const double ph = peerBox[3];
    const double bx = drawn[0];
    const double by = drawn[1];
    const double bw = drawn[2];
    const double bh = drawn[3];

    if (isHorizontalLocation(loc)) {
        double scaleX = pw > 0.0 ? bw / pw : 1.0;
        result[0] = bx + (result[0] - px) * scaleX;
        result[2] *= scaleX;
        bool north
            = loc == GO_PROPERTY_VALUE_NORTH_STR || loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR;
        result[1] += north ? (by + bh) - (py + ph) : by - py;
    } else {
        double scaleY = ph > 0.0 ? bh / ph : 1.0;
        result[1] = by + (result[1] - py) * scaleY;
        result[3] *= scaleY;
        result[0] += isEastLocation(loc) ? (bx + bw) - (px + pw) : bx - px;
    }
    return result;
}
//=============================================================================
// An inside colorbar on the default axes: a 16pt bar, 8pt in from the axes edges.
// Each entry is a normalized anchor plus a pixel offset converted with the figure size.
static const double COLORBAR_AXES_LEFT = 0.13;
static const double COLORBAR_AXES_BOTTOM = 0.11;
static const double COLORBAR_AXES_WIDTH = 0.775;
static const double COLORBAR_AXES_HEIGHT = 0.815;
static const double COLORBAR_THICKNESS_PX = 64.0 / 3.0; // 16 pt at 96 dpi
static const double COLORBAR_INSET_PX = 32.0 / 3.0; // 8 pt at 96 dpi
//=============================================================================
static std::vector<double>
composeColorBarRect(const std::array<double, 4>& base, const std::array<double, 4>& pixelOffset,
    double figureWidth, double figureHeight)
{
    const double dimension[4] = { figureWidth, figureHeight, figureWidth, figureHeight };
    std::vector<double> result(4);
    for (size_t k = 0; k < 4; ++k) {
        double dim = dimension[k] > 0.0 ? dimension[k] : 1.0;
        result[k] = base[k] + pixelOffset[k] / dim;
    }
    return result;
}
//=============================================================================
std::vector<double>
defaultColorBarPosition(const std::vector<double>& base, const std::wstring& loc,
    double figureWidth, double figureHeight)
{
    if (base.size() < 4) {
        return defaultColorBarPosition(
            { COLORBAR_AXES_LEFT, COLORBAR_AXES_BOTTOM, COLORBAR_AXES_WIDTH, COLORBAR_AXES_HEIGHT },
            loc, figureWidth, figureHeight);
    }
    const double L = base[0];
    const double B = base[1];
    const double W = base[2];
    const double H = base[3];
    const double t = COLORBAR_THICKNESS_PX;
    const double i = COLORBAR_INSET_PX;
    if (loc == GO_PROPERTY_VALUE_WEST_STR) {
        return composeColorBarRect({ L, B, 0.0, H }, { i, i, t, -t }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_NORTH_STR) {
        return composeColorBarRect(
            { L, B + H, W, 0.0 }, { i, -(t + i), -t, t }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_SOUTH_STR) {
        return composeColorBarRect({ L, B, W, 0.0 }, { i, i, -t, t }, figureWidth, figureHeight);
    }
    return composeColorBarRect(
        { L + W, B, 0.0, H }, { -(t + i), i, t, -t }, figureWidth, figureHeight);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
