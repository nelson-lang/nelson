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
isColorBarPeerContour(GraphicsObject* peer)
{
    if (!peer) {
        return false;
    }
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        peer->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 handle : children->data()) {
        if (handle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(handle)) {
            GraphicsObject* child = findGraphicsObject(handle, false);
            if (child && child->isType(GO_PROPERTY_VALUE_CONTOUR_STR)) {
                return true;
            }
            if (child && child->isType(GO_PROPERTY_VALUE_PATCH_STR)
                && child->findStringProperty(GO_TAG_PROPERTY_NAME_STR) == L"contourslice") {
                return true;
            }
        }
    }
    return false;
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
static double
constrainedPlotBoxAspect(GOAxis* axis)
{
    // Effective drawn plot-box aspect (width : height, in pixels) when the peer
    // axes locks its aspect ratio. Returns 0.0 when the axes is unconstrained.
    if (!axis->isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)) {
        std::vector<double> pba
            = axis->findVectorDoubleProperty(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR);
        if (pba.size() >= 2 && pba[0] > 0.0 && pba[1] > 0.0) {
            return pba[0] / pba[1];
        }
    }
    if (!axis->isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)) {
        std::vector<double> xlim = axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        std::vector<double> ylim = axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        std::vector<double> dar
            = axis->findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR);
        if (xlim.size() >= 2 && ylim.size() >= 2 && dar.size() >= 2) {
            double dx = std::fabs(xlim[1] - xlim[0]);
            double dy = std::fabs(ylim[1] - ylim[0]);
            if (dx > 0.0 && dy > 0.0 && dar[0] > 0.0 && dar[1] > 0.0) {
                return (dx * dar[1]) / (dy * dar[0]);
            }
        }
    }
    return 0.0;
}
//=============================================================================
std::vector<double>
constrainedColorBarPosition(GraphicsObject* peer, const std::wstring& loc,
    const std::vector<double>& colorBarPosition, const std::vector<double>& peerBox,
    double figureWidth, double figureHeight)
{
    std::vector<double> result = colorBarPosition;
    GOAxis* axis = dynamic_cast<GOAxis*>(peer);
    if (!axis || !axis->is2D() || result.size() < 4 || peerBox.size() < 4 || figureWidth <= 0.0
        || figureHeight <= 0.0 || peerBox[2] <= 0.0 || peerBox[3] <= 0.0) {
        return result;
    }
    double target = constrainedPlotBoxAspect(axis);
    if (!(target > 0.0) || !std::isfinite(target)) {
        return result;
    }
    double pxW = peerBox[2] * figureWidth;
    double pxH = peerBox[3] * figureHeight;
    double boxW = pxW;
    double boxH = pxH;
    if (pxW / pxH >= target) {
        boxW = pxH * target;
    } else {
        boxH = pxW / target;
    }
    const double px = peerBox[0];
    const double py = peerBox[1];
    const double pw = peerBox[2];
    const double ph = peerBox[3];
    const double bw = boxW / figureWidth;
    const double bh = boxH / figureHeight;
    const double bx = px + (pw - bw) * 0.5;
    const double by = py + (ph - bh) * 0.5;

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
// The default peer-axes margins are fixed in NORMALIZED units (0.13, 0.11, 0.775, 0.815).
// The colorbar thickness, its gap to the peer axes and the peer axes shrink are fixed in
// PIXELS. Each geometry entry is a normalized anchor plus a pixel offset converted with the
// figure dimensions, so the layout holds at any figure size.
// Peer kind: 0 = 2D axes, 1 = filled contour, 2 = 3D axes.
static const double COLORBAR_AXES_LEFT = 0.13;
static const double COLORBAR_AXES_BOTTOM = 0.11;
static const double COLORBAR_AXES_WIDTH = 0.775;
static const double COLORBAR_AXES_HEIGHT = 0.815;
static const double COLORBAR_AXES_RIGHT = COLORBAR_AXES_LEFT + COLORBAR_AXES_WIDTH; // 0.905
static const double COLORBAR_AXES_TOP = COLORBAR_AXES_BOTTOM + COLORBAR_AXES_HEIGHT; // 0.925
static const double COLORBAR_THICKNESS_PX = 64.0 / 3.0; // 16 pt at 96 dpi
static const double COLORBAR_INSET_PX = 32.0 / 3.0; // 8 pt at 96 dpi
//=============================================================================
static int
colorBarPeerKind(bool peer3D, bool peerContour)
{
    return peerContour ? 1 : (peer3D ? 2 : 0);
}
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
defaultColorBarPosition(
    const std::wstring& loc, bool peer3D, bool peerContour, double figureWidth, double figureHeight)
{
    return defaultColorBarPosition(
        { COLORBAR_AXES_LEFT, COLORBAR_AXES_BOTTOM, COLORBAR_AXES_WIDTH, COLORBAR_AXES_HEIGHT },
        loc, peer3D, peerContour, figureWidth, figureHeight);
}
//=============================================================================
std::vector<double>
defaultColorBarPosition(const std::vector<double>& base, const std::wstring& loc, bool peer3D,
    bool peerContour, double figureWidth, double figureHeight)
{
    const int kind = colorBarPeerKind(peer3D, peerContour);
    if (base.size() < 4) {
        return defaultColorBarPosition(loc, peer3D, peerContour, figureWidth, figureHeight);
    }
    const double L = base[0];
    const double B = base[1];
    const double W = base[2];
    const double H = base[3];
    const double R = L + W;
    const double Tp = B + H;
    const double t = COLORBAR_THICKNESS_PX;
    const double i = COLORBAR_INSET_PX;
    static const double eastOutside[3] = { -42.33, -41.83, -39.89 };
    static const double westOutside[3] = { 16.67, 16.67, 15.57 };
    static const double northOutside[3] = { -43.33, -42.83, -41.60 };
    static const double southOutside[3] = { 17.67, 17.67, 17.28 };

    if (loc == GO_PROPERTY_VALUE_EAST_STR) {
        return composeColorBarRect(
            { R, B, 0.0, H }, { -(t + i), i, t, -t }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_WEST_STR) {
        return composeColorBarRect({ L, B, 0.0, H }, { i, i, t, -t }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_NORTH_STR) {
        return composeColorBarRect(
            { L, Tp, W, 0.0 }, { i, -(t + i), -t, t }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_SOUTH_STR) {
        return composeColorBarRect({ L, B, W, 0.0 }, { i, i, -t, t }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR) {
        return composeColorBarRect(
            { L, B, 0.0, H }, { westOutside[kind], 0.0, t, 0.0 }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR) {
        return composeColorBarRect(
            { L, Tp, W, 0.0 }, { 0.0, northOutside[kind], 0.0, t }, figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR) {
        return composeColorBarRect(
            { L, B, W, 0.0 }, { 0.0, southOutside[kind], 0.0, t }, figureWidth, figureHeight);
    }
    return composeColorBarRect(
        { R, B, 0.0, H }, { eastOutside[kind], 0.0, t, 0.0 }, figureWidth, figureHeight);
}
//=============================================================================
std::vector<double>
defaultColorBarPeerPositionForOutside(
    const std::wstring& loc, bool peer3D, bool peerContour, double figureWidth, double figureHeight)
{
    return defaultColorBarPeerPositionForOutside(
        { COLORBAR_AXES_LEFT, COLORBAR_AXES_BOTTOM, COLORBAR_AXES_WIDTH, COLORBAR_AXES_HEIGHT },
        loc, peer3D, peerContour, figureWidth, figureHeight);
}
//=============================================================================
std::vector<double>
defaultColorBarPeerPositionForOutside(const std::vector<double>& base, const std::wstring& loc,
    bool peer3D, bool peerContour, double figureWidth, double figureHeight)
{
    const int kind = colorBarPeerKind(peer3D, peerContour);
    if (base.size() < 4) {
        return defaultColorBarPeerPositionForOutside(
            loc, peer3D, peerContour, figureWidth, figureHeight);
    }
    static const double eastShiftX[3] = { -7.63, -8.60, -11.09 };
    static const double eastShrinkW[3] = { -45.47, -51.28, -66.13 };
    static const double westShiftX[3] = { 68.73, 68.73, 74.24 };
    static const double westShrinkW[3] = { -61.22, -61.22, -66.13 };
    static const double northShiftY[3] = { -6.45, -7.28, -9.39 };
    static const double northShrinkH[3] = { -47.81, -53.93, -69.55 };
    static const double southShiftY[3] = { 70.31, 70.31, 75.95 };
    static const double southShrinkH[3] = { -64.38, -64.38, -69.55 };
    const std::array<double, 4> baseRect = { base[0], base[1], base[2], base[3] };

    if (loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR) {
        return composeColorBarRect(baseRect, { 0.0, northShiftY[kind], 0.0, northShrinkH[kind] },
            figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR) {
        return composeColorBarRect(baseRect, { 0.0, southShiftY[kind], 0.0, southShrinkH[kind] },
            figureWidth, figureHeight);
    }
    if (loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR) {
        return composeColorBarRect(
            baseRect, { westShiftX[kind], 0.0, westShrinkW[kind], 0.0 }, figureWidth, figureHeight);
    }
    return composeColorBarRect(
        baseRect, { eastShiftX[kind], 0.0, eastShrinkW[kind], 0.0 }, figureWidth, figureHeight);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
