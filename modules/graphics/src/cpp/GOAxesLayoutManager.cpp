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
#include "GOAxesLayoutManager.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOUIComponentUnits.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const double LAYOUT_GAP_POINTS = 8.0;
//=============================================================================
static GOAxis*
layoutAxis(GraphicsObject* peer)
{
    return dynamic_cast<GOAxis*>(peer);
}
//=============================================================================
static bool
parentSizeInPixels(GOAxis* axis, double& width, double& height)
{
    width = 0;
    height = 0;
    return graphicsParentSizeInPixels(axis, axis->getParentFigure(), width, height) && width > 0
        && height > 0;
}
//=============================================================================
static bool
sameBox(const std::vector<double>& a, const std::vector<double>& b)
{
    if (a.size() < 4 || b.size() < 4) {
        return false;
    }
    for (size_t k = 0; k < 4; ++k) {
        if (std::abs(a[k] - b[k]) > 1e-9 * std::max(1.0, std::abs(b[k]))) {
            return false;
        }
    }
    return true;
}
//=============================================================================
// Takes slot pixels off one side of a pixel box, keeping at least one pixel.
static std::vector<double>
shrinkBox(std::vector<double> box, AxesLayoutSide side, double slot)
{
    if (box.size() < 4) {
        return box;
    }
    switch (side) {
    case AxesLayoutSide::East:
        slot = std::min(slot, box[2] - 1.0);
        box[2] -= slot;
        break;
    case AxesLayoutSide::West:
        slot = std::min(slot, box[2] - 1.0);
        box[0] += slot;
        box[2] -= slot;
        break;
    case AxesLayoutSide::North:
        slot = std::min(slot, box[3] - 1.0);
        box[3] -= slot;
        break;
    case AxesLayoutSide::South:
        slot = std::min(slot, box[3] - 1.0);
        box[1] += slot;
        box[3] -= slot;
        break;
    default:
        break;
    }
    return box;
}
//=============================================================================
static std::vector<double>
pixelsToUnits(
    const std::wstring& units, const std::vector<double>& box, double width, double height)
{
    return convertFromPixelsPixelOrigin(units, box[0], box[1], box[2], box[3], width, height);
}
//=============================================================================
static std::vector<double>
normalizedBox(const std::vector<double>& box, double width, double height)
{
    if (box.size() < 4) {
        return {};
    }
    return { box[0] / width, box[1] / height, box[2] / width, box[3] / height };
}
//=============================================================================
static bool
peerBeingDeleted(GraphicsObject* peer)
{
    auto* deleting = static_cast<GOOnOffProperty*>(
        peer->findProperty(GO_BEING_DELETED_PROPERTY_NAME_STR, false));
    return deleting && deleting->asBool();
}
//=============================================================================
AxesLayoutSide
axesLayoutSideFromLocation(const std::wstring& location)
{
    if (location.find(L"outside") == std::wstring::npos) {
        return AxesLayoutSide::None;
    }
    if (location.find(L"east") != std::wstring::npos || location == L"bestoutside") {
        return AxesLayoutSide::East;
    }
    if (location.find(L"west") != std::wstring::npos) {
        return AxesLayoutSide::West;
    }
    if (location.find(L"north") != std::wstring::npos) {
        return AxesLayoutSide::North;
    }
    if (location.find(L"south") != std::wstring::npos) {
        return AxesLayoutSide::South;
    }
    return AxesLayoutSide::None;
}
//=============================================================================
double
axesLayoutGapPixels()
{
    return LAYOUT_GAP_POINTS * 96.0 / 72.0;
}
//=============================================================================
double
axesTightInsetPixels(GraphicsObject* peer, AxesLayoutSide side)
{
    GOAxis* axis = layoutAxis(peer);
    if (!axis) {
        return 0.0;
    }
    std::vector<double> inset = axis->tightInsetPixels();
    if (inset.size() < 4) {
        return 0.0;
    }
    switch (side) {
    case AxesLayoutSide::West:
        return inset[0];
    case AxesLayoutSide::South:
        return inset[1];
    case AxesLayoutSide::East:
        return inset[2];
    case AxesLayoutSide::North:
        return inset[3];
    default:
        return 0.0;
    }
}
//=============================================================================
std::vector<double>
axesPositionNormalized(GraphicsObject* peer)
{
    if (!peer) {
        return {};
    }
    GOAxis* axis = layoutAxis(peer);
    double width = 0;
    double height = 0;
    if (!axis || !parentSizeInPixels(axis, width, height)) {
        return peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    }
    return normalizedBox(
        axis->layoutUnitsToPixels(axis->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR)),
        width, height);
}
//=============================================================================
// Drawn box aspect (w:h px) of a peer with a manual aspect or view angle, 0 otherwise:
// the projection fits the data box scaled by DataAspectRatio (PBAR only shapes the limits).
static double
lockedPlotBoxAspect(GOAxis* axis)
{
    bool locked = !axis->isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)
        || !axis->isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)
        || !axis->isAuto(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR);
    if (!locked) {
        return 0.0;
    }
    std::vector<double> xlim = axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
    std::vector<double> ylim = axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
    std::vector<double> dar
        = axis->findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR);
    if (dar.size() < 2 || !(dar[0] > 0.0) || !(dar[1] > 0.0) || !std::isfinite(dar[0])
        || !std::isfinite(dar[1])) {
        dar = { 1.0, 1.0 };
    }
    if (xlim.size() >= 2 && ylim.size() >= 2) {
        double dx = std::abs(xlim[1] - xlim[0]);
        double dy = std::abs(ylim[1] - ylim[0]);
        if (dx > 0.0 && dy > 0.0 && std::isfinite(dx) && std::isfinite(dy)) {
            return (dx / dar[0]) / (dy / dar[1]);
        }
    }
    return 0.0;
}
//=============================================================================
std::vector<double>
axesDrawnBox(GraphicsObject* peer, const std::vector<double>& box, double width, double height)
{
    GOAxis* axis = layoutAxis(peer);
    if (!axis || !axis->is2D() || box.size() < 4 || width <= 0.0 || height <= 0.0 || box[2] <= 0.0
        || box[3] <= 0.0) {
        return box;
    }
    double target = lockedPlotBoxAspect(axis);
    if (!(target > 0.0) || !std::isfinite(target)) {
        return box;
    }
    double boxW = box[2] * width;
    double boxH = box[3] * height;
    if (boxW / boxH >= target) {
        boxW = boxH * target;
    } else {
        boxH = boxW / target;
    }
    const double w = boxW / width;
    const double h = boxH / height;
    return { box[0] + (box[2] - w) * 0.5, box[1] + (box[3] - h) * 0.5, w, h };
}
//=============================================================================
std::vector<double>
reserveAxesLayoutSlot(GraphicsObject* peer, AxesLayoutSide side, double slotPixels,
    AxesLayoutReservation& reservation)
{
    GOAxis* axis = layoutAxis(peer);
    double width = 0;
    double height = 0;
    if (!axis || side == AxesLayoutSide::None || !parentSizeInPixels(axis, width, height)) {
        return axesPositionNormalized(peer);
    }
    if (!reservation.active()) {
        reservation.basePosition = axis->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        reservation.baseOuterPosition
            = axis->findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
        if (reservation.basePosition.size() < 4) {
            reservation = AxesLayoutReservation {};
            return axesPositionNormalized(peer);
        }
        // An auto layout gives the slot from its outer box and re-derives Position;
        // a manual Position gives it from Position itself, keeping the far edge.
        reservation.outerDriven = reservation.baseOuterPosition.size() == 4
            && axis->stringCheck(
                GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR)
            && !axis->usesUiAxesLayout();
        // A Position that no longer follows its outer box (subplot, a title that
        // took room) is the box to keep: the outer one would widen the axes.
        if (reservation.outerDriven) {
            std::vector<double> fromOuter = axis->positionFromOuterPixels(
                axis->layoutUnitsToPixels(reservation.baseOuterPosition));
            std::vector<double> current = axis->layoutUnitsToPixels(reservation.basePosition);
            for (size_t k = 0; k < 4 && reservation.outerDriven; ++k) {
                if (fromOuter.size() < 4 || std::abs(fromOuter[k] - current[k]) > 0.5) {
                    reservation.outerDriven = false;
                }
            }
        }
    }
    std::wstring units = axis->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    bool written = false;
    std::vector<double> box;
    if (reservation.outerDriven) {
        std::vector<double> outer
            = shrinkBox(axis->layoutUnitsToPixels(reservation.baseOuterPosition), side, slotPixels);
        std::vector<double> outerUnits = pixelsToUnits(units, outer, width, height);
        if (sameBox(
                outerUnits, axis->findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR))) {
            box = axis->layoutUnitsToPixels(
                axis->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        } else {
            box = axis->positionFromOuterPixels(outer);
            axis->setLayoutOuterPosition(outerUnits);
            axis->setLayoutPosition(pixelsToUnits(units, box, width, height));
            written = true;
        }
    } else {
        box = shrinkBox(axis->layoutUnitsToPixels(reservation.basePosition), side, slotPixels);
        std::vector<double> boxUnits = pixelsToUnits(units, box, width, height);
        if (!sameBox(boxUnits, axis->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR))) {
            axis->setFourVectorDefault(
                GO_POSITION_PROPERTY_NAME_STR, boxUnits[0], boxUnits[1], boxUnits[2], boxUnits[3]);
            written = true;
        }
    }
    if (written) {
        requestPeerAxesUpdate(axis);
    }
    return normalizedBox(box, width, height);
}
//=============================================================================
void
releaseAxesLayoutSlot(GraphicsObject* peer, AxesLayoutReservation& reservation)
{
    if (!reservation.active()) {
        return;
    }
    AxesLayoutReservation saved = reservation;
    reservation = AxesLayoutReservation {};
    if (!peer || peerBeingDeleted(peer)) {
        return;
    }
    GOAxis* axis = layoutAxis(peer);
    const std::vector<double>& position = saved.basePosition;
    if (axis && saved.outerDriven) {
        axis->setLayoutOuterPosition(saved.baseOuterPosition);
        axis->setLayoutPosition(position);
    } else {
        peer->setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
    }
    requestPeerAxesUpdate(peer);
}
//=============================================================================
std::vector<double>
layoutDecorationAroundAxes(GraphicsObject* peer, const std::wstring& location, double pixelW,
    double pixelH, double figW, double figH, AxesLayoutReservation& reservation)
{
    if (figW <= 0 || figH <= 0) {
        return {};
    }
    std::wstring loc = location;
    if (loc == L"best") {
        loc = L"northeast";
    } else if (loc == L"bestoutside") {
        loc = L"northeastoutside";
    } else if (loc == L"layout") {
        loc = GO_PROPERTY_VALUE_EASTOUTSIDE_STR;
    }
    const double w = pixelW / figW;
    const double h = pixelH / figH;
    // inside: 8 points in from the corner or edge
    double gapX = axesLayoutGapPixels() / figW;
    double gapY = axesLayoutGapPixels() / figH;
    std::vector<double> base = { 0.13, 0.11, 0.775, 0.815 };
    AxesLayoutSide side = axesLayoutSideFromLocation(loc);
    if (side != AxesLayoutSide::None && peer) {
        bool horizontal = side == AxesLayoutSide::East || side == AxesLayoutSide::West;
        double gapPixels = axesTightInsetPixels(peer, side) + axesLayoutGapPixels();
        base = reserveAxesLayoutSlot(
            peer, side, (horizontal ? pixelW : pixelH) + gapPixels, reservation);
        gapX = gapPixels / figW;
        gapY = gapPixels / figH;
    } else {
        releaseAxesLayoutSlot(peer, reservation);
        std::vector<double> peerPosition = axesPositionNormalized(peer);
        if (peerPosition.size() >= 4) {
            base = peerPosition;
        }
    }
    if (base.size() < 4) {
        return {};
    }
    // The decoration follows the box the peer really draws (a locked aspect
    // ratio centres a smaller box inside Position).
    base = axesDrawnBox(peer, base, figW, figH);
    // Inside: the gap in from the corner or edge. Outside: the gap away from the
    // box, aligned with its top, bottom or middle (east/west) or centered (north/south).
    double x = base[0] + base[2] - w - gapX;
    double y = base[1] + base[3] - h - gapY;
    const bool alignTop = loc.compare(0, 5, L"north") == 0;
    const bool alignBottom = loc.compare(0, 5, L"south") == 0;
    const double sideY = alignTop ? base[1] + base[3] - h
        : alignBottom             ? base[1]
                                  : base[1] + (base[3] - h) / 2.0;
    switch (side) {
    case AxesLayoutSide::East:
        x = base[0] + base[2] + gapX;
        y = sideY;
        break;
    case AxesLayoutSide::West:
        x = base[0] - gapX - w;
        y = sideY;
        break;
    case AxesLayoutSide::North:
        x = base[0] + (base[2] - w) / 2.0;
        y = base[1] + base[3] + gapY;
        break;
    case AxesLayoutSide::South:
        x = base[0] + (base[2] - w) / 2.0;
        y = base[1] - gapY - h;
        break;
    default:
        if (loc == GO_PROPERTY_VALUE_NORTH_STR) {
            x = base[0] + (base[2] - w) / 2.0;
        } else if (loc == GO_PROPERTY_VALUE_SOUTH_STR) {
            x = base[0] + (base[2] - w) / 2.0;
            y = base[1] + gapY;
        } else if (loc == GO_PROPERTY_VALUE_EAST_STR) {
            y = base[1] + (base[3] - h) / 2.0;
        } else if (loc == GO_PROPERTY_VALUE_WEST_STR) {
            x = base[0] + gapX;
            y = base[1] + (base[3] - h) / 2.0;
        } else if (loc == L"northwest") {
            x = base[0] + gapX;
        } else if (loc == L"southeast") {
            y = base[1] + gapY;
        } else if (loc == L"southwest") {
            x = base[0] + gapX;
            y = base[1] + gapY;
        }
        break;
    }
    return { x, y, w, h };
}
//=============================================================================
void
requestPeerAxesUpdate(GraphicsObject* peer)
{
    if (!peer) {
        return;
    }
    if (GOAxis* axis = layoutAxis(peer)) {
        axis->requestLayoutUpdate();
        return;
    }
    peer->updateState();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
