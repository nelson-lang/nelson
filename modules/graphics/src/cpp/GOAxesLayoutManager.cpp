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
#include <map>
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
// A 3-D peer keeps a fixed inset instead of its tight inset.
static const double INSET_3D_POINTS = 20.0;
// An outside colorbar: a 16pt bar (at most a tenth of the axes) and 20pt of tick labels.
static const double COLORBAR_THICKNESS_POINTS = 16.0;
static const double COLORBAR_LABELS_POINTS = 20.0;
//=============================================================================
namespace {
    // Peer geometry before the first slot was taken, in the peer's own Units.
    struct AxesLayoutBase
    {
        std::vector<double> position;
        std::vector<double> outerPosition;
        bool outerDriven = false;
    };
    // Room one decoration takes on one side of the peer.
    struct DecorationSlot
    {
        GraphicsObject* decoration = nullptr;
        AxesLayoutSide side = AxesLayoutSide::None;
        double pixels = 0.0;
        bool colorBar = false;
    };
    // Decorations of one peer in arrival order, all reserved from one base geometry.
    struct DecorationStack
    {
        AxesLayoutBase base;
        std::vector<DecorationSlot> slots;
    };
} // namespace
//=============================================================================
static GOAxis*
layoutAxis(GraphicsObject* peer);
static bool
parentSizeInPixels(GOAxis* axis, double& width, double& height);
static bool
sameBox(const std::vector<double>& a, const std::vector<double>& b);
static std::vector<double>
shrinkBox(std::vector<double> box, AxesLayoutSide side, double slot);
static std::vector<double>
pixelsToUnits(
    const std::wstring& units, const std::vector<double>& box, double width, double height);
static std::vector<double>
normalizedBox(const std::vector<double>& box, double width, double height);
static bool
peerBeingDeleted(GraphicsObject* peer);
static double
lockedPlotBoxAspect(GOAxis* axis);
static double
pointsToPixels(double points);
static std::map<GraphicsObject*, DecorationStack>&
decorationStacks();
static bool
captureLayoutBase(GOAxis* axis, AxesLayoutBase& base);
static bool
registerDecorationSlot(GOAxis* axis, const DecorationSlot& slot);
static std::vector<double>
applyDecorationStack(GOAxis* axis, double width, double height, bool& written);
static std::vector<double>
reserveDecorationSlot(GraphicsObject* peer, const DecorationSlot& slot);
static double
sideInsetPixels(GraphicsObject* peer, AxesLayoutSide side);
static double
colorBarThicknessPixels(double axesExtentPixels);
static double
slotOffsetPixels(
    GraphicsObject* peer, GraphicsObject* decoration, AxesLayoutSide side, double axesExtentPixels);
static void
relayoutOtherDecorations(GraphicsObject* peer, GraphicsObject* decoration);
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
    return pointsToPixels(LAYOUT_GAP_POINTS);
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
bool
axesLayoutBaseOuterPosition(GraphicsObject* peer, std::vector<double>& outerPosition)
{
    auto& stacks = decorationStacks();
    auto it = stacks.find(peer);
    if (it == stacks.end() || !it->second.base.outerDriven
        || it->second.base.outerPosition.size() < 4) {
        return false;
    }
    outerPosition = it->second.base.outerPosition;
    return true;
}
//=============================================================================
void
releaseAxesLayoutSlot(GraphicsObject* peer, GraphicsObject* decoration)
{
    auto& stacks = decorationStacks();
    auto it = stacks.find(peer);
    if (it == stacks.end()) {
        return;
    }
    std::vector<DecorationSlot>& slots = it->second.slots;
    auto found = std::find_if(slots.begin(), slots.end(),
        [decoration](const DecorationSlot& slot) { return slot.decoration == decoration; });
    if (found == slots.end()) {
        return;
    }
    slots.erase(found);
    // A peer being deleted takes its decorations with it: nothing to give back.
    if (peerBeingDeleted(peer)) {
        stacks.erase(it);
        return;
    }
    GOAxis* axis = layoutAxis(peer);
    if (slots.empty()) {
        AxesLayoutBase base = it->second.base;
        stacks.erase(it);
        if (axis && base.outerDriven) {
            axis->setLayoutOuterPosition(base.outerPosition);
            axis->setLayoutPosition(base.position);
        } else {
            peer->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, base.position[0],
                base.position[1], base.position[2], base.position[3]);
        }
        requestPeerAxesUpdate(peer);
        return;
    }
    double width = 0;
    double height = 0;
    if (axis && parentSizeInPixels(axis, width, height)) {
        bool written = false;
        applyDecorationStack(axis, width, height, written);
    }
    relayoutOtherDecorations(peer, decoration);
}
//=============================================================================
void
forgetAxesLayoutDecoration(GraphicsObject* decoration)
{
    auto& stacks = decorationStacks();
    for (auto it = stacks.begin(); it != stacks.end();) {
        std::vector<DecorationSlot>& slots = it->second.slots;
        slots.erase(
            std::remove_if(slots.begin(), slots.end(),
                [decoration](const DecorationSlot& slot) { return slot.decoration == decoration; }),
            slots.end());
        it = slots.empty() ? stacks.erase(it) : std::next(it);
    }
}
//=============================================================================
std::vector<double>
layoutDecorationAroundAxes(GraphicsObject* peer, GraphicsObject* decoration,
    const std::wstring& location, double pixelW, double pixelH, double figW, double figH)
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
        DecorationSlot slot;
        slot.decoration = decoration;
        slot.side = side;
        slot.pixels = (horizontal ? pixelW : pixelH) + axesLayoutGapPixels();
        base = reserveDecorationSlot(peer, slot);
        if (base.size() < 4) {
            return {};
        }
        // Decorations that came first on this side sit between the box and this one.
        double extent = horizontal ? base[2] * figW : base[3] * figH;
        double gapPixels = sideInsetPixels(peer, side) + axesLayoutGapPixels()
            + slotOffsetPixels(peer, decoration, side, extent);
        gapX = gapPixels / figW;
        gapY = gapPixels / figH;
    } else {
        releaseAxesLayoutSlot(peer, decoration);
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
std::vector<double>
layoutColorBarAroundAxes(GraphicsObject* peer, GraphicsObject* decoration,
    const std::wstring& location, double figW, double figH)
{
    AxesLayoutSide side = axesLayoutSideFromLocation(location);
    if (!peer || side == AxesLayoutSide::None || figW <= 0 || figH <= 0) {
        return {};
    }
    const bool horizontal = side == AxesLayoutSide::East || side == AxesLayoutSide::West;
    const double gap = axesLayoutGapPixels();
    DecorationSlot slot;
    slot.decoration = decoration;
    slot.side = side;
    slot.pixels = pointsToPixels(COLORBAR_THICKNESS_POINTS + COLORBAR_LABELS_POINTS) + gap;
    slot.colorBar = true;
    std::vector<double> box = reserveDecorationSlot(peer, slot);
    if (box.size() < 4) {
        return {};
    }
    const double extent = horizontal ? box[2] * figW : box[3] * figH;
    const double thickness = colorBarThicknessPixels(extent);
    const double distance
        = sideInsetPixels(peer, side) + gap + slotOffsetPixels(peer, decoration, side, extent);
    // The bar spans the box the peer really draws.
    std::vector<double> drawn = axesDrawnBox(peer, box, figW, figH);
    switch (side) {
    case AxesLayoutSide::East:
        return { drawn[0] + drawn[2] + distance / figW, drawn[1], thickness / figW, drawn[3] };
    case AxesLayoutSide::West:
        return { drawn[0] - (distance + thickness) / figW, drawn[1], thickness / figW, drawn[3] };
    case AxesLayoutSide::North:
        return { drawn[0], drawn[1] + drawn[3] + distance / figH, drawn[2], thickness / figH };
    default:
        return { drawn[0], drawn[1] - (distance + thickness) / figH, drawn[2], thickness / figH };
    }
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
    if (!peer) {
        return true;
    }
    auto* deleting = static_cast<GOOnOffProperty*>(
        peer->findProperty(GO_BEING_DELETED_PROPERTY_NAME_STR, false));
    return deleting && deleting->asBool();
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
static double
pointsToPixels(double points)
{
    return points * 96.0 / 72.0;
}
//=============================================================================
static std::map<GraphicsObject*, DecorationStack>&
decorationStacks()
{
    static std::map<GraphicsObject*, DecorationStack> stacks;
    return stacks;
}
//=============================================================================
static bool
captureLayoutBase(GOAxis* axis, AxesLayoutBase& base)
{
    base.position = axis->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    base.outerPosition = axis->findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (base.position.size() < 4) {
        return false;
    }
    // An auto layout gives the slot from its outer box and re-derives Position;
    // a manual Position gives it from Position itself, keeping the far edge.
    base.outerDriven = base.outerPosition.size() == 4
        && axis->stringCheck(
            GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR)
        && !axis->usesUiAxesLayout();
    // A Position that no longer follows its outer box (subplot, a title that
    // took room) is the box to keep: the outer one would widen the axes.
    if (base.outerDriven) {
        std::vector<double> fromOuter
            = axis->positionFromOuterPixels(axis->layoutUnitsToPixels(base.outerPosition));
        std::vector<double> current = axis->layoutUnitsToPixels(base.position);
        for (size_t k = 0; k < 4 && base.outerDriven; ++k) {
            if (fromOuter.size() < 4 || std::abs(fromOuter[k] - current[k]) > 0.5) {
                base.outerDriven = false;
            }
        }
    }
    return true;
}
//=============================================================================
// Adds or updates the slot of a decoration; a side change moves it to the end of the
// stack. Returns true when the stack changed.
static bool
registerDecorationSlot(GOAxis* axis, const DecorationSlot& slot)
{
    auto& stacks = decorationStacks();
    auto it = stacks.find(axis);
    if (it == stacks.end()) {
        DecorationStack stack;
        if (!captureLayoutBase(axis, stack.base)) {
            return false;
        }
        it = stacks.emplace(axis, stack).first;
    }
    std::vector<DecorationSlot>& slots = it->second.slots;
    auto found = std::find_if(slots.begin(), slots.end(),
        [&slot](const DecorationSlot& entry) { return entry.decoration == slot.decoration; });
    if (found != slots.end() && found->side == slot.side) {
        bool changed
            = std::abs(found->pixels - slot.pixels) > 1e-9 || found->colorBar != slot.colorBar;
        *found = slot;
        return changed;
    }
    if (found != slots.end()) {
        slots.erase(found);
    }
    slots.push_back(slot);
    return true;
}
//=============================================================================
// Shrinks the base geometry by every used side (inset counted once, then each slot)
// and writes it when it differs. Returns the peer Position in pixels.
static std::vector<double>
applyDecorationStack(GOAxis* axis, double width, double height, bool& written)
{
    written = false;
    const DecorationStack& stack = decorationStacks()[axis];
    const AxesLayoutSide sides[4] = { AxesLayoutSide::East, AxesLayoutSide::West,
        AxesLayoutSide::North, AxesLayoutSide::South };
    double take[4] = { 0.0, 0.0, 0.0, 0.0 };
    bool used[4] = { false, false, false, false };
    for (const DecorationSlot& slot : stack.slots) {
        for (size_t k = 0; k < 4; ++k) {
            if (slot.side == sides[k]) {
                take[k] += slot.pixels;
                used[k] = true;
            }
        }
    }
    for (size_t k = 0; k < 4; ++k) {
        if (used[k]) {
            take[k] += sideInsetPixels(axis, sides[k]);
        }
    }
    std::wstring units = axis->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    std::vector<double> box;
    if (stack.base.outerDriven) {
        std::vector<double> outer = axis->layoutUnitsToPixels(stack.base.outerPosition);
        for (size_t k = 0; k < 4; ++k) {
            if (used[k]) {
                outer = shrinkBox(outer, sides[k], take[k]);
            }
        }
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
        box = axis->layoutUnitsToPixels(stack.base.position);
        for (size_t k = 0; k < 4; ++k) {
            if (used[k]) {
                box = shrinkBox(box, sides[k], take[k]);
            }
        }
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
    return box;
}
//=============================================================================
// Keeps the slot of a decoration and returns the peer Position (parent fractions);
// the other decorations follow when the stack or the peer changed.
static std::vector<double>
reserveDecorationSlot(GraphicsObject* peer, const DecorationSlot& slot)
{
    GOAxis* axis = layoutAxis(peer);
    double width = 0;
    double height = 0;
    if (!axis || slot.side == AxesLayoutSide::None || !parentSizeInPixels(axis, width, height)) {
        return axesPositionNormalized(peer);
    }
    bool changed = registerDecorationSlot(axis, slot);
    if (decorationStacks().count(axis) == 0) {
        return axesPositionNormalized(peer);
    }
    bool written = false;
    std::vector<double> box = applyDecorationStack(axis, width, height, written);
    if (changed || written) {
        relayoutOtherDecorations(peer, slot.decoration);
    }
    return normalizedBox(box, width, height);
}
//=============================================================================
static double
sideInsetPixels(GraphicsObject* peer, AxesLayoutSide side)
{
    GOAxis* axis = layoutAxis(peer);
    if (axis && !axis->is2D()) {
        return pointsToPixels(INSET_3D_POINTS);
    }
    return axesTightInsetPixels(peer, side);
}
//=============================================================================
static double
colorBarThicknessPixels(double axesExtentPixels)
{
    return std::max(
        1.0, std::min(pointsToPixels(COLORBAR_THICKNESS_POINTS), 0.1 * axesExtentPixels));
}
//=============================================================================
// Room taken by the decorations stacked before this one on the same side.
static double
slotOffsetPixels(
    GraphicsObject* peer, GraphicsObject* decoration, AxesLayoutSide side, double axesExtentPixels)
{
    auto& stacks = decorationStacks();
    auto it = stacks.find(peer);
    if (it == stacks.end()) {
        return 0.0;
    }
    double offset = 0.0;
    for (const DecorationSlot& slot : it->second.slots) {
        if (slot.decoration == decoration) {
            break;
        }
        if (slot.side != side) {
            continue;
        }
        offset += slot.colorBar ? colorBarThicknessPixels(axesExtentPixels)
                + pointsToPixels(COLORBAR_LABELS_POINTS) + axesLayoutGapPixels()
                                : slot.pixels;
    }
    return offset;
}
//=============================================================================
// Moves the other decorations of the peer after its box or their stack changed.
static void
relayoutOtherDecorations(GraphicsObject* peer, GraphicsObject* decoration)
{
    static bool relayoutRunning = false;
    if (relayoutRunning) {
        return;
    }
    auto& stacks = decorationStacks();
    auto it = stacks.find(peer);
    if (it == stacks.end()) {
        return;
    }
    std::vector<GraphicsObject*> others;
    for (const DecorationSlot& slot : it->second.slots) {
        if (slot.decoration && slot.decoration != decoration) {
            others.push_back(slot.decoration);
        }
    }
    relayoutRunning = true;
    for (GraphicsObject* other : others) {
        other->updateState();
    }
    relayoutRunning = false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
