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
#include "GOColorBar.hpp"
#include "GOChartDecorators.hpp"
#include "GOFontSpecHelpers.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAxisHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorBarGeometry.hpp"
#include "GOColorProperty.hpp"
#include "GOFigure.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOInOutProperty.hpp"
#include "GOLayoutOptions.hpp"
#include "GONormalReverseProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOText.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOVectorProperty.hpp"
#include "StringHelpers.hpp"
#include "TexToUnicode.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const std::vector<std::wstring> COLORBAR_LOCATION_DICT
    = { GO_PROPERTY_VALUE_EASTOUTSIDE_STR, GO_PROPERTY_VALUE_WESTOUTSIDE_STR,
          GO_PROPERTY_VALUE_NORTHOUTSIDE_STR, GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR,
          GO_PROPERTY_VALUE_EAST_STR, GO_PROPERTY_VALUE_WEST_STR, GO_PROPERTY_VALUE_NORTH_STR,
          GO_PROPERTY_VALUE_SOUTH_STR, GO_PROPERTY_VALUE_MANUAL_STR, L"layout" };
//=============================================================================
static const std::vector<std::wstring> COLORBAR_TICK_DIRECTION_DICT = { GO_PROPERTY_VALUE_IN_STR,
    GO_PROPERTY_VALUE_OUT_STR, GO_PROPERTY_VALUE_BOTH_STR, GO_PROPERTY_VALUE_NONE_STR };
//=============================================================================
class GOColorBarLocationProperty : public GORestrictedStringProperty
{
public:
    GOColorBarLocationProperty() : GORestrictedStringProperty(COLORBAR_LOCATION_DICT) { }
    void
    set(ArrayOf arg) override
    {
        if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
            Error(L"Nelson:graphics:expectedStringProperty", L"Expecting a string for property.");
        }
        std::wstring value(arg.asWideString());
        StringHelpers::to_lower(value);
        GORestrictedStringProperty::set(ArrayOf::characterArray(value));
    }
};
//=============================================================================
class GOColorBarTickDirectionProperty : public GORestrictedStringProperty
{
public:
    GOColorBarTickDirectionProperty() : GORestrictedStringProperty(COLORBAR_TICK_DIRECTION_DICT) { }
    void
    set(ArrayOf arg) override
    {
        if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
            Error(L"Nelson:graphics:expectedStringProperty", L"Expecting a string for property.");
        }
        std::wstring value(arg.asWideString());
        StringHelpers::to_lower(value);
        GORestrictedStringProperty::set(ArrayOf::characterArray(value));
    }
};
//=============================================================================
static bool
isHorizontalLocation(const std::wstring& loc)
{
    return loc == GO_PROPERTY_VALUE_NORTH_STR || loc == GO_PROPERTY_VALUE_SOUTH_STR
        || loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR || loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR;
}
//=============================================================================
static bool
isSameColorBarPosition(const std::vector<double>& a, const std::vector<double>& b);
//=============================================================================
static bool
isOutsideLocation(const std::wstring& loc)
{
    return loc == GO_PROPERTY_VALUE_EASTOUTSIDE_STR || loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR
        || loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR || loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR;
}
//=============================================================================
static std::vector<double>
genericColorBarPosition(const std::vector<double>& base, const std::wstring& loc)
{
    double x = base[0];
    double y = base[1];
    double w = base[2];
    double h = base[3];
    double gap = 0.035;
    double inset = 0.015;
    double thickV = std::max(0.012, std::min(0.025, w * 0.0256));
    double thickH = std::max(0.018, std::min(0.035, h * 0.0359));

    if (loc == GO_PROPERTY_VALUE_NORTH_STR) {
        return { x + inset * w, y + h - thickH - inset * h, w * (1 - 2 * inset), thickH };
    }
    if (loc == GO_PROPERTY_VALUE_SOUTH_STR) {
        return { x + inset * w, y + inset * h, w * (1 - 2 * inset), thickH };
    }
    if (loc == GO_PROPERTY_VALUE_WEST_STR) {
        return { x + inset * w, y + inset * h, thickV, h * (1 - 2 * inset) };
    }
    if (loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR) {
        return { x, y + h + gap, w, thickH };
    }
    if (loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR) {
        return { x, std::max(0.0, y - gap - thickH), w, thickH };
    }
    if (loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR) {
        return { std::max(0.0, x - gap - thickV), y, thickV, h };
    }
    if (loc == GO_PROPERTY_VALUE_EAST_STR) {
        return { x + w - thickV - inset * w, y + inset * h, thickV, h * (1 - 2 * inset) };
    }
    return { x + w + gap, y, thickV, h };
}
//=============================================================================
static std::vector<double>
genericPeerPositionForOutside(const std::vector<double>& base, const std::wstring& loc)
{
    double x = base[0];
    double y = base[1];
    double w = base[2];
    double h = base[3];
    double reserveX = std::min(w * 0.0794, 0.08);
    double reserveY = std::min(h * 0.117, 0.12);
    if (loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR) {
        return { x, y, w, std::max(0.01, h - reserveY) };
    }
    if (loc == GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR) {
        return { x, y + reserveY, w, std::max(0.01, h - reserveY) };
    }
    if (loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR) {
        return { x + reserveX, y, std::max(0.01, w - reserveX), h };
    }
    return { x, y, std::max(0.01, w - reserveX), h };
}
//=============================================================================
static std::vector<double>
visibleTicks(const std::vector<double>& ticks, const std::vector<double>& limits)
{
    std::vector<double> result;
    if (limits.size() < 2) {
        return result;
    }
    double low = std::min(limits[0], limits[1]);
    double high = std::max(limits[0], limits[1]);
    double tolerance = std::max(1e-12, std::fabs(high - low) * 1e-12);
    for (double tick : ticks) {
        if (std::isfinite(tick) && tick >= low - tolerance && tick <= high + tolerance) {
            if (std::fabs(tick - low) <= tolerance) {
                tick = low;
            } else if (std::fabs(tick - high) <= tolerance) {
                tick = high;
            }
            result.push_back(tick);
        }
    }
    return result;
}
//=============================================================================
std::wstring
GOColorBar::getType()
{
    return GO_PROPERTY_VALUE_COLORBAR_STR;
}
//=============================================================================
GOColorBar::GOColorBar()
{
    constructProperties();
    setupDefaults();
    clearAllChanged();
}
//=============================================================================
GOColorBar::~GOColorBar()
{
    if (m_labelHandle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(m_labelHandle)) {
        deleteGraphicsObject(m_labelHandle, false, false);
    }
    GOGObjectsProperty* layout
        = static_cast<GOGObjectsProperty*>(findProperty(GO_LAYOUT_PROPERTY_NAME_STR, false));
    if (layout && !layout->data().empty()) {
        int64 layoutHandle = layout->data()[0];
        if (layoutHandle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(layoutHandle)) {
            GraphicsObject* layoutObject = findGraphicsObject(layoutHandle, false);
            if (layoutObject) {
                freeGraphicsObject(layoutHandle);
                delete layoutObject;
            }
        }
    }
}
//=============================================================================
void
GOColorBar::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LIMITS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOColorBarLocationProperty, GO_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOInOutProperty, GO_AXIS_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_AXIS_LOCATION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOColorBarTickDirectionProperty, GO_TICK_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty(true), GO_TICK_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TICK_LABELS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_TICK_LENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty(true), GO_TICKS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TICKS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    registerProperty(new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    registerProperty(new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOColorBar::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(
        GO_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(GO_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 9);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setTwoVectorDefault(GO_LIMITS_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_LIMITS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EASTOUTSIDE_STR);
    setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, 0.867896825396826, 0.11, 0.0198412698412699, 0.815);
    setRestrictedStringDefault(GO_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUT_STR);
    setRestrictedStringDefault(GO_AXIS_LOCATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_TICK_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_IN_STR);
    setStringDefault(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setRestrictedStringDefault(GO_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_TICK_LENGTH_PROPERTY_NAME_STR, 0.01);
    setVectorDoubleDefault(GO_TICKS_PROPERTY_NAME_STR, {});
    setRestrictedStringDefault(GO_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, L"Colorbar");
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    updateFont();
}
//=============================================================================
int64
GOColorBar::getPeerAxesHandle() const
{
    return m_peerAxesHandle;
}
//=============================================================================
void
GOColorBar::setPeerAxesHandle(int64 handle)
{
    m_peerAxesHandle = handle;
}
//=============================================================================
void
GOColorBar::setLabelHandle(int64 handle)
{
    m_labelHandle = handle;
    setGoProperty(GO_LABEL_PROPERTY_NAME_STR, handle);
}
//=============================================================================
int64
GOColorBar::getLabelHandle() const
{
    return m_labelHandle;
}
//=============================================================================
int64
GOColorBar::createLabel(int64 colorBarHandle)
{
    GOText* label = new GOText;
    int64 labelHandle = assignGraphicsObject(label);
    label->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, colorBarHandle);
    label->setStringDefault(GO_STRING_PROPERTY_NAME_STR, L"");
    label->setStringDefault(
        GO_FONT_NAME_PROPERTY_NAME_STR, findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    label->setScalarDoubleDefault(
        GO_FONT_SIZE_PROPERTY_NAME_STR, findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    std::vector<double> color = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    if (color.size() >= 3) {
        label->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, color[0], color[1], color[2]);
    }
    label->updateState();
    setLabelHandle(labelHandle);
    return labelHandle;
}
//=============================================================================
int64
GOColorBar::createLayoutOptions(int64 colorBarHandle, const std::wstring& edgeTile)
{
    GOLayoutOptions* opts = new GOLayoutOptions;
    int64 optsHandle = assignGraphicsObject(opts);
    opts->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, colorBarHandle);
    GOGenericProperty* tileProp = opts->findProperty(GO_TILE_PROPERTY_NAME_STR, false);
    if (tileProp) {
        if (!edgeTile.empty()) {
            tileProp->set(ArrayOf::characterArray(edgeTile));
        } else {
            tileProp->set(ArrayOf::doubleScalar(0.0));
        }
    }
    setGoProperty(GO_LAYOUT_PROPERTY_NAME_STR, optsHandle);
    return optsHandle;
}
//=============================================================================
bool
GOColorBar::isVertical() const
{
    auto* self = const_cast<GOColorBar*>(this);
    return !isHorizontalLocation(self->findStringProperty(GO_LOCATION_PROPERTY_NAME_STR));
}
//=============================================================================
bool
GOColorBar::isOutside() const
{
    auto* self = const_cast<GOColorBar*>(this);
    return isOutsideLocation(self->findStringProperty(GO_LOCATION_PROPERTY_NAME_STR));
}
//=============================================================================
void
GOColorBar::restorePeerAxesPosition()
{
    if (m_peerPositionBeforeResize.empty() || m_peerAxesHandle < HANDLE_OFFSET_OBJECT
        || isDeletedGraphicsObject(m_peerAxesHandle)) {
        return;
    }
    GraphicsObject* peer = findGraphicsObject(m_peerAxesHandle, false);
    if (!peer) {
        return;
    }
    GOOnOffProperty* deleting = static_cast<GOOnOffProperty*>(
        peer->findProperty(GO_BEING_DELETED_PROPERTY_NAME_STR, false));
    if (deleting && deleting->asBool()) {
        return;
    }
    peer->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, m_peerPositionBeforeResize[0],
        m_peerPositionBeforeResize[1], m_peerPositionBeforeResize[2],
        m_peerPositionBeforeResize[3]);
    peer->updateState();
    m_peerPositionBeforeResize.clear();
}
//=============================================================================
void
GOColorBar::applyLocationToPeerAxes()
{
    if (m_peerAxesHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(m_peerAxesHandle)) {
        return;
    }
    GraphicsObject* peer = findGraphicsObject(m_peerAxesHandle, false);
    if (!peer) {
        return;
    }
    std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    if (loc == GO_PROPERTY_VALUE_MANUAL_STR || loc == L"layout") {
        return;
    }
    bool outside = isOutsideLocation(loc);
    bool repeatedLocation = outside && loc == m_layoutAppliedLocation;
    if (!outside && !m_peerPositionBeforeResize.empty()) {
        peer->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, m_peerPositionBeforeResize[0],
            m_peerPositionBeforeResize[1], m_peerPositionBeforeResize[2],
            m_peerPositionBeforeResize[3]);
    }
    std::vector<double> base = outside && !m_peerPositionBeforeResize.empty() && !repeatedLocation
        ? peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR)
        : m_peerPositionBeforeResize.empty()
        ? peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR)
        : m_peerPositionBeforeResize;
    if (base.size() < 4) {
        return;
    }
    bool peerInLayout = isColorBarPeerInTiledLayout(peer);
    if (peerInLayout) {
        if (!m_peerPositionBeforeResize.empty()) {
            m_peerPositionBeforeResize.clear();
        }
        base = tiledColorBarPeerPosition(peer, base);
        peer->setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, base[0], base[1], base[2], base[3]);
        peer->updateState();
    }
    bool peer3D = isColorBarPeer3D(peer);
    bool peerContour = isColorBarPeerContour(peer);
    double figureWidth = GO_DEFAULT_FIGURE_WIDTH;
    double figureHeight = GO_DEFAULT_FIGURE_HEIGHT;
    GOFigure* figure = getParentFigure();
    if (figure) {
        std::vector<double> figurePosition
            = figure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (figurePosition.size() >= 4 && figurePosition[2] > 0.0 && figurePosition[3] > 0.0) {
            figureWidth = figurePosition[2];
            figureHeight = figurePosition[3];
        } else {
            figureWidth = figure->getWidth();
            figureHeight = figure->getHeight();
        }
    }
    bool defaultColorBarGeometry = isDefaultColorBarAxesPosition(base)
        || (!m_peerPositionBeforeResize.empty()
            && isDefaultColorBarAxesPosition(m_peerPositionBeforeResize));
    std::vector<double> cbPos = peerInLayout ? tiledColorBarPosition(base, loc, peer3D)
        : defaultColorBarGeometry
        ? defaultColorBarPosition(base, loc, peer3D, peerContour, figureWidth, figureHeight)
        : genericColorBarPosition(base, loc);
    // The peer axes shrinks to make room for an outside colorbar; the colorbar
    // is anchored to that shrunk rectangle (or to the axes rectangle for an
    // inside colorbar). Compute it first so a locked-aspect peer can hug the
    // actual drawn box instead of the full Position rectangle.
    std::vector<double> newPeer;
    if (outside) {
        newPeer = defaultColorBarGeometry ? defaultColorBarPeerPositionForOutside(base, loc, peer3D,
                                                peerContour, figureWidth, figureHeight)
                                          : genericPeerPositionForOutside(base, loc);
    }
    if (!peerInLayout) {
        const std::vector<double>& peerBox = outside ? newPeer : base;
        cbPos = constrainedColorBarPosition(peer, loc, cbPos, peerBox, figureWidth, figureHeight);
    }
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, cbPos[0], cbPos[1], cbPos[2], cbPos[3]);
    m_autoLayoutPosition = cbPos;
    if (peerInLayout) {
        m_layoutAppliedLocation = loc;
        return;
    }
    if (outside) {
        if (m_peerPositionBeforeResize.empty()) {
            m_peerPositionBeforeResize = base;
        }
        peer->setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, newPeer[0], newPeer[1], newPeer[2], newPeer[3]);
        peer->updateState();
    } else if (!m_peerPositionBeforeResize.empty()) {
        m_peerPositionBeforeResize.clear();
        peer->updateState();
    }
    m_layoutAppliedLocation = loc;
}
//=============================================================================
void
GOColorBar::updateModes()
{
    if (hasChanged(GO_LIMITS_PROPERTY_NAME_STR)) {
        toManual(GO_LIMITS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_TICKS_PROPERTY_NAME_STR)) {
        toManual(GO_TICKS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_TICK_LABELS_PROPERTY_NAME_STR)) {
        toManual(GO_TICK_LABELS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_AXIS_LOCATION_PROPERTY_NAME_STR)) {
        toManual(GO_AXIS_LOCATION_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        GOFigure* fig = getParentFigure();
        std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
        if (fig && fig->resized() && loc != GO_PROPERTY_VALUE_MANUAL_STR) {
            clearChanged(GO_POSITION_PROPERTY_NAME_STR);
            return;
        }
        // A Position change equal to the value the automatic layout last applied
        // is self-induced, not a user override: switching to 'manual' here would
        // call restorePeerAxesPosition() and grow the peer axes back over the
        // colorbar. Only a genuine (different) Position becomes a manual override.
        if (loc != GO_PROPERTY_VALUE_MANUAL_STR
            && isSameColorBarPosition(
                findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR), m_autoLayoutPosition)) {
            clearChanged(GO_POSITION_PROPERTY_NAME_STR);
            return;
        }
        restorePeerAxesPosition();
        setRestrictedStringDefault(GO_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
}
//=============================================================================
void
GOColorBar::updateAutoAxisLocation()
{
    if (!isAuto(GO_AXIS_LOCATION_MODE_PROPERTY_NAME_STR)) {
        return;
    }
    std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    setRestrictedStringDefault(GO_AXIS_LOCATION_PROPERTY_NAME_STR,
        (isOutside() || loc == L"layout" || loc == GO_PROPERTY_VALUE_MANUAL_STR)
            ? GO_PROPERTY_VALUE_OUT_STR
            : GO_PROPERTY_VALUE_IN_STR);
}
//=============================================================================
void
GOColorBar::updateAutoLimits()
{
    if (!isAuto(GO_LIMITS_MODE_PROPERTY_NAME_STR) || m_peerAxesHandle < HANDLE_OFFSET_OBJECT
        || isDeletedGraphicsObject(m_peerAxesHandle)) {
        return;
    }
    GraphicsObject* peer = findGraphicsObject(m_peerAxesHandle, false);
    if (!peer || !peer->haveProperty(GO_C_LIM_PROPERTY_NAME_STR)) {
        return;
    }
    std::vector<double> clim = peer->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR);
    if (clim.size() >= 2) {
        setTwoVectorDefault(GO_LIMITS_PROPERTY_NAME_STR, clim[0], clim[1]);
    }
}
//=============================================================================
void
GOColorBar::updateAutoTicks()
{
    std::vector<double> limits = findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR);
    if (limits.size() < 2 || limits[0] == limits[1]) {
        return;
    }
    GOTextInterpreterProperty* interpreter = static_cast<GOTextInterpreterProperty*>(
        findProperty(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR));
    std::vector<double> ticks;
    wstringVector labels;
    if (isAuto(GO_TICKS_MODE_PROPERTY_NAME_STR)) {
        double start = limits[0];
        double stop = limits[1];
        int tickCount = 10;
        formatAxisAuto(interpreter->getAsEnum(), limits[0], limits[1], tickCount, false, start,
            stop, ticks, labels);
        ticks = visibleTicks(ticks, limits);
        static_cast<GOVectorProperty*>(findProperty(GO_TICKS_PROPERTY_NAME_STR))->data(ticks);
    } else {
        ticks = findVectorDoubleProperty(GO_TICKS_PROPERTY_NAME_STR);
        ticks = visibleTicks(ticks, limits);
    }
    if (isAuto(GO_TICK_LABELS_MODE_PROPERTY_NAME_STR)) {
        formatAxisTickLabels(interpreter->getAsEnum(), ticks, false, labels);
        static_cast<GOStringVectorProperty*>(findProperty(GO_TICK_LABELS_PROPERTY_NAME_STR))
            ->data(labels);
    }
}
//=============================================================================
void
GOColorBar::updateFont()
{
    std::wstring fontName = findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    double fontSize = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    std::wstring fontAngle = findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    std::wstring fontWeight = findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    m_font = FontSpec {};
    m_font.family = fontName;
    m_font.pointSizeF = static_cast<double>(static_cast<int>(fontSize));
    m_font.italic
        = fontAngle == GO_PROPERTY_VALUE_ITALIC_STR || fontAngle == GO_PROPERTY_VALUE_OBLIQUE_STR;
    if (fontWeight == GO_PROPERTY_VALUE_BOLD_STR) {
        m_font.setWeight(FontSpec::WeightBold);
    } else if (fontWeight == GO_PROPERTY_VALUE_LIGHT_STR) {
        m_font.setWeight(FontSpec::WeightLight);
    } else if (fontWeight == GO_PROPERTY_VALUE_DEMI_STR) {
        m_font.setWeight(FontSpec::WeightDemiBold);
    } else {
        m_font.setWeight(FontSpec::WeightNormal);
    }
    m_font.antialias = true;
}
//=============================================================================
void
GOColorBar::updateLabelDefaults()
{
    if (m_labelHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(m_labelHandle)) {
        return;
    }
    GraphicsObject* label = findGraphicsObject(m_labelHandle, false);
    if (!label) {
        return;
    }
    std::vector<double> color = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    if (color.size() >= 3) {
        label->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, color[0], color[1], color[2]);
    }
    label->setStringDefault(
        GO_FONT_NAME_PROPERTY_NAME_STR, findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    label->setScalarDoubleDefault(
        GO_FONT_SIZE_PROPERTY_NAME_STR, findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    label->setRestrictedStringDefault(
        GO_FONT_ANGLE_PROPERTY_NAME_STR, findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    label->setRestrictedStringDefault(
        GO_FONT_WEIGHT_PROPERTY_NAME_STR, findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
    label->setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR,
        findStringProperty(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR));
    label->updateState();
}
//=============================================================================
void
GOColorBar::updateState()
{
    updateUnits();
    updateModes();
    std::wstring location = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    bool tiledPeer = location != GO_PROPERTY_VALUE_MANUAL_STR && location != L"layout"
        && m_peerAxesHandle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(m_peerAxesHandle)
        && isColorBarPeerInTiledLayout(findGraphicsObject(m_peerAxesHandle, false));
    if (hasChanged(GO_LOCATION_PROPERTY_NAME_STR) || tiledPeer) {
        applyLocationToPeerAxes();
    }
    if (hasChanged(GO_LOCATION_PROPERTY_NAME_STR) || hasChanged(GO_LAYOUT_PROPERTY_NAME_STR)
        || location == L"layout") {
        updateLayoutTile();
    }
    updateAutoAxisLocation();
    updateAutoLimits();
    updateAutoTicks();
    updateFont();
    updateLabelDefaults();
    clearAllChanged();
}
//=============================================================================
std::vector<double>
GOColorBar::currentColorMap()
{
    if (m_peerAxesHandle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(m_peerAxesHandle)) {
        GraphicsObject* peer = findGraphicsObject(m_peerAxesHandle, false);
        if (peer && peer->haveProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)) {
            return peer->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
        }
    }
    GOFigure* fig = getParentFigure();
    if (fig) {
        return fig->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    return { 0, 0, 1, 1, 1, 0 };
}
//=============================================================================
static std::array<double, 3>
interpolatedColorMapColor(const std::vector<double>& cmap, double t)
{
    size_t count = cmap.size() / 3;
    if (count == 1) {
        return { cmap[0], cmap[1], cmap[2] };
    }
    t = std::clamp(t, 0.0, 1.0);
    double scaled = t * static_cast<double>(count - 1);
    size_t left = static_cast<size_t>(std::floor(scaled));
    size_t right = std::min(left + 1, count - 1);
    double weight = scaled - static_cast<double>(left);
    std::array<double, 3> color {};
    for (size_t k = 0; k < 3; ++k) {
        double a = cmap[left * 3 + k];
        double b = cmap[right * 3 + k];
        color[k] = a + (b - a) * weight;
    }
    return color;
}
//=============================================================================
void
GOColorBar::drawRamp(RenderInterface& gc, const std::vector<double>& pos)
{
    std::vector<double> cmap = currentColorMap();
    size_t count = cmap.size() / 3;
    if (count == 0 || pos[2] <= 0 || pos[3] <= 0) {
        return;
    }
    bool reverse = stringCheck(GO_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR);
    double rampPixels = isVertical() ? pos[3] : pos[2];
    size_t steps = std::max(count, static_cast<size_t>(std::ceil(rampPixels)));
    for (size_t i = 0; i < steps; ++i) {
        double t = steps == 1 ? 0.0 : static_cast<double>(i) / static_cast<double>(steps - 1);
        if (reverse) {
            t = 1.0 - t;
        }
        std::array<double, 3> color = interpolatedColorMapColor(cmap, t);
        gc.color(color[0], color[1], color[2]);
        if (isVertical()) {
            double y1 = pos[1] + pos[3] * static_cast<double>(i) / static_cast<double>(steps);
            double y2
                = pos[1] + pos[3] * static_cast<double>(i + 1) / static_cast<double>(steps) + 1;
            gc.rectFill(pos[0], y1, pos[0] + pos[2], y2);
        } else {
            double x1 = pos[0] + pos[2] * static_cast<double>(i) / static_cast<double>(steps);
            double x2
                = pos[0] + pos[2] * static_cast<double>(i + 1) / static_cast<double>(steps) + 1;
            gc.rectFill(x1, pos[1], x2, pos[1] + pos[3]);
        }
    }
}
//=============================================================================
void
GOColorBar::drawFrameAndTicks(RenderInterface& gc, const std::vector<double>& pos)
{
    std::vector<double> color = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    std::vector<double> limits = findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR);
    std::vector<double> ticks = findVectorDoubleProperty(GO_TICKS_PROPERTY_NAME_STR);
    wstringVector labels = findStringVectorProperty(GO_TICK_LABELS_PROPERTY_NAME_STR);
    if (color.size() < 3 || limits.size() < 2 || limits[0] == limits[1]) {
        return;
    }
    bool reverse = stringCheck(GO_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR);
    bool out = stringCheck(GO_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUT_STR);
    std::wstring tickDirection = findStringProperty(GO_TICK_DIRECTION_PROPERTY_NAME_STR);
    std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    double tickLen = findScalarDoubleProperty(GO_TICK_LENGTH_PROPERTY_NAME_STR)
        * (isVertical() ? pos[3] : pos[2]);
    tickLen = std::max(2.0, tickLen);
    double textGap = 5.0;
    gc.color(color);
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    if (stringCheck(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        gc.rect(pos[0], pos[1], pos[0] + pos[2], pos[1] + pos[3]);
    }
    for (size_t k = 0; k < ticks.size(); ++k) {
        double t = (ticks[k] - limits[0]) / (limits[1] - limits[0]);
        if (!std::isfinite(t) || t < -1e-12 || t > 1 + 1e-12) {
            continue;
        }
        t = std::max(0.0, std::min(1.0, t));
        if (reverse) {
            t = 1.0 - t;
        }
        std::wstring label = k < labels.size() ? labels[k] : L"";
        label = texToUnicode(label);
        if (isVertical()) {
            bool leftSide
                = (loc == GO_PROPERTY_VALUE_WEST_STR || loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR)
                ? out
                : !out;
            double y = pos[1] + t * pos[3];
            double edge = leftSide ? pos[0] : pos[0] + pos[2];
            double inward = leftSide ? edge + tickLen : edge - tickLen;
            double outward = leftSide ? edge - tickLen : edge + tickLen;
            bool drawTick = tickDirection != GO_PROPERTY_VALUE_NONE_STR;
            bool drawOutward = tickDirection == GO_PROPERTY_VALUE_OUT_STR
                || tickDirection == GO_PROPERTY_VALUE_BOTH_STR;
            if (drawTick) {
                if (tickDirection == GO_PROPERTY_VALUE_BOTH_STR) {
                    gc.line(inward, y, outward, y);
                } else if (tickDirection == GO_PROPERTY_VALUE_OUT_STR) {
                    gc.line(edge, y, outward, y);
                } else {
                    gc.line(edge, y, inward, y);
                }
            }
            double tx = leftSide ? edge - textGap - (drawOutward ? tickLen : 0.0)
                                 : edge + textGap + (drawOutward ? tickLen : 0.0);
            gc.putText(tx, y, label, color, leftSide ? RenderInterface::Max : RenderInterface::Min,
                RenderInterface::Mean, m_font, 0);
        } else {
            bool topSide
                = (loc == GO_PROPERTY_VALUE_NORTH_STR || loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR)
                ? out
                : !out;
            double x = pos[0] + t * pos[2];
            double edge = topSide ? pos[1] + pos[3] : pos[1];
            double inward = topSide ? edge - tickLen : edge + tickLen;
            double outward = topSide ? edge + tickLen : edge - tickLen;
            bool drawTick = tickDirection != GO_PROPERTY_VALUE_NONE_STR;
            bool drawOutward = tickDirection == GO_PROPERTY_VALUE_OUT_STR
                || tickDirection == GO_PROPERTY_VALUE_BOTH_STR;
            if (drawTick) {
                if (tickDirection == GO_PROPERTY_VALUE_BOTH_STR) {
                    gc.line(x, inward, x, outward);
                } else if (tickDirection == GO_PROPERTY_VALUE_OUT_STR) {
                    gc.line(x, edge, x, outward);
                } else {
                    gc.line(x, edge, x, inward);
                }
            }
            double ty = topSide ? edge + textGap + (drawOutward ? tickLen : 0.0)
                                : edge - textGap - (drawOutward ? tickLen : 0.0);
            gc.putText(x, ty, label, color, RenderInterface::Mean,
                topSide ? RenderInterface::Min : RenderInterface::Max, m_font, 0);
        }
    }
}
//=============================================================================
void
GOColorBar::drawLabel(RenderInterface& gc, const std::vector<double>& pos)
{
    if (m_labelHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(m_labelHandle)) {
        return;
    }
    GraphicsObject* label = findGraphicsObject(m_labelHandle, false);
    if (!label || label->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::wstring txt = label->findStringProperty(GO_STRING_PROPERTY_NAME_STR);
    if (txt.empty()) {
        return;
    }
    txt = texToUnicode(txt);
    FontSpec labelFont;
    labelFont.family = label->findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    labelFont.pointSizeF = static_cast<double>(
        static_cast<int>(label->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR)));
    std::vector<double> color = label->findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    bool out = stringCheck(GO_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUT_STR);
    std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    if (isVertical()) {
        bool leftSide
            = (loc == GO_PROPERTY_VALUE_WEST_STR || loc == GO_PROPERTY_VALUE_WESTOUTSIDE_STR)
            ? out
            : !out;
        double x = leftSide ? pos[0] - 27.4 : pos[0] + pos[2] + 27.4;
        gc.putText(x, pos[1] + pos[3] / 2.0, txt, color, RenderInterface::Mean,
            RenderInterface::Mean, labelFont, 90);
    } else {
        bool topSide
            = (loc == GO_PROPERTY_VALUE_NORTH_STR || loc == GO_PROPERTY_VALUE_NORTHOUTSIDE_STR)
            ? out
            : !out;
        double y = topSide ? pos[1] + pos[3] + 34.0 : pos[1] - 34.0;
        gc.putText(pos[0] + pos[2] / 2.0, y, txt, color, RenderInterface::Mean,
            RenderInterface::Mean, labelFont, 0);
    }
}
//=============================================================================
void
GOColorBar::drawSelectionHighlight(RenderInterface& gc, const std::vector<double>& pos)
{
    if (!stringCheck(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
        || stringCheck(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    gc.lineWidth(1.0);
    gc.color(0.0, 0.4470, 0.7410);
    double pad = 3.0;
    gc.rect(pos[0] - pad, pos[1] - pad, pos[0] + pos[2] + pad, pos[1] + pos[3] + pad);
}
//=============================================================================
void
GOColorBar::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::vector<double> pos = positionAsPixels(gc);
    gc.setupDirectDraw();
    drawRamp(gc, pos);
    drawFrameAndTicks(gc, pos);
    drawLabel(gc, pos);
    drawSelectionHighlight(gc, pos);
    gc.releaseDirectDraw();
}
//=============================================================================
static bool
isSameColorBarPosition(const std::vector<double>& a, const std::vector<double>& b)
{
    if (a.size() < 4 || b.size() < 4) {
        return false;
    }
    for (size_t k = 0; k < 4; ++k) {
        if (std::fabs(a[k] - b[k]) > 1e-9) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
