//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cwctype>
#include <cmath>
#include <iomanip>
#include <sstream>
#include "GOBubbleLegend.hpp"
#include "GOChartDecorators.hpp"
#include "GOFontSpecHelpers.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOFigure.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOLegendLayout.hpp"
#include "GOLayoutOptions.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarAlphaProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOText.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "RenderHelpers.hpp"
#include "GOSurfaceScale.hpp"
#include "TexToUnicode.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    const std::vector<std::wstring> BUBBLE_LEGEND_LOCATION_DICT = { GO_PROPERTY_VALUE_NORTH_STR,
        GO_PROPERTY_VALUE_SOUTH_STR, GO_PROPERTY_VALUE_EAST_STR, GO_PROPERTY_VALUE_WEST_STR,
        L"northeast", L"northwest", L"southeast", L"southwest", GO_PROPERTY_VALUE_NORTHOUTSIDE_STR,
        GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR, GO_PROPERTY_VALUE_EASTOUTSIDE_STR,
        GO_PROPERTY_VALUE_WESTOUTSIDE_STR, L"northeastoutside", L"northwestoutside",
        L"southeastoutside", L"southwestoutside", GO_PROPERTY_VALUE_NONE_STR, L"layout", L"n", L"s",
        L"e", L"w", L"ne", L"nw", L"se", L"sw" };
    //=============================================================================
    class GOBubbleLegendLocationProperty : public GORestrictedStringProperty
    {
    public:
        GOBubbleLegendLocationProperty() : GORestrictedStringProperty(BUBBLE_LEGEND_LOCATION_DICT)
        {
        }
    };
    //=============================================================================
    class GOBubbleLegendStringProperty : public GORestrictedStringProperty
    {
    public:
        GOBubbleLegendStringProperty(const std::vector<std::wstring>& dict)
            : GORestrictedStringProperty(dict)
        {
        }
    };
    //=============================================================================
    std::wstring
    canonicalLocation(const std::wstring& value)
    {
        if (value == L"n") {
            return GO_PROPERTY_VALUE_NORTH_STR;
        }
        if (value == L"s") {
            return GO_PROPERTY_VALUE_SOUTH_STR;
        }
        if (value == L"e") {
            return GO_PROPERTY_VALUE_EAST_STR;
        }
        if (value == L"w") {
            return GO_PROPERTY_VALUE_WEST_STR;
        }
        if (value == L"ne") {
            return L"northeast";
        }
        if (value == L"nw") {
            return L"northwest";
        }
        if (value == L"se") {
            return L"southeast";
        }
        if (value == L"sw") {
            return L"southwest";
        }
        return value;
    }
    //=============================================================================
    bool
    hasText(GraphicsObject* go)
    {
        return go && go->haveProperty(GO_STRING_PROPERTY_NAME_STR)
            && !go->findStringProperty(GO_STRING_PROPERTY_NAME_STR).empty();
    }
    //=============================================================================
    bool
    isOutsideLocation(const std::wstring& loc)
    {
        return loc.find(L"outside") != std::wstring::npos;
    }
    //=============================================================================
    std::wstring
    formatLimitLabel(double value)
    {
        if (!std::isfinite(value)) {
            return L"";
        }
        double rounded = std::round(value);
        std::wostringstream stream;
        if (std::abs(value - rounded) < 1e-9) {
            stream << static_cast<long long>(rounded);
        } else {
            stream << std::setprecision(6) << value;
        }
        return stream.str();
    }
    //=============================================================================
    void
    updateSizeRangeFromValues(
        const std::vector<double>& values, bool& hasValue, double& minValue, double& maxValue)
    {
        for (double value : values) {
            if (!std::isfinite(value)) {
                continue;
            }
            value = std::max(0., value);
            if (!hasValue) {
                minValue = value;
                maxValue = value;
                hasValue = true;
            } else {
                minValue = std::min(minValue, value);
                maxValue = std::max(maxValue, value);
            }
        }
    }
    //=============================================================================
    std::pair<double, double>
    bubbleValueRange(GraphicsObject* peer)
    {
        double minSize = 0.;
        double maxSize = 1.;
        if (peer
            && peer->findStringProperty(GO_BUBBLE_LIMITS_MODE_INTERNAL_PROPERTY_NAME_STR)
                == GO_PROPERTY_VALUE_MANUAL_STR) {
            std::vector<double> limits
                = peer->findVectorDoubleProperty(GO_BUBBLE_LIMITS_INTERNAL_PROPERTY_NAME_STR);
            if (limits.size() >= 2 && limits[1] > limits[0]) {
                return { limits[0], limits[1] };
            }
        }
        bool hasValue = false;
        auto* children = peer ? static_cast<GOGObjectsProperty*>(
                                    peer->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
                              : nullptr;
        if (children) {
            for (int64 handle : children->data()) {
                GraphicsObject* child = findGraphicsObject(handle, false);
                if (!child || child->getType() != GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
                    continue;
                }
                updateSizeRangeFromValues(
                    child->findVectorDoubleProperty(GO_SIZE_DATA_PROPERTY_NAME_STR), hasValue,
                    minSize, maxSize);
            }
        }
        if (!hasValue) {
            return { 0., 1. };
        }
        if (minSize == maxSize) {
            if (maxSize > 0.) {
                minSize = maxSize * 0.9;
            } else {
                maxSize = 1.;
            }
        }
        return { minSize, maxSize };
    }
    //=============================================================================
    std::vector<double>
    bubbleDiameterRange(GraphicsObject* peer)
    {
        if (!peer) {
            return { 3., 50. };
        }
        std::vector<double> range
            = peer->findVectorDoubleProperty(GO_BUBBLE_SIZE_RANGE_INTERNAL_PROPERTY_NAME_STR);
        if (range.size() < 2 || range[0] <= 0. || range[1] <= range[0]) {
            return { 3., 50. };
        }
        return range;
    }
    //=============================================================================
    constexpr double BUBBLE_RENDER_DIAMETER_SCALE = 1.0;
    //=============================================================================
    GraphicsObject*
    firstBubbleChart(GraphicsObject* peer)
    {
        auto* children = peer ? static_cast<GOGObjectsProperty*>(
                                    peer->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
                              : nullptr;
        if (!children) {
            return nullptr;
        }
        for (int64 handle : children->data()) {
            GraphicsObject* child = findGraphicsObject(handle, false);
            if (child && child->getType() == GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
                return child;
            }
        }
        return nullptr;
    }
    //=============================================================================
    int
    bubbleChartCount(GraphicsObject* peer)
    {
        auto* children = peer ? static_cast<GOGObjectsProperty*>(
                                    peer->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
                              : nullptr;
        if (!children) {
            return 0;
        }
        int count = 0;
        for (int64 handle : children->data()) {
            GraphicsObject* child = findGraphicsObject(handle, false);
            if (child && child->getType() == GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
                ++count;
            }
        }
        return count;
    }
    //=============================================================================
    bool
    hasEdgeLayoutTile(GraphicsObject* object)
    {
        auto* layoutProperty = object ? dynamic_cast<GOGObjectsProperty*>(object->findProperty(
                                            GO_LAYOUT_PROPERTY_NAME_STR, false))
                                      : nullptr;
        if (!layoutProperty || layoutProperty->data().empty()) {
            return false;
        }
        GraphicsObject* layoutOptions = findGraphicsObject(layoutProperty->data()[0], false);
        if (!layoutOptions || !layoutOptions->isType(L"layoutoptions")) {
            return false;
        }
        auto* tileProperty = dynamic_cast<GOArrayOfProperty*>(
            layoutOptions->findProperty(GO_TILE_PROPERTY_NAME_STR, false));
        if (!tileProperty) {
            return false;
        }
        ArrayOf value = tileProperty->get();
        if (!value.isRowVectorCharacterArray() && !value.isScalarStringArray()) {
            return false;
        }
        std::wstring tile = value.asWideString();
        std::transform(tile.begin(), tile.end(), tile.begin(), ::towlower);
        return tile == L"east" || tile == L"west" || tile == L"north" || tile == L"south";
    }
    //=============================================================================
    std::vector<double>
    firstCDataColor(GraphicsObject* bubble)
    {
        if (!bubble || !bubble->haveProperty(GO_C_DATA_PROPERTY_NAME_STR)) {
            return { 0., 0.4470, 0.7410 };
        }
        ArrayOf cdata = bubble->findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR);
        if (cdata.isEmpty()) {
            return { 0., 0.4470, 0.7410 };
        }
        cdata.promoteType(NLS_DOUBLE);
        const double* ptr = static_cast<const double*>(cdata.dataPointer());
        if (!ptr || cdata.elementCount() < 3) {
            return { 0., 0.4470, 0.7410 };
        }
        if (cdata.is2D() && cdata.columns() == 3) {
            indexType rows = cdata.rows();
            return { ptr[0], ptr[rows], ptr[2 * rows] };
        }
        return { ptr[0], ptr[1], ptr[2] };
    }
    //=============================================================================
    std::vector<double>
    bubbleMarkerColor(GraphicsObject* bubble, const std::wstring& propertyName,
        const std::vector<double>& fallback)
    {
        if (!bubble || !bubble->haveProperty(propertyName)) {
            return fallback;
        }
        auto* colorProperty
            = dynamic_cast<GOAutoFlatColorProperty*>(bubble->findProperty(propertyName, false));
        if (!colorProperty || colorProperty->isEqual(GO_PROPERTY_VALUE_FLAT_STR)
            || colorProperty->isEqual(GO_PROPERTY_VALUE_AUTO_STR)) {
            return fallback;
        }
        std::vector<double> color = colorProperty->colorSpec();
        return color.size() >= 3 ? color : fallback;
    }
    //=============================================================================
    double
    bubbleMarkerAlpha(GraphicsObject* bubble, const std::wstring& propertyName, double fallback)
    {
        if (!bubble || !bubble->haveProperty(propertyName)) {
            return fallback;
        }
        auto* alphaProperty
            = dynamic_cast<GOScalarAlphaProperty*>(bubble->findProperty(propertyName, false));
        return alphaProperty ? alphaProperty->data() : fallback;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
GOBubbleLegend::getType()
{
    return GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR;
}
//=============================================================================
GOBubbleLegend::GOBubbleLegend()
{
    constructProperties();
    setupDefaults();
    clearAllChanged();
}
//=============================================================================
GOBubbleLegend::~GOBubbleLegend() { deleteInternalHandles(); }
//=============================================================================
void
GOBubbleLegend::deleteInternalHandles()
{
    for (int64 handle : { m_layoutHandle }) {
        if (handle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(handle)) {
            GraphicsObject* object = findGraphicsObject(handle, false);
            if (object) {
                freeGraphicsObject(handle);
                delete object;
            }
        }
    }
}
//=============================================================================
void
GOBubbleLegend::constructProperties()
{
    registerProperty(new GOScalarPositiveIntegerValueProperty, GO_NUM_BUBBLES_PROPERTY_NAME_STR);
    registerProperty(new GOBubbleLegendStringProperty({ GO_PROPERTY_VALUE_VERTICAL_STR,
                         GO_PROPERTY_VALUE_HORIZONTAL_STR, L"telescopic" }),
        GO_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOBubbleLegendStringProperty({ L"descending", L"ascending" }),
        GO_BUBBLE_SIZE_ORDER_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty(true), GO_LIMIT_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOBubbleLegendLocationProperty, GO_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_TITLE_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_TEXT_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
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
GOBubbleLegend::setupDefaults()
{
    setScalarPositiveIntegerValueDefault(GO_NUM_BUBBLES_PROPERTY_NAME_STR, 3);
    setRestrictedStringDefault(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
    setRestrictedStringDefault(GO_BUBBLE_SIZE_ORDER_PROPERTY_NAME_STR, L"descending");
    setRestrictedStringDefault(GO_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EASTOUTSIDE_STR);
    setRestrictedStringDefault(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    setThreeVectorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 9);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, 0.786157658128247, 0.385312408084396, 0.13, 0.26);
    setThreeVectorDefault(
        GO_TEXT_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    updateFont();
}
//=============================================================================
int64
GOBubbleLegend::getPeerAxesHandle() const
{
    return m_peerAxesHandle;
}
//=============================================================================
void
GOBubbleLegend::setPeerAxesHandle(int64 handle)
{
    m_peerAxesHandle = handle;
}
//=============================================================================
void
GOBubbleLegend::updateFont()
{
    m_font = FontSpec {};
    m_font.family = findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    m_font.pointSizeF = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    m_font.italic = findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR) == L"italic";
    m_font.setWeight(findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR) == L"bold"
            ? FontSpec::WeightBold
            : FontSpec::WeightNormal);
}
//=============================================================================
int64
GOBubbleLegend::createTitle(int64 bubbleLegendHandle, const std::wstring& titleText)
{
    GOText* title = new GOText;
    int64 titleHandle = assignGraphicsObject(title);
    title->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, bubbleLegendHandle);
    title->setStringDefault(GO_STRING_PROPERTY_NAME_STR, titleText);
    title->setStringDefault(
        GO_FONT_NAME_PROPERTY_NAME_STR, findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    title->setScalarDoubleDefault(
        GO_FONT_SIZE_PROPERTY_NAME_STR, findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    title->setRestrictedStringDefault(
        GO_FONT_ANGLE_PROPERTY_NAME_STR, findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    title->setRestrictedStringDefault(
        GO_FONT_WEIGHT_PROPERTY_NAME_STR, findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
    std::vector<double> textColor = findVectorDoubleProperty(GO_TEXT_COLOR_PROPERTY_NAME_STR);
    if (textColor.size() >= 3) {
        title->setThreeVectorDefault(
            GO_COLOR_PROPERTY_NAME_STR, textColor[0], textColor[1], textColor[2]);
    }
    title->updateState();
    m_titleHandle = titleHandle;
    setGoProperty(GO_TITLE_PROPERTY_NAME_STR, titleHandle);
    return titleHandle;
}
//=============================================================================
int64
GOBubbleLegend::createLayoutOptions(int64 bubbleLegendHandle, const std::wstring& edgeTile)
{
    GOLayoutOptions* opts = new GOLayoutOptions;
    int64 optsHandle = assignGraphicsObject(opts);
    opts->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, bubbleLegendHandle);
    GOGenericProperty* tileProp = opts->findProperty(GO_TILE_PROPERTY_NAME_STR, false);
    if (tileProp) {
        if (!edgeTile.empty()) {
            tileProp->set(ArrayOf::characterArray(edgeTile));
        } else {
            tileProp->set(ArrayOf::doubleScalar(0.0));
        }
    }
    m_layoutHandle = optsHandle;
    setGoProperty(GO_LAYOUT_PROPERTY_NAME_STR, optsHandle);
    return optsHandle;
}
//=============================================================================
std::pair<double, double>
GOBubbleLegend::preferredSizeInPixels() const
{
    auto* self = const_cast<GOBubbleLegend*>(this);
    double fontSize = self->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    auto* numBubblesProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
        self->findProperty(GO_NUM_BUBBLES_PROPERTY_NAME_STR));
    int count = std::max(1, (int)numBubblesProperty->data());
    bool horizontal
        = self->findStringProperty(GO_STYLE_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    if (horizontal) {
        return { std::max(126.0, count * 54.0), 58.0 + fontSize * 1.4 };
    }
    return { 153.0, std::max(172.0, 58.0 + count * 30.0 + fontSize * 1.6) };
}
//=============================================================================
std::vector<double>
GOBubbleLegend::positionAsPixels(RenderInterface& gc)
{
    return positionAsPixels(gc.width(), gc.height());
}
//=============================================================================
std::vector<double>
GOBubbleLegend::positionAsPixels(double width, double height)
{
    std::vector<double> pos = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    std::wstring units = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    if (units == GO_PROPERTY_VALUE_PIXELS_STR && pos.size() >= 4) {
        return positionOnSurface(this, units, pos, 1.0, width, height);
    }
    if (hasEdgeLayoutTile(this) && pos.size() >= 4) {
        return { pos[0] * width, pos[1] * height, pos[2] * width, pos[3] * height };
    }
    std::pair<double, double> preferred = preferredSizeInPixels();
    double w = pos.size() >= 4 && pos[2] > 0 ? pos[2] * width : preferred.first;
    double h = pos.size() >= 4 && pos[3] > 0 ? pos[3] * height : preferred.second;
    std::wstring location = canonicalLocation(findStringProperty(GO_LOCATION_PROPERTY_NAME_STR));
    double x = width - w - 16.0;
    double y = height - h - 16.0;
    if (location.find(L"east") != std::wstring::npos) {
        x = width - w - 12.0;
        y = (height - h) / 2.0;
    } else if (location.find(L"west") != std::wstring::npos) {
        x = 12.0;
        y = (height - h) / 2.0;
    } else if (location.find(L"north") != std::wstring::npos) {
        x = (width - w) / 2.0;
        y = height - h - 12.0;
    } else if (location.find(L"south") != std::wstring::npos) {
        x = (width - w) / 2.0;
        y = 12.0;
    }
    if (location == GO_PROPERTY_VALUE_NONE_STR && pos.size() >= 4) {
        x = pos[0] * width;
        y = pos[1] * height;
    }
    return { x, y, w, h };
}
//=============================================================================
void
GOBubbleLegend::restorePeerAxesPosition()
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
GOBubbleLegend::applyLocationToPeerAxes()
{
    if (m_peerAxesHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(m_peerAxesHandle)) {
        return;
    }
    GraphicsObject* peer = findGraphicsObject(m_peerAxesHandle, false);
    if (!peer) {
        return;
    }
    std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    if (!isOutsideLocation(loc) || loc == L"layout") {
        if (!m_peerPositionBeforeResize.empty()) {
            restorePeerAxesPosition();
        }
        return;
    }
    if (m_peerPositionBeforeResize.empty()) {
        m_peerPositionBeforeResize = peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    }
    std::vector<double> base = m_peerPositionBeforeResize;
    if (base.size() < 4) {
        return;
    }
    double reserveX = std::min(base[2] * 0.18, 0.16);
    double reserveY = std::min(base[3] * 0.18, 0.16);
    if (loc.find(L"west") != std::wstring::npos) {
        peer->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, base[0] + reserveX, base[1],
            base[2] - reserveX, base[3]);
    } else if (loc.find(L"east") != std::wstring::npos) {
        peer->setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, base[0], base[1], base[2] - reserveX, base[3]);
    } else if (loc.find(L"south") != std::wstring::npos) {
        peer->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, base[0], base[1] + reserveY,
            base[2], base[3] - reserveY);
    } else if (loc.find(L"north") != std::wstring::npos) {
        peer->setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, base[0], base[1], base[2], base[3] - reserveY);
    }
    peer->updateState();
}
//=============================================================================
void
GOBubbleLegend::updateAutoLayout()
{
    if (applyLegendLayoutTile(this, m_peerAxesHandle, preferredSizeInPixels())) {
        return;
    }
    std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    if (loc == GO_PROPERTY_VALUE_NONE_STR) {
        return;
    }
    GOFigure* fig = getParentFigure();
    if (!fig) {
        return;
    }
    double figW = static_cast<double>(fig->getWidth());
    double figH = static_cast<double>(fig->getHeight());
    if (figW <= 0. || figH <= 0.) {
        return;
    }
    std::vector<double> base = m_peerPositionBeforeResize.size() >= 4
        ? m_peerPositionBeforeResize
        : std::vector<double> { 0.13, 0.11, 0.775, 0.815 };
    auto [pixelW, pixelH] = preferredSizeInPixels();
    double w = pixelW / figW;
    double h = pixelH / figH;
    double gapX = 8.0 / figW;
    double gapY = 8.0 / figH;
    if (loc == L"layout") {
        loc = GO_PROPERTY_VALUE_EASTOUTSIDE_STR;
    }
    double x = base[0] + base[2] - w - gapX;
    double y = base[1] + base[3] - h - gapY;
    if (loc == GO_PROPERTY_VALUE_NORTH_STR) {
        x = base[0] + (base[2] - w) / 2.0;
        y = base[1] + base[3] - h - gapY;
    } else if (loc == GO_PROPERTY_VALUE_SOUTH_STR) {
        x = base[0] + (base[2] - w) / 2.0;
        y = base[1] + gapY;
    } else if (loc == GO_PROPERTY_VALUE_EAST_STR) {
        x = base[0] + base[2] - w - gapX;
        y = base[1] + (base[3] - h) / 2.0;
    } else if (loc == GO_PROPERTY_VALUE_WEST_STR) {
        x = base[0] + gapX;
        y = base[1] + (base[3] - h) / 2.0;
    } else if (loc == L"northwest") {
        x = base[0] + gapX;
        y = base[1] + base[3] - h - gapY;
    } else if (loc == L"southeast") {
        x = base[0] + base[2] - w - gapX;
        y = base[1] + gapY;
    } else if (loc == L"southwest") {
        x = base[0] + gapX;
        y = base[1] + gapY;
    } else if (isOutsideLocation(loc)) {
        if (loc.find(L"west") != std::wstring::npos) {
            x = std::max(0.0, base[0] - w - gapX);
            y = base[1] + base[3] - h;
        } else if (loc.find(L"south") != std::wstring::npos) {
            x = base[0] + base[2] - w;
            y = std::max(0.0, base[1] - h - gapY);
        } else if (loc.find(L"north") != std::wstring::npos
            && loc.find(L"east") == std::wstring::npos) {
            x = base[0] + base[2] - w;
            y = base[1] + base[3] + gapY;
        } else {
            x = base[0] + base[2] + gapX;
            y = base[1] + base[3] - h;
        }
    }
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, x, y, w, h);
    clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    applyLocationToPeerAxes();
}
//=============================================================================
void
GOBubbleLegend::drawBackground(RenderInterface& gc, const std::vector<double>& pos)
{
    std::vector<double> color = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    gc.color(color);
    gc.rectFill(pos[0], pos[1], pos[0] + pos[2], pos[1] + pos[3]);
    if (stringCheck(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        gc.color(findVectorDoubleProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR));
        gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
        gc.rect(pos[0], pos[1], pos[0] + pos[2], pos[1] + pos[3]);
    }
}
//=============================================================================
void
GOBubbleLegend::drawTitle(RenderInterface& gc, const std::vector<double>& pos, double& y)
{
    int64 titleHandle = findGoProperty(GO_TITLE_PROPERTY_NAME_STR);
    GraphicsObject* title
        = titleHandle >= HANDLE_OFFSET_OBJECT ? findGraphicsObject(titleHandle, false) : nullptr;
    if (!hasText(title)) {
        return;
    }
    std::wstring txt = title->findStringProperty(GO_STRING_PROPERTY_NAME_STR);
    if (findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TEX_STR) {
        txt = texToUnicode(txt);
    }
    FontSpec titleFont = m_font;
    titleFont.setWeight(FontSpec::WeightBold);
    gc.putText(pos[0] + pos[2] / 2.0, y, txt,
        findVectorDoubleProperty(GO_TEXT_COLOR_PROPERTY_NAME_STR), RenderInterface::Mean,
        RenderInterface::Max, titleFont, 0);
    int titleW = 0;
    int titleH = 0;
    measureGOText(txt, titleFont, titleW, titleH);
    y -= titleH + 5.0;
    gc.color(findVectorDoubleProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR));
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.line(pos[0], y + 2.0, pos[0] + pos[2], y + 2.0);
}
//=============================================================================
void
GOBubbleLegend::drawBubbles(RenderInterface& gc, const std::vector<double>& pos, double y)
{
    auto* numBubblesProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
        findProperty(GO_NUM_BUBBLES_PROPERTY_NAME_STR));
    int count = std::max(1, (int)numBubblesProperty->data());
    std::wstring style = findStringProperty(GO_STYLE_PROPERTY_NAME_STR);
    bool horizontal = style == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    bool telescopic = style == L"telescopic";
    bool ascending = findStringProperty(GO_BUBBLE_SIZE_ORDER_PROPERTY_NAME_STR) == L"ascending";
    GraphicsObject* peer = m_peerAxesHandle >= HANDLE_OFFSET_OBJECT
        ? findGraphicsObject(m_peerAxesHandle, false)
        : nullptr;
    GraphicsObject* bubble = firstBubbleChart(peer);
    std::vector<double> baseColor = firstCDataColor(bubble);
    bool neutralColor = bubbleChartCount(peer) > 1;
    std::vector<double> edge = neutralColor
        ? std::vector<double> { 0.4, 0.4, 0.4 }
        : bubbleMarkerColor(bubble, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, baseColor);
    std::vector<double> face = neutralColor
        ? std::vector<double> { 0.4, 0.4, 0.4 }
        : bubbleMarkerColor(bubble, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, baseColor);
    edge.push_back(
        neutralColor ? 1. : bubbleMarkerAlpha(bubble, GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR, 1.));
    face.push_back(
        neutralColor ? 1. : bubbleMarkerAlpha(bubble, GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR, 0.6));
    std::vector<double> diameterRange = bubbleDiameterRange(peer);
    double minD = diameterRange[0] * BUBBLE_RENDER_DIAMETER_SCALE;
    double maxD = diameterRange[1] * BUBBLE_RENDER_DIAMETER_SCALE;
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    std::vector<std::wstring> labels = findStringVectorProperty(GO_LIMIT_LABELS_PROPERTY_NAME_STR);
    if (labels.empty()) {
        auto [minValue, maxValue] = bubbleValueRange(peer);
        labels.resize(count);
        if (count == 1) {
            labels[0] = formatLimitLabel(maxValue);
        } else if (ascending) {
            labels.front() = formatLimitLabel(minValue);
            labels.back() = formatLimitLabel(maxValue);
        } else {
            labels.front() = formatLimitLabel(maxValue);
            labels.back() = formatLimitLabel(minValue);
        }
    }
    std::vector<double> diameters(count);
    for (int k = 0; k < count; ++k) {
        double t = count == 1 ? 1.0 : (double)k / (double)(count - 1);
        if (!ascending) {
            t = 1.0 - t;
        }
        diameters[k] = minD + std::sqrt(std::max(0.0, t)) * (maxD - minD);
    }
    std::vector<double> centersY(count);
    if (!horizontal && !telescopic) {
        double bubbleGap = 8.0;
        centersY[0] = pos[1] + pos[3] * 0.72;
        for (int k = 1; k < count; ++k) {
            centersY[k] = centersY[k - 1] - (diameters[k - 1] + diameters[k]) * 0.67 - bubbleGap;
        }
        double bottomLimit = pos[1] + pos[3] * 0.03;
        if (centersY.back() < bottomLimit) {
            double shift = bottomLimit - centersY.back();
            for (double& center : centersY) {
                center += shift;
            }
        }
    } else if (telescopic) {
        double baseY = pos[1] + pos[3] * 0.22;
        for (int k = 0; k < count; ++k) {
            centersY[k] = baseY + diameters[k] * 0.35;
        }
    }
    for (int k = 0; k < count; ++k) {
        double d = diameters[k];
        double x = horizontal ? pos[0] + 24.0 + k * ((pos[2] - 48.0) / std::max(1, count - 1))
                              : pos[0] + pos[2] * 0.34;
        double cy = horizontal ? y - 20.0 : centersY[k];
        centersY[k] = cy;
        DrawSymbol(gc, RenderInterface::Circle, x, cy, 0, d, edge, face, 0.5);
        if (horizontal && k < (int)labels.size() && !labels[k].empty()) {
            gc.putText(x + d / 2.0 + 6.0, cy, labels[k],
                findVectorDoubleProperty(GO_TEXT_COLOR_PROPERTY_NAME_STR), RenderInterface::Min,
                RenderInterface::Mean, m_font, 0);
        }
    }
    if (horizontal || count == 0) {
        return;
    }
    int topIndex = ascending ? count - 1 : 0;
    int bottomIndex = ascending ? 0 : count - 1;
    double scaleX = pos[0] + pos[2] * 0.65;
    double tick = 8.0;
    double topY = centersY[topIndex];
    double bottomY = centersY[bottomIndex];
    gc.color(findVectorDoubleProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR));
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.line(scaleX, topY, scaleX, bottomY);
    gc.line(scaleX - tick, topY, scaleX, topY);
    gc.line(scaleX - tick, bottomY, scaleX, bottomY);
    std::vector<double> textColor = findVectorDoubleProperty(GO_TEXT_COLOR_PROPERTY_NAME_STR);
    if (topIndex < (int)labels.size() && !labels[topIndex].empty()) {
        gc.putText(scaleX + 12.0, topY, labels[topIndex], textColor, RenderInterface::Min,
            RenderInterface::Mean, m_font, 0);
    }
    if (bottomIndex < (int)labels.size() && !labels[bottomIndex].empty()) {
        gc.putText(scaleX + 12.0, bottomY, labels[bottomIndex], textColor, RenderInterface::Min,
            RenderInterface::Mean, m_font, 0);
    }
}
//=============================================================================
void
GOBubbleLegend::updateState()
{
    if (hasChanged(GO_FONT_NAME_PROPERTY_NAME_STR) || hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)
        || hasChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR)
        || hasChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR)) {
        updateFont();
    }
    if (hasChanged(GO_LOCATION_PROPERTY_NAME_STR)) {
        setRestrictedStringDefault(GO_LOCATION_PROPERTY_NAME_STR,
            canonicalLocation(findStringProperty(GO_LOCATION_PROPERTY_NAME_STR)));
    }
    updateAutoLayout();
}
//=============================================================================
void
GOBubbleLegend::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::vector<double> pos = positionAsPixels(gc);
    gc.setupDirectDraw();
    drawBackground(gc, pos);
    double y = pos[1] + pos[3] - 6.0;
    drawTitle(gc, pos, y);
    drawBubbles(gc, pos, y);
    if (stringCheck(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
        && stringCheck(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        gc.color(0.0, 0.4470, 0.7410);
        gc.lineWidth(1.0);
        gc.rect(pos[0] - 3.0, pos[1] - 3.0, pos[0] + pos[2] + 3.0, pos[1] + pos[3] + 3.0);
    }
    gc.releaseDirectDraw();
}
//=============================================================================
bool
GOBubbleLegend::hitTest(double x, double y, double width, double height)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
        || stringCheck(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
        || stringCheck(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        return false;
    }
    std::vector<double> pos = positionAsPixels(width, height);
    return pos.size() >= 4 && x >= pos[0] && x <= pos[0] + pos[2] && y >= pos[1]
        && y <= pos[1] + pos[3];
}
//=============================================================================
} // namespace Nelson
//=============================================================================
