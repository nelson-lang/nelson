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
#include <set>
#include "GOLegend.hpp"
#include "GOAxesLayoutManager.hpp"
#include "GOChartDecorators.hpp"
#include "GOFontSpecHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOArrayOfProperty.hpp"
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
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOText.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "StringHelpers.hpp"
#include "GOTexLayout.hpp"
#include "TexToUnicode.hpp"
#include "GOSurfaceScale.hpp"
#include "UnitsConversionHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// ItemTokenSize reads back as a 2-by-1 column.
class GOLegendTokenSizeProperty : public GOTwoVectorProperty
{
public:
    ArrayOf
    get() override
    {
        ArrayOf row = GOTwoVectorProperty::get();
        const double* data = static_cast<const double*>(row.dataPointer());
        double* column = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2));
        column[0] = data[0];
        column[1] = data[1];
        return ArrayOf(NLS_DOUBLE, Dimensions(2, 1), column);
    }
};
//=============================================================================
static const std::vector<std::wstring> LEGEND_LOCATION_DICT
    = { GO_PROPERTY_VALUE_NORTH_STR, GO_PROPERTY_VALUE_SOUTH_STR, GO_PROPERTY_VALUE_EAST_STR,
          GO_PROPERTY_VALUE_WEST_STR, L"northeast", L"northwest", L"southeast", L"southwest",
          GO_PROPERTY_VALUE_NORTHOUTSIDE_STR, GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR,
          GO_PROPERTY_VALUE_EASTOUTSIDE_STR, GO_PROPERTY_VALUE_WESTOUTSIDE_STR, L"northeastoutside",
          L"northwestoutside", L"southeastoutside", L"southwestoutside", L"best", L"bestoutside",
          GO_PROPERTY_VALUE_NONE_STR, L"layout" };
//=============================================================================
static const std::vector<std::wstring> LEGEND_ORIENTATION_DICT
    = { GO_PROPERTY_VALUE_VERTICAL_STR, GO_PROPERTY_VALUE_HORIZONTAL_STR };
//=============================================================================
static const std::vector<std::wstring> LEGEND_DIRECTION_DICT
    = { GO_PROPERTY_VALUE_NORMAL_STR, GO_PROPERTY_VALUE_REVERSE_STR };
//=============================================================================
static std::wstring
lowerString(const std::wstring& value)
{
    std::wstring lowered(value);
    StringHelpers::to_lower(lowered);
    return lowered;
}
//=============================================================================
static std::wstring
canonicalLocation(const std::wstring& value)
{
    std::wstring loc = lowerString(value);
    if (loc == L"n" || loc == GO_PROPERTY_VALUE_NORTH_STR) {
        return GO_PROPERTY_VALUE_NORTH_STR;
    }
    if (loc == L"s" || loc == GO_PROPERTY_VALUE_SOUTH_STR) {
        return GO_PROPERTY_VALUE_SOUTH_STR;
    }
    if (loc == L"e" || loc == GO_PROPERTY_VALUE_EAST_STR) {
        return GO_PROPERTY_VALUE_EAST_STR;
    }
    if (loc == L"w" || loc == GO_PROPERTY_VALUE_WEST_STR) {
        return GO_PROPERTY_VALUE_WEST_STR;
    }
    if (loc == L"ne" || loc == L"northeast") {
        return L"northeast";
    }
    if (loc == L"nw" || loc == L"northwest") {
        return L"northwest";
    }
    if (loc == L"se" || loc == L"southeast") {
        return L"southeast";
    }
    if (loc == L"sw" || loc == L"southwest") {
        return L"southwest";
    }
    return loc;
}
//=============================================================================
class GOLegendLocationProperty : public GORestrictedStringProperty
{
public:
    GOLegendLocationProperty() : GORestrictedStringProperty(LEGEND_LOCATION_DICT) { }
    void
    set(ArrayOf arg) override
    {
        if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
            Error(L"Nelson:graphics:expectedStringProperty", L"Expecting a string for property.");
        }
        GORestrictedStringProperty::set(
            ArrayOf::characterArray(canonicalLocation(arg.asWideString())));
    }
};
//=============================================================================
class GOLegendLowerStringProperty : public GORestrictedStringProperty
{
public:
    GOLegendLowerStringProperty(const std::vector<std::wstring>& dict)
        : GORestrictedStringProperty(dict)
    {
    }
    void
    set(ArrayOf arg) override
    {
        if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
            Error(L"Nelson:graphics:expectedStringProperty", L"Expecting a string for property.");
        }
        GORestrictedStringProperty::set(ArrayOf::characterArray(lowerString(arg.asWideString())));
    }
};
//=============================================================================
static std::pair<double, double>
figurePixelSize(GOFigure* fig)
{
    std::vector<double> defaultPosition = defaultFigurePosition();
    double width = fig ? fig->getWidth() : defaultPosition[2];
    double height = fig ? fig->getHeight() : defaultPosition[3];
    if (fig) {
        std::vector<double> figurePosition
            = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (figurePosition.size() >= 4 && figurePosition[2] > 0.0 && figurePosition[3] > 0.0) {
            width = figurePosition[2];
            height = figurePosition[3];
        }
    }
    return { width, height };
}
//=============================================================================
static bool
isLegendablePeerChild(GraphicsObject* child);
//=============================================================================
static bool
hasText(const GraphicsObject* go, const std::wstring& name)
{
    if (!go) {
        return false;
    }
    auto* self = const_cast<GraphicsObject*>(go);
    return self->haveProperty(name) && !self->findStringProperty(name).empty();
}
//=============================================================================
std::wstring
GOLegend::getType()
{
    return GO_PROPERTY_VALUE_LEGEND_STR;
}
//=============================================================================
GOLegend::GOLegend()
{
    constructProperties();
    setupDefaults();
    clearAllChanged();
}
//=============================================================================
GOLegend::~GOLegend() { deleteInternalHandles(); }
//=============================================================================
void
GOLegend::deleteInternalHandles()
{
    std::vector<int64> handles;
    handles.push_back(m_titleHandle);
    handles.push_back(m_layoutHandle);
    handles.insert(handles.end(), m_iconHandles.begin(), m_iconHandles.end());
    handles.insert(handles.end(), m_textHandles.begin(), m_textHandles.end());
    for (int64 handle : handles) {
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
GOLegend::constructProperties()
{
    registerProperty(new GOGObjectsProperty, GO_TITLE_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_AUTO_UPDATE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_BACKGROUND_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_ITEM_HIT_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOLegendLocationProperty, GO_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarPositiveIntegerValueProperty, GO_NUM_COLUMNS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_NUM_COLUMNS_MODE_PROPERTY_NAME_STR);
    registerProperty(
        new GOLegendLowerStringProperty(LEGEND_ORIENTATION_DICT), GO_ORIENTATION_PROPERTY_NAME_STR);
    registerProperty(
        new GOScalarPositiveIntegerValueProperty, GO_ICON_COLUMN_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_ICON_COLUMN_WIDTH_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOLegendTokenSizeProperty, GO_ITEM_TOKEN_SIZE_PROPERTY_NAME_STR);
    registerProperty(
        new GOLegendLowerStringProperty(LEGEND_DIRECTION_DICT), GO_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_DIRECTION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty(true, false, true), GO_STRING_PROPERTY_NAME_STR);
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
GOLegend::setupDefaults()
{
    setRestrictedStringDefault(GO_AUTO_UPDATE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(GO_BACKGROUND_ALPHA_PROPERTY_NAME_STR, 1);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    setThreeVectorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 9);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_LOCATION_PROPERTY_NAME_STR, L"northeast");
    setScalarPositiveIntegerValueDefault(GO_NUM_COLUMNS_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_NUM_COLUMNS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
    setScalarPositiveIntegerValueDefault(GO_ICON_COLUMN_WIDTH_PROPERTY_NAME_STR, 30);
    setTwoVectorDefault(GO_ITEM_TOKEN_SIZE_PROPERTY_NAME_STR, 30, 18);
    setRestrictedStringDefault(
        GO_ICON_COLUMN_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_DIRECTION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.822082557713741, 0.843717752472412,
        0.070530725960995, 0.0613390070735117);
    setThreeVectorDefault(
        GO_TEXT_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEGEND_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    updateFont();
}
//=============================================================================
int64
GOLegend::getPeerAxesHandle() const
{
    return m_peerAxesHandle;
}
//=============================================================================
void
GOLegend::setPeerAxesHandle(int64 handle)
{
    if (m_peerAxesHandle != handle) {
        m_seenPeerChildren.clear();
    }
    m_peerAxesHandle = handle;
}
//=============================================================================
void
GOLegend::setPlotHandles(const std::vector<int64>& handles)
{
    m_plotHandles = handles;
    m_seenPeerChildren.insert(handles.begin(), handles.end());
    GraphicsObject* peer = peerAxes();
    GOGObjectsProperty* children = peer
        ? static_cast<GOGObjectsProperty*>(peer->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
        : nullptr;
    if (!children) {
        return;
    }
    for (int64 childHandle : children->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (isLegendablePeerChild(child)) {
            m_seenPeerChildren.insert(childHandle);
        }
    }
}
//=============================================================================
std::vector<int64>
GOLegend::getPlotHandles() const
{
    return m_plotHandles;
}
//=============================================================================
std::vector<int64>
GOLegend::getIconHandles() const
{
    return m_iconHandles;
}
//=============================================================================
std::vector<int64>
GOLegend::getTextHandles() const
{
    return m_textHandles;
}
//=============================================================================
void
GOLegend::setLabels(const std::vector<std::wstring>& labels)
{
    std::vector<std::wstring> filtered;
    for (const auto& label : labels) {
        if (!label.empty()) {
            filtered.push_back(label);
        }
    }
    static_cast<GOStringVectorProperty*>(findProperty(GO_STRING_PROPERTY_NAME_STR))->data(filtered);
}
//=============================================================================
int64
GOLegend::createTitle(int64 legendHandle)
{
    GOText* title = new GOText;
    int64 titleHandle = assignGraphicsObject(title);
    title->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, legendHandle);
    title->setStringDefault(GO_STRING_PROPERTY_NAME_STR, L"");
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
GOLegend::createLayoutOptions(int64 legendHandle, const std::wstring& edgeTile)
{
    GOLayoutOptions* opts = new GOLayoutOptions;
    int64 optsHandle = assignGraphicsObject(opts);
    opts->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, legendHandle);
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
int64
GOLegend::getTitleHandle() const
{
    return m_titleHandle;
}
//=============================================================================
int64
GOLegend::getLayoutHandle() const
{
    return m_layoutHandle;
}
//=============================================================================
std::vector<std::wstring>
GOLegend::visibleLabels() const
{
    auto* self = const_cast<GOLegend*>(this);
    std::vector<std::wstring> labels = self->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    labels.erase(std::remove(labels.begin(), labels.end(), L""), labels.end());
    return labels;
}
//=============================================================================
int
GOLegend::effectiveNumColumns(size_t labelCount) const
{
    auto* self = const_cast<GOLegend*>(this);
    auto* numColumnsProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
        self->findProperty(GO_NUM_COLUMNS_PROPERTY_NAME_STR));
    int numColumns = (int)numColumnsProperty->data();
    if (self->isAuto(GO_NUM_COLUMNS_MODE_PROPERTY_NAME_STR)
        && self->findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR)
            == GO_PROPERTY_VALUE_HORIZONTAL_STR) {
        numColumns = (int)std::max<size_t>(1, labelCount);
    }
    return std::max(1, numColumns);
}
//=============================================================================
// Height of a text line of the legend font: ascent plus descent, 1.117 em.
static double
textLineHeight(const FontSpec& font)
{
    return 1.117 * fontSpecPixelSize(font);
}
//=============================================================================
bool
GOLegend::markerOnlyEntries(double& markerPixels) const
{
    markerPixels = 0.0;
    if (m_plotHandles.empty()) {
        return false;
    }
    for (int64 handle : m_plotHandles) {
        if (handle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(handle)) {
            return false;
        }
        GraphicsObject* object = findGraphicsObject(handle, false);
        if (!object || !object->haveProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)
            || !object->haveProperty(GO_MARKER_PROPERTY_NAME_STR)
            || object->findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)
                != GO_PROPERTY_VALUE_NONE_STR
            || object->findStringProperty(GO_MARKER_PROPERTY_NAME_STR)
                == GO_PROPERTY_VALUE_NONE_STR) {
            return false;
        }
        double markerSize = object->haveProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR)
            ? object->findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR)
            : 6.0;
        markerPixels = std::max(markerPixels, markerSize * 96.0 / 72.0);
    }
    return true;
}
//=============================================================================
GOLegend::LegendLayout
GOLegend::layoutMetrics() const
{
    if (m_metricsValid) {
        return m_metrics;
    }
    auto* self = const_cast<GOLegend*>(this);
    LegendLayout m;
    std::vector<std::wstring> labels = visibleLabels();
    int maxWidth = 0;
    const bool tex
        = self->findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TEX_STR;
    for (const auto& label : labels) {
        int w = 0;
        int h = 0;
        if (tex && texHasScript(label) && label.find(L'\n') == std::wstring::npos) {
            w = (int)std::lround(buildTexBlockLayout(texToRuns(label), m_font).totalWidth);
        } else {
            measureGOText(tex ? texToUnicode(label) : label, m_font, w, h);
        }
        maxWidth = std::max(maxWidth, w);
    }
    m.maxTextWidth = maxWidth;
    // The title band is one legend text line plus 4 pixels; the title itself
    // needs 16.8 pixels more than its text.
    if (m_titleHandle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(m_titleHandle)) {
        GraphicsObject* title = findGraphicsObject(m_titleHandle, false);
        if (hasText(title, GO_STRING_PROPERTY_NAME_STR)) {
            int w = 0;
            int h = 0;
            measureGOText(title->findStringProperty(GO_STRING_PROPERTY_NAME_STR), m_font, w, h);
            m.titleWidth = w;
            m.titleBand = textLineHeight(m_font) + 4.0;
        }
    }
    m.columns = effectiveNumColumns(labels.size());
    m.rows = labels.empty() ? 0 : (int)std::ceil((double)labels.size() / (double)m.columns);
    // The icon column is the token width, given in points; marker-only entries
    // only need the marker.
    auto* iconWidthProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
        self->findProperty(GO_ICON_COLUMN_WIDTH_PROPERTY_NAME_STR));
    m.iconWidth = iconWidthProperty->data() * 96.0 / 72.0;
    double markerPixels = 0.0;
    if (markerOnlyEntries(markerPixels)) {
        m.iconWidth = markerPixels + 2.0;
    }
    m.rowPitch = textLineHeight(m_font) + 0.5;
    m.columnWidth = m.padLeft + m.iconWidth + m.iconTextGap + m.maxTextWidth + m.padRight;
    m.width = std::max(m.columns * m.columnWidth, m.titleWidth + 16.8);
    m.height = m.titleBand + m.padTop + std::max(1, m.rows) * m.rowPitch + m.padBottom;
    m_metrics = m;
    m_metricsValid = true;
    return m;
}
//=============================================================================
std::pair<double, double>
GOLegend::preferredSizeInPixels() const
{
    LegendLayout m = layoutMetrics();
    return { m.width, m.height };
}
//=============================================================================
void
GOLegend::updateFont()
{
    std::wstring fontName = findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    double fontSize = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    std::wstring fontAngle = findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    std::wstring fontWeight = findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    // Legends use the raw point size (no Nelson->Qt 1.25 factor).
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
GOLegend::updateTitleDefaults()
{
    if (m_titleHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(m_titleHandle)) {
        return;
    }
    GraphicsObject* title = findGraphicsObject(m_titleHandle, false);
    if (!title) {
        return;
    }
    title->setStringDefault(
        GO_FONT_NAME_PROPERTY_NAME_STR, findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    title->setScalarDoubleDefault(
        GO_FONT_SIZE_PROPERTY_NAME_STR, findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    title->setRestrictedStringDefault(
        GO_FONT_ANGLE_PROPERTY_NAME_STR, findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    title->setRestrictedStringDefault(
        GO_FONT_WEIGHT_PROPERTY_NAME_STR, findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
    title->setStringDefault(
        GO_INTERPRETER_PROPERTY_NAME_STR, findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR));
    std::vector<double> color = findVectorDoubleProperty(GO_TEXT_COLOR_PROPERTY_NAME_STR);
    if (color.size() >= 3) {
        title->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, color[0], color[1], color[2]);
    }
    title->updateState();
}
//=============================================================================
void
GOLegend::updateModes()
{
    if (hasChanged(GO_NUM_COLUMNS_PROPERTY_NAME_STR)) {
        toManual(GO_NUM_COLUMNS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_ITEM_TOKEN_SIZE_PROPERTY_NAME_STR)) {
        // The token width drives the icon column; the token height is stored only.
        std::vector<double> tokenSize
            = findVectorDoubleProperty(GO_ITEM_TOKEN_SIZE_PROPERTY_NAME_STR);
        if (!tokenSize.empty() && tokenSize[0] >= 1) {
            setScalarPositiveIntegerValueDefault(
                GO_ICON_COLUMN_WIDTH_PROPERTY_NAME_STR, std::round(tokenSize[0]));
            toManual(GO_ICON_COLUMN_WIDTH_MODE_PROPERTY_NAME_STR);
        }
    }
    if (hasChanged(GO_ICON_COLUMN_WIDTH_PROPERTY_NAME_STR)) {
        toManual(GO_ICON_COLUMN_WIDTH_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_DIRECTION_PROPERTY_NAME_STR)) {
        toManual(GO_DIRECTION_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        // A figure resize flags every child Position without changing it: only a
        // Position that differs from the automatic layout is a user override.
        if (isAutoLayoutPosition(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR))) {
            clearChanged(GO_POSITION_PROPERTY_NAME_STR);
            return;
        }
        restorePeerAxesPosition();
        setRestrictedStringDefault(GO_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    }
}
//=============================================================================
bool
GOLegend::isAutoLayoutPosition(const std::vector<double>& position) const
{
    if (position.size() < 4 || m_autoLayoutPosition.size() < 4) {
        return false;
    }
    for (size_t k = 0; k < 4; ++k) {
        if (std::abs(position[k] - m_autoLayoutPosition[k]) > 1e-12) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
GOLegend::updateUnits()
{
    if (!hasChanged(GO_UNITS_PROPERTY_NAME_STR)) {
        return;
    }
    GOUnitsProperty* unitsProperty
        = static_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR));
    GOFigure* fig = getParentFigure();
    auto [width, height] = figurePixelSize(fig);
    std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    std::wstring previousUnits = unitsProperty->getPreviousUnits();
    std::wstring currentUnits = unitsProperty->data();
    if (previousUnits == GO_PROPERTY_VALUE_PIXELS_STR
        && currentUnits != GO_PROPERTY_VALUE_PIXELS_STR && position.size() >= 2) {
        position[0] -= 1.0;
        position[1] -= 1.0;
    }
    convertPosition(previousUnits, currentUnits, position, width, height);
    if (previousUnits != GO_PROPERTY_VALUE_PIXELS_STR
        && currentUnits == GO_PROPERTY_VALUE_PIXELS_STR && position.size() >= 2) {
        position[0] += 1.0;
        position[1] += 1.0;
    }
    setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
    clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    clearChanged(GO_UNITS_PROPERTY_NAME_STR);
}
//=============================================================================
std::vector<double>
GOLegend::positionAsPixels(RenderInterface& gc)
{
    return positionAsPixels(gc.width(), gc.height());
}
//=============================================================================
std::vector<double>
GOLegend::positionAsPixels(double width, double height)
{
    return positionOnSurface(this, findStringProperty(GO_UNITS_PROPERTY_NAME_STR),
        findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR), 1.0, width, height);
}
//=============================================================================
GraphicsObject*
GOLegend::peerAxes()
{
    if (m_peerAxesHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(m_peerAxesHandle)) {
        return nullptr;
    }
    return findGraphicsObject(m_peerAxesHandle, false);
}
//=============================================================================
void
GOLegend::restorePeerAxesPosition()
{
    releaseAxesLayoutSlot(peerAxes(), m_peerReservation);
}
//=============================================================================
void
GOLegend::updateAutoLayout()
{
    // The peer axes repaint re-runs every legend: the layout runs once and the
    // nested call falls through.
    if (m_inAutoLayout) {
        return;
    }
    struct AutoLayoutGuard
    {
        bool& flag;
        explicit AutoLayoutGuard(bool& f) : flag(f) { flag = true; }
        ~AutoLayoutGuard() { flag = false; }
    } autoLayoutGuard(m_inAutoLayout);
    if (applyLegendLayoutTile(this, m_peerAxesHandle, preferredSizeInPixels())) {
        return;
    }
    GraphicsObject* peer = peerAxes();
    std::wstring loc = findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    if (loc == GO_PROPERTY_VALUE_NONE_STR) {
        releaseAxesLayoutSlot(peer, m_peerReservation);
        return;
    }
    auto [figW, figH] = figurePixelSize(getParentFigure());
    auto [pixelW, pixelH] = preferredSizeInPixels();
    std::vector<double> placed
        = layoutDecorationAroundAxes(peer, loc, pixelW, pixelH, figW, figH, m_peerReservation);
    if (placed.size() < 4) {
        return;
    }
    // the layout works in normalized units, Position is stored in the Units of the legend
    convertPositionPixelOrigin(GO_PROPERTY_VALUE_NORMALIZED_STR,
        findStringProperty(GO_UNITS_PROPERTY_NAME_STR), placed, figW, figH);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, placed[0], placed[1], placed[2], placed[3]);
    clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    m_autoLayoutPosition = placed;
}
//=============================================================================
void
GOLegend::updateState()
{
    m_metricsValid = false;
    updateUnits();
    updateModes();
    updateFont();
    updateTitleDefaults();
    updateDynamicEntries();
    updateAutoLayout();
    clearAllChanged();
}
//=============================================================================
static bool
isLegendablePeerChild(GraphicsObject* child)
{
    if (!child) {
        return false;
    }
    if (child->haveProperty(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR)
        && lowerString(child->findStringProperty(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR))
            == GO_PROPERTY_VALUE_OFF_STR) {
        return false;
    }
    if (legendIconHidden(child)) {
        return false;
    }
    const std::wstring type = child->getType();
    return type == GO_PROPERTY_VALUE_LINE_STR || type == GO_PROPERTY_VALUE_STAIR_STR
        || type == GO_PROPERTY_VALUE_ANIMATED_LINE_STR || type == GO_PROPERTY_VALUE_SCATTER_STR
        || type == GO_PROPERTY_VALUE_COMPASS_PLOT_STR || type == GO_PROPERTY_VALUE_PATCH_STR
        || type == GO_PROPERTY_VALUE_SURFACE_STR || type == GO_PROPERTY_VALUE_CONTOUR_STR
        || type == GO_PROPERTY_VALUE_HGGROUP_STR || type == GO_PROPERTY_VALUE_HGTRANSFORM_STR
        || type == GO_PROPERTY_VALUE_IMAGE_STR || type == GO_PROPERTY_VALUE_BAR_STR
        || type == GO_PROPERTY_VALUE_BUBBLE_CHART_STR
        || type == GO_PROPERTY_VALUE_RAIN_CLOUD_PLOT_STR;
}
//=============================================================================
static std::wstring
entryLabel(GraphicsObject* child, size_t index)
{
    std::wstring label;
    if (child && child->haveProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR)) {
        label = child->findStringProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    }
    if (label.empty()) {
        label = L"data" + std::to_wstring(index);
    }
    return label;
}
//=============================================================================
// True for the generated 'data1', 'data2'... labels, which never overwrite a DisplayName.
static bool
isGeneratedLabel(const std::wstring& label)
{
    if (label.size() < 5 || label.compare(0, 4, L"data") != 0) {
        return false;
    }
    return std::all_of(
        label.begin() + 4, label.end(), [](wchar_t c) { return c >= L'0' && c <= L'9'; });
}
//=============================================================================
static std::wstring
displayNameOf(GraphicsObject* child)
{
    if (child && child->haveProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR)) {
        return child->findStringProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    }
    return std::wstring();
}
//=============================================================================
void
GOLegend::updateDynamicEntries()
{
    std::vector<std::wstring> labels = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    bool autoUpdate = stringCheck(GO_AUTO_UPDATE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    if (!autoUpdate) {
        return;
    }

    std::vector<int64> handles;
    std::vector<std::wstring> refreshedLabels;
    size_t count = std::min(m_plotHandles.size(), labels.size());
    handles.reserve(m_plotHandles.size());
    refreshedLabels.reserve(labels.size());
    const bool stringChanged = hasChanged(GO_STRING_PROPERTY_NAME_STR);
    for (size_t k = 0; k < count; ++k) {
        int64 handle = m_plotHandles[k];
        // An entry whose IconDisplayStyle was turned off after the legend was built is dropped.
        if (handle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(handle)
            && !legendIconHidden(findGraphicsObject(handle, false))) {
            GraphicsObject* child = findGraphicsObject(handle, false);
            std::wstring label = labels[k];
            // An explicit label is written to DisplayName, which then drives the
            // entry.
            if (stringChanged && !isGeneratedLabel(label) && child
                && child->haveProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR)) {
                child->findProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR)
                    ->set(ArrayOf::characterArray(label));
            }
            std::wstring displayName = displayNameOf(child);
            handles.push_back(handle);
            refreshedLabels.push_back(displayName.empty() ? label : displayName);
        }
    }
    if (GraphicsObject* peer = peerAxes()) {
        GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
            peer->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (children) {
            std::set<int64> known(m_seenPeerChildren.begin(), m_seenPeerChildren.end());
            known.insert(handles.begin(), handles.end());
            for (int64 childHandle : children->data()) {
                if (known.find(childHandle) != known.end()) {
                    continue;
                }
                GraphicsObject* child = findGraphicsObject(childHandle, false);
                if (isLegendablePeerChild(child)) {
                    handles.push_back(childHandle);
                    size_t unnamed = 0;
                    for (int64 existing : handles) {
                        if (displayNameOf(findGraphicsObject(existing, false)).empty()) {
                            ++unnamed;
                        }
                    }
                    refreshedLabels.push_back(entryLabel(child, unnamed));
                    known.insert(childHandle);
                    m_seenPeerChildren.insert(childHandle);
                }
            }
        }
    }
    m_plotHandles.swap(handles);
    static_cast<GOStringVectorProperty*>(findProperty(GO_STRING_PROPERTY_NAME_STR))
        ->data(refreshedLabels);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
