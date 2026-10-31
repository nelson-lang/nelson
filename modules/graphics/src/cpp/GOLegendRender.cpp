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
#include "GOLegend.hpp"
#include "GOFontSpecHelpers.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GORestrictedStringColorProperty.hpp"
#include "GOTexLayout.hpp"
#include "RenderHelpers.hpp"
#include "TexToUnicode.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Draws a legend title or entry string using the legend Interpreter. In 'tex' mode
// a script such as x^2 is laid out as a raised run, like titles and axis labels.
static void
drawLegendLabel(RenderInterface& gc, double x, double y, const std::wstring& raw,
    const std::vector<double>& color, RenderInterface::AlignmentFlag xalign,
    RenderInterface::AlignmentFlag yalign, const FontSpec& font, bool tex)
{
    if (tex && texHasScript(raw) && raw.find(L'\n') == std::wstring::npos) {
        TexBlockLayout layout = buildTexBlockLayout(texToRuns(raw), font);
        drawTexBlockLayout(gc, x, y, layout, color, xalign, yalign, 0.0);
        return;
    }
    const std::wstring txt = tex ? texToUnicode(raw) : raw;
    gc.putText(x, y, txt, color, xalign, yalign, font, 0.0);
}
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
static std::vector<double>
objectColor(GraphicsObject* object, const std::wstring& name, const std::vector<double>& fallback)
{
    if (object && object->haveProperty(name)) {
        GOGenericProperty* property = object->findProperty(name, false);
        GORestrictedStringColorProperty* restrictedColor
            = dynamic_cast<GORestrictedStringColorProperty*>(property);
        if (restrictedColor) {
            if (restrictedColor->isEqual(GO_PROPERTY_VALUE_AUTO_STR)
                || restrictedColor->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
                return fallback;
            }
            if (restrictedColor->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
                return { -1, -1, -1 };
            }
            std::vector<double> value = restrictedColor->colorSpec();
            if (value.size() >= 3 && value[0] >= 0 && value[1] >= 0 && value[2] >= 0) {
                return value;
            }
        } else {
            std::vector<double> value = object->findVectorDoubleProperty(name);
            if (value.size() >= 3 && value[0] >= 0 && value[1] >= 0 && value[2] >= 0) {
                return value;
            }
        }
    }
    return fallback;
}
//=============================================================================
static std::vector<double>
firstCDataColor(GraphicsObject* object)
{
    if (!object || !object->haveProperty(GO_C_DATA_PROPERTY_NAME_STR)) {
        return { 0., 0.4470, 0.7410 };
    }
    ArrayOf cdata = object->findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR);
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
static std::vector<double>
bubbleMarkerColor(
    GraphicsObject* object, const std::wstring& propertyName, const std::vector<double>& fallback)
{
    auto* colorProperty = object
        ? dynamic_cast<GOAutoFlatColorProperty*>(object->findProperty(propertyName, false))
        : nullptr;
    if (!colorProperty || colorProperty->isEqual(GO_PROPERTY_VALUE_AUTO_STR)
        || colorProperty->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
        return fallback;
    }
    if (colorProperty->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
        return { -1, -1, -1 };
    }
    std::vector<double> value = colorProperty->colorSpec();
    return value.size() >= 3 ? value : fallback;
}
//=============================================================================
static bool
isDrawableColor(const std::vector<double>& color)
{
    return color.size() >= 3 && color[0] >= 0 && color[1] >= 0 && color[2] >= 0;
}
//=============================================================================
static bool
usesFilledLegendIcon(GraphicsObject* object)
{
    if (!object) {
        return false;
    }
    const std::wstring type = object->getType();
    return type == GO_PROPERTY_VALUE_BAR_STR || type == GO_PROPERTY_VALUE_PATCH_STR
        || type == GO_PROPERTY_VALUE_RAIN_CLOUD_PLOT_STR;
}
//=============================================================================
void
GOLegend::drawBackground(RenderInterface& gc, const std::vector<double>& pos)
{
    if (pos.size() < 4 || pos[2] <= 0 || pos[3] <= 0) {
        return;
    }
    std::vector<double> color = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    color.push_back(findScalarDoubleProperty(GO_BACKGROUND_ALPHA_PROPERTY_NAME_STR));
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
GOLegend::drawTitle(RenderInterface& gc, const std::vector<double>& pos, double& y)
{
    if (m_titleHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(m_titleHandle)) {
        return;
    }
    GraphicsObject* title = findGraphicsObject(m_titleHandle, false);
    if (!hasText(title, GO_STRING_PROPERTY_NAME_STR)) {
        return;
    }
    std::wstring txt = title->findStringProperty(GO_STRING_PROPERTY_NAME_STR);
    const bool tex
        = title->findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TEX_STR;
    std::vector<double> color = title->findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    drawLegendLabel(gc, pos[0] + pos[2] / 2.0, y, txt, color, RenderInterface::Mean,
        RenderInterface::Max, m_font, tex);
    int titleW = 0;
    int titleH = 0;
    measureGOText(tex ? texToUnicode(txt) : txt, m_font, titleW, titleH);
    y -= titleH + 5.0;
}
//=============================================================================
void
GOLegend::drawEntries(RenderInterface& gc, const std::vector<double>& pos, double y)
{
    std::vector<std::wstring> labels = visibleLabels();
    if (labels.empty()) {
        return;
    }
    struct LegendEntry
    {
        std::wstring label;
        int64 plotHandle;
    };
    std::vector<LegendEntry> entries;
    entries.reserve(labels.size());
    for (size_t i = 0; i < labels.size(); ++i) {
        int64 plotHandle = i < m_plotHandles.size() ? m_plotHandles[i] : 0;
        entries.push_back({ labels[i], plotHandle });
    }
    if (findStringProperty(GO_DIRECTION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_REVERSE_STR) {
        std::reverse(entries.begin(), entries.end());
    }
    double rowHeight = entryRowHeight();
    int columns = effectiveNumColumns(entries.size());
    int rows = (int)std::ceil((double)entries.size() / (double)columns);
    auto* iconWidthProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
        findProperty(GO_ICON_COLUMN_WIDTH_PROPERTY_NAME_STR));
    double iconWidth = iconWidthProperty->data();
    double columnWidth = std::max(1.0, (pos[2] - 8.0) / (double)columns);
    std::vector<double> textColor = findVectorDoubleProperty(GO_TEXT_COLOR_PROPERTY_NAME_STR);
    for (size_t i = 0; i < entries.size(); ++i) {
        int col = (int)i / rows;
        int row = (int)i % rows;
        double baseX = pos[0] + 6.0 + col * columnWidth;
        double baseY = y - row * rowHeight - rowHeight / 2.0;
        GraphicsObject* plotObject = nullptr;
        if (entries[i].plotHandle >= HANDLE_OFFSET_OBJECT
            && !isDeletedGraphicsObject(entries[i].plotHandle)) {
            plotObject = findGraphicsObject(entries[i].plotHandle, false);
        }
        std::vector<double> lineColor
            = objectColor(plotObject, GO_COLOR_PROPERTY_NAME_STR, textColor);
        gc.color(lineColor);
        double lw = plotObject && plotObject->haveProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR)
            ? plotObject->findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR)
            : findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
        gc.lineWidth(lw);
        if (plotObject && plotObject->getType() == GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
            std::vector<double> baseColor = firstCDataColor(plotObject);
            std::vector<double> edge
                = bubbleMarkerColor(plotObject, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, baseColor);
            std::vector<double> face
                = bubbleMarkerColor(plotObject, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, baseColor);
            DrawSymbol(gc, RenderInterface::Circle, baseX + iconWidth * 0.35, baseY, 0,
                std::max(5.0, rowHeight * 0.55), edge, face, lw);
        } else if (usesFilledLegendIcon(plotObject)) {
            std::vector<double> faceColor
                = objectColor(plotObject, GO_FACE_COLOR_PROPERTY_NAME_STR, lineColor);
            std::vector<double> edgeColor
                = objectColor(plotObject, GO_EDGE_COLOR_PROPERTY_NAME_STR, textColor);
            double halfHeight = std::max(3.0, rowHeight * 0.22);
            double x2 = baseX + iconWidth * 0.7;
            if (isDrawableColor(faceColor)) {
                gc.color(faceColor);
                gc.rectFill(baseX, baseY - halfHeight, x2, baseY + halfHeight);
            }
            if (isDrawableColor(edgeColor)) {
                gc.color(edgeColor);
                gc.lineWidth(lw);
                gc.setLineStyle(L"-");
                gc.rect(baseX, baseY - halfHeight, x2, baseY + halfHeight);
            }
        } else {
            std::wstring lineStyle
                = plotObject && plotObject->haveProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)
                ? plotObject->findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)
                : L"-";
            gc.setLineStyle(lineStyle);
            gc.line(baseX, baseY, baseX + iconWidth * 0.7, baseY);
            if (plotObject && plotObject->haveProperty(GO_MARKER_PROPERTY_NAME_STR)) {
                RenderInterface::SymbolType marker
                    = StringToSymbol(plotObject->findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
                if (marker != RenderInterface::None) {
                    std::vector<double> edge = objectColor(
                        plotObject, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, lineColor);
                    std::vector<double> face = objectColor(
                        plotObject, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, { -1, -1, -1 });
                    double markerSize = plotObject->haveProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR)
                        ? plotObject->findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR)
                        : 3.0;
                    DrawSymbol(
                        gc, marker, baseX + iconWidth * 0.35, baseY, 0, markerSize, edge, face, lw);
                }
            }
        }
        const bool tex
            = findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TEX_STR;
        drawLegendLabel(gc, baseX + iconWidth, baseY, entries[i].label, textColor,
            RenderInterface::Min, RenderInterface::Mean, m_font, tex);
    }
}
//=============================================================================
void
GOLegend::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::vector<double> pos = positionAsPixels(gc);
    gc.setupDirectDraw();
    drawBackground(gc, pos);
    double y = pos[1] + pos[3] - 4.0;
    drawTitle(gc, pos, y);
    drawEntries(gc, pos, y);
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
GOLegend::hitTest(double x, double y, double width, double height)
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
bool
GOLegend::hitTestEntry(
    double x, double y, double width, double height, int64& peerHandleOut, int& regionOut)
{
    peerHandleOut = 0;
    regionOut = 0;
    if (!hitTest(x, y, width, height)) {
        return false;
    }
    std::vector<std::wstring> labels = visibleLabels();
    int n = (int)labels.size();
    if (n == 0) {
        return false;
    }
    std::vector<double> pos = positionAsPixels(width, height);
    // same layout as drawEntries(): entries fill column-major in rows of
    // entryRowHeight, starting from y0 = pos top minus 4.
    double rowHeight = entryRowHeight();
    int columns = effectiveNumColumns((size_t)n);
    int rows = (int)std::ceil((double)n / (double)columns);
    if (rows < 1 || rowHeight <= 0) {
        return false;
    }
    double columnWidth = std::max(1.0, (pos[2] - 8.0) / (double)columns);
    double y0 = pos[1] + pos[3] - 4.0;
    int col = (int)std::floor((x - (pos[0] + 6.0)) / columnWidth);
    int row = (int)std::floor((y0 - y) / rowHeight);
    if (col < 0) {
        col = 0;
    }
    if (col > columns - 1) {
        col = columns - 1;
    }
    if (row < 0 || row > rows - 1) {
        return false;
    }
    int displayedIndex = col * rows + row;
    if (displayedIndex < 0 || displayedIndex >= n) {
        return false;
    }
    int plotIndex = displayedIndex;
    if (findStringProperty(GO_DIRECTION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_REVERSE_STR) {
        plotIndex = n - 1 - displayedIndex;
    }
    double baseX = pos[0] + 6.0 + col * columnWidth;
    auto* iconWidthProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
        findProperty(GO_ICON_COLUMN_WIDTH_PROPERTY_NAME_STR));
    double iconWidth = iconWidthProperty->data();
    regionOut = (x <= baseX + iconWidth) ? 0 : 1;
    if (plotIndex >= 0 && plotIndex < (int)m_plotHandles.size()) {
        peerHandleOut = m_plotHandles[plotIndex];
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
