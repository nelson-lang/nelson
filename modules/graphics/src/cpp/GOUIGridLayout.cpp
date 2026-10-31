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
#include <cwctype>
#include <limits>
#include <set>
#include <utility>
#include "GOUIGridLayout.h"
#include "GOLayoutOptions.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOHelpers.hpp"
#include "GOUIContainer.h"
#include "GOFigure.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIGridLayout::GOUIGridLayout()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIGridLayout::getType()
{
    return GO_PROPERTY_VALUE_UIGRIDLAYOUT_STR;
}
//=============================================================================
std::string
GOUIGridLayout::getClassName()
{
    return GO_CLASS_NAME_UI_GRIDLAYOUT_STR;
}
//=============================================================================
void
GOUIGridLayout::constructProperties()
{
    registerUIComponentCommonProperties(false, true, true);
    registerProperty(new GOArrayOfProperty, GO_ROW_HEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLUMN_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0.0, (double)std::numeric_limits<int>::max()),
        GO_ROW_SPACING_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0.0, (double)std::numeric_limits<int>::max()),
        GO_COLUMN_SPACING_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_PADDING_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SCROLLABLE_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
static ArrayOf
defaultTrackCell()
{
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, 2);
    elements[0] = ArrayOf::characterArray(L"1x");
    elements[1] = ArrayOf::characterArray(L"1x");
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 2), elements);
}
//=============================================================================
void
GOUIGridLayout::setupDefaults()
{
    setupUIComponentCommonDefaults(1, 1, 100, 100, false, true);
    GOArrayOfProperty* rowHeight
        = static_cast<GOArrayOfProperty*>(findProperty(GO_ROW_HEIGHT_PROPERTY_NAME_STR));
    rowHeight->data(defaultTrackCell());
    GOArrayOfProperty* columnWidth
        = static_cast<GOArrayOfProperty*>(findProperty(GO_COLUMN_WIDTH_PROPERTY_NAME_STR));
    columnWidth->data(defaultTrackCell());
    setScalarDoubleDefault(GO_ROW_SPACING_PROPERTY_NAME_STR, 10);
    setScalarDoubleDefault(GO_COLUMN_SPACING_PROPERTY_NAME_STR, 10);
    setFourVectorDefault(GO_PADDING_PROPERTY_NAME_STR, 10, 10, 10, 10);
    setRestrictedStringDefault(GO_SCROLLABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
void
GOUIGridLayout::updateState()
{
    if (parentGoWindow && parentGoWindow->isHeadlessHost()) {
        performLayout(); // no widget in WEB_GUI, but children need Positions
        clearAllChanged();
        return;
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUIGridLayout::onGeometryChanged()
{
    performLayout();
    GOUIContainer::onGeometryChanged();
}
//=============================================================================
std::vector<GOUIGridLayout::TrackSize>
GOUIGridLayout::parseTrackSizes(const std::wstring& propertyName)
{
    std::vector<TrackSize> tracks;
    GOArrayOfProperty* property
        = static_cast<GOArrayOfProperty*>(findProperty(propertyName, false));
    if (!property) {
        return tracks;
    }
    ArrayOf value = property->data();
    ArrayOfVector elements;
    if (value.isCell()) {
        ArrayOf* cellElements = (ArrayOf*)value.dataPointer();
        for (indexType k = 0; k < value.elementCount(); ++k) {
            elements << cellElements[k];
        }
    } else if (value.isNumeric() && !value.isEmpty()) {
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        const double* values = (const double*)asDouble.dataPointer();
        for (indexType k = 0; k < value.elementCount(); ++k) {
            elements << ArrayOf::doubleScalar(values[k]);
        }
    } else if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        elements << value;
    }
    for (const ArrayOf& element : elements) {
        TrackSize track;
        if (element.isNumeric() && !element.isEmpty()) {
            track.kind = TrackSize::Kind::Fixed;
            track.value = ArrayOf(element).asDoubleScalar();
        } else if (element.isRowVectorCharacterArray() || element.isScalarStringArray()) {
            std::wstring text = element.asWideString();
            StringHelpers::to_lower(text);
            if (text == L"fit") {
                track.kind = TrackSize::Kind::Fit;
            } else if (!text.empty() && text.back() == L'x') {
                track.kind = TrackSize::Kind::Weight;
                try {
                    track.value = std::stod(text.substr(0, text.size() - 1));
                } catch (...) {
                    track.value = 1.0;
                }
            }
        }
        tracks.push_back(track);
    }
    return tracks;
}
//=============================================================================
std::vector<double>
GOUIGridLayout::computeTrackPixels(const std::vector<TrackSize>& tracks, double available,
    double spacing, bool /*isRow*/, size_t implicitCount)
{
    size_t count = tracks.size() + implicitCount;
    std::vector<double> pixels(count, 0.0);
    if (count == 0) {
        return pixels;
    }
    double remaining = available - spacing * (double)(count - 1);
    double totalWeight = 0.0;
    for (size_t k = 0; k < count; ++k) {
        TrackSize track;
        if (k < tracks.size()) {
            track = tracks[k];
        } // implicit tracks default to weight 1x
        switch (track.kind) {
        case TrackSize::Kind::Fixed:
            pixels[k] = track.value;
            remaining -= track.value;
            break;
        case TrackSize::Kind::Fit:
            // approximation: fit tracks get a fixed natural size
            pixels[k] = 30.0;
            remaining -= pixels[k];
            break;
        case TrackSize::Kind::Weight:
            totalWeight += track.value;
            break;
        }
    }
    if (remaining < 0) {
        remaining = 0;
    }
    for (size_t k = 0; k < count; ++k) {
        TrackSize track;
        if (k < tracks.size()) {
            track = tracks[k];
        }
        if (track.kind == TrackSize::Kind::Weight && totalWeight > 0) {
            pixels[k] = remaining * (track.value / totalWeight);
        }
    }
    return pixels;
}
//=============================================================================
static GOLayoutOptions*
layoutOptionsOfChild(GraphicsObject* child)
{
    GOGObjectsProperty* layoutProperty
        = static_cast<GOGObjectsProperty*>(child->findProperty(GO_LAYOUT_PROPERTY_NAME_STR, false));
    if (!layoutProperty || layoutProperty->data().empty()) {
        return nullptr;
    }
    return dynamic_cast<GOLayoutOptions*>(findGraphicsObject(layoutProperty->data()[0], false));
}
//=============================================================================
static void
readSpan(GOLayoutOptions* options, const std::wstring& propertyName, int& first, int& last)
{
    first = 0;
    last = 0;
    if (!options) {
        return;
    }
    GOGenericProperty* property = options->findProperty(propertyName, false);
    if (!property) {
        return;
    }
    ArrayOf value = property->get();
    if (value.isNumeric() && !value.isEmpty()) {
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        const double* values = (const double*)asDouble.dataPointer();
        first = (int)values[0];
        last = (value.elementCount() > 1) ? (int)values[1] : first;
    }
}
//=============================================================================
static void
writeSpan(GOLayoutOptions* options, const std::wstring& propertyName, int value)
{
    if (!options) {
        return;
    }
    GOGenericProperty* property = options->findProperty(propertyName, false);
    if (property) {
        property->set(ArrayOf::doubleScalar((double)value));
        property->clearModified();
    }
}
//=============================================================================
void
GOUIGridLayout::performLayout()
{
    // Headless figure host (WEB_GUI): there are no widgets, so lay out against
    // the parent's Qt-free size (nested container Position, else the figure
    // render size). With a widget backend, the binding sizes the grid widget
    // to fill its parent and calls performLayout(gridW, gridH) itself.
    if (!parentGoWindow || !parentGoWindow->isHeadlessHost()) {
        return;
    }
    double gridW = 0;
    double gridH = 0;
    GOGObjectsProperty* parentProperty
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    GraphicsObject* parentObject = nullptr;
    if (parentProperty && !parentProperty->data().empty()
        && parentProperty->data()[0] >= HANDLE_OFFSET_OBJECT) {
        parentObject = findGraphicsObject(parentProperty->data()[0], false);
    }
    if (dynamic_cast<GOUIContainer*>(parentObject)) {
        std::vector<double> parentPosition(
            parentObject->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        if (parentPosition.size() == 4) {
            gridW = parentPosition[2];
            gridH = parentPosition[3];
        }
    } else {
        GOFigure* fig = parentGoWindow->hostFigure();
        if (fig) {
            gridW = fig->getWidth();
            gridH = fig->getHeight();
        }
    }
    performLayout(gridW, gridH);
}
//=============================================================================
void
GOUIGridLayout::performLayout(double gridW, double gridH)
{
    if (gridW <= 0 || gridH <= 0) {
        return;
    }

    std::vector<TrackSize> rowTracks = parseTrackSizes(GO_ROW_HEIGHT_PROPERTY_NAME_STR);
    std::vector<TrackSize> columnTracks = parseTrackSizes(GO_COLUMN_WIDTH_PROPERTY_NAME_STR);
    size_t rowCount = rowTracks.size();
    size_t columnCount = std::max<size_t>(columnTracks.size(), 1);

    std::vector<double> padding(findVectorDoubleProperty(GO_PADDING_PROPERTY_NAME_STR));
    if (padding.size() != 4) {
        padding = { 10, 10, 10, 10 };
    }
    double rowSpacing = findScalarDoubleProperty(GO_ROW_SPACING_PROPERTY_NAME_STR);
    double columnSpacing = findScalarDoubleProperty(GO_COLUMN_SPACING_PROPERTY_NAME_STR);

    // Auto placement fills the first free cell, left to right then top to bottom, and adds
    // implicit rows when the grid is full.
    std::vector<int64> childHandles = getChildrenHandles();
    struct Placement
    {
        GraphicsObject* child = nullptr;
        GOLayoutOptions* options = nullptr;
        int rowFirst = 0, rowLast = 0, columnFirst = 0, columnLast = 0;
    };
    std::vector<Placement> placements;
    std::set<std::pair<int, int>> occupiedCells;
    for (int64 childHandle : childHandles) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (!child) {
            continue;
        }
        Placement placement;
        placement.child = child;
        placement.options = layoutOptionsOfChild(child);
        readSpan(
            placement.options, GO_ROW_PROPERTY_NAME_STR, placement.rowFirst, placement.rowLast);
        readSpan(placement.options, GO_COLUMN_PROPERTY_NAME_STR, placement.columnFirst,
            placement.columnLast);
        if (placement.rowFirst > 0 && placement.columnFirst > 0) {
            for (int r = placement.rowFirst; r <= std::max(placement.rowFirst, placement.rowLast);
                ++r) {
                for (int c = placement.columnFirst;
                    c <= std::max(placement.columnFirst, placement.columnLast); ++c) {
                    occupiedCells.insert({ r, c });
                }
            }
        }
        placements.push_back(placement);
    }
    for (Placement& placement : placements) {
        if (placement.rowFirst > 0 && placement.columnFirst > 0) {
            continue;
        }
        int row = 1;
        int column = 1;
        for (;;) {
            if (occupiedCells.find({ row, column }) == occupiedCells.end()) {
                break;
            }
            ++column;
            if (column > (int)columnCount) {
                column = 1;
                ++row;
            }
        }
        placement.rowFirst = row;
        placement.rowLast = row;
        placement.columnFirst = column;
        placement.columnLast = column;
        occupiedCells.insert({ row, column });
        writeSpan(placement.options, GO_ROW_PROPERTY_NAME_STR, row);
        writeSpan(placement.options, GO_COLUMN_PROPERTY_NAME_STR, column);
    }
    size_t maxRowUsed = rowCount;
    for (const Placement& placement : placements) {
        maxRowUsed = std::max<size_t>(maxRowUsed, (size_t)placement.rowLast);
    }
    size_t implicitRows = maxRowUsed > rowCount ? (maxRowUsed - rowCount) : 0;

    double availableWidth = gridW - padding[0] - padding[2]; // left/right
    double availableHeight = gridH - padding[1] - padding[3]; // bottom/top
    std::vector<double> rowPixels
        = computeTrackPixels(rowTracks, availableHeight, rowSpacing, true, implicitRows);
    std::vector<double> columnPixels
        = computeTrackPixels(columnTracks, availableWidth, columnSpacing, false, 0);

    auto offsetsOf = [](const std::vector<double>& sizes, double origin, double spacing) {
        std::vector<double> offsets(sizes.size(), origin);
        double position = origin;
        for (size_t k = 0; k < sizes.size(); ++k) {
            offsets[k] = position;
            position += sizes[k] + spacing;
        }
        return offsets;
    };
    std::vector<double> rowOffsets = offsetsOf(rowPixels, padding[3], rowSpacing);
    std::vector<double> columnOffsets = offsetsOf(columnPixels, padding[0], columnSpacing);

    double gridHeight = gridH;
    for (const Placement& placement : placements) {
        int rowFirst = std::max(1, placement.rowFirst);
        int rowLast = std::min((int)rowPixels.size(), std::max(rowFirst, placement.rowLast));
        int columnFirst = std::max(1, std::min((int)columnPixels.size(), placement.columnFirst));
        int columnLast
            = std::min((int)columnPixels.size(), std::max(columnFirst, placement.columnLast));
        if (rowFirst > (int)rowPixels.size()) {
            rowFirst = (int)rowPixels.size();
            rowLast = rowFirst;
        }
        double x = columnOffsets[columnFirst - 1];
        double yTop = rowOffsets[rowFirst - 1];
        double w = 0;
        for (int c = columnFirst; c <= columnLast; ++c) {
            w += columnPixels[c - 1];
        }
        w += columnSpacing * (double)(columnLast - columnFirst);
        double h = 0;
        for (int r = rowFirst; r <= rowLast; ++r) {
            h += rowPixels[r - 1];
        }
        h += rowSpacing * (double)(rowLast - rowFirst);

        // bottom-left origin position, relative to the grid widget
        double yBottom = gridHeight - yTop - h;
        GOGenericProperty* positionProperty
            = placement.child->findProperty(GO_POSITION_PROPERTY_NAME_STR, false);
        if (positionProperty) {
            placement.child->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, x, yBottom, w, h);
            positionProperty->setModified(true);
        }
        placement.child->updateState();
    }

    double gridWidth = gridW;
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0, 0, gridWidth, gridHeight);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, gridWidth, gridHeight);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, padding[0], padding[1],
        std::max(0.0, gridWidth - padding[0] - padding[2]),
        std::max(0.0, gridHeight - padding[1] - padding[3]));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
