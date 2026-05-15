//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <algorithm>
#include <limits>
#include "GOTiledChartLayout.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOText.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOTiledChartLayout::getType()
{
    return GO_PROPERTY_VALUE_TILED_LAYOUT_STR;
}
//=============================================================================
GOTiledChartLayout::GOTiledChartLayout()
{
    registerProperties();

    // Initialise defaults
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setTwoVectorDefault(GO_GRID_SIZE_PROPERTY_NAME_STR, 1.0, 1.0);
    setRestrictedStringDefault(GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FIXED_STR);
    setRestrictedStringDefault(GO_TILE_SPACING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LOOSE_STR);
    setRestrictedStringDefault(GO_PADDING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LOOSE_STR);
    setRestrictedStringDefault(GO_TILE_INDEXING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ROWMAJOR_STR);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setRestrictedStringDefault(
        GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);

    m_tileOccupancy.assign(1, -1); // 1×1 grid initially
    m_nextTileIndex = 1;
}
//=============================================================================
GOTiledChartLayout::~GOTiledChartLayout()
{
    const std::wstring properties[] = { GO_TITLE_PROPERTY_NAME_STR, GO_SUBTITLE_PROPERTY_NAME_STR,
        GO_X_LABEL_PROPERTY_NAME_STR, GO_Y_LABEL_PROPERTY_NAME_STR };
    for (const auto& propertyName : properties) {
        GOGObjectsProperty* prop
            = dynamic_cast<GOGObjectsProperty*>(findProperty(propertyName, false));
        if (!prop) {
            continue;
        }
        std::vector<int64> handles = prop->data();
        prop->data(std::vector<int64>());
        for (int64 handle : handles) {
            if (handle != -1 && !isDeletedGraphicsObject(handle)) {
                deleteGraphicsObject(handle, false, false);
            }
        }
    }
}
//=============================================================================
void
GOTiledChartLayout::registerProperties()
{
    // Identification
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);

    // Callback / deletion
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_GRID_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);

    // Layout
    {
        static const wchar_t* tileArrangeVals[]
            = { L"fixed", L"flow", L"vertical", L"horizontal", nullptr };
        registerProperty(new GORestrictedStringProperty(tileArrangeVals),
            GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR, false);
    }
    registerProperty(new GOTwoVectorProperty, GO_GRID_SIZE_PROPERTY_NAME_STR);
    {
        static const wchar_t* spacingVals[] = { L"loose", L"compact", L"tight", L"none", nullptr };
        registerProperty(
            new GORestrictedStringProperty(spacingVals), GO_TILE_SPACING_PROPERTY_NAME_STR);
    }
    {
        static const wchar_t* paddingVals[] = { L"loose", L"compact", L"tight", nullptr };
        registerProperty(new GORestrictedStringProperty(paddingVals), GO_PADDING_PROPERTY_NAME_STR);
    }
    {
        static const wchar_t* indexingVals[] = { L"rowmajor", L"columnmajor", nullptr };
        registerProperty(
            new GORestrictedStringProperty(indexingVals), GO_TILE_INDEXING_PROPERTY_NAME_STR);
    }

    // Label / title sub-objects
    registerProperty(new GOGObjectsProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_SUBTITLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_X_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_Y_LABEL_PROPERTY_NAME_STR);

    // Position
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    {
        static const wchar_t* pcVals[] = { L"outerposition", L"innerposition", nullptr };
        registerProperty(
            new GORestrictedStringProperty(pcVals), GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    }
    {
        static const wchar_t* unitsVals[] = { L"normalized", L"inches", L"centimeters",
            L"characters", L"points", L"pixels", nullptr };
        registerProperty(new GORestrictedStringProperty(unitsVals), GO_UNITS_PROPERTY_NAME_STR);
    }

    // Parent / child
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    {
        static const wchar_t* hvVals[] = { L"on", L"off", L"callback", nullptr };
        registerProperty(
            new GORestrictedStringProperty(hvVals), GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    }

    // Misc
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_TOOL_BAR_PROPERTY_NAME_STR);

    sortProperties();
}
//=============================================================================
void
GOTiledChartLayout::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    const std::wstring properties[] = { GO_TITLE_PROPERTY_NAME_STR, GO_SUBTITLE_PROPERTY_NAME_STR,
        GO_X_LABEL_PROPERTY_NAME_STR, GO_Y_LABEL_PROPERTY_NAME_STR };
    for (const auto& propertyName : properties) {
        GOGObjectsProperty* prop
            = dynamic_cast<GOGObjectsProperty*>(findProperty(propertyName, false));
        if (!prop) {
            continue;
        }
        for (int64 handle : prop->data()) {
            if (handle == -1 || isDeletedGraphicsObject(handle)) {
                continue;
            }
            GraphicsObject* child = findGraphicsObject(handle, false);
            if (child) {
                child->paintMe(gc);
            }
        }
    }

    GOGObjectsProperty* childrenProperty
        = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!childrenProperty) {
        return;
    }
    for (int64 handle : childrenProperty->data()) {
        if (handle == -1 || isDeletedGraphicsObject(handle)) {
            continue;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child && child->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            child->paintMe(gc);
        }
    }
}
//=============================================================================
void
GOTiledChartLayout::updateState()
{
    std::wstring arrangement = findStringProperty(GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR);
    if (arrangement == GO_PROPERTY_VALUE_FLOW_STR) {
        reflowTiles();
    } else {
        rebuildOccupancy();
    }
    updateChildPositions();
}
//=============================================================================
int
GOTiledChartLayout::getGridRows() const
{
    std::vector<double> gs = const_cast<GOTiledChartLayout*>(this)->findVectorDoubleProperty(
        GO_GRID_SIZE_PROPERTY_NAME_STR);
    return (gs.size() >= 1) ? std::max(1, (int)gs[0]) : 1;
}
//=============================================================================
int
GOTiledChartLayout::getGridCols() const
{
    std::vector<double> gs = const_cast<GOTiledChartLayout*>(this)->findVectorDoubleProperty(
        GO_GRID_SIZE_PROPERTY_NAME_STR);
    return (gs.size() >= 2) ? std::max(1, (int)gs[1]) : 1;
}
//=============================================================================
bool
GOTiledChartLayout::hasTileChildren() const
{
    for (const auto& kv : m_tileSpan) {
        if (kv.first != -1 && !isDeletedGraphicsObject(kv.first)) {
            return true;
        }
    }
    for (const auto& kv : m_edgeTiles) {
        if (kv.second != -1 && !isDeletedGraphicsObject(kv.second)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
void
GOTiledChartLayout::setTileArrangementInternal(const std::wstring& arrangement)
{
    setRestrictedStringDefault(GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR, arrangement);
}
//=============================================================================
void
GOTiledChartLayout::setGridSizeInternal(int rows, int cols, bool fireCallback)
{
    rows = std::max(1, rows);
    cols = std::max(1, cols);
    int oldRows = getGridRows();
    int oldCols = getGridCols();
    if (oldRows == rows && oldCols == cols) {
        rebuildOccupancy();
        return;
    }

    GOTwoVectorProperty* gsp
        = dynamic_cast<GOTwoVectorProperty*>(findProperty(GO_GRID_SIZE_PROPERTY_NAME_STR));
    if (gsp) {
        gsp->value((double)rows, (double)cols);
    }
    rebuildOccupancy();

    if (fireCallback) {
        GOCallbackProperty* cb = dynamic_cast<GOCallbackProperty*>(
            findProperty(GO_GRID_SIZE_CHANGED_FCN_PROPERTY_NAME_STR, false));
        if (cb) {
            cb->executeNow(this);
        }
    }
}
//=============================================================================
void
GOTiledChartLayout::setGridSizeFromUser(int rows, int cols)
{
    if (hasTileChildren()) {
        Error(_W("GridSize can only be set when all tiles in the layout are empty."));
    }
    setTileArrangementInternal(GO_PROPERTY_VALUE_FIXED_STR);
    setGridSizeInternal(rows, cols);
    m_nextTileIndex = 1;
}
//=============================================================================
static go_handle
createLayoutTextObject(go_handle layoutHandle, const std::wstring& propertyName)
{
    GOText* text = new GOText;
    go_handle textHandle = assignGraphicsObject(text);
    text->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, layoutHandle);
    text->setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    text->setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
    text->setScalarDoubleDefault(GO_MARGIN_PROPERTY_NAME_STR, 3.0);
    if (propertyName == GO_TITLE_PROPERTY_NAME_STR) {
        text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.5, 1.0, 0.0);
        text->setRestrictedStringDefault(
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
        text->setRestrictedStringDefault(
            GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOLD_STR);
    } else if (propertyName == GO_SUBTITLE_PROPERTY_NAME_STR) {
        text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.5, 0.96, 0.0);
        text->setRestrictedStringDefault(
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
    } else if (propertyName == GO_X_LABEL_PROPERTY_NAME_STR) {
        text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.5, 0.0, 0.0);
        text->setRestrictedStringDefault(
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR);
    } else if (propertyName == GO_Y_LABEL_PROPERTY_NAME_STR) {
        text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.0, 0.5, 0.0);
        text->setRestrictedStringDefault(
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
        text->setScalarDoubleDefault(GO_ROTATION_PROPERTY_NAME_STR, 90.0);
    }
    text->updateState();
    return textHandle;
}
//=============================================================================
void
GOTiledChartLayout::initializeTextObjects(go_handle layoutHandle)
{
    const std::wstring properties[] = { GO_TITLE_PROPERTY_NAME_STR, GO_SUBTITLE_PROPERTY_NAME_STR,
        GO_X_LABEL_PROPERTY_NAME_STR, GO_Y_LABEL_PROPERTY_NAME_STR };
    for (const auto& propertyName : properties) {
        GOGObjectsProperty* prop
            = dynamic_cast<GOGObjectsProperty*>(findProperty(propertyName, false));
        if (prop && prop->data().empty()) {
            prop->data(std::vector<int64> { createLayoutTextObject(layoutHandle, propertyName) });
        }
    }
}
//=============================================================================
// Spacing helpers
double
GOTiledChartLayout::getSpacingFraction()
{
    std::wstring ts = findStringProperty(GO_TILE_SPACING_PROPERTY_NAME_STR);
    if (ts == GO_PROPERTY_VALUE_COMPACT_STR) {
        return SPACING_COMPACT;
    }
    if (ts == GO_PROPERTY_VALUE_TIGHT_STR || ts == L"none") {
        return SPACING_TIGHT;
    }
    return SPACING_LOOSE;
}
//=============================================================================
double
GOTiledChartLayout::getPaddingFraction()
{
    std::wstring pd = findStringProperty(GO_PADDING_PROPERTY_NAME_STR);
    if (pd == GO_PROPERTY_VALUE_COMPACT_STR) {
        return PADDING_COMPACT;
    }
    if (pd == GO_PROPERTY_VALUE_TIGHT_STR) {
        return PADDING_TIGHT;
    }
    return PADDING_LOOSE;
}
//=============================================================================
static bool
layoutTextHasString(GOTiledChartLayout* layout, const std::wstring& propertyName)
{
    if (!layout) {
        return false;
    }
    GOGObjectsProperty* prop
        = dynamic_cast<GOGObjectsProperty*>(layout->findProperty(propertyName, false));
    if (!prop || prop->data().empty()) {
        return false;
    }
    int64 handle = prop->data()[0];
    if (handle == -1 || isDeletedGraphicsObject(handle)) {
        return false;
    }
    GraphicsObject* text = findGraphicsObject(handle, false);
    if (!text || text->getType() != GO_PROPERTY_VALUE_TEXT_STR) {
        return false;
    }
    return !text->findStringProperty(GO_STRING_PROPERTY_NAME_STR).empty();
}
//=============================================================================
std::pair<int, int>
GOTiledChartLayout::tileIndexToRowCol(int tileIndex)
{
    int m = getGridRows();
    int n = getGridCols();

    std::wstring indexing = findStringProperty(GO_TILE_INDEXING_PROPERTY_NAME_STR);
    int idx = tileIndex - 1; // 0-based
    if (idx < 0 || idx >= m * n) {
        return { -1, -1 };
    }
    if (indexing == GO_PROPERTY_VALUE_COLUMNMAJOR_STR) {
        int col = idx / m + 1;
        int row = idx % m + 1;
        return { row, col };
    }
    // rowmajor (default)
    int row = idx / n + 1;
    int col = idx % n + 1;
    return { row, col };
}
//=============================================================================
int
GOTiledChartLayout::rowColToTileIndex(int row, int col)
{
    int m = getGridRows();
    int n = getGridCols();

    if (row < 1 || row > m || col < 1 || col > n) {
        return -1;
    }
    std::wstring indexing = findStringProperty(GO_TILE_INDEXING_PROPERTY_NAME_STR);
    if (indexing == GO_PROPERTY_VALUE_COLUMNMAJOR_STR) {
        return (col - 1) * m + row;
    }
    return (row - 1) * n + col;
}
//=============================================================================
int
GOTiledChartLayout::findNextEmptyTile(int spanRows, int spanCols)
{
    rebuildOccupancy();
    int m = getGridRows();
    int n = getGridCols();
    int total = m * n;

    for (int t = m_nextTileIndex; t <= total; ++t) {
        auto [row, col] = tileIndexToRowCol(t);
        if (row < 1 || col < 1) {
            continue;
        }
        // Check that all tiles in the spanRows×spanCols block are free
        bool allFree = true;
        for (int dr = 0; dr < spanRows && allFree; ++dr) {
            for (int dc = 0; dc < spanCols && allFree; ++dc) {
                int idx = rowColToTileIndex(row + dr, col + dc);
                if (idx < 1 || idx > (int)m_tileOccupancy.size()) {
                    allFree = false;
                } else if (m_tileOccupancy[idx - 1] != -1) {
                    allFree = false;
                }
            }
        }
        if (allFree) {
            return t;
        }
    }
    return total + 1; // Overflow — caller should extend grid or wrap
}
//=============================================================================
void
GOTiledChartLayout::addLayoutChild(go_handle childHandle)
{
    GOGObjectsProperty* childrenProperty
        = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!childrenProperty) {
        return;
    }
    std::vector<int64> children = childrenProperty->data();
    if (std::find(children.begin(), children.end(), childHandle) == children.end()) {
        children.push_back(childHandle);
        childrenProperty->data(children);
    }
}
//=============================================================================
void
GOTiledChartLayout::removeLayoutChild(go_handle childHandle)
{
    GOGObjectsProperty* childrenProperty
        = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!childrenProperty) {
        return;
    }
    std::vector<int64> children = childrenProperty->data();
    children.erase(std::remove(children.begin(), children.end(), childHandle), children.end());
    childrenProperty->data(children);
}
//=============================================================================
void
GOTiledChartLayout::rebuildOccupancy()
{
    int m = getGridRows();
    int n = getGridCols();
    int total = m * n;
    m_tileOccupancy.assign(total, -1);

    for (const auto& kv : m_tileSpan) {
        go_handle childHandle = kv.first;
        if (childHandle == -1 || isDeletedGraphicsObject(childHandle)) {
            continue;
        }
        const TileSpanInfo& info = kv.second;
        auto [startRow, startCol] = tileIndexToRowCol(info.tile);
        if (startRow < 1 || startCol < 1) {
            continue;
        }
        for (int dr = 0; dr < info.spanRows; ++dr) {
            for (int dc = 0; dc < info.spanCols; ++dc) {
                int idx = rowColToTileIndex(startRow + dr, startCol + dc);
                if (idx >= 1 && idx <= total) {
                    m_tileOccupancy[idx - 1] = childHandle;
                }
            }
        }
    }
}
//=============================================================================
void
GOTiledChartLayout::registerChildTile(go_handle childHandle, int tile, int spanRows, int spanCols)
{
    m_tileSpan[childHandle] = TileSpanInfo { tile, std::max(1, spanRows), std::max(1, spanCols) };
    addLayoutChild(childHandle);
    rebuildOccupancy();
}
//=============================================================================
void
GOTiledChartLayout::unregisterChild(go_handle childHandle)
{
    removeLayoutChild(childHandle);
    for (auto it = m_edgeTiles.begin(); it != m_edgeTiles.end();) {
        if (it->second == childHandle) {
            it = m_edgeTiles.erase(it);
        } else {
            ++it;
        }
    }
    m_tileSpan.erase(childHandle);
    rebuildOccupancy();
}
//=============================================================================
GOTiledChartLayout::TileSpanInfo
GOTiledChartLayout::getTileSpanInfo(go_handle childHandle) const
{
    auto it = m_tileSpan.find(childHandle);
    if (it == m_tileSpan.end()) {
        return TileSpanInfo { 0, 1, 1 };
    }
    return it->second;
}
//=============================================================================
go_handle
GOTiledChartLayout::findChildAtExactRegion(int tile, int spanRows, int spanCols) const
{
    for (const auto& kv : m_tileSpan) {
        if (kv.first == -1 || isDeletedGraphicsObject(kv.first)) {
            continue;
        }
        const TileSpanInfo& info = kv.second;
        if (info.tile == tile && info.spanRows == spanRows && info.spanCols == spanCols) {
            return kv.first;
        }
    }
    return -1;
}
//=============================================================================
go_handle
GOTiledChartLayout::findChildAtUpperLeftTile(int tile) const
{
    for (const auto& kv : m_tileSpan) {
        if (kv.first == -1 || isDeletedGraphicsObject(kv.first)) {
            continue;
        }
        if (kv.second.tile == tile) {
            return kv.first;
        }
    }
    return -1;
}
//=============================================================================
go_handle
GOTiledChartLayout::findChildCoveringTile(int tile) const
{
    auto [targetRow, targetCol] = const_cast<GOTiledChartLayout*>(this)->tileIndexToRowCol(tile);
    if (targetRow < 1 || targetCol < 1) {
        return -1;
    }
    for (const auto& kv : m_tileSpan) {
        if (kv.first == -1 || isDeletedGraphicsObject(kv.first)) {
            continue;
        }
        const TileSpanInfo& info = kv.second;
        auto [startRow, startCol]
            = const_cast<GOTiledChartLayout*>(this)->tileIndexToRowCol(info.tile);
        if (startRow < 1 || startCol < 1) {
            continue;
        }
        if (targetRow >= startRow && targetRow < startRow + info.spanRows && targetCol >= startCol
            && targetCol < startCol + info.spanCols) {
            return kv.first;
        }
    }
    return -1;
}
//=============================================================================
std::vector<go_handle>
GOTiledChartLayout::findChildrenOverlappingRegion(int tile, int spanRows, int spanCols) const
{
    std::vector<go_handle> overlapping;
    auto [startRow, startCol] = const_cast<GOTiledChartLayout*>(this)->tileIndexToRowCol(tile);
    if (startRow < 1 || startCol < 1) {
        return overlapping;
    }

    for (const auto& kv : m_tileSpan) {
        go_handle childHandle = kv.first;
        if (childHandle == -1 || isDeletedGraphicsObject(childHandle)) {
            continue;
        }
        const TileSpanInfo& info = kv.second;
        auto [childRow, childCol]
            = const_cast<GOTiledChartLayout*>(this)->tileIndexToRowCol(info.tile);
        if (childRow < 1 || childCol < 1) {
            continue;
        }
        bool rowsOverlap = startRow < childRow + info.spanRows && childRow < startRow + spanRows;
        bool colsOverlap = startCol < childCol + info.spanCols && childCol < startCol + spanCols;
        if (rowsOverlap && colsOverlap
            && std::find(overlapping.begin(), overlapping.end(), childHandle)
                == overlapping.end()) {
            overlapping.push_back(childHandle);
        }
    }
    return overlapping;
}
//=============================================================================
void
GOTiledChartLayout::ensureDynamicGridCanPlace(int spanRows, int spanCols)
{
    std::wstring arrangement = findStringProperty(GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR);
    if (arrangement == GO_PROPERTY_VALUE_FIXED_STR) {
        return;
    }

    int rows = getGridRows();
    int cols = getGridCols();
    if (arrangement == GO_PROPERTY_VALUE_VERTICAL_STR) {
        while (findNextEmptyTile(spanRows, spanCols) > rows * cols) {
            ++rows;
            setGridSizeInternal(rows, cols);
        }
        return;
    }
    if (arrangement == GO_PROPERTY_VALUE_HORIZONTAL_STR) {
        while (findNextEmptyTile(spanRows, spanCols) > rows * cols) {
            ++cols;
            setGridSizeInternal(rows, cols);
        }
        return;
    }

    int guard = 0;
    while (findNextEmptyTile(spanRows, spanCols) > rows * cols && guard < 128) {
        double nextRowsScore = std::abs(((double)(rows + 1) / (double)std::max(1, cols)) - 0.75);
        double nextColsScore = std::abs(((double)rows / (double)(cols + 1)) - 0.75);
        if (rows < spanRows || nextRowsScore < nextColsScore) {
            ++rows;
        } else {
            ++cols;
        }
        cols = std::max(cols, spanCols);
        rows = std::max(rows, spanRows);
        setGridSizeInternal(rows, cols);
        ++guard;
    }
}
//=============================================================================
void
GOTiledChartLayout::advanceNextTileIndex()
{
    ++m_nextTileIndex;
}
//=============================================================================
void
GOTiledChartLayout::registerEdgeTile(const std::wstring& edge, go_handle childHandle)
{
    m_edgeTiles[edge] = childHandle;
}
//=============================================================================
go_handle
GOTiledChartLayout::getEdgeTile(const std::wstring& edge) const
{
    auto it = m_edgeTiles.find(edge);
    if (it == m_edgeTiles.end()) {
        return -1;
    }
    return it->second;
}
//=============================================================================
bool
GOTiledChartLayout::isEdgeTileHandle(go_handle h) const
{
    for (auto& kv : m_edgeTiles) {
        if (kv.second == h) {
            return true;
        }
    }
    return false;
}
//=============================================================================
void
GOTiledChartLayout::updateChildPositions()
{
    int m = getGridRows();
    int n = getGridCols();

    std::vector<double> outerPos = findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (outerPos.size() < 4) {
        return;
    }

    double x0 = outerPos[0];
    double y0 = outerPos[1];
    double w = outerPos[2];
    double h = outerPos[3];

    bool hasTitleText = layoutTextHasString(this, GO_TITLE_PROPERTY_NAME_STR);
    bool hasSubtitleText = layoutTextHasString(this, GO_SUBTITLE_PROPERTY_NAME_STR);
    bool hasXLabelText = layoutTextHasString(this, GO_X_LABEL_PROPERTY_NAME_STR);
    bool hasYLabelText = layoutTextHasString(this, GO_Y_LABEL_PROPERTY_NAME_STR);

    double topReserve = 0.0;
    if (hasTitleText && hasSubtitleText) {
        topReserve = 0.10 * h;
    } else if (hasTitleText || hasSubtitleText) {
        topReserve = 0.06 * h;
    }
    double bottomReserve = hasXLabelText ? 0.08 * h : 0.0;
    double leftReserve = hasYLabelText ? 0.08 * w : 0.0;
    double rightReserve = 0.0;

    double contentX0 = x0 + leftReserve;
    double contentY0 = y0 + bottomReserve;
    double contentW = w - leftReserve - rightReserve;
    double contentH = h - topReserve - bottomReserve;
    if (contentW <= 0.0 || contentH <= 0.0) {
        return;
    }

    double spacing = getSpacingFraction();
    double padding = getPaddingFraction();

    std::wstring spacingMode = findStringProperty(GO_TILE_SPACING_PROPERTY_NAME_STR);
    double insetLeft = 0.13;
    double insetBottom = 0.11;
    double insetWidth = 0.775;
    double insetHeight = 0.815;
    if (spacingMode == GO_PROPERTY_VALUE_COMPACT_STR) {
        insetLeft = 0.075;
        insetBottom = 0.085;
        insetWidth = 0.90;
        insetHeight = 0.89;
    } else if (spacingMode == GO_PROPERTY_VALUE_TIGHT_STR || spacingMode == L"none") {
        insetLeft = 0.08;
        insetBottom = 0.09;
        insetWidth = 0.89;
        insetHeight = 0.88;
    }

    bool hasNorth = getEdgeTile(L"north") != -1 && !isDeletedGraphicsObject(getEdgeTile(L"north"));
    bool hasSouth = getEdgeTile(L"south") != -1 && !isDeletedGraphicsObject(getEdgeTile(L"south"));
    bool hasEast = getEdgeTile(L"east") != -1 && !isDeletedGraphicsObject(getEdgeTile(L"east"));
    bool hasWest = getEdgeTile(L"west") != -1 && !isDeletedGraphicsObject(getEdgeTile(L"west"));

    int totalRows = m + (hasNorth ? 1 : 0) + (hasSouth ? 1 : 0);
    int totalCols = n + (hasEast ? 1 : 0) + (hasWest ? 1 : 0);

    double tileW = (contentW - 2.0 * padding - (totalCols - 1) * spacing) / totalCols;
    double tileH = (contentH - 2.0 * padding - (totalRows - 1) * spacing) / totalRows;

    if (tileW <= 0.0 || tileH <= 0.0) {
        return;
    }

    int westOffset = hasWest ? 1 : 0;
    int southOffset = hasSouth ? 1 : 0;

    double centerLeft = contentX0 + padding + westOffset * (tileW + spacing);
    double centerBottom = contentY0 + padding + southOffset * (tileH + spacing);

    auto setChildPosition = [&](go_handle childHandle, double left, double bottom, double width,
                                double height) {
        if (isDeletedGraphicsObject(childHandle)) {
            return;
        }
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (!child) {
            return;
        }

        double innerLeft = left + width * insetLeft;
        double innerBottom = bottom + height * insetBottom;
        double innerWidth = width * insetWidth;
        double innerHeight = height * insetHeight;

        GOFourVectorProperty* outerPosProp = dynamic_cast<GOFourVectorProperty*>(
            child->findProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR, false));
        if (outerPosProp) {
            outerPosProp->value(left, bottom, width, height);
        }

        GOFourVectorProperty* innerPosProp = dynamic_cast<GOFourVectorProperty*>(
            child->findProperty(GO_INNER_POSITION_PROPERTY_NAME_STR, false));
        if (innerPosProp) {
            innerPosProp->value(innerLeft, innerBottom, innerWidth, innerHeight);
        }

        GOFourVectorProperty* posProp = dynamic_cast<GOFourVectorProperty*>(
            child->findProperty(GO_POSITION_PROPERTY_NAME_STR, false));
        if (posProp) {
            posProp->value(innerLeft, innerBottom, innerWidth, innerHeight);
        }

        GOGenericProperty* posModeProp
            = child->findProperty(GO_POSITION_MODE_PROPERTY_NAME_STR, false);
        if (posModeProp) {
            posModeProp->set(ArrayOf::characterArrayConstructor(GO_PROPERTY_VALUE_MANUAL_STR));
        }
    };

    for (auto& kv : m_tileSpan) {
        go_handle childHandle = kv.first;
        TileSpanInfo info = kv.second;

        if (isEdgeTileHandle(childHandle)) {
            continue;
        }

        auto [row, col] = tileIndexToRowCol(info.tile);
        if (row < 1 || col < 1) {
            continue;
        }

        double left = centerLeft + (col - 1) * (tileW + spacing);
        double bottom = centerBottom + (m - row - info.spanRows + 1) * (tileH + spacing);
        double width = info.spanCols * tileW + (info.spanCols - 1) * spacing;
        double height = info.spanRows * tileH + (info.spanRows - 1) * spacing;

        setChildPosition(childHandle, left, bottom, width, height);
    }

    double centerWidth = n * tileW + (n - 1) * spacing;
    double centerHeight = m * tileH + (m - 1) * spacing;

    if (hasNorth) {
        setChildPosition(getEdgeTile(L"north"), centerLeft, centerBottom + centerHeight + spacing,
            centerWidth, tileH);
    }
    if (hasSouth) {
        setChildPosition(
            getEdgeTile(L"south"), centerLeft, contentY0 + padding, centerWidth, tileH);
    }
    if (hasEast) {
        setChildPosition(getEdgeTile(L"east"), centerLeft + centerWidth + spacing, centerBottom,
            tileW, centerHeight);
    }
    if (hasWest) {
        setChildPosition(
            getEdgeTile(L"west"), contentX0 + padding, centerBottom, tileW, centerHeight);
    }
}
//=============================================================================
void
GOTiledChartLayout::reflowTiles()
{
    int nChildren = 0;
    int minRows = 1;
    int minCols = 1;
    int minCapacity = 1;
    for (const auto& kv : m_tileSpan) {
        if (kv.first == -1 || isDeletedGraphicsObject(kv.first) || isEdgeTileHandle(kv.first)) {
            continue;
        }
        ++nChildren;
        minRows = std::max(minRows, kv.second.spanRows);
        minCols = std::max(minCols, kv.second.spanCols);
        minCapacity = std::max(minCapacity, kv.second.tile);
    }
    if (nChildren == 0) {
        rebuildOccupancy();
        return;
    }

    int bestRows = minRows;
    int bestCols = minCols;
    double bestScore = std::numeric_limits<double>::infinity();
    double contentWidth = 1.0;
    double contentHeight = 1.0;
    std::vector<double> outerPos = findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (outerPos.size() >= 4 && outerPos[2] > 0.0 && outerPos[3] > 0.0) {
        contentWidth = outerPos[2];
        contentHeight = outerPos[3];
    }
    double targetRowsPerColumn = (4.0 / 3.0) * (contentHeight / contentWidth);
    int limit = std::max(nChildren + minRows + minCols + 2, minRows * minCols + nChildren + 2);
    for (int rows = minRows; rows <= limit; ++rows) {
        for (int cols = minCols; cols <= limit; ++cols) {
            if (rows * cols < nChildren || rows * cols < minCapacity) {
                continue;
            }
            double aspectScore = std::abs(((double)rows / (double)cols) - targetRowsPerColumn);
            double areaScore = 0.001 * (double)(rows * cols);
            double score = aspectScore + areaScore;
            if (score < bestScore) {
                bestScore = score;
                bestRows = rows;
                bestCols = cols;
            }
        }
    }

    setGridSizeInternal(bestRows, bestCols);

    updateChildPositions();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
