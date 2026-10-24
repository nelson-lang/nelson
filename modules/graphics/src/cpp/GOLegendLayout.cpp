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
#include "GOLegendLayout.hpp"
#include "GOBubbleLegend.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOLegend.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOTiledChartLayout.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    isLayoutOptions(GraphicsObject* object)
    {
        return object && object->isType(L"layoutoptions");
    }
    //=============================================================================
    GraphicsObject*
    layoutOptionsObject(GraphicsObject* object)
    {
        auto* layoutProperty = object ? dynamic_cast<GOGObjectsProperty*>(object->findProperty(
                                            GO_LAYOUT_PROPERTY_NAME_STR, false))
                                      : nullptr;
        if (!layoutProperty || layoutProperty->data().empty()) {
            return nullptr;
        }
        return findGraphicsObject(layoutProperty->data()[0], false);
    }
    //=============================================================================
    std::wstring
    layoutTileFromOptions(GraphicsObject* layoutOptions)
    {
        if (!isLayoutOptions(layoutOptions)) {
            return L"";
        }
        auto* tileProp = dynamic_cast<GOArrayOfProperty*>(
            layoutOptions->findProperty(GO_TILE_PROPERTY_NAME_STR, false));
        if (!tileProp) {
            return L"";
        }
        ArrayOf value = tileProp->get();
        if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
            std::wstring tile = value.asWideString();
            StringHelpers::to_lower(tile);
            return tile;
        }
        return L"";
    }
    //=============================================================================
    void
    setLayoutTile(GraphicsObject* layoutOptions, const std::wstring& tile)
    {
        auto* tileProp = isLayoutOptions(layoutOptions)
            ? layoutOptions->findProperty(GO_TILE_PROPERTY_NAME_STR, false)
            : nullptr;
        if (tileProp) {
            tileProp->set(ArrayOf::characterArray(tile));
        }
    }
    //=============================================================================
    bool
    isEdgeTile(const std::wstring& tile)
    {
        return tile == L"east" || tile == L"west" || tile == L"north" || tile == L"south";
    }
    //=============================================================================
    int64
    peerAxesHandle(GraphicsObject* object)
    {
        if (auto* legend = dynamic_cast<GOLegend*>(object)) {
            return legend->getPeerAxesHandle();
        }
        if (auto* bubbleLegend = dynamic_cast<GOBubbleLegend*>(object)) {
            return bubbleLegend->getPeerAxesHandle();
        }
        return HANDLE_UNASSIGNED_OBJECT;
    }
    //=============================================================================
    std::pair<double, double>
    preferredSize(GraphicsObject* object, const std::pair<double, double>& fallback)
    {
        if (auto* legend = dynamic_cast<GOLegend*>(object)) {
            return legend->preferredSizeInPixels();
        }
        if (auto* bubbleLegend = dynamic_cast<GOBubbleLegend*>(object)) {
            return bubbleLegend->preferredSizeInPixels();
        }
        return fallback;
    }
    //=============================================================================
    GOTiledChartLayout*
    findLayoutContainingChild(GraphicsObject* candidate, int64 childHandle)
    {
        if (!candidate || candidate->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            return nullptr;
        }
        auto* layout = dynamic_cast<GOTiledChartLayout*>(candidate);
        if (!layout) {
            return nullptr;
        }
        GOTiledChartLayout::TileSpanInfo info = layout->getTileSpanInfo(childHandle);
        if (info.tile != 0 || layout->isEdgeTileHandle(childHandle)) {
            return layout;
        }
        auto* children = dynamic_cast<GOGObjectsProperty*>(
            layout->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            return nullptr;
        }
        for (int64 handle : children->data()) {
            if (handle == childHandle) {
                return layout;
            }
            if (auto* nested
                = findLayoutContainingChild(findGraphicsObject(handle, false), childHandle)) {
                return nested;
            }
        }
        return nullptr;
    }
    //=============================================================================
    GOTiledChartLayout*
    findLayoutForPeerAxes(GraphicsObject* object, int64 peerHandle)
    {
        if (!object || peerHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(peerHandle)) {
            return nullptr;
        }
        GOFigure* fig = object->getParentFigure();
        auto* children = fig ? dynamic_cast<GOGObjectsProperty*>(
                                   fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
                             : nullptr;
        if (!children) {
            return nullptr;
        }
        for (int64 handle : children->data()) {
            if (auto* layout
                = findLayoutContainingChild(findGraphicsObject(handle, false), peerHandle)) {
                return layout;
            }
        }
        return nullptr;
    }
    //=============================================================================
    std::vector<GraphicsObject*>
    edgeObjects(GraphicsObject* object, int64 peerHandle, const std::wstring& edge)
    {
        std::vector<GraphicsObject*> result;
        GOFigure* fig = object ? object->getParentFigure() : nullptr;
        auto* children = fig ? dynamic_cast<GOGObjectsProperty*>(
                                   fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
                             : nullptr;
        if (!children) {
            return result;
        }
        for (int64 handle : children->data()) {
            GraphicsObject* child = findGraphicsObject(handle, false);
            if (!child || peerAxesHandle(child) != peerHandle) {
                continue;
            }
            if (layoutTileFromOptions(layoutOptionsObject(child)) == edge) {
                result.push_back(child);
            }
        }
        return result;
    }
    //=============================================================================
    void
    setNormalizedPosition(GraphicsObject* object, double x, double y, double w, double h)
    {
        object->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, x, y, w, h);
        object->setRestrictedStringDefault(
            GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    }
    //=============================================================================
    void
    setAxesPosition(GraphicsObject* object, const std::vector<double>& pos)
    {
        if (!object || pos.size() < 4) {
            return;
        }
        object->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, pos[0], pos[1], pos[2], pos[3]);
        if (object->haveProperty(GO_INNER_POSITION_PROPERTY_NAME_STR)) {
            object->setFourVectorDefault(
                GO_INNER_POSITION_PROPERTY_NAME_STR, pos[0], pos[1], pos[2], pos[3]);
        }
        if (object->haveProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR)) {
            object->setFourVectorDefault(
                GO_OUTER_POSITION_PROPERTY_NAME_STR, pos[0], pos[1], pos[2], pos[3]);
        }
    }
    //=============================================================================
    void
    applyVerticalStack(GraphicsObject* object, int64 peerHandle, const std::wstring& edge,
        const std::pair<double, double>& fallback, GOFigure* fig)
    {
        GraphicsObject* peer = findGraphicsObject(peerHandle, false);
        if (!peer || !fig) {
            return;
        }
        std::vector<double> peerPos = peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (peerPos.size() < 4) {
            return;
        }
        std::vector<GraphicsObject*> objects = edgeObjects(object, peerHandle, edge);
        if (objects.empty()) {
            objects.push_back(object);
        }
        std::stable_sort(objects.begin(), objects.end(), [](GraphicsObject* a, GraphicsObject* b) {
            bool aBubble = a && a->getType() == GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR;
            bool bBubble = b && b->getType() == GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR;
            return aBubble && !bBubble;
        });
        double figW = std::max(1, fig->getLayoutWidth());
        double figH = std::max(1, fig->getLayoutHeight());
        double gapX = edge == L"east" ? 21.333 / figW : 8.0 / figW;
        double gapY = 8.0 / figH;
        if (edge == L"east") {
            peerPos = { 0.123915737298637, 0.11, 0.726641883046506, 0.815 };
        }
        std::vector<std::pair<double, double>> sizes;
        sizes.reserve(objects.size());
        double maxW = 0.0;
        double totalH = gapY * std::max<int>(0, (int)objects.size() - 1);
        for (auto* item : objects) {
            auto size = preferredSize(item, fallback);
            if (item && item->getType() == GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR) {
                size.first = std::max(size.first, 129.0);
                size.second = std::max(size.second, 224.0);
            }
            double w = std::max(32.0, size.first) / figW;
            double h = std::max(24.0, size.second) / figH;
            sizes.push_back({ w, h });
            maxW = std::max(maxW, w);
            totalH += h;
        }
        if (edge == L"east") {
            double rightMargin = 10.0 / figW;
            double maxPeerRight = 1.0 - maxW - gapX - rightMargin;
            peerPos[2] = std::max(0.1, std::min(peerPos[2], maxPeerRight - peerPos[0]));
            setAxesPosition(peer, peerPos);
        }
        double x = edge == L"east" ? peerPos[0] + peerPos[2] + gapX : peerPos[0] - maxW - gapX;
        x = std::max(0.0, std::min(1.0 - maxW, x));
        double y = peerPos[1] + (peerPos[3] - totalH) / 2.0;
        y = std::max(0.0, y);
        for (int k = (int)objects.size() - 1; k >= 0; --k) {
            setNormalizedPosition(objects[k], x, y, sizes[k].first, sizes[k].second);
            y += sizes[k].second + gapY;
        }
    }
    //=============================================================================
    void
    applyHorizontalStack(GraphicsObject* object, int64 peerHandle, const std::wstring& edge,
        const std::pair<double, double>& fallback, GOFigure* fig)
    {
        GraphicsObject* peer = findGraphicsObject(peerHandle, false);
        if (!peer || !fig) {
            return;
        }
        std::vector<double> peerPos = peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (peerPos.size() < 4) {
            return;
        }
        std::vector<GraphicsObject*> objects = edgeObjects(object, peerHandle, edge);
        if (objects.empty()) {
            objects.push_back(object);
        }
        double figW = std::max(1, fig->getLayoutWidth());
        double figH = std::max(1, fig->getLayoutHeight());
        double gapX = 8.0 / figW;
        double gapY = 8.0 / figH;
        std::vector<std::pair<double, double>> sizes;
        sizes.reserve(objects.size());
        double totalW = gapX * std::max<int>(0, (int)objects.size() - 1);
        double maxH = 0.0;
        for (auto* item : objects) {
            auto size = preferredSize(item, fallback);
            double w = std::max(32.0, size.first) / figW;
            double h = std::max(24.0, size.second) / figH;
            sizes.push_back({ w, h });
            totalW += w;
            maxH = std::max(maxH, h);
        }
        double x = peerPos[0] + (peerPos[2] - totalW) / 2.0;
        double y = edge == L"north" ? peerPos[1] + peerPos[3] + gapY : peerPos[1] - maxH - gapY;
        x = std::max(0.0, x);
        y = std::max(0.0, std::min(1.0 - maxH, y));
        for (size_t k = 0; k < objects.size(); ++k) {
            setNormalizedPosition(objects[k], x, y, sizes[k].first, maxH);
            x += sizes[k].first + gapX;
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
applyLegendLayoutTile(GraphicsObject* object, int64 peerAxesHandle,
    const std::pair<double, double>& preferredSizeInPixels)
{
    GraphicsObject* layoutOptions = layoutOptionsObject(object);
    std::wstring tile = layoutTileFromOptions(layoutOptions);
    if (tile.empty() && object
        && object->findStringProperty(GO_LOCATION_PROPERTY_NAME_STR) == L"layout") {
        tile = L"east";
        setLayoutTile(layoutOptions, tile);
    }
    if (!isEdgeTile(tile)) {
        return false;
    }
    GOTiledChartLayout* layout = findLayoutForPeerAxes(object, peerAxesHandle);
    if (!layout) {
        return false;
    }
    int64 objectHandle = findGraphicsObjectHandle(object);
    if (objectHandle < HANDLE_OFFSET_OBJECT) {
        return false;
    }
    if (layout->getEdgeTile(tile) == objectHandle) {
        layout->registerEdgeTile(tile, -1);
    }
    layout->updateChildPositions();
    GOFigure* fig = object->getParentFigure();
    if (tile == L"east" || tile == L"west") {
        applyVerticalStack(object, peerAxesHandle, tile, preferredSizeInPixels, fig);
    } else {
        applyHorizontalStack(object, peerAxesHandle, tile, preferredSizeInPixels, fig);
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
