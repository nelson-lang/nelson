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
#include "GOColorBar.hpp"
#include "GOColorBarGeometry.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOTiledChartLayout.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isLayoutOptions(GraphicsObject* object)
{
    return object && object->isType(L"layoutoptions");
}
//=============================================================================
static std::wstring
layoutTileFromOptions(GraphicsObject* layoutOptions)
{
    if (!isLayoutOptions(layoutOptions)) {
        return L"";
    }
    GOArrayOfProperty* tileProp = dynamic_cast<GOArrayOfProperty*>(
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
static double
layoutTileNumberFromOptions(GraphicsObject* layoutOptions)
{
    if (!isLayoutOptions(layoutOptions)) {
        return 0.0;
    }
    GOArrayOfProperty* tileProp = dynamic_cast<GOArrayOfProperty*>(
        layoutOptions->findProperty(GO_TILE_PROPERTY_NAME_STR, false));
    if (!tileProp) {
        return 0.0;
    }
    ArrayOf value = tileProp->get();
    if (!value.isNumeric() || value.isComplex() || value.elementCount() != 1) {
        return 0.0;
    }
    std::vector<double> current = value.asDoubleVector();
    return current.empty() ? 0.0 : current[0];
}
//=============================================================================
static void
setDefaultLayoutTileFromPeer(
    GraphicsObject* layoutOptions, GOTiledChartLayout* layout, int64 peerHandle)
{
    if (!isLayoutOptions(layoutOptions) || !layout) {
        return;
    }
    GOArrayOfProperty* tileProp = dynamic_cast<GOArrayOfProperty*>(
        layoutOptions->findProperty(GO_TILE_PROPERTY_NAME_STR, false));
    if (!tileProp) {
        return;
    }
    ArrayOf value = tileProp->get();
    if (!value.isNumeric() || value.isComplex() || value.elementCount() != 1) {
        return;
    }
    std::vector<double> current = value.asDoubleVector();
    if (current.empty() || current[0] != 0.0) {
        return;
    }
    GOTiledChartLayout::TileSpanInfo info = layout->getTileSpanInfo(peerHandle);
    if (info.tile > 0) {
        tileProp->set(ArrayOf::doubleScalar((double)info.tile));
    }
}
//=============================================================================
static void
setObjectPosition(GraphicsObject* object, const std::vector<double>& pos)
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
static bool
applyLayoutEdgePosition(GOColorBar* colorBar, const std::wstring& edge)
{
    GraphicsObject* peer = findGraphicsObject(colorBar->getPeerAxesHandle(), false);
    if (!peer) {
        return false;
    }
    std::vector<double> peerPos = peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (peerPos.size() < 4) {
        return false;
    }
    peerPos = tiledColorBarPeerPosition(peer, peerPos);
    setObjectPosition(peer, peerPos);

    double thickV = 0.0186219739292365;
    double thickH = 0.0309597523219815;
    double gap = 0.0248292985723153;
    if (edge == L"east") {
        colorBar->setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, peerPos[0] + peerPos[2] + gap, 0.11, thickV, 0.815);
        return true;
    }
    if (edge == L"west") {
        colorBar->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR,
            std::max(0.0, peerPos[0] - gap - thickV), 0.11, thickV, 0.815);
        return true;
    }
    if (edge == L"north") {
        colorBar->setFourVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, 0.13, peerPos[1] + peerPos[3] + gap, 0.775, thickH);
        return true;
    }
    if (edge == L"south") {
        colorBar->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13,
            std::max(0.0, peerPos[1] - gap - thickH), 0.775, thickH);
        return true;
    }
    return false;
}
//=============================================================================
static bool
applyLayoutTilePosition(GOColorBar* colorBar, GraphicsObject* layoutOptions)
{
    if (layoutTileNumberFromOptions(layoutOptions) <= 0.0) {
        return false;
    }
    GraphicsObject* peer = findGraphicsObject(colorBar->getPeerAxesHandle(), false);
    if (!peer) {
        return false;
    }
    std::vector<double> peerPos = peer->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (peerPos.size() < 4) {
        return false;
    }
    double width = 0.0285714285714286;
    double x = peerPos[0] + peerPos[2] * 0.455005624296963;
    colorBar->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, x, peerPos[1], width, peerPos[3]);
    if (peerPos[2] < 0.5
        && colorBar->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR) == 9.0) {
        colorBar->setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 8.1);
    }
    return true;
}
//=============================================================================
static GOTiledChartLayout*
findLayoutContainingChild(GraphicsObject* candidate, int64 childHandle)
{
    if (!candidate || candidate->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return nullptr;
    }
    GOTiledChartLayout* layout = dynamic_cast<GOTiledChartLayout*>(candidate);
    if (!layout) {
        return nullptr;
    }
    GOTiledChartLayout::TileSpanInfo info = layout->getTileSpanInfo(childHandle);
    if (info.tile != 0 || layout->isEdgeTileHandle(childHandle)) {
        return layout;
    }
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        layout->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return nullptr;
    }
    for (int64 handle : children->data()) {
        if (handle == childHandle) {
            return layout;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        GOTiledChartLayout* nested = findLayoutContainingChild(child, childHandle);
        if (nested) {
            return nested;
        }
    }
    return nullptr;
}
//=============================================================================
static GOTiledChartLayout*
findLayoutForPeerAxes(GOColorBar* colorBar)
{
    if (!colorBar || colorBar->getPeerAxesHandle() < HANDLE_OFFSET_OBJECT
        || isDeletedGraphicsObject(colorBar->getPeerAxesHandle())) {
        return nullptr;
    }
    GOFigure* fig = colorBar->getParentFigure();
    if (!fig) {
        return nullptr;
    }
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return nullptr;
    }
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        GOTiledChartLayout* layout
            = findLayoutContainingChild(child, colorBar->getPeerAxesHandle());
        if (layout) {
            return layout;
        }
    }
    return nullptr;
}
//=============================================================================
void
GOColorBar::updateLayoutTile()
{
    GOGObjectsProperty* layoutProperty
        = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_LAYOUT_PROPERTY_NAME_STR, false));
    if (!layoutProperty || layoutProperty->data().empty()) {
        return;
    }
    GraphicsObject* layoutOptions = findGraphicsObject(layoutProperty->data()[0], false);
    GOTiledChartLayout* layout = findLayoutForPeerAxes(this);
    if (!layout) {
        return;
    }
    int64 thisHandle = findGraphicsObjectHandle(this);
    if (thisHandle < HANDLE_OFFSET_OBJECT) {
        return;
    }
    const std::wstring edges[] = { L"east", L"west", L"north", L"south" };
    bool removed = false;
    for (const auto& edge : edges) {
        if (layout->getEdgeTile(edge) == thisHandle) {
            layout->registerEdgeTile(edge, -1);
            removed = true;
        }
    }
    if (findStringProperty(GO_LOCATION_PROPERTY_NAME_STR) != L"layout") {
        if (removed) {
            layout->updateChildPositions();
        }
        return;
    }
    setDefaultLayoutTileFromPeer(layoutOptions, layout, getPeerAxesHandle());
    std::wstring tile = layoutTileFromOptions(layoutOptions);
    if (tile == L"east" || tile == L"west" || tile == L"north" || tile == L"south") {
        layout->updateChildPositions();
        applyLayoutEdgePosition(this, tile);
        return;
    } else if (removed) {
        layout->updateChildPositions();
    }
    applyLayoutTilePosition(this, layoutOptions);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
