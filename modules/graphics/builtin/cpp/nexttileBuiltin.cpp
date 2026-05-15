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
#include "nexttileBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOAxis.hpp"
#include "GOTiledChartLayout.hpp"
#include "GOLayoutOptions.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringHelpers.hpp"
#include "LayoutHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static bool
isGraphicsHandleScalar(const ArrayOf& arg);
static int
positiveIntegerScalar(const ArrayOf& arg, const std::wstring& name);
static std::pair<int, int>
positiveSpanVector(const ArrayOf& arg);
static int64
findParentFigureHandleFromLayout(GraphicsObject* layout);
static void
addHandleToFigure(GOFigure* fig, int64 handle);
static bool
tryResolveLayoutHandle(const ArrayOf& arg, int64& layoutHandle, int64& figHandle, GOFigure*& fig,
    GOTiledChartLayout*& layout);
static int64
createFlowLayoutOnFigure(int64 figHandle, GOFigure* fig);
static GOTiledChartLayout*
resolveLayout(
    const ArrayOfVector& argIn, size_t& pos, int64& layoutHandle, int64& figHandle, GOFigure*& fig);
static int64
createAxesInTile(GOTiledChartLayout* layout, int64 figHandle, GOFigure* fig, int tileNum,
    int spanRows, int spanCols, const std::wstring& edgeTile = L"");
static void
makeCurrentAxesIfPossible(GOFigure* fig, go_handle handle);
static void
ensureRegionFits(GOTiledChartLayout* layout, int tileNum, int spanRows, int spanCols);
//=============================================================================
ArrayOfVector
nexttileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;

    size_t pos = 0;
    int64 layoutHandle = -1;
    int64 figHandle = -1;
    GOFigure* fig = nullptr;
    GOTiledChartLayout* layout = resolveLayout(argIn, pos, layoutHandle, figHandle, fig);

    if (!layout || !fig) {
        Error(_W("No tiled layout found."));
    }

    // Be tolerant if the layout handle was not consumed during resolveLayout.
    // This can happen depending on how the handle is marshalled from Nelson.
    if (pos < argIn.size()) {
        int64 retryLayoutHandle = -1;
        int64 retryFigHandle = -1;
        GOFigure* retryFig = nullptr;
        GOTiledChartLayout* retryLayout = nullptr;
        if (tryResolveLayoutHandle(
                argIn[pos], retryLayoutHandle, retryFigHandle, retryFig, retryLayout)) {
            layoutHandle = retryLayoutHandle;
            figHandle = retryFigHandle;
            fig = retryFig;
            layout = retryLayout;
            ++pos;
            fig->setCurrentTiledLayout(layoutHandle);
        }
    }

    int spanRows = 1;
    int spanCols = 1;
    int tileNum = -1;
    bool tileSpecified = false;
    bool spanSpecified = false;
    std::wstring edgeTile;

    if (pos < argIn.size()) {
        ArrayOf a = argIn[pos];
        if (isGraphicsHandleScalar(a)) {
            const auto* ptr = static_cast<const int64*>(a.getDataPointer());
            int64 h = ptr[0];
            GraphicsObject* go = findGraphicsObject(h, false);
            if (go && go->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
                layout = static_cast<GOTiledChartLayout*>(go);
                layoutHandle = h;
                figHandle = findParentFigureHandleFromLayout(layout);
                if (figHandle != -1) {
                    fig = findGOFigure(figHandle);
                    if (fig) {
                        fig->setCurrentTiledLayout(layoutHandle);
                    }
                }
                pos++;
            } else {
                Error(_W("Invalid tiled layout handle."));
            }
        } else if (a.isNumeric() && !a.isComplex()) {
            if (a.isScalar()) {
                tileNum = positiveIntegerScalar(a, L"tilelocation");
                tileSpecified = true;
                ++pos;
            } else {
                auto [rows, cols] = positiveSpanVector(a);
                spanRows = rows;
                spanCols = cols;
                spanSpecified = true;
                ++pos;
            }
        } else if (a.isScalarStringArray() || a.isRowVectorCharacterArray()) {
            std::wstring str = StringHelpers::to_lower_copy(a.getContentAsWideString());
            if (str == L"north" || str == L"south" || str == L"east" || str == L"west") {
                edgeTile = str;
                tileSpecified = true;
                ++pos;
            } else {
                Error(_W("Invalid tile location."));
            }
        } else {
            Error(_W("Invalid tile location."));
        }
    }

    if (pos < argIn.size()) {
        if (!edgeTile.empty()) {
            Error(_W("Span is not valid for edge tiles."));
        }
        auto [rows, cols] = positiveSpanVector(argIn[pos]);
        spanRows = rows;
        spanCols = cols;
        spanSpecified = true;
        ++pos;
    }

    if (pos != argIn.size()) {
        Error(_W("Too many input arguments."));
    }

    if (!edgeTile.empty()) {
        go_handle existing = layout->getEdgeTile(edgeTile);
        if (existing != -1 && !isDeletedGraphicsObject(existing)) {
            makeCurrentAxesIfPossible(fig, existing);
            if (nLhs > 0) {
                retval << ArrayOf::graphicsObjectConstructor(existing);
            }
            return retval;
        }
        int64 axHandle = createAxesInTile(layout, figHandle, fig, 0, 1, 1, edgeTile);
        layout->registerEdgeTile(edgeTile, axHandle);
        layout->updateChildPositions();
        if (nLhs > 0) {
            retval << ArrayOf::graphicsObjectConstructor(axHandle);
        }
        return retval;
    }

    std::wstring arrangement = layout->findStringProperty(GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR);

    if (!tileSpecified) {
        layout->ensureDynamicGridCanPlace(spanRows, spanCols);
        tileNum = layout->findNextEmptyTile(spanRows, spanCols);
        std::vector<double> gs = layout->findVectorDoubleProperty(GO_GRID_SIZE_PROPERTY_NAME_STR);
        int rows = (gs.size() >= 1) ? std::max(1, (int)gs[0]) : 1;
        int cols = (gs.size() >= 2) ? std::max(1, (int)gs[1]) : 1;
        if (tileNum > rows * cols) {
            if (arrangement == GO_PROPERTY_VALUE_FIXED_STR) {
                Error(_W("No empty tile is available in the fixed tiled layout."));
            }
            layout->ensureDynamicGridCanPlace(spanRows, spanCols);
            tileNum = layout->findNextEmptyTile(spanRows, spanCols);
        }
    }

    ensureRegionFits(layout, tileNum, spanRows, spanCols);

    std::vector<go_handle> deferredDeletes;

    if (tileSpecified && !spanSpecified) {
        go_handle upperLeft = layout->findChildAtUpperLeftTile(tileNum);
        if (upperLeft != -1 && !isDeletedGraphicsObject(upperLeft)) {
            makeCurrentAxesIfPossible(fig, upperLeft);
            if (nLhs > 0) {
                retval << ArrayOf::graphicsObjectConstructor(upperLeft);
            }
            return retval;
        }
        go_handle covering = layout->findChildCoveringTile(tileNum);
        if (covering != -1) {
            // Unregister first so the replacement axes gets a fresh handle and
            // deterministic tile mapping, then delete old object after creation.
            layout->unregisterChild(covering);
            deferredDeletes.push_back(covering);
        }
    } else if (tileSpecified && spanSpecified) {
        go_handle exact = layout->findChildAtExactRegion(tileNum, spanRows, spanCols);
        if (exact != -1 && !isDeletedGraphicsObject(exact)) {
            makeCurrentAxesIfPossible(fig, exact);
            if (nLhs > 0) {
                retval << ArrayOf::graphicsObjectConstructor(exact);
            }
            return retval;
        }
        std::vector<go_handle> overlaps
            = layout->findChildrenOverlappingRegion(tileNum, spanRows, spanCols);
        for (go_handle h : overlaps) {
            if (h == -1 || isDeletedGraphicsObject(h)) {
                continue;
            }
            layout->unregisterChild(h);
            deferredDeletes.push_back(h);
        }
    }

    int64 axHandle = createAxesInTile(layout, figHandle, fig, tileNum, spanRows, spanCols);
    layout->setNextTileIndex(tileNum + 1);
    if (arrangement == GO_PROPERTY_VALUE_FLOW_STR) {
        layout->reflowTiles();
    } else {
        layout->updateChildPositions();
    }

    // Delete replaced axes after the new one has been created to avoid
    // immediate handle reuse that can break equality-based tests.
    for (go_handle h : deferredDeletes) {
        if (h != -1 && !isDeletedGraphicsObject(h)) {
            deleteGraphicsObject(h, false, true);
        }
    }

    if (nLhs > 0) {
        retval << ArrayOf::graphicsObjectConstructor(axHandle);
    }
    return retval;
}
//=============================================================================
bool
isGraphicsHandleScalar(const ArrayOf& arg)
{
    return arg.isScalar() && arg.getDataClass() == NLS_GO_HANDLE;
}
//=============================================================================
int
positiveIntegerScalar(const ArrayOf& arg, const std::wstring& name)
{
    if (!arg.isNumeric() || arg.isComplex() || !arg.isScalar()) {
        Error(name + _W(" must be a positive integer scalar."));
    }
    int value = arg.getContentAsInteger32Scalar(false, true);
    if (value < 1) {
        Error(name + _W(" must be a positive integer scalar."));
    }
    return value;
}
//=============================================================================
std::pair<int, int>
positiveSpanVector(const ArrayOf& arg)
{
    if (!arg.isNumeric() || arg.isComplex() || arg.isScalar() || arg.getElementCount() != 2) {
        Error(_W("Tile span must be a two-element positive integer vector."));
    }
    std::vector<double> values = arg.getContentAsDoubleVector();
    if (values[0] < 1 || values[1] < 1 || values[0] != std::floor(values[0])
        || values[1] != std::floor(values[1])) {
        Error(_W("Tile span must be a two-element positive integer vector."));
    }
    return { (int)values[0], (int)values[1] };
}
//=============================================================================
int64
findParentFigureHandleFromLayout(GraphicsObject* layout)
{
    GOGObjectsProperty* parent = dynamic_cast<GOGObjectsProperty*>(
        layout->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return -1;
    }
    int64 current = parent->data()[0];
    while (current >= HANDLE_OFFSET_OBJECT) {
        GraphicsObject* parentGO = findGraphicsObject(current, false);
        if (!parentGO) {
            return -1;
        }
        GOGObjectsProperty* pp = dynamic_cast<GOGObjectsProperty*>(
            parentGO->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!pp || pp->data().empty()) {
            return -1;
        }
        current = pp->data()[0];
    }
    return current;
}
//=============================================================================
void
addHandleToFigure(GOFigure* fig, int64 handle)
{
    GOGObjectsProperty* chProp
        = dynamic_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    if (!chProp) {
        return;
    }
    std::vector<int64> children = chProp->data();
    if (std::find(children.begin(), children.end(), handle) == children.end()) {
        children.push_back(handle);
        chProp->data(children);
    }
}
//=============================================================================
bool
tryResolveLayoutHandle(const ArrayOf& arg, int64& layoutHandle, int64& figHandle, GOFigure*& fig,
    GOTiledChartLayout*& layout)
{
    layoutHandle = -1;
    figHandle = -1;
    fig = nullptr;
    layout = nullptr;

    int64 h = -1;
    if (isGraphicsHandleScalar(arg)) {
        const auto* ptr = static_cast<const int64*>(arg.getDataPointer());
        h = ptr[0];
    } else if (arg.isGraphicsObject()) {
        h = arg.getContentAsGraphicsObjectScalar();
    } else if (arg.isNumeric() && !arg.isComplex() && arg.isScalar()) {
        h = arg.getContentAsInteger64Scalar();
    } else {
        return false;
    }

    if (h < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(h)) {
        return false;
    }

    GraphicsObject* go = findGraphicsObject(h, false);
    if (!go || go->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return false;
    }

    GOGObjectsProperty* parentProp
        = dynamic_cast<GOGObjectsProperty*>(go->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parentProp || parentProp->data().empty()) {
        return false;
    }

    int64 parentHandle = parentProp->data()[0];
    if (parentHandle >= HANDLE_OFFSET_OBJECT || parentHandle < 0
        || isDeletedGraphicsObject(parentHandle)) {
        return false;
    }

    GOFigure* parentFigure = findGOFigure(parentHandle);
    if (!parentFigure) {
        return false;
    }

    layoutHandle = h;
    figHandle = parentHandle;
    fig = parentFigure;
    layout = static_cast<GOTiledChartLayout*>(go);
    return true;
}
//=============================================================================
int64
createFlowLayoutOnFigure(int64 figHandle, GOFigure* fig)
{
    GOTiledChartLayout* layout = new GOTiledChartLayout;
    int64 layoutHandle = assignGraphicsObject(layout);

    layout->initializeTextObjects(layoutHandle);
    layout->setTileArrangementInternal(GO_PROPERTY_VALUE_FLOW_STR);
    layout->setGridSizeInternal(1, 1, false);
    layout->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, figHandle);

    addHandleToFigure(fig, layoutHandle);
    fig->setCurrentTiledLayout(layoutHandle);
    layout->updateState();
    fig->setRenderingStateInvalid(true);
    return layoutHandle;
}
//=============================================================================
GOTiledChartLayout*
resolveLayout(
    const ArrayOfVector& argIn, size_t& pos, int64& layoutHandle, int64& figHandle, GOFigure*& fig)
{
    layoutHandle = -1;
    figHandle = -1;
    fig = nullptr;

    if (!argIn.empty()) {
        GOTiledChartLayout* candidateLayout = nullptr;
        if (tryResolveLayoutHandle(argIn[0], layoutHandle, figHandle, fig, candidateLayout)) {
            pos = 1;
            fig->setCurrentTiledLayout(layoutHandle);
            return candidateLayout;
        }
    }

    int64 id = getCurrentFigure();
    if (id == NO_FIGURE) {
        id = createNewFigure();
    }
    figHandle = id;
    fig = findGOFigure(id);
    if (!fig) {
        return nullptr;
    }

    layoutHandle = fig->getCurrentTiledLayout();
    if (layoutHandle == -1 || isDeletedGraphicsObject(layoutHandle)) {
        GOGObjectsProperty* figChildren
            = dynamic_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        if (figChildren) {
            std::vector<int64> children = figChildren->data();
            for (auto it = children.rbegin(); it != children.rend(); ++it) {
                int64 ch = *it;
                if (isDeletedGraphicsObject(ch)) {
                    continue;
                }
                GraphicsObject* child = findGraphicsObject(ch, false);
                if (child && child->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
                    layoutHandle = ch;
                    fig->setCurrentTiledLayout(layoutHandle);
                    break;
                }
            }
        }
    }

    if (layoutHandle == -1 || isDeletedGraphicsObject(layoutHandle)) {
        layoutHandle = createFlowLayoutOnFigure(figHandle, fig);
    }

    GraphicsObject* go = findGraphicsObject(layoutHandle, false);
    if (!go || go->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return nullptr;
    }
    pos = 0;
    return static_cast<GOTiledChartLayout*>(go);
}
//=============================================================================
int64
createAxesInTile(GOTiledChartLayout* layout, int64 figHandle, GOFigure* fig, int tileNum,
    int spanRows, int spanCols, const std::wstring& edgeTile)
{
    GOAxis* ax = new GOAxis;
    int64 axHandle = assignGraphicsObject(ax);

    ax->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, figHandle);
    fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, axHandle);
    addHandleToFigure(fig, axHandle);

    setLayoutOptions(ax, axHandle, tileNum, spanRows, spanCols, edgeTile);
    layout->registerChildTile(axHandle, tileNum, spanRows, spanCols);

    ax->updateState();
    layout->updateChildPositions();
    fig->setRenderingStateInvalid(true);
    return axHandle;
}
//=============================================================================
void
makeCurrentAxesIfPossible(GOFigure* fig, go_handle handle)
{
    GraphicsObject* go = findGraphicsObject(handle, false);
    if (go && go->getType() == GO_PROPERTY_VALUE_AXES_STR) {
        fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);
    }
}
//=============================================================================
void
ensureRegionFits(GOTiledChartLayout* layout, int tileNum, int spanRows, int spanCols)
{
    std::wstring arrangement = layout->findStringProperty(GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR);
    std::vector<double> gs = layout->findVectorDoubleProperty(GO_GRID_SIZE_PROPERTY_NAME_STR);
    int rows = (gs.size() >= 1) ? std::max(1, (int)gs[0]) : 1;
    int cols = (gs.size() >= 2) ? std::max(1, (int)gs[1]) : 1;

    auto regionFits = [&]() {
        auto [row, col] = layout->tileIndexToRowCol(tileNum);
        return row >= 1 && col >= 1 && row + spanRows - 1 <= rows && col + spanCols - 1 <= cols;
    };

    if (regionFits()) {
        return;
    }
    if (arrangement == GO_PROPERTY_VALUE_FIXED_STR) {
        Error(_W("Tile location exceeds the fixed tiled layout grid."));
    }

    int guard = 0;
    while (!regionFits() && guard < 128) {
        if (arrangement == GO_PROPERTY_VALUE_VERTICAL_STR) {
            ++rows;
        } else if (arrangement == GO_PROPERTY_VALUE_HORIZONTAL_STR) {
            ++cols;
        } else {
            double growRowsScore
                = std::abs(((double)(rows + 1) / (double)std::max(1, cols)) - 0.75);
            double growColsScore = std::abs(((double)rows / (double)(cols + 1)) - 0.75);
            if (growRowsScore < growColsScore) {
                ++rows;
            } else {
                ++cols;
            }
        }
        layout->setGridSizeInternal(rows, cols);
        ++guard;
    }

    if (!regionFits()) {
        Error(_W("Unable to place tile in layout."));
    }
}
//=============================================================================
}
//=============================================================================
