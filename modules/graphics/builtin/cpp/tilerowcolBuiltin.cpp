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
#include <limits>
#include "tilerowcolBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Dimensions.hpp"
#include "GOTiledChartLayout.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static GOFigure*
findParentFigureFromHandle(int64 handle);
static GOTiledChartLayout*
findAnyTopLevelLayoutOnFigure(GOFigure* fig);
static GOTiledChartLayout*
findAnyTopLevelLayoutOnFigure(GOFigure* fig);
static bool
getTileFromLayoutOptions(int64 objHandle, double& tileNum, bool& isEdgeTile);
static GOTiledChartLayout*
findLayoutContainingChild(GraphicsObject* candidate, int64 objHandle);
static GOTiledChartLayout*
getParentLayoutForObj(int64 objHandle);
static ArrayOf
rowVectorFromDoubles(const std::vector<double>& values);
//=============================================================================
ArrayOfVector
tilerowcolBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval;
    constexpr double NaN = std::numeric_limits<double>::quiet_NaN();

    if (argIn.size() == 2) {
        ArrayOf tArg = argIn[0];
        if (!tArg.isGraphicsObject()) {
            Error(_W("First argument must be a TiledChartLayout handle."));
        }
        int64 h = tArg.getContentAsGraphicsObjectScalar();
        GraphicsObject* go = findGraphicsObject(h, false);
        if (!go || go->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            Error(_W("First argument must be a TiledChartLayout handle."));
        }
        GOTiledChartLayout* layout = dynamic_cast<GOTiledChartLayout*>(go);

        std::vector<double> tnums = argIn[1].getContentAsDoubleVector();
        std::vector<double> rows(tnums.size());
        std::vector<double> cols(tnums.size());
        for (size_t i = 0; i < tnums.size(); ++i) {
            auto [r, c] = layout->tileIndexToRowCol((int)tnums[i]);
            rows[i] = (r < 1) ? NaN : (double)r;
            cols[i] = (c < 1) ? NaN : (double)c;
        }
        if (nLhs >= 1) {
            retval << rowVectorFromDoubles(rows);
        }
        if (nLhs >= 2) {
            retval << rowVectorFromDoubles(cols);
        }
        return retval;
    }

    if (argIn.size() == 1) {
        ArrayOf objArg = argIn[0];
        if (!objArg.isGraphicsObject()) {
            Error(_W("Argument must be a graphics object handle."));
        }

        std::vector<int64> handles;
        if (objArg.isScalar()) {
            handles.push_back(objArg.getContentAsGraphicsObjectScalar());
        } else {
            ArrayOf dbl = objArg;
            dbl.promoteType(NLS_DOUBLE);
            auto* ptr = static_cast<const double*>(dbl.getDataPointer());
            indexType n = dbl.getElementCount();
            for (indexType i = 0; i < n; ++i) {
                handles.push_back((int64)ptr[i]);
            }
        }

        std::vector<double> rows(handles.size());
        std::vector<double> cols(handles.size());
        for (size_t i = 0; i < handles.size(); ++i) {
            int64 objH = handles[i];

            double tileFromLayout = NaN;
            bool isEdgeTile = false;
            if (getTileFromLayoutOptions(objH, tileFromLayout, isEdgeTile)) {
                if (isEdgeTile || !(tileFromLayout > 0.0)) {
                    rows[i] = NaN;
                    cols[i] = NaN;
                    continue;
                }
                GraphicsObject* obj = findGraphicsObject(objH, false);
                GOGObjectsProperty* parentProp = obj
                    ? dynamic_cast<GOGObjectsProperty*>(
                          obj->findProperty(GO_PARENT_PROPERTY_NAME_STR, false))
                    : nullptr;
                GOFigure* fig = (parentProp && !parentProp->data().empty())
                    ? findParentFigureFromHandle(parentProp->data()[0])
                    : nullptr;
                GOTiledChartLayout* currentLayout = nullptr;
                if (fig) {
                    go_handle currentLayoutHandle = fig->getCurrentTiledLayout();
                    GraphicsObject* currentLayoutObj
                        = findGraphicsObject(currentLayoutHandle, false);
                    if (currentLayoutObj
                        && currentLayoutObj->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
                        currentLayout = dynamic_cast<GOTiledChartLayout*>(currentLayoutObj);
                    }
                }
                if (currentLayout) {
                    auto [r, c] = currentLayout->tileIndexToRowCol((int)tileFromLayout);
                    rows[i] = (r < 1) ? NaN : (double)r;
                    cols[i] = (c < 1) ? NaN : (double)c;
                    continue;
                }

                GOTiledChartLayout* anyLayout = findAnyTopLevelLayoutOnFigure(fig);
                if (anyLayout) {
                    auto [r, c] = anyLayout->tileIndexToRowCol((int)tileFromLayout);
                    rows[i] = (r < 1) ? NaN : (double)r;
                    cols[i] = (c < 1) ? NaN : (double)c;
                    continue;
                }
            }

            GOTiledChartLayout* layout = getParentLayoutForObj(objH);
            if (!layout || layout->isEdgeTileHandle(objH)) {
                rows[i] = NaN;
                cols[i] = NaN;
                continue;
            }
            GOTiledChartLayout::TileSpanInfo info = layout->getTileSpanInfo(objH);
            if (info.tile == 0) {
                if (tileFromLayout > 0.0) {
                    auto [r, c] = layout->tileIndexToRowCol((int)tileFromLayout);
                    rows[i] = (r < 1) ? NaN : (double)r;
                    cols[i] = (c < 1) ? NaN : (double)c;
                } else {
                    rows[i] = NaN;
                    cols[i] = NaN;
                }
            } else {
                auto [r, c] = layout->tileIndexToRowCol(info.tile);
                rows[i] = (r < 1) ? NaN : (double)r;
                cols[i] = (c < 1) ? NaN : (double)c;
            }
        }
        if (nLhs >= 1) {
            retval << rowVectorFromDoubles(rows);
        }
        if (nLhs >= 2) {
            retval << rowVectorFromDoubles(cols);
        }
        return retval;
    }

    Error(_W("Invalid number of arguments."));
    return retval;
}
//=============================================================================
GOFigure*
findParentFigureFromHandle(int64 handle)
{
    if (handle < 0) {
        return nullptr;
    }
    if (handle < HANDLE_OFFSET_OBJECT) {
        return findGOFigure(handle);
    }
    if (isDeletedGraphicsObject(handle)) {
        return nullptr;
    }
    GraphicsObject* go = findGraphicsObject(handle, false);
    if (!go) {
        return nullptr;
    }
    GOGObjectsProperty* parentProp
        = dynamic_cast<GOGObjectsProperty*>(go->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parentProp || parentProp->data().empty()) {
        return nullptr;
    }
    return findParentFigureFromHandle(parentProp->data()[0]);
}
//=============================================================================
GOTiledChartLayout*
findAnyTopLevelLayoutOnFigure(GOFigure* fig)
{
    if (!fig) {
        return nullptr;
    }
    GOGObjectsProperty* figChildren = dynamic_cast<GOGObjectsProperty*>(
        fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!figChildren) {
        return nullptr;
    }
    for (int64 childHandle : figChildren->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (child && child->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            return dynamic_cast<GOTiledChartLayout*>(child);
        }
    }
    return nullptr;
}
//=============================================================================
bool
getTileFromLayoutOptions(int64 objHandle, double& tileNum, bool& isEdgeTile)
{
    tileNum = std::numeric_limits<double>::quiet_NaN();
    isEdgeTile = false;

    GraphicsObject* obj = findGraphicsObject(objHandle, false);
    if (!obj) {
        return false;
    }

    GOGObjectsProperty* layoutProp
        = dynamic_cast<GOGObjectsProperty*>(obj->findProperty(GO_LAYOUT_PROPERTY_NAME_STR, false));
    if (!layoutProp || layoutProp->data().empty()) {
        return false;
    }

    GraphicsObject* layoutOptions = findGraphicsObject(layoutProp->data()[0], false);
    if (!layoutOptions) {
        return false;
    }

    GOArrayOfProperty* tileProp = dynamic_cast<GOArrayOfProperty*>(
        layoutOptions->findProperty(GO_TILE_PROPERTY_NAME_STR, false));
    if (!tileProp) {
        return false;
    }

    ArrayOf tileValue = tileProp->get();
    if (tileValue.isNumeric() && !tileValue.isComplex() && tileValue.isScalar()) {
        tileNum = tileValue.getContentAsDoubleScalar();
        return true;
    }
    if (tileValue.isRowVectorCharacterArray()
        || (tileValue.isStringArray() && tileValue.isScalar())) {
        isEdgeTile = true;
        return true;
    }
    return false;
}
//=============================================================================
GOTiledChartLayout*
findLayoutContainingChild(GraphicsObject* candidate, int64 objHandle)
{
    if (!candidate || candidate->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return nullptr;
    }
    GOTiledChartLayout* layout = dynamic_cast<GOTiledChartLayout*>(candidate);

    // Prefer direct tile-span metadata when available; this remains reliable
    // even during transient updates of Children vectors.
    GOTiledChartLayout::TileSpanInfo info = layout->getTileSpanInfo(objHandle);
    if (info.tile != 0 || layout->isEdgeTileHandle(objHandle)) {
        return layout;
    }

    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        layout->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return nullptr;
    }
    for (int64 childHandle : children->data()) {
        if (childHandle == objHandle) {
            return layout;
        }
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (child && child->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            GOTiledChartLayout* nested = findLayoutContainingChild(child, objHandle);
            if (nested) {
                return nested;
            }
        }
    }
    return nullptr;
}
//=============================================================================
GOTiledChartLayout*
getParentLayoutForObj(int64 objHandle)
{
    if (isDeletedGraphicsObject(objHandle)) {
        return nullptr;
    }
    GraphicsObject* obj = findGraphicsObject(objHandle, false);
    if (!obj) {
        return nullptr;
    }
    GOGObjectsProperty* parentProp
        = dynamic_cast<GOGObjectsProperty*>(obj->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parentProp || parentProp->data().empty()) {
        return nullptr;
    }
    GOFigure* fig = findParentFigureFromHandle(parentProp->data()[0]);
    if (!fig) {
        return nullptr;
    }
    GOGObjectsProperty* figChildren = dynamic_cast<GOGObjectsProperty*>(
        fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!figChildren) {
        return nullptr;
    }
    for (int64 childHandle : figChildren->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        GOTiledChartLayout* layout = findLayoutContainingChild(child, objHandle);
        if (layout) {
            return layout;
        }
    }

    go_handle currentLayoutHandle = fig->getCurrentTiledLayout();
    GraphicsObject* currentLayoutObj = findGraphicsObject(currentLayoutHandle, false);
    if (currentLayoutObj && currentLayoutObj->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return dynamic_cast<GOTiledChartLayout*>(currentLayoutObj);
    }

    for (int64 childHandle : figChildren->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (child && child->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            return dynamic_cast<GOTiledChartLayout*>(child);
        }
    }

    return nullptr;
}
//=============================================================================
ArrayOf
rowVectorFromDoubles(const std::vector<double>& values)
{
    if (values.size() == 1) {
        return ArrayOf::doubleConstructor(values[0]);
    }
    Dimensions dims(1, values.size());
    double* ptr
        = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.getElementCount()));
    for (size_t i = 0; i < values.size(); ++i) {
        ptr[i] = values[i];
    }
    return ArrayOf(NLS_DOUBLE, dims, ptr);
}
//=============================================================================
}
//=============================================================================
