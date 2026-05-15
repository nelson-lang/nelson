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
#include "tiledlayoutBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOTiledChartLayout.hpp"
#include "GOLayoutOptions.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RefreshFigure.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static bool
isArrangementValue(const std::wstring& value);
static int
positiveIntegerScalar(const ArrayOf& arg, const std::wstring& name);
static std::pair<int, int>
positiveGridVector(const ArrayOf& arg, const std::wstring& name);
static GOFigure*
findParentFigureFromHandle(int64 handle);
static int64
resolveParent(const ArrayOfVector& argIn, size_t& pos, int64& figHandle, GOFigure*& fig);
static void
clearFigureAxesAndLayouts(GOFigure* fig);
static std::wstring
canonicalPropertyName(GraphicsObject* go, const std::wstring& requested);
static ArrayOf
normalizedPropertyValue(const std::wstring& propertyName, const ArrayOf& value);
static void
addHandleToChildren(GraphicsObject* parent, int64 childHandle);
static void
createNestedLayoutOptions(GOTiledChartLayout* layout, int64 layoutHandle);
//=============================================================================
ArrayOfVector
tiledlayoutBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;

    size_t pos = 0;
    int64 figHandle = -1;
    GOFigure* fig = nullptr;
    int64 parentHandle = resolveParent(argIn, pos, figHandle, fig);

    if (!fig || parentHandle == -1) {
        Error(_W("Invalid parent handle."));
    }

    if (parentHandle < HANDLE_OFFSET_OBJECT) {
        clearFigureAxesAndLayouts(fig);
        fig->setCurrentTiledLayout(-1);
    }

    int gridM = 1;
    int gridN = 1;
    std::wstring arrangement = GO_PROPERTY_VALUE_FLOW_STR;

    if (pos < argIn.size()) {
        const ArrayOf& first = argIn[pos];
        if (first.isNumeric() && !first.isComplex()) {
            gridM = positiveIntegerScalar(first, L"m");
            ++pos;
            if (pos >= argIn.size() || !argIn[pos].isNumeric() || argIn[pos].isComplex()) {
                Error(_W("tiledlayout(m,n) requires both m and n grid dimensions."));
            }
            gridN = positiveIntegerScalar(argIn[pos], L"n");
            ++pos;
            arrangement = GO_PROPERTY_VALUE_FIXED_STR;
        } else if ((first.isScalarStringArray() || first.isRowVectorCharacterArray())
            && isArrangementValue(first.getContentAsWideString())) {
            arrangement = first.getContentAsWideString();
            ++pos;
        }
    }

    GOTiledChartLayout* layout = new GOTiledChartLayout;
    int64 layoutHandle = assignGraphicsObject(layout);
    layout->initializeTextObjects(layoutHandle);
    layout->setGridSizeInternal(gridM, gridN, false);
    layout->setTileArrangementInternal(arrangement);

    while (pos < argIn.size()) {
        if (pos + 1 >= argIn.size()) {
            Error(_W("Name-value arguments must appear in pairs."));
        }
        if (!argIn[pos].isScalarStringArray() && !argIn[pos].isRowVectorCharacterArray()) {
            Error(_W("Property name must be a string scalar or character row vector."));
        }

        std::wstring propName = canonicalPropertyName(layout, argIn[pos].getContentAsWideString());
        if (propName == GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR) {
            Error(_W("Property is read-only: ") + propName);
        }

        if (propName == GO_GRID_SIZE_PROPERTY_NAME_STR) {
            auto [rows, cols] = positiveGridVector(argIn[pos + 1], propName);
            layout->setGridSizeFromUser(rows, cols);
        } else {
            if (!layout->isWritable(propName) && layout->haveProperty(propName)) {
                Error(_W("Property is read-only: ") + propName);
            }
            ArrayOf value = normalizedPropertyValue(propName, argIn[pos + 1]);
            layout->findProperty(propName)->set(value);
        }
        pos += 2;
    }

    layout->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, parentHandle);

    if (parentHandle < HANDLE_OFFSET_OBJECT) {
        addHandleToChildren(fig, layoutHandle);
    } else {
        GraphicsObject* parentLayoutGO = findGraphicsObject(parentHandle, false);
        if (!parentLayoutGO || parentLayoutGO->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            Error(_W("Parent must be a figure or TiledChartLayout."));
        }
        addHandleToChildren(parentLayoutGO, layoutHandle);
        createNestedLayoutOptions(layout, layoutHandle);
    }

    fig->setCurrentTiledLayout(layoutHandle);
    layout->updateState();
    fig->setRenderingStateInvalid(true);

    if (nLhs > 0) {
        retval << ArrayOf::graphicsObjectConstructor(layoutHandle);
    }
    return retval;
}
//=============================================================================
bool
isArrangementValue(const std::wstring& value)
{
    return value == GO_PROPERTY_VALUE_FLOW_STR || value == GO_PROPERTY_VALUE_VERTICAL_STR
        || value == GO_PROPERTY_VALUE_HORIZONTAL_STR;
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
positiveGridVector(const ArrayOf& arg, const std::wstring& name)
{
    if (!arg.isNumeric() || arg.isComplex() || arg.getElementCount() != 2) {
        Error(name + _W(" must be a two-element positive integer vector."));
    }
    std::vector<double> values = arg.getContentAsDoubleVector();
    int rows = (int)values[0];
    int cols = (int)values[1];
    if (rows < 1 || cols < 1 || values[0] != std::floor(values[0])
        || values[1] != std::floor(values[1])) {
        Error(name + _W(" must be a two-element positive integer vector."));
    }
    return { rows, cols };
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
    GraphicsObject* go = findGraphicsObject(handle, false);
    if (!go) {
        return nullptr;
    }
    if (go->getType() == GO_PROPERTY_VALUE_AXES_STR) {
        return go->getParentFigure();
    }
    GOGObjectsProperty* parent
        = dynamic_cast<GOGObjectsProperty*>(go->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return nullptr;
    }
    return findParentFigureFromHandle(parent->data()[0]);
}
//=============================================================================
int64
resolveParent(const ArrayOfVector& argIn, size_t& pos, int64& figHandle, GOFigure*& fig)
{
    pos = 0;
    figHandle = -1;
    fig = nullptr;
    int64 parentHandle = -1;

    if (!argIn.empty() && argIn[0].isGraphicsObject()) {
        int64 h = argIn[0].getContentAsGraphicsObjectScalar();
        if (h != HANDLE_ROOT_OBJECT && h < HANDLE_OFFSET_OBJECT) {
            GOFigure* candidate = findGOFigure(h);
            if (candidate) {
                pos = 1;
                parentHandle = h;
                figHandle = h;
                fig = candidate;
                return parentHandle;
            }
        } else if (h >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* candidate = findGraphicsObject(h, false);
            if (candidate && candidate->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
                GOFigure* parentFigure = findParentFigureFromHandle(h);
                if (parentFigure) {
                    pos = 1;
                    parentHandle = h;
                    fig = parentFigure;
                    GOGObjectsProperty* layoutParent = dynamic_cast<GOGObjectsProperty*>(
                        candidate->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
                    int64 resolved = layoutParent && !layoutParent->data().empty()
                        ? layoutParent->data()[0]
                        : -1;
                    while (resolved >= HANDLE_OFFSET_OBJECT) {
                        GraphicsObject* parentGO = findGraphicsObject(resolved, false);
                        if (!parentGO) {
                            break;
                        }
                        GOGObjectsProperty* pp = dynamic_cast<GOGObjectsProperty*>(
                            parentGO->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
                        if (!pp || pp->data().empty()) {
                            break;
                        }
                        resolved = pp->data()[0];
                    }
                    figHandle = resolved < HANDLE_OFFSET_OBJECT ? resolved : getCurrentFigure();
                    return parentHandle;
                }
            }
        }
    }

    int64 id = getCurrentFigure();
    if (id == NO_FIGURE) {
        id = createNewFigure();
    } else {
        // If current figure already has axes content (nexttile was called), create a new figure.
        // This matches MATLAB behavior where tiledlayout without a parent figure creates a new
        // figure rather than overwriting an existing one that has been used.
        GOFigure* existingFig = findGOFigure(id);
        if (existingFig) {
            GOGObjectsProperty* chProp = dynamic_cast<GOGObjectsProperty*>(
                existingFig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
            if (chProp) {
                bool hasAxesContent = false;
                for (int64 ch : chProp->data()) {
                    if (isDeletedGraphicsObject(ch)) {
                        continue;
                    }
                    GraphicsObject* go = findGraphicsObject(ch, false);
                    if (go && go->getType() == GO_PROPERTY_VALUE_AXES_STR) {
                        hasAxesContent = true;
                        break;
                    }
                }
                if (hasAxesContent) {
                    id = createNewFigure();
                }
            }
        }
    }
    parentHandle = id;
    figHandle = id;
    fig = findGOFigure(id);
    return parentHandle;
}
//=============================================================================
void
clearFigureAxesAndLayouts(GOFigure* fig)
{
    GOGObjectsProperty* chProp
        = dynamic_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    if (!chProp) {
        return;
    }
    std::vector<int64> children = chProp->data();
    std::vector<int64> toRemove;
    for (int64 ch : children) {
        if (isDeletedGraphicsObject(ch)) {
            continue;
        }
        GraphicsObject* go = findGraphicsObject(ch, false);
        if (!go) {
            continue;
        }
        std::wstring t = go->getType();
        if (t == GO_PROPERTY_VALUE_AXES_STR || t == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
            toRemove.push_back(ch);
        }
    }
    for (int64 ch : toRemove) {
        deleteGraphicsObject(ch, false, true);
    }
    std::vector<int64> retained;
    retained.reserve(children.size());
    for (int64 ch : children) {
        if (std::find(toRemove.begin(), toRemove.end(), ch) == toRemove.end()
            && !isDeletedGraphicsObject(ch)) {
            retained.push_back(ch);
        }
    }
    chProp->data(retained);
}
//=============================================================================
std::wstring
canonicalPropertyName(GraphicsObject* go, const std::wstring& requested)
{
    if (go->haveProperty(requested)) {
        return requested;
    }
    for (const auto& fieldName : go->getFieldnames()) {
        if (fieldName == requested) {
            return fieldName;
        }
    }
    Error(_W("Unknown property: ") + requested);
    return requested;
}
//=============================================================================
ArrayOf
normalizedPropertyValue(const std::wstring& propertyName, const ArrayOf& value)
{
    if (!value.isScalarStringArray() && !value.isRowVectorCharacterArray()) {
        return value;
    }
    std::wstring str = value.getContentAsWideString();
    if (propertyName == GO_TILE_SPACING_PROPERTY_NAME_STR) {
        if (str == GO_PROPERTY_VALUE_NORMAL_STR) {
            str = GO_PROPERTY_VALUE_LOOSE_STR;
        }
        if (str == GO_PROPERTY_VALUE_LOOSE_STR || str == GO_PROPERTY_VALUE_COMPACT_STR
            || str == GO_PROPERTY_VALUE_TIGHT_STR || str == GO_PROPERTY_VALUE_NONE_STR) {
            return ArrayOf::characterArrayConstructor(str);
        }
    } else if (propertyName == GO_PADDING_PROPERTY_NAME_STR) {
        if (str == L"normal") {
            str = GO_PROPERTY_VALUE_LOOSE_STR;
        } else if (str == L"none") {
            str = GO_PROPERTY_VALUE_TIGHT_STR;
        }
        if (str == GO_PROPERTY_VALUE_LOOSE_STR || str == GO_PROPERTY_VALUE_COMPACT_STR
            || str == GO_PROPERTY_VALUE_TIGHT_STR) {
            return ArrayOf::characterArrayConstructor(str);
        }
    } else if (propertyName == GO_TILE_INDEXING_PROPERTY_NAME_STR
        || propertyName == GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR
        || propertyName == GO_UNITS_PROPERTY_NAME_STR
        || propertyName == GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR
        || propertyName == GO_VISIBLE_PROPERTY_NAME_STR) {
        return ArrayOf::characterArrayConstructor(str);
    }
    return value;
}
//=============================================================================
void
addHandleToChildren(GraphicsObject* parent, int64 childHandle)
{
    GOGObjectsProperty* chProp = dynamic_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!chProp) {
        return;
    }
    std::vector<int64> children = chProp->data();
    if (std::find(children.begin(), children.end(), childHandle) == children.end()) {
        children.push_back(childHandle);
        chProp->data(children);
    }
}
//=============================================================================
void
createNestedLayoutOptions(GOTiledChartLayout* layout, int64 layoutHandle)
{
    GOLayoutOptions* opts = new GOLayoutOptions;
    int64 optsHandle = assignGraphicsObject(opts);
    opts->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, layoutHandle);
    layout->setGoProperty(GO_LAYOUT_PROPERTY_NAME_STR, optsHandle);
}
//=============================================================================
}
//=============================================================================
