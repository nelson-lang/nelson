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
#include <utility>
#include "graphics_object_setBuiltin.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOFiguresManager.hpp"
#include "GOTiledChartLayout.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
setChildren(GraphicsObject* fp, ArrayOf childrenToAdd);
//=============================================================================
static void
setParent(GraphicsObject* fp, ArrayOf newParent);
//=============================================================================
static bool
setTiledLayoutProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value);
//=============================================================================
static void
updateParentTiledLayoutIfNeeded(GraphicsObject* fp);
//=============================================================================
static void
setObjectReferenceProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value);
//=============================================================================
static bool
isReplaceableTextReferenceProperty(const std::wstring& propname);
//=============================================================================
static void
deleteReplacedTextObjects(
    const std::vector<int64>& oldHandles, const std::vector<int64>& newHandles);
//=============================================================================
ArrayOfVector
graphics_object_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3);

    if (!argIn[0].isGraphicsObject()) {
        Error(_W("Expected graphics object(s)."));
    }
    nelson_handle* gobjects = (nelson_handle*)(argIn[0].getDataPointer());
    indexType nbElements = argIn[0].getElementCount();
    for (indexType k = 0; k < nbElements; k++) {
        GraphicsObject* fp = nullptr;
        if (gobjects[k] == HANDLE_ROOT_OBJECT) {
            fp = getGraphicsRootObject();
        } else if (gobjects[k] >= HANDLE_OFFSET_OBJECT) {
            fp = findGraphicsObject(gobjects[k]);
        } else {
            fp = static_cast<GraphicsObject*>(findGOFigure(gobjects[k]));
        }
        if (!fp) {
            Error(_W("Invalid handle."));
        }
        int ptr = 1;
        while (argIn.size() >= (ptr + 2)) {
            std::wstring propname = argIn[ptr].getContentAsWideString();
            if (propname == GO_CHILDREN_PROPERTY_NAME_STR) {
                setChildren(fp, argIn[ptr + 1]);
            } else if (propname == GO_PARENT_PROPERTY_NAME_STR) {
                setParent(fp, argIn[ptr + 1]);
            } else if (fp->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
                setTiledLayoutProperty(fp, propname, argIn[ptr + 1]);
            } else {
                try {
                    GOGenericProperty* hp = fp->findProperty(propname);
                    if (hp) {
                        if (!fp->isWritable(propname) && fp->haveProperty(propname)) {
                            Error(_W("Property is readable only: ") + propname);
                        }
                        if (isReplaceableTextReferenceProperty(propname)
                            && dynamic_cast<GOGObjectsProperty*>(hp)) {
                            setObjectReferenceProperty(fp, propname, argIn[ptr + 1]);
                        } else {
                            hp->set(argIn[ptr + 1]);
                        }
                    }
                } catch (const Exception& e) {
                    Error(_W("Got error for property:") + L" " + propname + L"\n" + e.what());
                }
            }
            ptr += 2;
        }
        fp->updateState();
        updateParentTiledLayoutIfNeeded(fp);
        if (!fp->isType(GO_PROPERTY_VALUE_FIGURE_STR) && !fp->isType(GO_PROPERTY_VALUE_ROOT_STR)
            && !fp->isType(GO_PROPERTY_VALUE_UICONTROL_STR)) {
            GOFigure* fig = fp->getParentFigure();
            if (fig) {
                fig->setRenderingStateInvalid(true);
                fig->repaint();
            }
        }
    }

    return ArrayOfVector();
}
//=============================================================================
static bool
isReplaceableTextReferenceProperty(const std::wstring& propname)
{
    return propname == GO_TITLE_PROPERTY_NAME_STR || propname == GO_SUBTITLE_PROPERTY_NAME_STR
        || propname == GO_X_LABEL_PROPERTY_NAME_STR || propname == GO_Y_LABEL_PROPERTY_NAME_STR
        || propname == GO_Z_LABEL_PROPERTY_NAME_STR;
}
//=============================================================================
static void
setObjectReferenceProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value)
{
    GOGObjectsProperty* hp = static_cast<GOGObjectsProperty*>(fp->findProperty(propname));
    std::vector<int64> oldHandles(hp->data());
    hp->set(value);
    deleteReplacedTextObjects(oldHandles, hp->data());
}
//=============================================================================
static void
deleteReplacedTextObjects(
    const std::vector<int64>& oldHandles, const std::vector<int64>& newHandles)
{
    for (int64 handle : oldHandles) {
        if (handle < HANDLE_OFFSET_OBJECT) {
            continue;
        }
        if (std::find(newHandles.begin(), newHandles.end(), handle) != newHandles.end()) {
            continue;
        }
        GraphicsObject* oldObject = findGraphicsObject(handle, false);
        if (oldObject && oldObject->isType(GO_PROPERTY_VALUE_TEXT_STR)) {
            deleteGraphicsObject(handle, false, false);
        }
    }
}
//=============================================================================
static std::wstring
lowerCopy(const std::wstring& value)
{
    std::wstring result(value);
    StringHelpers::to_lower(result);
    return result;
}
//=============================================================================
static std::wstring
canonicalLayoutPropertyName(GOTiledChartLayout* layout, const std::wstring& requested)
{
    if (layout->haveProperty(requested)) {
        return requested;
    }
    std::wstring lowered = lowerCopy(requested);
    for (const auto& fieldName : layout->getFieldnames()) {
        if (lowerCopy(fieldName) == lowered) {
            return fieldName;
        }
    }
    return requested;
}
//=============================================================================
static std::pair<int, int>
gridSizeFromArray(const ArrayOf& value)
{
    if (!value.isNumeric() || value.isComplex() || value.getElementCount() != 2) {
        Error(_W("GridSize must be a two-element positive integer vector."));
    }
    std::vector<double> values = value.getContentAsDoubleVector();
    int rows = (int)values[0];
    int cols = (int)values[1];
    if (rows < 1 || cols < 1 || values[0] != std::floor(values[0])
        || values[1] != std::floor(values[1])) {
        Error(_W("GridSize must be a two-element positive integer vector."));
    }
    return { rows, cols };
}
//=============================================================================
static ArrayOf
normalizeLayoutValue(const std::wstring& propname, ArrayOf value)
{
    if (!value.isRowVectorCharacterArray() && !value.isScalarStringArray()) {
        return value;
    }

    std::wstring lowered = lowerCopy(value.getContentAsWideString());
    if (propname == GO_TILE_SPACING_PROPERTY_NAME_STR) {
        if (lowered == L"normal") {
            lowered = GO_PROPERTY_VALUE_LOOSE_STR;
        }
        return ArrayOf::characterArrayConstructor(lowered);
    }
    if (propname == GO_PADDING_PROPERTY_NAME_STR) {
        if (lowered == L"normal") {
            lowered = GO_PROPERTY_VALUE_LOOSE_STR;
        } else if (lowered == L"none") {
            lowered = GO_PROPERTY_VALUE_TIGHT_STR;
        }
        return ArrayOf::characterArrayConstructor(lowered);
    }
    if (propname == GO_TILE_INDEXING_PROPERTY_NAME_STR
        || propname == GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR
        || propname == GO_UNITS_PROPERTY_NAME_STR
        || propname == GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR
        || propname == GO_VISIBLE_PROPERTY_NAME_STR) {
        return ArrayOf::characterArrayConstructor(lowered);
    }
    return value;
}
//=============================================================================
static bool
setTiledLayoutProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value)
{
    GOTiledChartLayout* layout = dynamic_cast<GOTiledChartLayout*>(fp);
    if (!layout) {
        return false;
    }
    std::wstring canonical = canonicalLayoutPropertyName(layout, propname);
    if (!layout->haveProperty(canonical)) {
        Error(_W("Unknown property: ") + propname);
    }
    if (canonical == GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR) {
        Error(_W("Property is readable only: ") + canonical);
    }
    if (canonical == GO_GRID_SIZE_PROPERTY_NAME_STR) {
        auto [rows, cols] = gridSizeFromArray(value);
        layout->setGridSizeFromUser(rows, cols);
        return true;
    }
    if (!layout->isWritable(canonical)) {
        Error(_W("Property is readable only: ") + canonical);
    }
    if (isReplaceableTextReferenceProperty(canonical)
        && dynamic_cast<GOGObjectsProperty*>(layout->findProperty(canonical))) {
        setObjectReferenceProperty(layout, canonical, value);
    } else {
        layout->findProperty(canonical)->set(normalizeLayoutValue(canonical, value));
    }
    return true;
}
//=============================================================================
static void
updateParentTiledLayoutIfNeeded(GraphicsObject* fp)
{
    if (!fp || fp->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return;
    }
    GOGObjectsProperty* parent
        = dynamic_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return;
    }
    int64 parentHandle = parent->data()[0];
    if (parentHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(parentHandle)) {
        return;
    }
    GraphicsObject* parentObject = findGraphicsObject(parentHandle, false);
    if (!parentObject || parentObject->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return;
    }
    parentObject->updateState();
}
//=============================================================================
static void
setParent(GraphicsObject* fp, ArrayOf newParent)
{

    go_handle goNewParent = newParent.getContentAsGraphicsObjectScalar();
    validateGO(goNewParent);

    GOGenericProperty* hParent = fp->findProperty(GO_PARENT_PROPERTY_NAME_STR, false);
    if (!hParent) {
        return;
    }
    hParent->set(newParent);
}
//=============================================================================
static void
setChildren(GraphicsObject* fp, ArrayOf childrenToAdd)
{
    const int64* dp = static_cast<const int64*>(childrenToAdd.getDataPointer());
    for (int i = 0; i < childrenToAdd.getElementCount(); i++) {
        validateGO(dp[i]);
    }
    GraphicsObject* gp = nullptr;
    GOGObjectsProperty* hp
        = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> my_children(hp->data());
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* gp = findGraphicsObject(handle);
            gp->dereference();
        }
    }
    for (int i = 0; i < childrenToAdd.getElementCount(); i++) {
        int64 handle = dp[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* gp = findGraphicsObject(handle);
            gp->reference();
        }
    }
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            gp = findGraphicsObject(handle);
            if (gp->referenceCount() <= 0) {
                freeGraphicsObject(handle);
                delete gp;
            }
        }
    }
    hp->set(childrenToAdd);
}
//=============================================================================void
}
//=============================================================================void
