//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <utility>
#include "axesBuiltin.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOFiguresManager.hpp"
#include "GOAxis.hpp"
#include "GOPolarAxes.hpp"
#include "GOUIAxes.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "GOLayoutOptions.hpp"
#include "GOUIContainer.h"
#include "GOUIGridLayout.h"
#include "GOUITabGroup.h"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GOGObjectsProperty.hpp"
#include "GraphicsObject.hpp"
#include "GOColorVectorProperty.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static GraphicsObject*
getGraphicsObjectFromGraphicHandle(nelson_handle go);
struct ResolvedAxesParent
{
    GOFigure* fig = nullptr;
    int64 figureId = -1;
    int64 parentId = -1;
    bool parentIsGridLayout = false;
};
//=============================================================================
static int64
createNewAxes(
    const ResolvedAxesParent& parent, const ArrayOfVector& properties, GraphicsObject* fp);
static void
copyColorMapFromFigureToAxes(GOFigure* fig, GraphicsObject* axes);
static void
addAxesToParentChildren(const ResolvedAxesParent& parent, int64 axesHandle);
static void
attachGridLayoutOptions(GraphicsObject* axes, int64 axesHandle);
static void
moveAxesToFront(GOFigure* fig, int64 handle);
static ArrayOfVector
buildReturnValue(int nLhs, int64 handle);
static ArrayOfVector
activateOrCreateAxesFromHandle(int nLhs, const ArrayOfVector& argIn);
static std::pair<GOFigure*, int64>
findFigureOwningChild(int64 childHandle);
static bool
findExplicitAxesParent(const ArrayOfVector& properties, ResolvedAxesParent& parent);
static ResolvedAxesParent
figureAxesParent(GOFigure* fig, int64 figureId);
static ResolvedAxesParent
resolveAxesParent(int64 handle);
//=============================================================================
ArrayOfVector
axesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    if (argIn.size() == 1) {
        return activateOrCreateAxesFromHandle(nLhs, argIn);
    }
    ArrayOfVector t(argIn);
    ResolvedAxesParent parent;

    if (argIn.size() != 0 && argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].asGraphicsObjectScalar();
        parent = resolveAxesParent(handle);
        t.pop_front();
    } else {
        if (!findExplicitAxesParent(t, parent)) {
            int64 currentFigureID = getCurrentFigure();
            if (currentFigureID == -1) {
                currentFigureID = createNewFigure();
            }
            parent = figureAxesParent(getCurrentGOFigure(), currentFigureID);
        }
    }
    int64 handle = createNewAxes(parent, t, new GOAxis);
    return buildReturnValue(nLhs, handle);
}
//=============================================================================
ArrayOfVector
polaraxesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    ArrayOfVector t(argIn);
    ResolvedAxesParent parent;

    if (argIn.size() != 0 && argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].asGraphicsObjectScalar();
        parent = resolveAxesParent(handle);
        t.pop_front();
    } else {
        if (!findExplicitAxesParent(t, parent)) {
            int64 currentFigureID = getCurrentFigure();
            if (currentFigureID == -1) {
                currentFigureID = createNewFigure();
            }
            parent = figureAxesParent(getCurrentGOFigure(), currentFigureID);
        }
    }
    int64 handle = createNewAxes(parent, t, new GOPolarAxes);
    return buildReturnValue(nLhs, handle);
}
//=============================================================================
ArrayOfVector
uiaxesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    ArrayOfVector t(argIn);
    ResolvedAxesParent parent;

    if (argIn.size() != 0 && argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].asGraphicsObjectScalar();
        parent = resolveAxesParent(handle);
        t.pop_front();
    } else {
        if (!findExplicitAxesParent(t, parent)) {
            int64 currentFigureID = getCurrentFigure();
            if (currentFigureID == -1) {
                currentFigureID = createNewFigure();
            }
            parent = figureAxesParent(getCurrentGOFigure(), currentFigureID);
        }
    }
    int64 handle = createNewAxes(parent, t, new GOUIAxes);
    return buildReturnValue(nLhs, handle);
}
//=============================================================================
ArrayOfVector
yyaxisBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOfVector t(argIn);
    GraphicsObject* axesObject = nullptr;
    int64 axesHandle = -1;

    if (t.size() == 2) {
        if (!t[0].isGraphicsObject()) {
            Error("Nelson:yyaxis:expectedAxesHandle",
                "First argument to yyaxis must be an axes handle.");
        }
        axesHandle = t[0].asGraphicsObjectScalar();
        GraphicsObject* hp = getGraphicsObjectFromGraphicHandle(axesHandle);
        if (!hp || !hp->isType(GO_PROPERTY_VALUE_AXES_STR)) {
            Error("Nelson:yyaxis:expectedAxesHandle",
                "First argument to yyaxis must be an axes handle.");
        }
        axesObject = hp;
        t.pop_front();
    }

    std::wstring side = t[0].asWideString();
    StringHelpers::to_lower(side);
    if (side != GO_PROPERTY_VALUE_LEFT_STR && side != GO_PROPERTY_VALUE_RIGHT_STR) {
        Error("Nelson:yyaxis:invalidSide", "yyaxis side must be 'left' or 'right'.");
    }

    GOFigure* fig = nullptr;
    if (!axesObject) {
        int64 figureId = getCurrentFigure();
        if (figureId == -1) {
            figureId = createNewFigure();
        }
        fig = getCurrentGOFigure();
        int64 currentAxes = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
        if (currentAxes == 0 || isDeletedGraphicsObject(currentAxes)) {
            ArrayOfVector empty;
            axesHandle = createNewAxes(figureAxesParent(fig, figureId), empty, new GOAxis);
            axesObject = getGraphicsObjectFromGraphicHandle(axesHandle);
        } else {
            axesHandle = currentAxes;
            axesObject = getGraphicsObjectFromGraphicHandle(currentAxes);
        }
    } else {
        fig = axesObject->getParentFigure();
    }

    GOAxis* axis = static_cast<GOAxis*>(axesObject);
    axis->enableDualYAxis();
    axis->setActiveYSide(side);
    axis->updateState();
    if (fig) {
        fig->setRenderingStateInvalid(true);
        if (!fig->isDrawLater()) {
            fig->repaint();
        }
    }
    return buildReturnValue(nLhs, axesHandle);
}
//=============================================================================
GraphicsObject*
getGraphicsObjectFromGraphicHandle(nelson_handle go)
{
    GraphicsObject* fp = nullptr;
    if (go == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
    } else if (go >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(go, false);
    } else {
        fp = static_cast<GraphicsObject*>(findGOFigure(go));
    }
    return fp;
}
//=============================================================================
int64
createNewAxes(const ResolvedAxesParent& parent, const ArrayOfVector& properties, GraphicsObject* fp)
{
    if (!parent.fig || parent.figureId == -1 || parent.parentId == -1) {
        delete fp;
        Error("Nelson:graphics:invalidFigureHandle", "Invalid figure handle.");
    }

    int64 handle = assignGraphicsObject(fp);
    static_cast<GOAxis*>(fp)->restoreToolbar(handle);

    ArrayOfVector t(properties);
    while (t.size() >= 2) {
        std::wstring propname(fp->resolvePartialPropertyName(t[0].asWideString()));
        std::wstring canonicalName(propname);
        StringHelpers::to_lower(canonicalName);
        if (canonicalName != L"parent") {
            fp->findProperty(propname)->set(t[1]);
        }
        t.pop_front();
        t.pop_front();
    }

    fp->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, parent.parentId);
    // New axes become the current axes, but not the current object: CurrentObject
    // reports what the user clicked, and building an axes is not a click.
    parent.fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);

    copyColorMapFromFigureToAxes(parent.fig, fp);
    addAxesToParentChildren(parent, handle);
    if (parent.parentIsGridLayout) {
        attachGridLayoutOptions(fp, handle);
    }
    parent.fig->applyThemeToObject(fp);

    fp->updateState();

    if (parent.parentIsGridLayout) {
        GraphicsObject* parentObject = findGraphicsObject(parent.parentId, false);
        if (parentObject) {
            parentObject->updateState();
        }
    }

    parent.fig->setRenderingStateInvalid(true);
    return handle;
}
//=============================================================================
void
copyColorMapFromFigureToAxes(GOFigure* fig, GraphicsObject* axes)
{
    GOColorVectorProperty* colormapFigure
        = (GOColorVectorProperty*)fig->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
    GOColorVectorProperty* colormapAxes
        = (GOColorVectorProperty*)axes->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
    if (colormapFigure && colormapAxes) {
        colormapAxes->data(colormapFigure->data());
    }
}
//=============================================================================
void
addAxesToParentChildren(const ResolvedAxesParent& parent, int64 axesHandle)
{
    GraphicsObject* parentObject = parent.parentId == parent.figureId
        ? static_cast<GraphicsObject*>(parent.fig)
        : findGraphicsObject(parent.parentId, false);
    if (!parentObject) {
        return;
    }
    GOGObjectsProperty* hp
        = (GOGObjectsProperty*)parentObject->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(hp->data());
    bool exists = false;
    for (int64 child : children) {
        if (child == axesHandle) {
            exists = true;
            break;
        }
    }
    if (!exists) {
        children.push_back(axesHandle);
    }
    hp->data(children);
}
//=============================================================================
void
attachGridLayoutOptions(GraphicsObject* axes, int64 axesHandle)
{
    GOLayoutOptions* options = new GOLayoutOptions(LayoutOptionsMode::GridLayout);
    int64 optionsHandle = assignGraphicsObject(options);
    options->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, axesHandle);
    axes->setGoProperty(GO_LAYOUT_PROPERTY_NAME_STR, optionsHandle);
}
//=============================================================================
void
moveAxesToFront(GOFigure* fig, int64 handle)
{
    if (!fig) {
        return;
    }
    GOGObjectsProperty* cp = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(cp->data());

    int64 i = 0;
    while (i < (int64)children.size()) {
        if (children[i] == handle) {
            children.erase(children.begin() + i);
        } else {
            i++;
        }
    }

    children.insert(children.begin(), 1, handle);
    cp->data(children);
}
//=============================================================================
ArrayOfVector
buildReturnValue(int nLhs, int64 handle)
{
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << graphicsObjectHandle(handle);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
activateOrCreateAxesFromHandle(int nLhs, const ArrayOfVector& argIn)
{
    int64 handle = argIn[0].asGraphicsObjectScalar();
    GraphicsObject* hp = getGraphicsObjectFromGraphicHandle(handle);
    if (!hp) {
        Error("Nelson:graphics:invalidHandle", "Invalid handle.");
    }

    if (hp->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        int64 currentAxes = hp->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
        if (currentAxes == 0 || isDeletedGraphicsObject(currentAxes)) {
            GOFigure* fig = dynamic_cast<GOFigure*>(hp);
            if (!fig) {
                Error("Nelson:graphics:invalidFigureCast",
                    "Failed to cast GraphicsObject to GOFigure.");
                return ArrayOfVector();
            }
            ArrayOfVector t;
            int64 newHandle = createNewAxes(figureAxesParent(fig, handle), t, new GOAxis);
            return buildReturnValue(nLhs, newHandle);
        }
        handle = currentAxes;
        hp = getGraphicsObjectFromGraphicHandle(handle);
    }

    if (!hp->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        Error("Nelson:axes:expectedAxesHandle",
            "Single argument to axes function must be handle for an axes.");
    }

    GOFigure* fig = hp->getParentFigure();
    if (!fig) {
        auto [ownerFigure, ownerFigureId] = findFigureOwningChild(handle);
        fig = ownerFigure;
        if (fig && ownerFigureId != -1) {
            hp->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, ownerFigureId);
        }
    }
    if (!fig) {
        Error("Nelson:axes:noParentFigure", "Axes object does not have a valid parent figure.");
    }
    // axes(h) promotes an existing axes to current; it does not pick an object.
    fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);
    moveAxesToFront(fig, handle);
    fig->setRenderingStateInvalid(true);
    if (!fig->isDrawLater()) {
        fig->repaint();
    }

    return buildReturnValue(nLhs, handle);
}
//=============================================================================
std::pair<GOFigure*, int64>
findFigureOwningChild(int64 childHandle)
{
    std::vector<int64> figures = getFigureGraphicsObjects();
    for (int64 figHandle : figures) {
        GOFigure* fig = findGOFigure(figHandle);
        if (!fig) {
            continue;
        }
        GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
            fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            continue;
        }
        for (int64 child : children->data()) {
            if (child == childHandle) {
                return { fig, figHandle };
            }
        }
    }
    return { nullptr, -1 };
}
//=============================================================================
bool
findExplicitAxesParent(const ArrayOfVector& properties, ResolvedAxesParent& parent)
{
    for (size_t k = 0; k + 1 < properties.size(); k += 2) {
        if (!properties[k].isCharacterArray()) {
            continue;
        }
        std::wstring propname(properties[k].asWideString());
        StringHelpers::to_lower(propname);
        if (propname != L"parent") {
            continue;
        }
        if (!properties[k + 1].isGraphicsObject()) {
            Error("Nelson:axes:invalidParent",
                "Parent must be a figure or UI container graphics object.");
        }
        parent = resolveAxesParent(properties[k + 1].asGraphicsObjectScalar());
        return true;
    }
    return false;
}
//=============================================================================
ResolvedAxesParent
figureAxesParent(GOFigure* fig, int64 figureId)
{
    ResolvedAxesParent parent;
    parent.fig = fig;
    parent.figureId = figureId;
    parent.parentId = figureId;
    return parent;
}
//=============================================================================
static int64
findAncestorFigureId(GraphicsObject* object)
{
    GraphicsObject* current = object;
    for (int guard = 0; guard < 64 && current; ++guard) {
        GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
            current->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!parent || parent->data().empty()) {
            return -1;
        }
        int64 parentId = parent->data()[0];
        if (parentId == HANDLE_ROOT_OBJECT || parentId == HANDLE_UNASSIGNED_OBJECT) {
            return -1;
        }
        if (parentId < HANDLE_OFFSET_OBJECT) {
            return parentId;
        }
        current = findGraphicsObject(parentId, false);
    }
    return -1;
}
//=============================================================================
ResolvedAxesParent
resolveAxesParent(int64 handle)
{
    GraphicsObject* object = getGraphicsObjectFromGraphicHandle(handle);
    if (!object) {
        Error("Nelson:graphics:invalidHandle", "Invalid handle.");
    }
    if (object->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        return figureAxesParent(static_cast<GOFigure*>(object), handle);
    }
    GOUIContainer* container = dynamic_cast<GOUIContainer*>(object);
    if (dynamic_cast<GOUITabGroup*>(container)) {
        Error("Nelson:axes:parentMustNotBeTabGroup", "Axes cannot be a child of TabGroup.");
    }
    if (!container) {
        Error("Nelson:axes:invalidParent",
            "Parent must be a figure or UI container graphics object.");
    }
    int64 figureId = findAncestorFigureId(container);
    GOFigure* fig = figureId == -1 ? nullptr : findGOFigure(figureId);
    if (!fig) {
        Error("Nelson:axes:parentNotAttachedToFigure",
            "Invalid parent: container is not attached to a figure.");
    }
    ResolvedAxesParent parent;
    parent.fig = fig;
    parent.figureId = figureId;
    parent.parentId = handle;
    parent.parentIsGridLayout = dynamic_cast<GOUIGridLayout*>(container) != nullptr;
    return parent;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
