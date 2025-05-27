//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "axesBuiltin.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOFiguresManager.hpp"
#include "GOAxis.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOGObjectsProperty.hpp"
#include "GraphicsObject.hpp"
#include "GOColorVectorProperty.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static GraphicsObject*
getGraphicsObjectFromGraphicHandle(nelson_handle go);
static ArrayOfVector
setupVector2DProperties();
static int64
createNewAxes(
    GOFigure* fig, int64 currentFigureID, const ArrayOfVector& properties, bool restoreLimMode);
static void
copyColorMapFromFigureToAxes(GOFigure* fig, GraphicsObject* axes);
static void
addAxesToFigureChildren(GOFigure* fig, int64 axesHandle);
static void
moveAxesToFront(GOFigure* fig, int64 handle);
static ArrayOfVector
buildReturnValue(int nLhs, int64 handle);
static ArrayOfVector
activateOrCreateAxesFromHandle(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
axesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    if (argIn.size() == 1) {
        return activateOrCreateAxesFromHandle(nLhs, argIn);
    }
    // Handle multiple arguments or no arguments
    ArrayOfVector t(argIn);
    GOFigure* fig = nullptr;
    int64 currentFigureID;

    if (argIn.size() != 0 && argIn[0].isGraphicsObject()) {
        int64 handle = (unsigned int)argIn[0].getContentAsGraphicsObjectScalar();
        GraphicsObject* hp = getGraphicsObjectFromGraphicHandle(handle);
        if (!hp->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
            Error(_("Single argument to axes function must be handle for a figure."));
        }
        t.pop_front();
        currentFigureID = handle;
        fig = (GOFigure*)hp;
    } else {
        currentFigureID = getCurrentFigure();
        if (currentFigureID == -1) {
            currentFigureID = createNewFigure();
        }
        fig = getCurrentGOFigure();
    }
    bool restoreLimMode = false;
    if (t.empty()) {
        t = setupVector2DProperties();
        restoreLimMode = true;
    }

    int64 handle = createNewAxes(fig, currentFigureID, t, restoreLimMode);
    return buildReturnValue(nLhs, handle);
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
ArrayOfVector
setupVector2DProperties()
{
    ArrayOfVector t;
    ArrayOf vector2D = ArrayOf::doubleVectorConstructor(2);
    double* ptr = (double*)vector2D.getDataPointer();
    ptr[0] = 0;
    ptr[1] = 1;
    t.push_back(ArrayOf::characterArrayConstructor(GO_X_LIM_PROPERTY_NAME_STR));
    t.push_back(vector2D);
    t.push_back(ArrayOf::characterArrayConstructor(GO_Y_LIM_PROPERTY_NAME_STR));
    t.push_back(vector2D);
    return t;
}
//=============================================================================
int64
createNewAxes(
    GOFigure* fig, int64 currentFigureID, const ArrayOfVector& properties, bool restoreLimMode)
{
    GraphicsObject* fp = new GOAxis;
    int64 handle = assignGraphicsObject(fp);

    // Set properties
    ArrayOfVector t(properties);
    while (t.size() >= 2) {
        std::wstring propname(t[0].getContentAsWideString());
        fp->findProperty(propname)->set(t[1]);
        t.pop_front();
        t.pop_front();
    }

    fp->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, currentFigureID);
    fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);

    copyColorMapFromFigureToAxes(fig, fp);
    addAxesToFigureChildren(fig, handle);

    fp->updateState();

    if (restoreLimMode) {
        fp->setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        fp->setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        fp->setRestrictedStringDefault(GO_Z_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    }

    fig->setRenderingStateInvalid(true);
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
addAxesToFigureChildren(GOFigure* fig, int64 axesHandle)
{
    GOGObjectsProperty* hp = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(hp->data());
    children.push_back(axesHandle);
    hp->data(children);
}
//=============================================================================
void
moveAxesToFront(GOFigure* fig, int64 handle)
{
    GOGObjectsProperty* cp = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(cp->data());

    // Remove handle if it exists
    int64 i = 0;
    while (i < (int64)children.size()) {
        if (children[i] == handle) {
            children.erase(children.begin() + i);
        } else {
            i++;
        }
    }

    // Insert at the beginning
    children.insert(children.begin(), 1, handle);
    cp->data(children);
}
//=============================================================================
ArrayOfVector
buildReturnValue(int nLhs, int64 handle)
{
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << ArrayOf::graphicsObjectConstructor(handle);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
activateOrCreateAxesFromHandle(int nLhs, const ArrayOfVector& argIn)
{
    int64 handle = (unsigned int)argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* hp = getGraphicsObjectFromGraphicHandle(handle);

    if (hp->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        int64 currentAxes = hp->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
        if (currentAxes == 0 || isDeletedGraphicsObject(currentAxes)) {
            // Create new axes for this figure
            GOFigure* fig = (GOFigure*)hp;
            ArrayOfVector t = setupVector2DProperties();
            int64 newHandle = createNewAxes(fig, handle, t, true);
            return buildReturnValue(nLhs, newHandle);
        }
        handle = currentAxes;
        hp = getGraphicsObjectFromGraphicHandle(handle);
    }

    if (!hp->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        Error(_("Single argument to axes function must be handle for an axes."));
    }

    GOFigure* fig = hp->getParentFigure();
    fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);
    moveAxesToFront(fig, handle);
    fig->setRenderingStateInvalid(true);
    fig->repaint();

    return buildReturnValue(nLhs, handle);
}
//=============================================================================
}
//=============================================================================
