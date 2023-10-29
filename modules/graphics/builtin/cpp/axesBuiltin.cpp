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
#include "Error.hpp"
#include "i18n.hpp"
#include "GOGObjectsProperty.hpp"
#include "GraphicsObject.hpp"
#include "GOColorVectorProperty.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
axesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 1) {
        bool restoreLimMode = false;
        GraphicsObject* fp = new GOAxis;
        int64 handle = assignGraphicsObject(fp);
        ArrayOfVector t(argIn);
        if (t.empty()) {
            ArrayOf vector2D = ArrayOf::doubleVectorConstructor(2);
            double* ptr = (double*)vector2D.getDataPointer();
            ptr[0] = 0;
            ptr[1] = 1;
            t.push_back(ArrayOf::characterArrayConstructor(GO_X_LIM_PROPERTY_NAME_STR));
            t.push_back(vector2D);
            t.push_back(ArrayOf::characterArrayConstructor(GO_Y_LIM_PROPERTY_NAME_STR));
            t.push_back(vector2D);
            restoreLimMode = true;
        }
        while (t.size() >= 2) {
            std::wstring propname(t[0].getContentAsWideString());
            fp->findProperty(propname)->set(t[1]);
            t.pop_front();
            t.pop_front();
        }
        int64 currentFigureID = getCurrentFigure();
        if (currentFigureID == -1) {
            currentFigureID = createNewFigure();
        }
        fp->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, currentFigureID);
        GOFigure* fig = getCurrentGOFigure();
        fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);

        GOColorVectorProperty* colormapFigure
            = (GOColorVectorProperty*)fig->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
        GOColorVectorProperty* colormapAxes
            = (GOColorVectorProperty*)fp->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
        if (colormapFigure && colormapAxes) {
            colormapAxes->data(colormapFigure->data());
        }

        GOGObjectsProperty* hp
            = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
        std::vector<int64> children(hp->data());
        children.push_back(handle);
        hp->data(children);
        fp->updateState();
        if (restoreLimMode) {
            fp->setRestrictedStringDefault(
                GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
            fp->setRestrictedStringDefault(
                GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
            fp->setRestrictedStringDefault(
                GO_Z_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        }
        ArrayOfVector retval;
        retval << ArrayOf::graphicsObjectConstructor(handle);
        return retval;
    }
    int64 handle = (unsigned int)argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* hp = findGraphicsObject(handle);
    if (!hp->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        Error(_("single argument to axes function must be handle for an axes."));
    }
    GOFigure* fig = getCurrentGOFigure();
    fig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);
    GOGObjectsProperty* cp = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(cp->data());
    int64 i = 0;
    while (i < (int64)children.size()) {
        if (children[i] == handle)
            children.erase(children.begin() + i);
        else {
            i++;
        }
    }
    children.insert(children.begin(), 1, handle);
    cp->data(children);
    fig->repaint();
    return ArrayOfVector();
}
//=============================================================================
}
//=============================================================================
