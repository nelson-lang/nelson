//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "figureBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RefreshFigure.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "QueueProcessing.hpp"
#include "SizeChangedDispatcher.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
figureBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval = {};

    processPendingCallbacksAndTimers();

    // A SizeChangedFcn given at creation runs before the constructor returns.
    SizeChangedScope sizeChangedScope;
    bool figureCreated = false;
    // A newly created figure whose window-open notification is deferred until its
    // creation properties (notably Position) are applied, so the native window
    // opens at the final size instead of popping at the default and resizing.
    bool lifecyclePending = false;
    int64 currentFigureID;
    if (argIn.size() == 0) {
        currentFigureID = createNewFigure();
        GOFigure* fig = findGOFigure(currentFigureID);
        if (!fig) {
            Error(L"Nelson:graphics:invalidFigureHandle", L"Invalid figure handle.");
        }
        retval << graphicsObjectHandle(currentFigureID);
        return retval;
    }
    size_t pos = 0;
    if (argIn[0].isNumeric()) {
        int64 fignum = argIn[0].asInt64Scalar();
        if ((fignum <= 0) || (fignum > MAX_FIGS)) {
            Error("Nelson:figure:numberOutOfRange",
                "figure number is out of range - it must be between 1 and 2147483647.");
        }
        currentFigureID = selectFigure(fignum - 1, false, true, &lifecyclePending);
        pos = pos + 1;
    } else if (argIn[0].isGraphicsObject()) {
        int64 handle = argIn[0].asGraphicsObjectScalar();
        if (handle == HANDLE_ROOT_OBJECT || handle >= HANDLE_OFFSET_OBJECT) {
            Error("Nelson:figure:expectedFigure", "figure graphics object expected.");
        } else {
            currentFigureID = selectFigure(handle, false, true, &lifecyclePending);
            pos = pos + 1;
        }
    } else {
        currentFigureID = createNewFigure(false, true);
        figureCreated = true;
        lifecyclePending = true;
    }
    GOFigure* fig = findGOFigure(currentFigureID);
    if (!fig) {
        Error(L"Nelson:graphics:invalidFigureHandle", L"Invalid figure handle.");
    }
    if ((argIn.size() - pos) % 2 == 0) {
        GraphicsObject* go = static_cast<GraphicsObject*>(fig);
        for (size_t k = pos; k < argIn.size(); k = k + 2) {
            if ((argIn[k].isStringArray() && argIn[k].isScalar())
                || argIn[k].isRowVectorCharacterArray()) {
                const std::wstring propertyName
                    = go->resolvePartialPropertyName(argIn[k].asWideString());
                if (!go->isWritable(propertyName) && go->haveProperty(propertyName)) {
                    Error(L"Nelson:figure:readOnlyProperty", L"Property is read-only: %s",
                        propertyName);
                }
                if (propertyName == GO_THEME_PROPERTY_NAME_STR) {
                    fig->setTheme(argIn[k + 1].asWideString(), true);
                } else if (propertyName == GO_THEME_MODE_PROPERTY_NAME_STR) {
                    fig->setThemeMode(argIn[k + 1].asWideString());
                } else {
                    GOGenericProperty* propertyValue = go->findProperty(propertyName);
                    propertyValue->set(argIn[k + 1]);
                }
            }
        }
        if (figureCreated) {
            // The window was sized before the properties were applied: report it again.
            forgetSizeChangedState(fig);
        }
        if (lifecyclePending) {
            // Open the figure's native window now, with Position applied, so it
            // opens at its final size. Before updateState(true): a 'Visible','off'
            // at creation then hides an existing window, instead of the hide
            // finding no window and the figure staying on screen.
            pushNewFigureLifecycle(currentFigureID);
        }
        fig->updateState(true);
        if (figureCreated) {
            notifyFigureShown(fig);
        }
    } else {
        Error("Nelson:wrongNumberOfInputs", "Wrong number of input arguments.");
    }
    retval << graphicsObjectHandle(currentFigureID);

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)fig->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->executeNow(fig);
    }
    fig->setRenderingStateInvalid(true);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
