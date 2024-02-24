//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__zoom__Builtin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOAxis.hpp"
#include "GOFiguresManager.hpp"
#include "axesBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
zoomOption(GOAxis* axis, const std::wstring& option);
//=============================================================================
static void
zoomFactor(GOAxis* axis, double factor);
//=============================================================================
ArrayOfVector
__zoom__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    // __zoom__(ax, factor)
    // zoom(ax, option) with option: 'on' | 'off' | 'reset' | 'out' | 'xon' | 'yon' | 'toggle'

    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);

    if (!argIn[0].isGraphicsObject()) {
        Error(_W("Expected graphics object(s)."));
    }
    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();

    bool isAxis = false;
    GraphicsObject* fp = nullptr;
    if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
        if (fp->getType() == GO_PROPERTY_VALUE_AXES_STR) {
            isAxis = true;
        }
    }
    if (!isAxis) {
        Error(_W("Axes graphic object expected."));
    }
    GOAxis* axis = (GOAxis*)fp;

    if (argIn[1].isDoubleClass()) {
        double factor = argIn[1].getContentAsDoubleScalar();
        zoomFactor(axis, factor);
    } else if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
        std::wstring option = argIn[1].getContentAsWideString();
        zoomOption(axis, option);
    } else {
        Error(_W("wrong type for #2 argument."));
    }
    return {};
}
//=============================================================================
void
zoomOption(GOAxis* axis, const std::wstring& option)
{
    if (option == L"on") {
        GOFigure* goFig = axis->getParentFigure();
        GOWindow* goWin = goFig->getGOWindow();
        if (goWin->getCurrentMouseMode() != MOUSE_MODE::ZOOM_IN) {
            goWin->onZoomInAction();
        }
        axis->zoomOn();
    } else if (option == L"off") {
        GOFigure* goFig = axis->getParentFigure();
        GOWindow* goWin = goFig->getGOWindow();
        if (goWin->getCurrentMouseMode() == MOUSE_MODE::ZOOM_IN) {
            goWin->onZoomInAction();
        }
        axis->zoomOff();
    } else if (option == L"reset") {
        axis->zoomReset();
    } else if (option == L"out") {
        axis->zoomOut();
    } else if (option == L"xon") {
        GOFigure* goFig = axis->getParentFigure();
        GOWindow* goWin = goFig->getGOWindow();
        if (goWin->getCurrentMouseMode() != MOUSE_MODE::ZOOM_IN) {
            goWin->onZoomInAction();
        }
        axis->zoomXMode();
    } else if (option == L"yon") {
        GOFigure* goFig = axis->getParentFigure();
        GOWindow* goWin = goFig->getGOWindow();
        if (goWin->getCurrentMouseMode() != MOUSE_MODE::ZOOM_IN) {
            goWin->onZoomInAction();
        }
        axis->zoomYMode();
    } else if (option == L"toggle") {
        GOFigure* goFig = axis->getParentFigure();
        GOWindow* goWin = goFig->getGOWindow();
        goWin->onZoomInAction();
    }
}
//=============================================================================
void
zoomFactor(GOAxis* axis, double factor)
{
    if (!std::isfinite(factor)) {
        return;
    }
    if (factor <= 0.) {
        return;
    }
    if (axis) {
        axis->zoom(factor);
    }
}
//=============================================================================
}
//=============================================================================
