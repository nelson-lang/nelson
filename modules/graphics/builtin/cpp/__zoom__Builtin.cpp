//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__zoom__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }
    int64 handle = argIn[0].asGraphicsObjectScalar();

    bool isAxis = false;
    GraphicsObject* fp = nullptr;
    if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
        if (fp->getType() == GO_PROPERTY_VALUE_AXES_STR) {
            isAxis = true;
        }
    }
    if (!isAxis) {
        Error(L"Nelson:graphics:expectedAxes", L"Axes graphic object expected.");
    }
    GOAxis* axis = (GOAxis*)fp;

    if (argIn[1].isDoubleClass()) {
        double factor = argIn[1].asDoubleScalar();
        zoomFactor(axis, factor);
    } else if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
        std::wstring option = argIn[1].asWideString();
        zoomOption(axis, option);
    } else {
        Error(L"Nelson:graphics:invalidArgumentType", L"Wrong type for #%d argument.", 2);
    }
    return {};
}
//=============================================================================
void
zoomOption(GOAxis* axis, const std::wstring& option)
{
    if (option == L"on") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        if (goWin->currentHostMouseMode() != MOUSE_MODE::ZOOM_IN) {
            goWin->toggleHostMouseMode(MOUSE_MODE::ZOOM_IN);
        }
        axis->zoomOn();
    } else if (option == L"off") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        if (goWin->currentHostMouseMode() == MOUSE_MODE::ZOOM_IN) {
            goWin->toggleHostMouseMode(MOUSE_MODE::ZOOM_IN);
        }
        axis->zoomOff();
    } else if (option == L"reset") {
        axis->zoomReset();
    } else if (option == L"out") {
        axis->zoomOut();
    } else if (option == L"xon") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        if (goWin->currentHostMouseMode() != MOUSE_MODE::ZOOM_IN) {
            goWin->toggleHostMouseMode(MOUSE_MODE::ZOOM_IN);
        }
        axis->zoomXMode();
    } else if (option == L"yon") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        if (goWin->currentHostMouseMode() != MOUSE_MODE::ZOOM_IN) {
            goWin->toggleHostMouseMode(MOUSE_MODE::ZOOM_IN);
        }
        axis->zoomYMode();
    } else if (option == L"toggle") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        goWin->toggleHostMouseMode(MOUSE_MODE::ZOOM_IN);
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
        axis->zoom(1. / factor);
    }
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
