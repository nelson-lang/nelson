//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__rotate3d__Builtin.hpp"
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
rotate3dOption(GOAxis* axis, const std::wstring& option);
//=============================================================================
ArrayOfVector
__rotate3d__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);

    // __rotate3d__(ax, 'on' | 'off' | 'toggle')

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

    std::wstring option = L"toggle";
    if (argIn.size() == 1) {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            option = argIn[1].asWideString();
        } else {
            Error(L"Nelson:graphics:invalidArgumentType", L"Wrong type for #%d argument.", 2);
        }
    }
    rotate3dOption(axis, option);
    return {};
}
//=============================================================================
void
rotate3dOption(GOAxis* axis, const std::wstring& option)
{
    if (option == L"on") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        if (goWin->currentHostMouseMode() != MOUSE_MODE::ROTATION) {
            goWin->toggleHostMouseMode(MOUSE_MODE::ROTATION);
        }
    } else if (option == L"off") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        if (goWin->currentHostMouseMode() == MOUSE_MODE::ROTATION) {
            goWin->toggleHostMouseMode(MOUSE_MODE::ROTATION);
        }
    } else if (option == L"toggle") {
        GOFigure* goFig = axis->getParentFigure();
        GOFigureHost* goWin = goFig->getHost();
        goWin->toggleHostMouseMode(MOUSE_MODE::ROTATION);
    }
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
