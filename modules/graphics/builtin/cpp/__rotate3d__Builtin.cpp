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
#include "PredefinedErrorMessages.hpp"
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
        raiseError(
            L"Nelson:graphics:ERROR_EXPECTED_GRAPHICS_OBJECT_S", ERROR_EXPECTED_GRAPHICS_OBJECT_S);
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
        raiseError(L"Nelson:graphics:ERROR_AXES_GRAPHIC_OBJECT_EXPECTED",
            ERROR_AXES_GRAPHIC_OBJECT_EXPECTED);
    }
    GOAxis* axis = (GOAxis*)fp;

    std::wstring option = L"toggle";
    if (argIn.size() == 1) {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            option = argIn[1].getContentAsWideString();
        } else {
            raiseError(L"Nelson:graphics:ERROR_WRONG_TYPE_FOR_2_ARGUMENT",
                ERROR_WRONG_TYPE_FOR_2_ARGUMENT);
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
        GOWindow* goWin = goFig->getGOWindow();
        if (goWin->getCurrentMouseMode() != MOUSE_MODE::ROTATION) {
            goWin->onRotateAction();
        }
    } else if (option == L"off") {
        GOFigure* goFig = axis->getParentFigure();
        GOWindow* goWin = goFig->getGOWindow();
        if (goWin->getCurrentMouseMode() == MOUSE_MODE::ROTATION) {
            goWin->onRotateAction();
        }
    } else if (option == L"toggle") {
        GOFigure* goFig = axis->getParentFigure();
        GOWindow* goWin = goFig->getGOWindow();
        goWin->onRotateAction();
    }
}
//=============================================================================
}
//=============================================================================
