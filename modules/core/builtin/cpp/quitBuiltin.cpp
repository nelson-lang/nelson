//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "quitBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebDesktopBridge.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "BuiltInFunctionDef.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static int
getExitCodeFromVariable(const ArrayOfVector& argIn);
static bool
isLoadedBuiltin(const std::string& name);
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::quitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(0, 0);
    // Close figure windows while the interpreter and the Qt event loop are still
    // alive: destroying a figure's OpenGL context during the C++ exit teardown
    // deadlocks in wglDeleteContext on Windows. Only the Qt/OpenGL figure hosts
    // need it (modes with a GUI event loop); WEB_GUI renders in the browser and has
    // no GL context. Best-effort, and skipped when the graphics gateway is not
    // loaded. Function-call syntax keeps the lexer off the command-shortcut path.
    if (eval->haveEventsLoop() && !WebDesktopBridge::getInstance()->hasWebFigureFrameHook()
        && isLoadedBuiltin("close")) {
        try {
            eval->evaluateString(L"close('all');\n", false);
        } catch (...) {
        }
    }
    ArrayOfVector retval;
    int iValue = 0;
    switch (argIn.size()) {
    case 0: {
        eval->setExitCode(iValue);
        eval->setState(NLS_STATE_QUIT);
        return retval;
    } break;
    case 1: {
        if ((argIn[0].isStringArray() && argIn[0].isScalar())
            || argIn[0].isRowVectorCharacterArray()) {

            std::wstring quitCommand = argIn[0].asWideString();
            if (quitCommand == L"cancel") {
                iValue = 0;
                eval->setExitCode(iValue);
                eval->setState(NLS_STATE_CANCEL_QUIT);
                return retval;

            } else if (quitCommand == L"force") {
                iValue = 0;
                eval->setExitCode(iValue);
                eval->setState(NLS_STATE_FORCE_QUIT);
                return retval;
            } else {
                Error("Nelson:quit:invalidOption", "Unknown option.");
            }
        }
        iValue = getExitCodeFromVariable(argIn);
        eval->setExitCode(iValue);
        eval->setState(NLS_STATE_QUIT);
    } break;
    case 2: {
        bool isForce = ((argIn[1].isStringArray() && argIn[1].isScalar())
                           || argIn[1].isRowVectorCharacterArray())
            && (argIn[1].asWideString() == L"force");
        if (!isForce) {
            Error("Nelson:quit:invalidOption", "Unknown option.");
        }
        iValue = getExitCodeFromVariable(argIn);
        eval->setExitCode(iValue);
        eval->setState(NLS_STATE_FORCE_QUIT);
    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
int
getExitCodeFromVariable(const ArrayOfVector& argIn)
{
    int iValue = 0;
    if ((argIn[0].isNumeric() || argIn[0].isLogical()) && argIn[0].isScalar()) {
        double dValue = argIn[0].asDoubleScalar();
        iValue = static_cast<int>(dValue);
#ifndef _MSC_VER
        if (iValue < 0 || iValue > 255) {
            Error("Nelson:quit:invalidValue", "Value between 0 and 255 expected.");
        }
#endif
        if (static_cast<double>(iValue) != dValue) {
            Error(L"Nelson:expectedInteger", L"%s must be an integer.", L"Argument #1");
        }
    } else {
        Error("Nelson:quit:invalidOption", "Unknown option.");
    }
    return iValue;
}
//=============================================================================
static bool
isLoadedBuiltin(const std::string& name)
{
    FunctionDefPtr definition = nullptr;
    if (!BuiltInFunctionDefManager::getInstance()->find(name, definition) || !definition) {
        return false;
    }
    auto* builtin = static_cast<BuiltInFunctionDef*>(definition);
    return !builtin->isLazyGateway();
}
//=============================================================================
