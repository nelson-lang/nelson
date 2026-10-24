//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "editorBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#ifdef __EMSCRIPTEN__
#include "WebDesktopBridge.hpp"
#else
#include "TextEditor.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "NelSon_engine_mode.h"
#include "SystemCommand.hpp"
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TextEditorGateway::editorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
#ifdef __EMSCRIPTEN__
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 0);

    std::wstring path;
    int line = 0;
    if (!argIn.empty()) {
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(L"Nelson:editor:invalidArgument:wrongTypeInputArgument",
                L"Wrong type for input argument #%d.", 1);
        }
        path = argIn[0].asWideString();
    }
    if (argIn.size() == 2) {
        if (path == L"new_file" && argIn[1].isEmpty() && argIn[1].isDoubleClass()) {
            path.clear();
        } else if (argIn[1].isNumeric() && argIn[1].isScalar()) {
            line = static_cast<int>(argIn[1].asDoubleScalar());
        } else {
            Error(L"Nelson:editor:invalidArgument:wrongValueInputArgument",
                L"Wrong value for input argument #%d.", 2);
        }
    }

    if (!WebDesktopBridge::getInstance()->openWebEditor(path, line)) {
        Error(L"Nelson:editor:unavailable", L"The web editor is unavailable.");
    }
    return {};
#else
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    // In the web desktop, route to the Monaco editor panel instead of the Qt
    // text editor (an empty path opens a new file). Configuration sub-commands
    // (editor_command / vscode_command) fall through to the normal handling.
    const auto mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (mode == WEB_GUI || mode == WASM_TERMINAL) {
        if (argIn.size() == 0) {
            WebDesktopBridge::getInstance()->openWebEditor(L"");
            return retval;
        }
        if (argIn.size() == 1) {
            WebDesktopBridge::getInstance()->openWebEditor(argIn[0].asWideString());
            return retval;
        }
        if (argIn.size() == 2 && argIn[0].isRowVectorCharacterArray()
            && argIn[0].asWideString() == L"new_file") {
            WebDesktopBridge::getInstance()->openWebEditor(L"");
            return retval;
        }
        if (argIn.size() == 2) {
            int line = argIn[1].isNumeric() && argIn[1].isScalar()
                ? static_cast<int>(argIn[1].asDoubleScalar())
                : 0;
            WebDesktopBridge::getInstance()->openWebEditor(argIn[0].asWideString(), line);
            return retval;
        }
    }
    switch (argIn.size()) {
    case 0: {
        if (NelsonConfiguration::getInstance()->useEmbeddedEditor()) {
            textEditor(eval);
        } else {
            std::wstring command;
#ifdef _MSC_VER
            command = L"start /b " + NelsonConfiguration::getInstance()->getCurrentEditor();
#else
            command = NelsonConfiguration::getInstance()->getCurrentEditor() + L" &";
#endif
            SystemCommand(command, 15, false, eval->getID());
        }
    } break;
    case 1: {
        std::wstring filename = argIn[0].asWideString();
        if (NelsonConfiguration::getInstance()->useEmbeddedEditor()) {
            textEditor(eval, filename);
        } else {
            std::wstring command;
#if _MSC_VER
            command = L"start /b " + NelsonConfiguration::getInstance()->getCurrentEditor()
                + std::wstring(L" ") + filename;
#else
            command
                = NelsonConfiguration::getInstance()->getCurrentEditor() + L" " + filename + L" &";
#endif
            SystemCommand(command, 15, false, eval->getID());
        }
    } break;
    case 2: {
        std::wstring option = argIn[0].asWideString();
        if (argIn[1].isNumeric() && argIn[1].isScalar()) {
            int lineNumber = static_cast<int>(argIn[1].asDoubleScalar());
            if (NelsonConfiguration::getInstance()->useEmbeddedEditor()) {
                textEditor(eval, option, lineNumber);
            } else {
                std::wstring command;
#if _MSC_VER
                command = L"start /b " + NelsonConfiguration::getInstance()->getCurrentEditor()
                    + std::wstring(L" ") + option;
#else
                command = NelsonConfiguration::getInstance()->getCurrentEditor() + L" " + option
                    + L" &";
#endif
                SystemCommand(command, 15, false, eval->getID());
            }
        } else if (option == L"new_file") {
            if (argIn[1].isEmpty() && argIn[1].isDoubleClass()) {
                if (NelsonConfiguration::getInstance()->useEmbeddedEditor()) {
                    textEditor(eval, true);
                }
            } else {
                Error(L"Nelson:editor:invalidArgument:wrongValueArgument",
                    L"Wrong value for #%d argument.", 2);
            }
        } else if (option == L"editor_command") {
            std::wstring commandLine = argIn[1].asWideString();
            NelsonConfiguration::getInstance()->setCurrentEditor(commandLine, true);
        } else if (option == L"vscode_command") {
            std::wstring commandLine = argIn[1].asWideString();
            NelsonConfiguration::getInstance()->setCurrentEditor(commandLine, false);
            NelsonConfiguration::getInstance()->setVsCodeMode(true);
        } else {
            Error(L"Nelson:editor:invalidArgument:wrongValueArgument",
                L"Wrong value for #%d argument.", 1);
        }
    } break;
    default: {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    } break;
    }
    return retval;
#endif
}
//=============================================================================
