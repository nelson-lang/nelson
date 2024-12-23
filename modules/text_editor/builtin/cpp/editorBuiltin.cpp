//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "editorBuiltin.hpp"
#include "Error.hpp"
#include "TextEditor.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "NelsonConfiguration.hpp"
#include "SystemCommand.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TextEditorGateway::editorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
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
        std::wstring filename = argIn[0].getContentAsWideString();
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
        std::wstring option = argIn[0].getContentAsWideString();
        if (option == L"new_file") {
            if (argIn[1].isEmpty() && argIn[1].isDoubleClass()) {
                if (NelsonConfiguration::getInstance()->useEmbeddedEditor()) {
                    textEditor(eval, true);
                }
            } else {
                Error(_W("Wrong value for #2 argument."));
            }
        } else if (option == L"editor_command") {
            std::wstring commandLine = argIn[1].getContentAsWideString();
            NelsonConfiguration::getInstance()->setCurrentEditor(commandLine, true);
        } else {
            Error(_W("Wrong value for #1 argument."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
