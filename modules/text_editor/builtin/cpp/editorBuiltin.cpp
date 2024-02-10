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
        textEditor(eval);
    } break;
    case 1: {
        std::wstring filename = argIn[0].getContentAsWideString();
        textEditor(eval, filename);
    } break;
    case 2: {
        std::wstring option = argIn[0].getContentAsWideString();
        if (option == L"new_file") {
            if (argIn[1].isEmpty() && argIn[1].isDoubleClass()) {
                textEditor(eval, true);
            } else {
                Error(_W("Wrong value for #2 argument."));
            }
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
