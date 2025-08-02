//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "questdlgBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "QuestionBox.hpp"
#include "Warning.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::questdlgBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // s = questdlg(quest)
    // s = questdlg(quest, dlgtitle)
    // s = questdlg(quest, dlgtitle, defbtn)
    // s = questdlg(quest, dlgtitle, btn1, btn2, activeBtn)
    // s = questdlg(quest, dlgtitle, btn1, btn2, btn3, activeBtn)
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 6);
    std::wstring title;
    std::wstring question;
    std::wstring defaultButton;
    std::wstring button1;
    std::wstring button2;
    std::wstring button3;
    int numButtons;

    ArrayOf param1 = argIn[0];
    if (param1.isRowVectorCharacterArray()) {
        question = param1.getContentAsWideString();
    } else if (param1.isStringArray()) {
        wstringVector lines = param1.getContentAsWideStringVector(false);
        question = lines[0];
        for (size_t k = 1; k < lines.size(); ++k) {
            question += L"\n";
            question += lines[k];
        }
    } else if (param1.isCellArrayOfCharacterVectors()) {
        wstringVector lines = param1.getContentAsWideStringVector();
        question = lines[0];
        for (size_t k = 1; k < lines.size(); ++k) {
            question += L"\n";
            question += lines[k];
        }
    } else {
        Error(_W("char, cell, or string expected."));
    }

    if (argIn.size() > 1) {
        title = argIn[1].getContentAsWideString();
    }
    if (argIn.size() == 3) {
        defaultButton = argIn[2].getContentAsWideString();
    }
    if (argIn.size() == 5) {
        button1 = argIn[2].getContentAsWideString();
        button2 = argIn[3].getContentAsWideString();
        defaultButton = argIn[4].getContentAsWideString();
        numButtons = 2;
    }
    if (argIn.size() == 6) {
        button1 = argIn[2].getContentAsWideString();
        button2 = argIn[3].getContentAsWideString();
        button3 = argIn[4].getContentAsWideString();
        defaultButton = argIn[5].getContentAsWideString();
        numButtons = 3;
    }

    if (argIn.size() == 1) {
        title = L" ";
    }
    if (argIn.size() <= 2) {
        defaultButton = _W("Yes");
    }
    if (argIn.size() == 3) {
        defaultButton = button1;
    }
    if (argIn.size() <= 3) {
        button1 = _W("Yes");
        button2 = _W("No");
        button3 = _W("Cancel");
        numButtons = 3;
    }

    retval << QuestionBox(title, question, button1, button2, button3, defaultButton, numButtons);
    return retval;
}
//=============================================================================
