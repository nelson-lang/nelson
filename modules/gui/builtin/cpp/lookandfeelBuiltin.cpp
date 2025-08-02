//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lookandfeelBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "QtLookAndFeel.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::lookandfeelBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    std::wstring previousLf = GetCurrentLookAndFeel();
    switch (argIn.size()) {
    case 0: {
        retval << ArrayOf::characterArrayConstructor(previousLf);
    } break;
    case 1: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"stylesheet") {
            std::wstring previousStyleSheet = GetCurrentStyleSheet();
            retval << ArrayOf::characterArrayConstructor(previousStyleSheet);
            return retval;
        }
        if (param1 == L"available") {
            wstringVector lfs = GetLookAndFeelAvailable();
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(lfs);
            return retval;
        }
        bool res = SetCurrentLookAndFeel(param1);
        if (!res) {
            Error(_W("look and feel not applied."));
        }
        retval << ArrayOf::characterArrayConstructor(previousLf);
    } break;
    case 2: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"stylesheet") {
            std::wstring param2 = argIn[1].getContentAsWideString();
            std::wstring previousStyleSheet = GetCurrentStyleSheet();
            SetCurrentStyleSheet(param2);
            retval << ArrayOf::characterArrayConstructor(previousStyleSheet);
        } else {
            Error(_W("\"stylesheet\" expected as first argument."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
