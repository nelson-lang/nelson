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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "QtLookAndFeel.hpp"
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
        retval << ArrayOf::characterArray(previousLf);
    } break;
    case 1: {
        std::wstring param1 = argIn[0].asWideString();
        if (param1 == L"stylesheet") {
            std::wstring previousStyleSheet = GetCurrentStyleSheet();
            retval << ArrayOf::characterArray(previousStyleSheet);
            return retval;
        }
        if (param1 == L"available") {
            wstringVector lfs = GetLookAndFeelAvailable();
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(lfs);
            return retval;
        }
        bool res = SetCurrentLookAndFeel(param1);
        if (!res) {
            Error(L"Nelson:lookandfeel:notApplied", L"look and feel not applied.");
        }
        retval << ArrayOf::characterArray(previousLf);
    } break;
    case 2: {
        std::wstring param1 = argIn[0].asWideString();
        if (param1 == L"stylesheet") {
            std::wstring param2 = argIn[1].asWideString();
            std::wstring previousStyleSheet = GetCurrentStyleSheet();
            SetCurrentStyleSheet(param2);
            retval << ArrayOf::characterArray(previousStyleSheet);
        } else {
            Error(L"Nelson:lookandfeel:invalidOption", L"'%s' expected.", L"stylesheet");
        }
    } break;
    default: {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
