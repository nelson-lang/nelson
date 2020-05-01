//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "lookandfeelBuiltin.hpp"
#include "Error.hpp"
#include "QtLookAndFeel.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::lookandfeelBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring previousLf = GetCurrentLookAndFeel();
    switch (argIn.size()) {
    case 0: {
        retval.push_back(ArrayOf::characterArrayConstructor(previousLf));
    } break;
    case 1: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"stylesheet") {
            std::wstring previousStyleSheet = GetCurrentStyleSheet();
            retval.push_back(ArrayOf::characterArrayConstructor(previousStyleSheet));
            return retval;
        } else if (param1 == L"available") {
            wstringVector lfs = GetLookAndFeelAvailable();
            retval.push_back(ToCellStringAsColumn(lfs));
            return retval;
        }
        bool res = SetCurrentLookAndFeel(param1);
        if (!res) {
            Error(_W("look and feel not applied."));
        }
        retval.push_back(ArrayOf::characterArrayConstructor(previousLf));
    } break;
    case 2: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"stylesheet") {
            std::wstring param2 = argIn[1].getContentAsWideString();
            std::wstring previousStyleSheet = GetCurrentStyleSheet();
            SetCurrentStyleSheet(param2);
            retval.push_back(ArrayOf::characterArrayConstructor(previousStyleSheet));
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
