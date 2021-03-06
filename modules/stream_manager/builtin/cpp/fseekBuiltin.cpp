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
#include "fseekBuiltin.hpp"
#include "Error.hpp"
#include "File.hpp"
#include "FileSeek.hpp"
#include "FilesManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fseekBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    int ORIGIN;
    if (param3.isRowVectorCharacterArray()) {
        std::wstring str = param3.getContentAsWideString();
        if ((str == L"bof") || (str == L"set")) {
            ORIGIN = -1;
        } else if ((str == L"cof") || (str == L"cur")) {
            ORIGIN = 0;
        } else if ((str == L"eof") || (str == L"end")) {
            ORIGIN = 1;
        } else {
            Error(_W("Invalid origin."));
        }
    } else {
        int iValue = static_cast<int>(param3.getContentAsDoubleScalar());
        switch (iValue) {
        case -1:
        case 0:
        case 1: {
            ORIGIN = iValue;
        } break;
        default: {
            Error(_W("Invalid origin."));
        } break;
        }
    }
    auto iOffset = static_cast<int64>(param2.getContentAsDoubleScalar());
    auto* fm = static_cast<FilesManager*>(eval->FileManager);
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
    if (fm->isOpened(iValue)) {
        File* f = fm->getFile(iValue);
        if (!FileSeek(f, iOffset, ORIGIN)) {
            retval << ArrayOf::doubleConstructor(-1);
        } else {
            retval << ArrayOf::doubleConstructor(0);
        }
    } else {
        Error(_W("Invalid file identifier."));
    }
    return retval;
}
//=============================================================================
