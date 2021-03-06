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
#include "filepartsBuiltin.hpp"
#include "Error.hpp"
#include "FileParts.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::filepartsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if ((argIn.size() == 1) || (argIn.size() == 2)) {
        std::wstring wpath;
        std::wstring wtype;
        if (argIn.size() == 2) {
            if (nLhs > 1) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (argIn[1].isRowVectorCharacterArray()) {
                wtype = argIn[1].getContentAsWideString();
                if (wtype == L"path") {
                    // OK
                } else if (wtype == L"filename") {
                    // OK
                } else if (wtype == L"extension") {
                    // OK
                } else {
                    Error(_W("Argument #2 must contain a valid string 'path', 'filename' or "
                             "'extension' expected."));
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else {
            if (nLhs > 3) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        }
        if (argIn[0].isRowVectorCharacterArray()) {
            wpath = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring respath;
        std::wstring resfilename;
        std::wstring resextension;
        FileParts(wpath, respath, resfilename, resextension);
        if (wtype.empty()) {
            retval << ArrayOf::characterArrayConstructor(respath);
            if (nLhs > 1) {
                retval << ArrayOf::characterArrayConstructor(resfilename);
            }
            if (nLhs > 2) {
                retval << ArrayOf::characterArrayConstructor(resextension);
            }
        } else {
            if (wtype == L"path") {
                retval << ArrayOf::characterArrayConstructor(respath);
            } else if (wtype == L"filename") {
                retval << ArrayOf::characterArrayConstructor(resfilename);
            } else if (wtype == L"extension") {
                retval << ArrayOf::characterArrayConstructor(resextension);
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
