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
#include "rmfileBuiltin.hpp"
#include "Error.hpp"
#include "RemoveFile.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::rmfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    std::wstring filenameToDelete = param1.getContentAsWideString();
    std::wstring msg;
    bool bRes = false;
    switch (nLhs) {
    case 0: {
        bRes = RemoveFile(filenameToDelete, msg);
        if (bRes == false) {
            Error(msg);
        }
    } break;
    case 1:
        bRes = RemoveFile(filenameToDelete, msg);
        retval.push_back(ArrayOf::logicalConstructor(bRes));
        break;
    case 2:
        bRes = RemoveFile(filenameToDelete, msg);
        retval.push_back(ArrayOf::logicalConstructor(bRes));
        retval.push_back(ArrayOf::characterArrayConstructor(msg));
        break;
    default:
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        break;
    }
    return retval;
}
//=============================================================================
