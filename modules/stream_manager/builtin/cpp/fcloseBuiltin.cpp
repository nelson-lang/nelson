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
#include "fcloseBuiltin.hpp"
#include "Error.hpp"
#include "FileClose.hpp"
#include "FilesManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fcloseBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    auto* fm = static_cast<FilesManager*>(eval->FileManager);
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        nargoutcheck(nLhs, 0, 1);
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm->isOpened(iValue)) {
            if (FileClose(fm, iValue)) {
                retval << ArrayOf::doubleConstructor(0.);
            } else {
                retval << ArrayOf::doubleConstructor(-1.);
            }
        } else {
            Error(_W("Invalid file identifier."));
        }
    } else if (param1.isRowVectorCharacterArray()) {
        nargoutcheck(nLhs, 0, 0);
        std::wstring str = param1.getContentAsWideString();
        if (str == L"all") {
            Nelson::FilesManager* nfm;
            try {
                nfm = new Nelson::FilesManager();
            } catch (const std::bad_alloc&) {
                nfm = nullptr;
            }
            if (nfm) {
                delete fm;
                eval->FileManager = (void*)nfm;
            } else {
                Error(_W("Cannot close files."));
            }
        } else {
            Error(_W("Wrong value for #1: 'all' expected."));
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_DOUBLE_EXPECTED);
    }
    return retval;
}
//=============================================================================
