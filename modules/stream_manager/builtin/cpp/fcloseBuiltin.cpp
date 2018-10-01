//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    FilesManager* fm = (FilesManager*)(eval->FileManager);
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        int32 iValue = (int32)param1.getContentAsDoubleScalar();
        if (fm->isOpened(iValue)) {
            if (FileClose(fm, iValue)) {
                retval.push_back(ArrayOf::doubleConstructor(0.));
            } else {
                retval.push_back(ArrayOf::doubleConstructor(-1.));
            }
        } else {
            Error(_W("Invalid file identifier."));
        }
    } else if (param1.isRowVectorCharacterArray()) {
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
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
