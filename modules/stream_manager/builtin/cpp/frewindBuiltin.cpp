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
#include "frewindBuiltin.hpp"
#include "Error.hpp"
#include "File.hpp"
#include "FileRewind.hpp"
#include "FilesManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::frewindBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    FilesManager* fm = (FilesManager*)(eval->FileManager);
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    ArrayOf param1 = argIn[0];
    int32 iValue = (int32)param1.getContentAsDoubleScalar();
    if (fm->isOpened(iValue)) {
        File* f = fm->getFile(iValue);
        if (f->isInterfaceMethod()) {
            Error(_W("Rewind failed."));
        } else {
            FileRewind(f);
        }
    } else {
        Error(_W("Invalid file identifier."));
    }
    return retval;
}
//=============================================================================
