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
#include "nfilenameBuiltin.hpp"
#include "Error.hpp"
#include "GetCurrentNFilename.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::nfilenameBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    int iExt = 0;
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring argstr = argIn[0].getContentAsWideString();
            if ((argstr.compare(L"fullpath") == 0) || (argstr.compare(L"fullpathext") == 0)) {
                if (argstr.compare(L"fullpath") == 0) {
                    iExt = 1;
                }
                if (argstr.compare(L"fullpathext") == 0) {
                    iExt = 2;
                }
            } else {
                Error(
                    _W("Wrong value for #1 argument, \'fullpathext\' or  \'fullpath\' expected."));
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    }
    boost::filesystem::path path(GetCurrentNFilenameW(eval));
    switch (iExt) {
    case 0:
        path = path.stem();
        break;
    case 1:
        path = path.replace_extension();
        break;
    case 2:
        break;
    }
    retval.push_back(ArrayOf::characterArrayConstructor(path.generic_wstring()));
    return retval;
}
//=============================================================================
