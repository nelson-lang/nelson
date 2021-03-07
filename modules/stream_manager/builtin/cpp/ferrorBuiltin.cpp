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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "ferrorBuiltin.hpp"
#include "Error.hpp"
#include "FilesManager.hpp"
#include "FileError.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::ferrorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    FilesManager* fm = static_cast<FilesManager*>(eval->FileManager);
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    bool withClear = false;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        std::wstring str = param2.getContentAsWideString();
        if (str != L"clear") {
            Error(_W("'clear' expected as second argument."));
        }
        withClear = true;
    }
    ArrayOf param1 = argIn[0];
    int32 iValue = static_cast<int32>(param1.getContentAsDoubleScalar(false));
    if (!fm->isOpened(iValue)) {
        Error(_W("Invalid file identifier."));
    }
    int errorCode = 0;
    std::string errorMessage;
    if (!FileError(fm, iValue, withClear, errorCode, errorMessage)) {
        Error(_W("Invalid file identifier."));
    }
    retval << ArrayOf::characterArrayConstructor(errorMessage);
    retval << ArrayOf::doubleConstructor((double)errorCode);
    return retval;
}
//=============================================================================
