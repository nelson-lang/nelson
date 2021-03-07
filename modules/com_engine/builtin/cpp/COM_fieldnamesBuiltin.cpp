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
#include "COM_fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "fieldnamesComHandleObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool fullList = false;
    ArrayOfVector retval;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        std::wstring param2str = param2.getContentAsWideString();
        if (param2str == L"-full") {
            fullList = true;
        } else {
            Error(_W("Unrecognized option. \"-full\" expected."));
        }
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    wstringVector fieldnames;
    fieldnamesComHandleObject(param1, fullList, fieldnames);
    retval << ToCellStringAsColumn(fieldnames);
    return retval;
}
//=============================================================================
