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
#include "clearfunBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClearFunction.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::clearfunBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    std::wstring functionname;
    if (param1.isRowVectorCharacterArray()) {
        functionname = argIn[0].getContentAsWideString();
    } else if (param1.isFunctionHandle()) {
        function_handle fh = param1.getContentAsFunctionHandle();
        BuiltInFunctionDefManager::getInstance()->find(fh, functionname);
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    retval << ArrayOf::logicalConstructor(ClearBuiltin(functionname));
    return retval;
}
//=============================================================================
