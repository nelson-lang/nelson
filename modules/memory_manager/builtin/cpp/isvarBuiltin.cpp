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
#include "isvarBuiltin.hpp"
#include "Error.hpp"
#include "IsVariable.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::isvarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring varName = param1.getContentAsWideString();
        bool res = IsVariable(eval, SCOPE_LEVEL::LOCAL_SCOPE, varName);
        retval << ArrayOf::logicalConstructor(res);
    } else {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        std::wstring scopeName = param1.getContentAsWideString();
        std::wstring varName = param2.getContentAsWideString();
        bool res = false;
        if (scopeName == L"global") {
            res = IsVariable(eval, SCOPE_LEVEL::GLOBAL_SCOPE, varName);
        } else if (scopeName == L"base") {
            res = IsVariable(eval, SCOPE_LEVEL::BASE_SCOPE, varName);
        } else if (scopeName == L"local") {
            res = IsVariable(eval, SCOPE_LEVEL::LOCAL_SCOPE, varName);
        } else if (scopeName == L"caller") {
            res = IsVariable(eval, SCOPE_LEVEL::CALLER_SCOPE, varName);
        } else {
            Error(_W("Argument #1 : 'global', 'base', 'local' or 'caller' expected."));
        }
        retval << ArrayOf::logicalConstructor(res);
    }
    return retval;
}
//=============================================================================
