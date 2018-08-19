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
    if (argIn.size() == 0 || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring varName = param1.getContentAsWideString();
        bool res = IsVariable(eval, SCOPE_LEVEL::LOCAL_SCOPE, varName);
        retval.push_back(ArrayOf::logicalConstructor(res));
    } else {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        std::wstring scopeName = param1.getContentAsWideString();
        std::wstring varName = param2.getContentAsWideString();
        bool res = false;
        if (scopeName.compare(L"global") == 0) {
            res = IsVariable(eval, SCOPE_LEVEL::GLOBAL_SCOPE, varName);
        } else if (scopeName.compare(L"base") == 0) {
            res = IsVariable(eval, SCOPE_LEVEL::BASE_SCOPE, varName);
        } else if (scopeName.compare(L"local") == 0) {
            res = IsVariable(eval, SCOPE_LEVEL::LOCAL_SCOPE, varName);
        } else if (scopeName.compare(L"caller") == 0) {
            res = IsVariable(eval, SCOPE_LEVEL::CALLER_SCOPE, varName);
        } else {
            Error(_W("Argument #1 : 'global', 'base', 'local' or 'caller' expected."));
        }
        retval.push_back(ArrayOf::logicalConstructor(res));
    }
    return retval;
}
//=============================================================================
