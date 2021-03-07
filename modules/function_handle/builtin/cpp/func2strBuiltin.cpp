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
#include "func2strBuiltin.hpp"
#include "OverloadFunction.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::func2strBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf arg1 = argIn[0];
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "func2str", bSuccess);
    }
    if (!bSuccess) {
        if (arg1.isFunctionHandle()) {
            function_handle fh = arg1.getContentAsFunctionHandle();
            std::wstring functionname;
            bool found = PathFuncManager::getInstance()->find(fh, functionname);
            if (!found) {
                found = BuiltInFunctionDefManager::getInstance()->find(fh, functionname);
            }
            if (found) {
                retval << ArrayOf::characterArrayConstructor(functionname);
            } else {
                Error(_W("#1 Argument must contain a valid function_handle."));
            }
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "func2str", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
