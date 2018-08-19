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
#include "handle_fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_fieldnamesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0 || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    std::wstring handleTypeName = param1.getHandleCategory();
    if (handleTypeName == utf8_to_wstring(NLS_HANDLE_STR) || handleTypeName == L"") {
        Error(_W("Invalid handle."));
    }
    std::wstring ufunctionNameGetHandle = handleTypeName + L"_fieldnames";
    std::string functionNameGetHandle = wstring_to_utf8(ufunctionNameGetHandle);
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    if (!context->lookupFunction(functionNameGetHandle, funcDef)) {
        std::wstring msg = ufunctionNameGetHandle + L" " + _W("not defined.");
        Error(msg);
    }
    if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
        ArrayOfVector argInCopy;
        argInCopy.push_back(param1);
        if (argIn.size() == 2) {
            argInCopy.push_back(argIn[1]);
        }
        retval = funcDef->evaluateFunction(eval, argInCopy, nLhs);
    } else {
        std::wstring msg = ufunctionNameGetHandle + L" " + _W("not defined.");
        Error(msg);
    }
    return retval;
}
//=============================================================================
