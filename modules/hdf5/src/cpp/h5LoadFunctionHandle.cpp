//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "h5LoadFunctionHandle.hpp"
#include "h5LoadStruct.hpp"
#include "PathFuncManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadFunctionHandle(hid_t fid, const std::string& location, const std::string& variableName,
    bool isEmpty, Dimensions dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    ArrayOf value;
    if (h5LoadStruct(fid, location, variableName, isEmpty, dims, false, "", value)) {
        ArrayOf funcStr = value.getField("function");
        std::string function_name = funcStr.getContentAsCString();
        FuncPtr fptr = nullptr;
        bool found = PathFuncManager::getInstance()->find(function_name, fptr);
        if (!found) {
            found = BuiltInFunctionDefManager::getInstance()->find(function_name, fptr);
        }
        if (found && fptr) {
            VariableValue
                = ArrayOf::functionHandleConstructor(utf8_to_wstring(function_name), fptr->hashid);
            bSuccess = true;
        }
    }
    return bSuccess;
}
//=============================================================================
};
//=============================================================================
