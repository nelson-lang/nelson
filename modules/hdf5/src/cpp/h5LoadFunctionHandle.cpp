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
    bool isEmpty, const Dimensions& dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    ArrayOf value;
    if (h5LoadStruct(fid, location, variableName, isEmpty, dims, false, "", value)) {
        ArrayOf nameStr = value.getField("name");
        ArrayOf anonymousStr = value.getField("anonymous");
        function_handle fh;
        fh.name = nameStr.getContentAsCString();
        fh.anonymous = anonymousStr.getContentAsCString();
        VariableValue = ArrayOf::functionHandleConstructor(fh);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
