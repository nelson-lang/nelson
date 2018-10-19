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
#include "ArrayOf.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isFunctionHandle()
{
    if (this->isClassStruct()) {
        std::string classString = this->getStructType();
        return (classString == NLS_FUNCTION_HANDLE_STR);
    }
    return false;
}
//=============================================================================
function_handle
ArrayOf::getContentAsFunctionHandle()
{
    function_handle fh = 0;
    std::string classString = this->getStructType();
    if (classString == NLS_FUNCTION_HANDLE_STR) {
        ArrayOf value1 = this->getField(NLS_FUNCTION_HANDLE_STR);
        fh = value1.getContentAsUnsignedInt64Scalar();
    } else {
        Error(_W("Expected a function_handle."));
    }
    return fh;
}
//=============================================================================
ArrayOf
ArrayOf::functionHandleConstructor(std::wstring functionName, function_handle fptr)
{
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back(NLS_FUNCTION_HANDLE_STR);
    fieldvalues.push_back(ArrayOf::uint64Constructor(fptr));
    ArrayOf res = structConstructor(fieldnames, fieldvalues);
    res.setStructType(NLS_FUNCTION_HANDLE_STR);
    return res;
}
//=============================================================================
}
//=============================================================================
