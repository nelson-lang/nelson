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
#include "ArrayOf.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isFunctionHandle() const
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
    function_handle fh;
    std::string classString = this->getStructType();
    if (classString == NLS_FUNCTION_HANDLE_STR) {
        ArrayOf nameField = this->getField("name");
        ArrayOf anonymousField = this->getField("anonymous");
        fh.name = nameField.getContentAsCString();
        fh.anonymous = anonymousField.getContentAsCString();
    } else {
        Error(_W("Expected a function_handle."));
    }
    return fh;
}
//=============================================================================
ArrayOf
ArrayOf::functionHandleConstructor(const std::wstring& functionName, const std::wstring &anonymous)
{
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back("name");
    fieldnames.push_back("anonymous");

    fieldvalues.push_back(ArrayOf::characterArrayConstructor(functionName));
    fieldvalues.push_back(ArrayOf::characterArrayConstructor(anonymous));

    ArrayOf res = structConstructor(fieldnames, fieldvalues);
    res.setStructType(NLS_FUNCTION_HANDLE_STR);
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::functionHandleConstructor(function_handle fptr)
{
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back("name");
    fieldnames.push_back("anonymous");

    fieldvalues.push_back(ArrayOf::characterArrayConstructor(fptr.name));
    fieldvalues.push_back(ArrayOf::characterArrayConstructor(fptr.anonymous));

    ArrayOf res = structConstructor(fieldnames, fieldvalues);
    res.setStructType(NLS_FUNCTION_HANDLE_STR);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
