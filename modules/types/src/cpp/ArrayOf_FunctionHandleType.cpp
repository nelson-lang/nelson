//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
ArrayOf::getContentAsFunctionHandle() const
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
ArrayOf::functionHandleConstructor(const std::wstring& functionName, const std::wstring& anonymous)
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
