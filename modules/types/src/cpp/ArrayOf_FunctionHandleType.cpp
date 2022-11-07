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
#include "i18n.hpp"
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
        ArrayOf anonymousHandle = this->getField("handle");
        fh.name = nameField.getContentAsCString();
        fh.anonymousHandle = (nelson_handle*)anonymousHandle.getContentAsUnsignedInteger64Scalar();

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
    fieldnames.push_back("handle");

    fieldvalues.push_back(ArrayOf::characterArrayConstructor(functionName));
    fieldvalues.push_back(ArrayOf::uint64Constructor(0));

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
    fieldnames.push_back("handle");

    fieldvalues.push_back(ArrayOf::characterArrayConstructor(fptr.name));
    nelson_handle fun_handle = reinterpret_cast<nelson_handle>(fptr.anonymousHandle);
    fieldvalues.push_back(ArrayOf::uint64Constructor(fun_handle));

    ArrayOf res = structConstructor(fieldnames, fieldvalues);
    res.setStructType(NLS_FUNCTION_HANDLE_STR);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
