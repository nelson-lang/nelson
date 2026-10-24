//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FunctionHandleOwner.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
functionHandleOwnerValue(nelson_handle* anonymousHandle);
//=============================================================================
bool
ArrayOf::isFunctionHandle() const
{
    return (this->dataClass() == NLS_FUNCTION_HANDLE);
}
//=============================================================================
function_handle
ArrayOf::asFunctionHandle() const
{
    function_handle fh;
    if (isFunctionHandle()) {
        ArrayOf anonymousHandle = this->field("handle");
        fh.anonymousHandle = (nelson_handle*)anonymousHandle.asUint64Scalar();
    } else {
        Error(L"Nelson:types:handleExpected:valueAFunctionHandle", L"%s must be a function handle.",
            L"Value");
    }
    return fh;
}
//=============================================================================
ArrayOf
ArrayOf::functionHandle(const std::wstring& functionName, const std::wstring& anonymous)
{
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back("name");
    fieldnames.push_back("handle");

    fieldvalues.push_back(ArrayOf::characterArray(functionName));
    fieldvalues.push_back(ArrayOf::uint64Scalar(0));
    ArrayOf res = structArray(fieldnames, fieldvalues);
    res.promoteType(NLS_FUNCTION_HANDLE);
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::functionHandle(function_handle fptr)
{
    // "handle" is the raw definition pointer read back by asFunctionHandle;
    // "owner" keeps that definition alive as long as any storage block of this
    // value exists (see FunctionHandleOwner).
    stringVector fieldnames { "handle", "owner" };
    Dimensions dims(1, 1);
    auto* fields
        = static_cast<ArrayOf*>(allocateArrayOf(NLS_FUNCTION_HANDLE, 1, fieldnames, false));
    fields[0] = ArrayOf::uint64Scalar(reinterpret_cast<nelson_handle>(fptr.anonymousHandle));
    fields[1] = functionHandleOwnerValue(fptr.anonymousHandle);
    return ArrayOf(NLS_FUNCTION_HANDLE, dims, fields, false, fieldnames);
}
//=============================================================================
static ArrayOf
functionHandleOwnerValue(nelson_handle* anonymousHandle)
{
    if (anonymousHandle == nullptr) {
        return ArrayOf(NLS_DOUBLE);
    }
    Dimensions dims(1, 1);
    auto* ref = static_cast<HandleRef*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, dims.elementCount(), stringVector(), false));
    ref[0] = std::make_shared<HandleState>(0, new FunctionHandleOwner(anonymousHandle), true);
    return ArrayOf(NLS_HANDLE, dims, ref);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
