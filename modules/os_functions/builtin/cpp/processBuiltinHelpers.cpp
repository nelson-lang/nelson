//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "processBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson::OsFunctionsGateway {
//=============================================================================
std::vector<HandleRef>
getProcessHandleRefs(const ArrayOf& value)
{
    if (!value.isHandle() || value.handleCategory() != NLS_HANDLE_PROCESS_CATEGORY_STR) {
        Error(L"Nelson:os_functions:processHandleExpected", L"%s expected.", L"process handle");
    }
    std::vector<HandleRef> refs;
    refs.reserve(static_cast<size_t>(value.elementCount()));
    const auto* data = static_cast<const HandleRef*>(value.dataPointer());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        refs.push_back(data[k]);
    }
    return refs;
}
//=============================================================================
std::vector<ProcessObject*>
getProcessObjects(const ArrayOf& value)
{
    std::vector<HandleRef> refs = getProcessHandleRefs(value);
    std::vector<ProcessObject*> objects;
    objects.reserve(refs.size());
    for (const HandleRef& ref : refs) {
        if (!ref || !ref->isValid()) {
            Error(L"Nelson:os_functions:invalidProcessHandle", L"Invalid %s.", L"process handle");
        }
        auto* object = dynamic_cast<ProcessObject*>(ref->object());
        if (object == nullptr) {
            Error(L"Nelson:os_functions:processHandleExpected", L"%s expected.", L"process handle");
        }
        objects.push_back(object);
    }
    return objects;
}
//=============================================================================
ProcessObject*
getProcessObjectScalar(const ArrayOf& value)
{
    if (!value.isScalar()) {
        Error(L"Nelson:os_functions:scalarProcessHandleExpected", L"%s expected.",
            L"Scalar process handle");
    }
    std::vector<ProcessObject*> objects = getProcessObjects(value);
    return objects[0];
}
//=============================================================================
} // namespace Nelson::OsFunctionsGateway
//=============================================================================
