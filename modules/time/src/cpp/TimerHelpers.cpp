//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TimerHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::vector<HandleRef>
getTimerHandleRefs(const ArrayOf& value)
{
    if (!value.isHandle() || value.handleCategory() != NLS_HANDLE_TIMER_CATEGORY_STR) {
        Error(L"Nelson:time:timerHandleExpected", L"%s expected.", L"timer handle");
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
std::vector<TimerObject*>
getTimerObjects(const ArrayOf& value)
{
    std::vector<HandleRef> refs = getTimerHandleRefs(value);
    std::vector<TimerObject*> objects;
    objects.reserve(refs.size());
    for (const HandleRef& ref : refs) {
        if (!ref || !ref->isValid()) {
            Error(L"Nelson:time:invalidTimerHandle", L"Invalid %s.", L"timer handle");
        }
        auto* object = dynamic_cast<TimerObject*>(ref->object());
        if (object == nullptr) {
            Error(L"Nelson:time:timerHandleExpected", L"%s expected.", L"timer handle");
        }
        objects.push_back(object);
    }
    return objects;
}
//=============================================================================
ArrayOf
timerHandleRefsToArray(const std::vector<HandleRef>& refs)
{
    if (refs.empty()) {
        Dimensions dims(0, 0);
        return ArrayOf::emptyHandle(NLS_HANDLE_TIMER_CATEGORY_STR, dims);
    }
    Dimensions dims(1, static_cast<indexType>(refs.size()));
    auto* data = static_cast<HandleRef*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, dims.elementCount(), stringVector(), false));
    for (size_t k = 0; k < refs.size(); ++k) {
        data[k] = refs[k];
    }
    return ArrayOf(NLS_HANDLE, dims, data);
}
//=============================================================================
ArrayOf
timerObjectsToArray(const std::vector<TimerObject*>& objects)
{
    std::vector<HandleRef> refs;
    refs.reserve(objects.size());
    for (TimerObject* object : objects) {
        if (object != nullptr) {
            refs.push_back(HandleManager::getInstance()->addHandleRef(object));
        }
    }
    return timerHandleRefsToArray(refs);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
