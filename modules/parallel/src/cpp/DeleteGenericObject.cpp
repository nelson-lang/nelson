//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "DeleteGenericObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "BackgroundPoolObject.hpp"
#include "FevalFutureObject.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isFutureCategory(const std::string& handleCategory)
{
    return handleCategory == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR
        || handleCategory == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR
        || handleCategory == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR;
}
//=============================================================================
static void
cancelFutureObject(HandleGenericObject* hlObj)
{
    if (auto* future = dynamic_cast<FevalFutureObject*>(hlObj)) {
        future->cancel();
    } else if (auto* future = dynamic_cast<AfterAllFutureObject*>(hlObj)) {
        future->cancel();
    } else if (auto* future = dynamic_cast<AfterEachFutureObject*>(hlObj)) {
        future->cancel();
    }
}
//=============================================================================
bool
DeleteGenericObject(const ArrayOf& A, const std::string& handleCategory)
{
    if (isFutureCategory(handleCategory)) {
        if (!A.isHandle()) {
            return false;
        }
        if (A.isEmpty()) {
            std::wstring validHandleExpectedMsg = fmt::format(L"{} valid handle expected.",
                std::wstring(handleCategory.begin(), handleCategory.end()));
            Error(L"Nelson:parallel:invalidHandle", validHandleExpectedMsg);
            return false;
        }

        std::wstring handleExpectedMsg = fmt::format(
            L"{} handle expected.", std::wstring(handleCategory.begin(), handleCategory.end()));

        Dimensions dims = A.dimensions();
        auto* qp = static_cast<const HandleRef*>(A.dataPointer());
        size_t elementCount = static_cast<size_t>(dims.elementCount());
        bool res = false;
        for (size_t k = 0; k < elementCount; ++k) {
            HandleRef ref = qp[k];
            HandleGenericObject* hlObj = ref ? ref->object() : nullptr;
            if (hlObj == nullptr) {
                continue;
            }
            if (hlObj->category() != handleCategory) {
                Error(L"Nelson:parallel:invalidHandle", handleExpectedMsg);
                continue;
            }
            ref->invalidate(nullptr);
            res = true;
        }
        return res;
    }

    auto deleter = [](HandleGenericObject* hlObj) {
        if (hlObj->category() == NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
            BackgroundPoolObject::getInstance()->destroy();
        } else if (isFutureCategory(hlObj->category())) {
            cancelFutureObject(hlObj);
            delete hlObj;
        } else {
            delete hlObj;
        }
    };

    std::wstring handleExpectedMsg = fmt::format(
        L"{} handle expected.", std::wstring(handleCategory.begin(), handleCategory.end()));
    std::wstring validHandleExpectedMsg = fmt::format(
        L"{} valid handle expected.", std::wstring(handleCategory.begin(), handleCategory.end()));

    return DeleteHandleObjects<HandleGenericObject>(
        A, handleCategory, handleExpectedMsg, validHandleExpectedMsg, deleter);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
