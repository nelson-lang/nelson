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
    auto deleter = [](HandleGenericObject* hlObj) {
        if (hlObj->getCategory() == NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
            BackgroundPoolObject::getInstance()->destroy();
        } else if (isFutureCategory(hlObj->getCategory())) {
            cancelFutureObject(hlObj);
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
