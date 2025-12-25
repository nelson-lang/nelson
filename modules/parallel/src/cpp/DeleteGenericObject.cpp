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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "DeleteGenericObject.hpp"
#include "BackgroundPoolObject.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteGenericObject(const ArrayOf& A, const std::string& handleCategory)
{
    auto deleter = [](HandleGenericObject* hlObj) {
        if (hlObj->getCategory() == NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
            BackgroundPoolObject::getInstance()->destroy();
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
