//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteDynamicLinkLibraryObject.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteDynamicLinkLibraryObject(const ArrayOf& A)
{
    return DeleteHandleObjects<DynamicLinkLibraryObject>(A, NLS_HANDLE_DLLIB_CATEGORY_STR,
        Nelson::resolveMessageTemplate(
            L"Nelson:dynamic_link:dllibHandle", L"dllib handle expected."),
        Nelson::resolveMessageTemplate(
            L"Nelson:dynamic_link:dllibValidHandle", L"dllib valid handle expected."));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
