//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteOnCleanupHandleObject.hpp"
#include "OnCleanupObjectHandle.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteOnCleanupHandleObject(Evaluator* eval, const ArrayOf& A)
{
    auto deleter = [eval](OnCleanupObjectHandle* obj) {
        if (obj) {
            obj->cleanup(eval);
            delete obj;
        }
    };

    return DeleteHandleObjects<OnCleanupObjectHandle>(A, NLS_HANDLE_ONCLEANUP_CATEGORY_STR,
        _W("onCleanup handle expected."), _W("onCleanup valid handle expected."), deleter);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
