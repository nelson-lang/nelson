//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteLibPointerObject.hpp"
#include "HandleManager.hpp"
#include "LibPointerObject.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteLibPointerObject(const ArrayOf& A)
{
    return DeleteHandleObjects<LibPointerObject>(A, NLS_HANDLE_LIBPOINTER_CATEGORY_STR,
        _W("libpointer handle expected."), _W("libpointer valid handle expected."));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
