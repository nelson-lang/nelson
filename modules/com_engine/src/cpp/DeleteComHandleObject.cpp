//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Windows.h>
#include "DeleteComHandleObject.hpp"
#include "ComHandleObject.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteComHandleObject(const ArrayOf& A)
{
    auto comDeleter = [](ComHandleObject* comhandleobj) {
        VARIANT* pVariant = static_cast<VARIANT*>(comhandleobj->getPointer());
        if (pVariant) {
            VariantClear(pVariant);
            delete pVariant;
            comhandleobj->setPointer(nullptr);
        }
        delete comhandleobj;
    };

    return DeleteHandleObjects<ComHandleObject>(A, NLS_HANDLE_COM_CATEGORY_STR,
        _W("COM handle expected."), _W("COM valid handle expected."), comDeleter);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
