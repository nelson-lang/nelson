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
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteComHandleObject(const ArrayOf& A)
{
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            auto* qp = (nelson_handle*)A.getDataPointer();
            indexType elementCount = A.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    if (hlObj->getCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
                        Error(_W("COM handle expected."));
                    }
                    auto* comhandleobj = (ComHandleObject*)hlObj;
                    VARIANT* pVariant = (VARIANT*)comhandleobj->getPointer();
                    if (pVariant) {
                        VariantClear(pVariant);
                        delete pVariant;
                        comhandleobj->setPointer(nullptr);
                    }
                    delete comhandleobj;
                    HandleManager::getInstance()->removeHandle(hl);
                    res = true;
                }
            }
        } else {
            Error(_W("COM valid handle expected."));
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
