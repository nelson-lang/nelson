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
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            Dimensions dims = A.getDimensions();
            nelson_handle* qp = (nelson_handle*)(A.getDataPointer());
            size_t elementCount = static_cast<size_t>(dims.getElementCount());
            for (size_t k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    if (hlObj->getCategory() != handleCategory) {
                        std::string msg = fmt::sprintf(_("%s handle expected."), handleCategory);
                        Error(msg);
                    }
                    HandleManager::getInstance()->removeHandle(hl);
                    if (hlObj->getCategory() == NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
                        BackgroundPoolObject::getInstance()->destroy();
                    } else {
                        delete hlObj;
                    }
                    hlObj = nullptr;
                    res = true;
                }
            }
        } else {
            std::string msg = fmt::sprintf(_("%s valid handle expected."), handleCategory);
            Error(msg);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
