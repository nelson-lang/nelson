//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include "ispropComHandleObject.hpp"
#include "ComHandleObject.hpp"
#include "ComHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ispropComHandleObject(ComHandleObject* comhandleobj, const std::wstring& propertyName)
{
    void* ptr = comhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("COM valid handle expected."));
    }
    auto* pVariant = static_cast<VARIANT*>(ptr);
    return isPropertyGetCom(pVariant->pdispVal, propertyName)
        || isPropertyPutCom(pVariant->pdispVal, propertyName);
}
//=============================================================================
ArrayOf
ispropComHandleObject(const ArrayOf& A, const std::wstring& propertyName)
{
    if (A.getHandleCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    bool res = ispropComHandleObject(comhandleobj, propertyName);
    return ArrayOf::logicalConstructor(res);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
