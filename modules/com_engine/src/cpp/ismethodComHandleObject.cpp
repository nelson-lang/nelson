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
#include "ismethodComHandleObject.hpp"
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
ismethodComHandleObject(ComHandleObject* comhandleobj, const std::wstring& methodname)
{
    void* ptr = comhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("COM valid handle expected."));
    }
    auto* pVariant = static_cast<VARIANT*>(ptr);
    return isMethodCom(pVariant->pdispVal, methodname);
}
//=============================================================================
ArrayOf
ismethodComHandleObject(const ArrayOf& A, const std::wstring& methodname)
{
    if (A.getHandleCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    bool res = ismethodComHandleObject(comhandleobj, methodname);
    return ArrayOf::logicalConstructor(res);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
