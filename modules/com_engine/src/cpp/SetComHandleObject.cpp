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
#include "SetComHandleObject.hpp"
#include "ComHandleObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "VariantConversionHelpers.hpp"
#include "characters_encoding.hpp"
#include "invokeCOM.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
SetComHandleObject(const ArrayOf& A, const std::wstring& propertyName, const ArrayOf& B)
{
    ArrayOf res;
    if (A.getHandleCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    void* ptr = comhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("COM valid handle expected."));
    }
    VARIANT* pVariant = (VARIANT*)ptr;
    VARIANT* pVarResult;
    try {
        pVarResult = new VARIANT;
    } catch (const std::bad_alloc&) {
        pVarResult = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    VariantInit(pVarResult);
    std::wstring errorMessage;
    VARIANT* param = nullptr;
    try {
        param = new VARIANT();
    } catch (const std::bad_alloc&) {
        delete pVarResult;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    VariantInit(param);
    bool bSuccess = NelsonToComVariant(B, param, errorMessage);
    if (!bSuccess) {
        Error(errorMessage);
    }
    errorMessage.clear();
    bSuccess = invokeCom(
        DISPATCH_PROPERTYPUT, pVarResult, errorMessage, pVariant->pdispVal, propertyName, 1, param);
    if (bSuccess) {
        bSuccess = ComVariantToNelson(pVarResult, res, errorMessage);
        delete pVarResult;
        pVarResult = nullptr;
        if (!bSuccess) {
            Error(errorMessage);
        }
    } else {
        delete pVarResult;
        pVarResult = nullptr;
        Error(errorMessage);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
