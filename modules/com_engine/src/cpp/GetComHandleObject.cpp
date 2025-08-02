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
#include "GetComHandleObject.hpp"
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
ArrayOf
GetComHandleObject(const ArrayOf& A, const std::wstring& propertyName, const ArrayOfVector& params)
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
    }
    if (pVarResult) {
        size_t nbParams = params.size();
        VARIANT* args = nullptr;
        if (nbParams > 0) {
            try {
                args = new VARIANT[nbParams];
            } catch (const std::bad_alloc&) {
                delete pVarResult;
                pVarResult = nullptr;
                Error(ERROR_MEMORY_ALLOCATION);
            }
            std::wstring errorMessage;
            for (size_t k = 0; k < nbParams; k++) {
                bool bSuccess = NelsonToComVariant(params[k], &args[k], errorMessage);
                if (!bSuccess) {
                    delete[] args;
                    args = nullptr;
                    Error(errorMessage);
                }
            }
        }
        VariantInit(pVarResult);
        std::wstring errorMessage;
        bool bSuccess = invokeCom(DISPATCH_PROPERTYGET | DISPATCH_METHOD, pVarResult, errorMessage,
            pVariant->pdispVal, propertyName, (int)nbParams, args);
        if (args) {
            delete[] args;
            args = nullptr;
        }
        if (bSuccess) {
            bSuccess = ComVariantToNelson(pVarResult, res, errorMessage);
            if (!bSuccess) {
                delete pVarResult;
                Error(errorMessage);
            }
        } else {
            delete pVarResult;
            Error(errorMessage);
        }
    } else {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
