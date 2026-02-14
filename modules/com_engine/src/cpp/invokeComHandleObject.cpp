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
#include "invokeComHandleObject.hpp"
#include "ComHandleObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "VariantConversionHelpers.hpp"
#include "characters_encoding.hpp"
#include "invokeCOM.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define NB_PARAMS_MAX 7
//=============================================================================
ArrayOf
invokeComHandleObject(const ArrayOf& A, const std::wstring& wmethodname,
    const ArrayOfVector& params, bool& haveReturnValue)
{
    ArrayOf res;
    haveReturnValue = false;
    if (params.size() > NB_PARAMS_MAX) {
        raiseError(L"Nelson:com_engine:ERROR_ONLY_7_INPUT_PARAMETERS_EXPECTED",
            ERROR_ONLY_7_INPUT_PARAMETERS_EXPECTED);
    }
    if (A.getHandleCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
        raiseError(L"Nelson:com_engine:ERROR_COM_HANDLE_EXPECTED", ERROR_COM_HANDLE_EXPECTED);
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    void* ptr = comhandleobj->getPointer();
    if (ptr == nullptr) {
        raiseError(
            L"Nelson:com_engine:ERROR_COM_VALID_HANDLE_EXPECTED", ERROR_COM_VALID_HANDLE_EXPECTED);
    }
    const VARIANT* pVariant = static_cast<VARIANT*>(ptr);
    VARIANT* pVarResult = nullptr;
    try {
        pVarResult = new VARIANT;
    } catch (const std::bad_alloc&) {
        raiseError(L"Nelson:error_manager:no_mem", ERROR_MEMORY_ALLOCATION);
    }
    if (pVarResult) {
        size_t nbParams = params.size();
        VARIANT* args = nullptr;
        if (nbParams > 0) {
            try {
                args = new VARIANT[nbParams + 1];
            } catch (const std::bad_alloc&) {
                delete pVarResult;
                pVarResult = nullptr;
                raiseError(L"Nelson:error_manager:no_mem", ERROR_MEMORY_ALLOCATION);
            }
            std::wstring errorMessage;
            for (size_t k = 0; k < nbParams; k++) {
                bool bSuccess = NelsonToComVariant(params[k], &args[k], errorMessage);
                if (!bSuccess) {
                    delete[] args;
                    args = nullptr;
                    Error(errorMessage, L"Nelson:com_engine:ERROR_COM_MESSAGE");
                }
            }
        }
        std::wstring errorMessage;
        bool bSuccess = invokeCom(DISPATCH_METHOD | DISPATCH_PROPERTYGET, pVarResult, errorMessage,
            pVariant->pdispVal, wmethodname, (int)nbParams, args);
        if (args) {
            delete[] args;
            args = nullptr;
        }
        if (bSuccess) {
            if (pVarResult->vt == VT_EMPTY) {
                haveReturnValue = false;
                delete pVarResult;
                pVarResult = nullptr;
            } else {
                haveReturnValue = true;
                bSuccess = ComVariantToNelson(pVarResult, res, errorMessage);
                if (!bSuccess) {
                    delete pVarResult;
                    pVarResult = nullptr;
                    Error(errorMessage, L"Nelson:com_engine:ERROR_COM_MESSAGE");
                }
            }
        } else {
            delete pVarResult;
            pVarResult = nullptr;
            Error(errorMessage, L"Nelson:com_engine:ERROR_COM_MESSAGE");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
