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
#include <atlconv.h>
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
        Error(_W("Only 7 input parameters expected."));
    }
    if (A.getHandleCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    void* ptr = comhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("COM valid handle expected."));
    }
    const VARIANT* pVariant = static_cast<VARIANT*>(ptr);
    VARIANT* pVarResult = nullptr;
    try {
        pVarResult = new VARIANT;
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
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
                    Error(errorMessage);
                }
            }
        } else {
            delete pVarResult;
            pVarResult = nullptr;
            Error(errorMessage);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
