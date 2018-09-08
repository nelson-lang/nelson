//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "GetComHandleObject.hpp"
#include "ComHandleObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "VariantConversionHelpers.hpp"
#include "characters_encoding.hpp"
#include "invokeCOM.hpp"
#include <Windows.h>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GetComHandleObject(ArrayOf A, const std::wstring& propertyName, ArrayOfVector params)
{
    ArrayOf res;
    if (A.getHandleCategory() != COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    ComHandleObject* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
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
}
//=============================================================================
