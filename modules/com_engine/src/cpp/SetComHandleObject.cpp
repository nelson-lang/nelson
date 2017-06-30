//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <Windows.h>
#include "SetComHandleObject.hpp"
#include "Exception.hpp"
#include "HandleGenericObject.hpp"
#include "ComHandleObject.hpp"
#include "characters_encoding.hpp"
#include "HandleManager.hpp"
#include "VariantConversionHelpers.hpp"
#include "invokeCOM.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    void SetComHandleObject(ArrayOf A, std::wstring propertyName, ArrayOf B)
    {
        ArrayOf res;
        if (!A.isHandle())
        {
            throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
        }
        if (!A.isScalar())
        {
            throw Exception(ERROR_SIZE_SCALAR_EXPECTED);
        }
        nelson_handle *qp = (nelson_handle*)A.getDataPointer();
        if (qp == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        nelson_handle hl = qp[0];
        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
        if (hlObj == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        if (hlObj->getCategory() != COM_CATEGORY_STR)
        {
            throw Exception(_W("COM handle expected."));
        }
        ComHandleObject *comhandleobj = (ComHandleObject *)hlObj;
        void *ptr = comhandleobj->getPointer();
        if (ptr == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        VARIANT *pVariant = (VARIANT*)ptr;
        VARIANT *pVarResult;
        try
        {
            pVarResult = new VARIANT;
        }
        catch (std::bad_alloc &e)
        {
            pVarResult = nullptr;
            throw Exception(ERROR_MEMORY_ALLOCATION);
        }
        VariantInit(pVarResult);
        std::wstring errorMessage;
        VARIANT *param;
        try
        {
            param = new VARIANT();
        }
        catch (std::bad_alloc &e)
        {
            delete pVarResult;
            throw Exception(ERROR_MEMORY_ALLOCATION);
        }
        VariantInit(param);
        bool bSuccess = NelsonToComVariant(B, param, errorMessage);
        if (!bSuccess)
        {
            throw Exception(errorMessage);
        }
        errorMessage = L"";
        bSuccess = invokeCom(DISPATCH_PROPERTYPUT, pVarResult, errorMessage, pVariant->pdispVal, propertyName, 1, param);
        if (bSuccess)
        {
            bSuccess = ComVariantToNelson(pVarResult, res, errorMessage);
            delete pVarResult;
            pVarResult = nullptr;
            if (!bSuccess)
            {
                throw Exception(errorMessage);
            }
        }
        else
        {
            delete pVarResult;
            pVarResult = nullptr;
            throw Exception(errorMessage);
        }
    }
    //=============================================================================
}
//=============================================================================
