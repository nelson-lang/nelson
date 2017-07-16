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
#include "GetComHandleObject.hpp"
#include "Exception.hpp"
#include "HandleManager.hpp"
#include "HandleGenericObject.hpp"
#include "ComHandleObject.hpp"
#include "characters_encoding.hpp"
#include "VariantConversionHelpers.hpp"
#include "invokeCOM.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf GetComHandleObject(ArrayOf A, std::wstring propertyName, ArrayOfVector params)
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
        catch (std::bad_alloc)
        {
            pVarResult = nullptr;
        }
        if (pVarResult)
        {
            size_t nbParams = params.size();
            VARIANT *args = nullptr;
            if (nbParams > 0)
            {
                try
                {
                    args = new VARIANT[nbParams];
                }
                catch (std::bad_alloc)
                {
                    delete pVarResult;
                    pVarResult = nullptr;
                    throw Exception(ERROR_MEMORY_ALLOCATION);
                }
                std::wstring errorMessage;
                for (size_t k = 0; k < nbParams; k++)
                {
                    bool bSuccess = NelsonToComVariant(params[k], &args[k], errorMessage);
                    if (!bSuccess)
                    {
                        delete[] args;
                        args = nullptr;
                        throw Exception(errorMessage);
                    }
                }
            }
            VariantInit(pVarResult);
            std::wstring errorMessage;
            bool bSuccess = invokeCom(DISPATCH_PROPERTYGET | DISPATCH_METHOD, pVarResult, errorMessage, pVariant->pdispVal, propertyName, (int)nbParams, args);
            if (args)
            {
                delete[] args;
                args = nullptr;
            }
            if (bSuccess)
            {
                bSuccess = ComVariantToNelson(pVarResult, res, errorMessage);
                if (!bSuccess)
                {
                    delete pVarResult;
                    throw Exception(errorMessage);
                }
            }
            else
            {
                delete pVarResult;
                throw Exception(errorMessage);
            }
        }
        else
        {
            throw Exception(ERROR_MEMORY_ALLOCATION);
        }
        return res;
    }
    //=============================================================================
}
//=============================================================================
