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
#include "SetComHandleObject.hpp"
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
void
SetComHandleObject(ArrayOf A, const std::wstring& propertyName, ArrayOf B)
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
    errorMessage = L"";
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
}
//=============================================================================
