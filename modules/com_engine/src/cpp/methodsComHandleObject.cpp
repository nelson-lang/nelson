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
#pragma once
//=============================================================================
#include <Windows.h>
#include <algorithm>
#include "methodsComHandleObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    void methodsComHandleObject(ArrayOf A, wstringVector &methods)
    {
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
        methodsComHandleObject(comhandleobj, methods);
    }
    //=============================================================================
    void methodsComHandleObject(ComHandleObject *comHandle, wstringVector &methods)
    {
        void *ptr = comHandle->getPointer();
        methods.clear();
        if (ptr == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        VARIANT *pVariant = (VARIANT *)ptr;
        ITypeInfo *ti;
        unsigned int tiCount;
        HRESULT hr;
        if ((hr = pVariant->pdispVal->GetTypeInfoCount(&tiCount)) == S_OK && tiCount == 1)
        {
            TYPEATTR *pAttr;
            hr = pVariant->pdispVal->GetTypeInfo(0, LOCALE_USER_DEFAULT, &ti);
            hr = ti->GetTypeAttr(&pAttr);
            for (int k = 0; k<pAttr->cFuncs; k++)
            {
                FUNCDESC *pFuncDesc;
                BSTR name;
                hr = ti->GetFuncDesc(k, &pFuncDesc);
                hr = ti->GetDocumentation(pFuncDesc->memid, &name, NULL, NULL, NULL);
                if (pFuncDesc->invkind & (DISPATCH_METHOD))
                {
                    std::wstring method = std::wstring(name);
                    if (std::find(methods.begin(), methods.end(), method) == methods.end())
                    {
                        methods.push_back(method);
                    }
                }
                SysFreeString(name);
                ti->ReleaseFuncDesc(pFuncDesc);
            }
            ti->ReleaseTypeAttr(pAttr);
        }
        std::sort(methods.begin(), methods.end());
    }
    //=============================================================================
}
//=============================================================================
