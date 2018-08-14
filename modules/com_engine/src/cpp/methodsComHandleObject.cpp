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
#pragma once
//=============================================================================
#include "methodsComHandleObject.hpp"
#include "HandleManager.hpp"
#include <Windows.h>
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
void
methodsComHandleObject(ArrayOf A, wstringVector& methods)
{
    if (A.getHandleCategory() != COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    ComHandleObject* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    methodsComHandleObject(comhandleobj, methods);
}
//=============================================================================
void
methodsComHandleObject(ComHandleObject* comHandle, wstringVector& methods)
{
    void* ptr = comHandle->getPointer();
    methods.clear();
    if (ptr == nullptr) {
        Error(_W("COM valid handle expected."));
    }
    VARIANT* pVariant = (VARIANT*)ptr;
    ITypeInfo* ti;
    unsigned int tiCount;
    HRESULT hr;
    if ((hr = pVariant->pdispVal->GetTypeInfoCount(&tiCount)) == S_OK && tiCount == 1) {
        TYPEATTR* pAttr;
        hr = pVariant->pdispVal->GetTypeInfo(0, LOCALE_USER_DEFAULT, &ti);
        if (FAILED(hr)) {
            return;
        }
        hr = ti->GetTypeAttr(&pAttr);
        if (FAILED(hr)) {
            return;
        }
        for (int k = 0; k < pAttr->cFuncs; k++) {
            FUNCDESC* pFuncDesc;
            BSTR name;
            hr = ti->GetFuncDesc(k, &pFuncDesc);
            if (FAILED(hr)) {
                return;
            }
            hr = ti->GetDocumentation(pFuncDesc->memid, &name, NULL, NULL, NULL);
            if (FAILED(hr)) {
                return;
            }
            if (pFuncDesc->invkind & (DISPATCH_METHOD)) {
                std::wstring method = std::wstring(name);
                if (std::find(methods.begin(), methods.end(), method) == methods.end()) {
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
