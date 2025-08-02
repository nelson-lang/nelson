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
#include <algorithm>
#include "methodsComHandleObject.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
methodsComHandleObject(const ArrayOf& A, wstringVector& methods)
{
    if (A.getHandleCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
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
    if (!methods.empty()) {
        std::sort(methods.begin(), methods.end());
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
