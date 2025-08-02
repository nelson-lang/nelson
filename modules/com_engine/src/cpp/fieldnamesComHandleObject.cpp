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
#include "fieldnamesComHandleObject.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
fieldnamesComHandleObject(const ArrayOf& A, bool fullList, wstringVector& fieldnames)
{
    if (A.getHandleCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    fieldnamesComHandleObject(comhandleobj, fullList, fieldnames);
}
//=============================================================================
void
fieldnamesComHandleObject(ComHandleObject* comHandle, bool fullList, wstringVector& fieldnames)
{
    void* ptr = comHandle->getPointer();
    fieldnames.clear();
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
            if (pFuncDesc->invkind & (DISPATCH_PROPERTYGET | DISPATCH_PROPERTYPUT)) {
                std::wstring fieldname = std::wstring(name);
                if (std::find(fieldnames.begin(), fieldnames.end(), fieldname)
                    == fieldnames.end()) {
                    fieldnames.push_back(fieldname);
                }
            }
            SysFreeString(name);
            ti->ReleaseFuncDesc(pFuncDesc);
        }
        ti->ReleaseTypeAttr(pAttr);
    }
    if (!fieldnames.empty()) {
        std::sort(fieldnames.begin(), fieldnames.end());
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
