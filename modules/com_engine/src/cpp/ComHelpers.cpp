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
#include <windows.h>
#include <atlconv.h>
#include "ComHelpers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isMethodCom(IDispatch* pDisp, std::wstring methodToSearch)
{
    ITypeInfo* ti;
    unsigned int tiCount;
    HRESULT hr;
    DISPID dispID;
    LPOLESTR name = W2OLE(const_cast<wchar_t*>(methodToSearch.c_str()));
    hr = pDisp->GetIDsOfNames(IID_NULL, &name, 1, LOCALE_USER_DEFAULT, &dispID);
    if (FAILED(hr)) {
        return false;
    }
    if ((hr = pDisp->GetTypeInfoCount(&tiCount)) == S_OK && tiCount == 1) {
        TYPEATTR* pAttr;
        hr = pDisp->GetTypeInfo(0, LOCALE_USER_DEFAULT, &ti);
        if (FAILED(hr)) {
            return false;
        }
        hr = ti->GetTypeAttr(&pAttr);
        if (FAILED(hr)) {
            return false;
        }
        for (int k = 0; k < pAttr->cFuncs; k++) {
            FUNCDESC* pFuncDesc;
            BSTR name;
            hr = ti->GetFuncDesc(k, &pFuncDesc);
            if (FAILED(hr)) {
                return false;
            }
            hr = ti->GetDocumentation(pFuncDesc->memid, &name, nullptr, nullptr, nullptr);
            if (FAILED(hr)) {
                return false;
            }
            if (pFuncDesc->invkind & (DISPATCH_METHOD)) {
                std::wstring method = std::wstring(name);
                if (StringHelpers::iequals(method, methodToSearch)) {
                    return true;
                }
            }
            SysFreeString(name);
            ti->ReleaseFuncDesc(pFuncDesc);
        }
        ti->ReleaseTypeAttr(pAttr);
    }
    return false;
}
//=============================================================================
bool
isPropertyGetCom(IDispatch* pDisp, std::wstring propertyToSearch)
{
    ITypeInfo* ti;
    unsigned int tiCount;
    HRESULT hr;
    DISPID dispID;
    LPOLESTR name = W2OLE(const_cast<wchar_t*>(propertyToSearch.c_str()));
    hr = pDisp->GetIDsOfNames(IID_NULL, &name, 1, LOCALE_USER_DEFAULT, &dispID);
    if (FAILED(hr)) {
        return false;
    }
    if ((hr = pDisp->GetTypeInfoCount(&tiCount)) == S_OK && tiCount == 1) {
        TYPEATTR* pAttr;
        hr = pDisp->GetTypeInfo(0, LOCALE_USER_DEFAULT, &ti);
        if (FAILED(hr)) {
            return false;
        }
        hr = ti->GetTypeAttr(&pAttr);
        if (FAILED(hr)) {
            return false;
        }
        for (int k = 0; k < pAttr->cFuncs; k++) {
            FUNCDESC* pFuncDesc;
            BSTR name;
            hr = ti->GetFuncDesc(k, &pFuncDesc);
            if (FAILED(hr)) {
                return false;
            }
            hr = ti->GetDocumentation(pFuncDesc->memid, &name, nullptr, nullptr, nullptr);
            if (FAILED(hr)) {
                return false;
            }
            if (pFuncDesc->invkind & (DISPATCH_PROPERTYGET)) {
                std::wstring method = std::wstring(name);
                if (StringHelpers::iequals(method, propertyToSearch)) {
                    return true;
                }
            }
            SysFreeString(name);
            ti->ReleaseFuncDesc(pFuncDesc);
        }
        ti->ReleaseTypeAttr(pAttr);
    }
    return false;
}
//=============================================================================
bool
isPropertyPutCom(IDispatch* pDisp, std::wstring propertyToSearch)
{
    ITypeInfo* ti;
    unsigned int tiCount;
    HRESULT hr;
    DISPID dispID;
    LPOLESTR name = W2OLE(const_cast<wchar_t*>(propertyToSearch.c_str()));
    hr = pDisp->GetIDsOfNames(IID_NULL, &name, 1, LOCALE_USER_DEFAULT, &dispID);
    if (FAILED(hr)) {
        return false;
    }
    if ((hr = pDisp->GetTypeInfoCount(&tiCount)) == S_OK && tiCount == 1) {
        TYPEATTR* pAttr;
        hr = pDisp->GetTypeInfo(0, LOCALE_USER_DEFAULT, &ti);
        if (FAILED(hr)) {
            return false;
        }
        hr = ti->GetTypeAttr(&pAttr);
        if (FAILED(hr)) {
            return false;
        }
        for (int k = 0; k < pAttr->cFuncs; k++) {
            FUNCDESC* pFuncDesc;
            BSTR name;
            hr = ti->GetFuncDesc(k, &pFuncDesc);
            if (FAILED(hr)) {
                return false;
            }
            hr = ti->GetDocumentation(pFuncDesc->memid, &name, nullptr, nullptr, nullptr);
            if (FAILED(hr)) {
                return false;
            }
            if (pFuncDesc->invkind & (DISPATCH_PROPERTYPUT)) {
                std::wstring method = std::wstring(name);
                if (StringHelpers::iequals(method, propertyToSearch)) {
                    return true;
                }
            }
            SysFreeString(name);
            ti->ReleaseFuncDesc(pFuncDesc);
        }
        ti->ReleaseTypeAttr(pAttr);
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
