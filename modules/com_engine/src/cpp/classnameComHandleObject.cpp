//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classnameComHandleObject.hpp"
#include "ClassName.hpp"
#include "ComHandleObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include <Ole2.h>
#include <Windows.h>
#include <ocidl.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
getClassInfoFromVariant(VARIANT* pVariant, std::wstring& className, std::wstring& classTypeName)
{
    bool haveError = true;
    classTypeName = L"Unknown";
    className = L"Unknown";
    IProvideClassInfo* ci = nullptr;
    ITypeInfo* ti = nullptr;
    unsigned int tiCount = 0;
    HRESULT hr;
    IDispatch* pDisp = pVariant->pdispVal;
    if (pDisp == nullptr) {
        return false;
    }
    hr = pDisp->QueryInterface(IID_IProvideClassInfo, (void**)&ci);
    if (hr == S_OK) {
        ci->GetClassInfo(&ti);
        ci->Release();
    }
    if (ti == NULL && (hr = pVariant->pdispVal->GetTypeInfoCount(&tiCount)) == S_OK
        && tiCount == 1) {
        pVariant->pdispVal->GetTypeInfo(0, LOCALE_USER_DEFAULT, &ti);
    }
    if (ti != NULL) {
        ITypeLib* pTypeLib = NULL;
        BSTR bSTRclassName;
        BSTR bSTRClassType;
        if (SUCCEEDED(ti->GetContainingTypeLib(&pTypeLib, 0))) {
            if (SUCCEEDED(
                    pTypeLib->GetDocumentation(MEMBERID_NIL, &bSTRclassName, NULL, NULL, NULL))) {
                className = std::wstring(bSTRclassName);
                SysFreeString(bSTRclassName);
            }
        }
        if (SUCCEEDED(ti->GetDocumentation(MEMBERID_NIL, &bSTRClassType, NULL, NULL, NULL))) {
            classTypeName = std::wstring(bSTRClassType);
            SysFreeString(bSTRClassType);
        }
        haveError = false;
        ti->Release();
    }
    return haveError;
}
//=============================================================================
void
classnameComHandle(ComHandleObject* comHandle, std::wstring& classname)
{
    classname = COM_CATEGORY_STR;
    if (comHandle) {
        VARIANT* pVariant = (VARIANT*)comHandle->getPointer();
        std::wstring className;
        std::wstring classTypeName;
        bool res = getClassInfoFromVariant(pVariant, className, classTypeName);
        if (!res) {
            classname = COM_CATEGORY_STR + std::wstring(L".") + className + classTypeName;
        }
    }
}
//=============================================================================
void
classnameComHandle(const ArrayOf& A, std::wstring& classname)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)hlObj;
    if (comhandleobj) {
        classnameComHandle(comhandleobj, classname);
    }
}
//=============================================================================
void
classnameComHandle(const ArrayOf& A, wstringVector& classname)
{
    classname.clear();
    if (!A.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    std::wstring className;
    ClassName(A, className);
    if (className != COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    Dimensions dimsA = A.getDimensions();
    auto* qp = (nelson_handle*)A.getDataPointer();
    if (qp) {
        indexType elementCount = dimsA.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            nelson_handle hl = qp[k];
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj != nullptr) {
                auto* comhandleobj = (ComHandleObject*)hlObj;
                std::wstring name;
                classnameComHandle(comhandleobj, name);
                classname.push_back(name);
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
