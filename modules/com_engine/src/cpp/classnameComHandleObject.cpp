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
#include <Ole2.h>
#include <Windows.h>
#include <ocidl.h>
#include "classnameComHandleObject.hpp"
#include "ClassName.hpp"
#include "ComHandleObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
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
classnameComHandle(ComHandleObject* comHandle, std::string& classname)
{
    classname = NLS_HANDLE_COM_CATEGORY_STR;
    if (comHandle) {
        VARIANT* pVariant = (VARIANT*)comHandle->getPointer();
        std::wstring className;
        std::wstring classTypeName;
        bool res = getClassInfoFromVariant(pVariant, className, classTypeName);
        if (!res) {
            classname = NLS_HANDLE_COM_CATEGORY_STR + std::string(".")
                + wstring_to_utf8(className + classTypeName);
        }
    }
}
//=============================================================================
void
classnameComHandle(const ArrayOf& A, std::string& classname)
{
    classname = "handle";
    stringVector classnames;
    classnameComHandle(A, classnames);
    std::string common;
    for (size_t k = 0; k < classnames.size(); ++k) {

        if (k == 0) {
            common = classnames[0];
        } else {
            if (common != classnames[1]) {
                return;
            }
        }
    }
    classname = common;
}
//=============================================================================
void
classnameComHandle(const ArrayOf& A, stringVector& classname)
{
    classname.clear();
    if (!A.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    std::string className;
    ClassName(A, className);
    if (className != NLS_HANDLE_COM_CATEGORY_STR) {
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
                std::string name;
                classnameComHandle(comhandleobj, name);
                classname.push_back(name);
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
