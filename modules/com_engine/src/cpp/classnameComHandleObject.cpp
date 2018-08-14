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
classnameComHandle(ArrayOf A, std::wstring& classname)
{
    HandleGenericObject* hlObj = A.getContentAsHandleScalar();
    if (hlObj->getCategory() != COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    ComHandleObject* comhandleobj = (ComHandleObject*)hlObj;
    if (comhandleobj) {
        classnameComHandle(comhandleobj, classname);
    }
}
//=============================================================================
void
classnameComHandle(ArrayOf A, wstringVector& classname)
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
    nelson_handle* qp = (nelson_handle*)A.getDataPointer();
    if (qp) {
        stringVector names;
        for (indexType k = 0; k < dimsA.getElementCount(); k++) {
            nelson_handle hl = qp[k];
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj != nullptr) {
                ComHandleObject* comhandleobj = (ComHandleObject*)hlObj;
                std::wstring name;
                classnameComHandle(comhandleobj, name);
                classname.push_back(name);
            }
        }
    }
}
//=============================================================================
}
//=============================================================================
