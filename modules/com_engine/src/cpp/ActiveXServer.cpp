//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Ole2.h>
#include <atlconv.h>
#include "ActiveXServer.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ComHandleObject*
ActiveXServer(const std::wstring& progId, const std::wstring& machine)
{
    IDispatch* pdispApplication = nullptr;
    CLSID clsApplication;
    ComHandleObject* res = nullptr;
    if (StringHelpers::starts_with(progId, L"{")) {
        if (FAILED(CLSIDFromString(progId.c_str(), &clsApplication))) {
            Error(_W("Error CLSIDFromString."));
        }
    } else {
        if (FAILED(CLSIDFromProgID(progId.c_str(), &clsApplication))) {
            Error(_W("Error CLSIDFromProgID."));
        }
    }
    if (!machine.empty()) {
        COSERVERINFO ServerInfo;
        ZeroMemory(&ServerInfo, sizeof(COSERVERINFO));
        COAUTHINFO athn;
        ZeroMemory(&athn, sizeof(COAUTHINFO));
        athn.dwAuthnLevel = RPC_C_AUTHN_LEVEL_NONE;
        athn.dwAuthnSvc = RPC_C_AUTHN_WINNT;
        athn.dwAuthzSvc = RPC_C_AUTHZ_NONE;
        athn.dwCapabilities = EOAC_NONE;
        athn.dwImpersonationLevel = RPC_C_IMP_LEVEL_IMPERSONATE;
        athn.pAuthIdentityData = nullptr;
        athn.pwszServerPrincName = nullptr;
        ServerInfo.pwszName = &std::wstring(machine)[0];
        ServerInfo.pAuthInfo = &athn;
        ServerInfo.dwReserved1 = 0;
        ServerInfo.dwReserved2 = 0;
        MULTI_QI qi = { nullptr, nullptr, 0 };
        ZeroMemory(&qi, sizeof(MULTI_QI));
        qi.pIID = &IID_IDispatch;
        if (FAILED(CoCreateInstanceEx(
                clsApplication, nullptr, CLSCTX_REMOTE_SERVER, &ServerInfo, 1, &qi))) {
            Error(_W("Error CoCreateInstanceEx."));
        }
        pdispApplication = (IDispatch*)qi.pItf;
    } else {
        if (FAILED(CoCreateInstance(clsApplication, nullptr, CLSCTX_SERVER, IID_IDispatch,
                (void**)&pdispApplication))) {
            Error(_W("Error CoCreateInstanceEx."));
        }
    }
    VARIANT* pVariantApplication = nullptr;
    try {
        pVariantApplication = new VARIANT;
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    VariantInit(pVariantApplication);
    pVariantApplication->vt = VT_DISPATCH;
    pVariantApplication->pdispVal = pdispApplication;
    try {
        res = new ComHandleObject(pVariantApplication);
    } catch (std::bad_alloc&) {
        res = nullptr;
    }
    return res;
}
//=============================================================================
ComHandleObject*
GetRunningActiveXServer(const std::wstring& progId)
{
    IUnknown* pUnknown;
    CLSID clsApplication;
    IDispatch* pdispApplication = nullptr;
    VARIANT* pVariantApplication = nullptr;
    LPOLESTR idName = W2OLE(const_cast<wchar_t*>(progId.c_str()));
    if (progId[0] == L'{') {
        if (FAILED(CLSIDFromString(idName, &clsApplication))) {
            Error(_W("Invalid PROGID."));
        }
    } else {
        if (FAILED(CLSIDFromProgID(idName, &clsApplication))) {
            Error(_W("Invalid PROGID."));
        }
    }
    HRESULT hRes = GetActiveObject(clsApplication, nullptr, &pUnknown);
    if (FAILED(hRes)) {
        Error(_W("Server is not running on this system."));
    }
    hRes = pUnknown->QueryInterface(IID_IDispatch, reinterpret_cast<void**>(&pdispApplication));
    pUnknown->Release();
    if (FAILED(hRes)) {
        Error(_W("Fails to connect to server."));
    }
    try {
        pVariantApplication = new VARIANT;
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    VariantInit(pVariantApplication);
    pVariantApplication->vt = VT_DISPATCH;
    pVariantApplication->pdispVal = pdispApplication;
    auto* res = new ComHandleObject(pVariantApplication);
    return res;
}
//=============================================================================
static std::wstring
reg_enum_key(HKEY hkey, DWORD i)
{
    wchar_t buf[4096];
    DWORD size_buf = sizeof(buf);
    FILETIME ft;
    LSTATUS err = RegEnumKeyExW(hkey, i, buf, &size_buf, nullptr, nullptr, nullptr, &ft);
    if (err == ERROR_SUCCESS) {
        return std::wstring(buf);
    }
    return std::wstring();
}
//=============================================================================
static std::wstring
GetStringRegKey(HKEY hKey, const std::wstring& strValueName)
{
    WCHAR szBuffer[4096];
    DWORD dwBufferSize = sizeof(szBuffer);
    ULONG nError;
    nError = RegQueryValueExW(hKey, strValueName.c_str(), nullptr, nullptr,
        reinterpret_cast<LPBYTE>(szBuffer), &dwBufferSize);
    if (ERROR_SUCCESS == nError) {
        return std::wstring(szBuffer);
    }
    return std::wstring();
}
//=============================================================================
ArrayOf
ActiveXContolList()
{
    ArrayOf res;
    HKEY hclsid;
    LONG err = RegOpenKeyExW(HKEY_LOCAL_MACHINE, L"SOFTWARE\\CLASSES\\CLSID", 0, KEY_READ, &hclsid);
    bool found = false;
    if (err != ERROR_SUCCESS) {
        RegCloseKey(hclsid);
        Error("Cannot read registry.");
    }
    wstringVector fieldsName;
    wstringVector fieldsProgId;
    wstringVector fieldsFilename;
    for (int i = 0; !found; i++) {
        std::wstring clsidString = reg_enum_key(hclsid, i);
        if (!clsidString.empty()) {
            if (clsidString != L"CLSID") {
                std::wstring name;
                std::wstring progid;
                std::wstring filename;
                HKEY hKey;
                std::wstring subKey;
                subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                subKey = subKey + clsidString;
                subKey = subKey + L"\\Control";
                LONG lRes = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                if (lRes == ERROR_SUCCESS) {
                    RegCloseKey(hKey);
                    subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                    subKey = subKey + clsidString;
                    subKey = subKey + L"\\VersionIndependentProgID";
                    LONG lRes
                        = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                    if (lRes == ERROR_SUCCESS) {
                        name = GetStringRegKey(hKey, L"");
                    }
                    RegCloseKey(hKey);
                    subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                    subKey = subKey + clsidString;
                    subKey = subKey + L"\\ProgID";
                    lRes = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                    if (lRes == ERROR_SUCCESS) {
                        progid = GetStringRegKey(hKey, L"");
                    }
                    RegCloseKey(hKey);
                    subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                    subKey = subKey + clsidString;
                    subKey = subKey + L"\\InprocServer32";
                    lRes = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                    if (lRes == ERROR_SUCCESS) {
                        filename = GetStringRegKey(hKey, L"");
                    }
                    RegCloseKey(hKey);
                    if (!filename.empty() && !name.empty() && !progid.empty()) {
                        fieldsName.push_back(name);
                        fieldsProgId.push_back(progid);
                        fieldsFilename.push_back(filename);
                    }
                }
            }
        } else {
            break;
        }
    }
    RegCloseKey(hclsid);
    Dimensions dims(fieldsName.size(), 3);
    ArrayOf* cell = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false));
    for (size_t k = 0; k < fieldsName.size(); k = k + 1) {
        cell[k] = ArrayOf::characterArrayConstructor(fieldsName[k]);
    }
    for (size_t k = 0; k < fieldsProgId.size(); k = k + 1) {
        cell[k + fieldsName.size()] = ArrayOf::characterArrayConstructor(fieldsProgId[k]);
    }
    for (size_t k = 0; k < fieldsFilename.size(); k = k + 1) {
        cell[k + fieldsName.size() + fieldsProgId.size()]
            = ArrayOf::characterArrayConstructor(fieldsFilename[k]);
    }
    res = ArrayOf(NLS_CELL_ARRAY, dims, cell);
    return res;
}
//=============================================================================
ArrayOf
ActiveXServerList()
{
    ArrayOf res;
    HKEY hclsid;
    LONG err = RegOpenKeyExW(HKEY_LOCAL_MACHINE, L"SOFTWARE\\CLASSES\\CLSID", 0, KEY_READ, &hclsid);
    bool found = false;
    if (err != ERROR_SUCCESS) {
        RegCloseKey(hclsid);
        Error("Cannot read registry.");
    }
    wstringVector fieldsName;
    wstringVector fieldsProgId;
    wstringVector fieldsFilename;
    for (int i = 0; !found; i++) {
        std::wstring clsidString = reg_enum_key(hclsid, i);
        if (!clsidString.empty()) {
            if (clsidString != L"CLSID") {
                std::wstring name;
                std::wstring progid;
                std::wstring filename;
                HKEY hKey;
                std::wstring subKey;
                subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                subKey = subKey + clsidString;
                subKey = subKey + L"\\TypeLib";
                LONG lRes = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                if (lRes == ERROR_SUCCESS) {
                    RegCloseKey(hKey);
                    subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                    subKey = subKey + clsidString;
                    subKey = subKey + L"\\VersionIndependentProgID";
                    LONG lRes
                        = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                    if (lRes == ERROR_SUCCESS) {
                        name = GetStringRegKey(hKey, L"");
                    }
                    RegCloseKey(hKey);
                    subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                    subKey = subKey + clsidString;
                    subKey = subKey + L"\\ProgID";
                    lRes = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                    if (lRes == ERROR_SUCCESS) {
                        progid = GetStringRegKey(hKey, L"");
                    }
                    RegCloseKey(hKey);
                    subKey = L"SOFTWARE\\CLASSES\\CLSID\\";
                    subKey = subKey + clsidString;
                    subKey = subKey + L"\\InprocServer32";
                    lRes = RegOpenKeyExW(HKEY_LOCAL_MACHINE, subKey.c_str(), 0, KEY_READ, &hKey);
                    if (lRes == ERROR_SUCCESS) {
                        filename = GetStringRegKey(hKey, L"");
                    }
                    RegCloseKey(hKey);
                    if (!filename.empty() && !name.empty() && !progid.empty()) {
                        fieldsName.push_back(name);
                        fieldsProgId.push_back(progid);
                        fieldsFilename.push_back(filename);
                    }
                }
            }
        } else {
            break;
        }
    }
    RegCloseKey(hclsid);
    Dimensions dims(fieldsName.size(), 3);
    ArrayOf* cell = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false));
    for (size_t k = 0; k < fieldsName.size(); k = k + 1) {
        cell[k] = ArrayOf::characterArrayConstructor(fieldsName[k]);
    }
    for (size_t k = 0; k < fieldsProgId.size(); k = k + 1) {
        cell[k + fieldsName.size()] = ArrayOf::characterArrayConstructor(fieldsProgId[k]);
    }
    for (size_t k = 0; k < fieldsFilename.size(); k = k + 1) {
        cell[k + fieldsName.size() + fieldsProgId.size()]
            = ArrayOf::characterArrayConstructor(fieldsFilename[k]);
    }
    res = ArrayOf(NLS_CELL_ARRAY, dims, cell);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
