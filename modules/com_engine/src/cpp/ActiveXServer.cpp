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
#include "ActiveXServer.hpp"
#include "Error.hpp"
#include <Ole2.h>
#include <Windows.h>
#include <atlconv.h>
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
ComHandleObject*
ActiveXServer(std::wstring progId, std::wstring machine)
{
    IDispatch* pdispApplication = nullptr;
    CLSID clsApplication;
    ComHandleObject* res = nullptr;
    if (boost::algorithm::starts_with(progId, L"{")) {
        if (FAILED(CLSIDFromString(progId.c_str(), &clsApplication))) {
            Error(_W("Error CLSIDFromString."));
        }
    } else {
        if (FAILED(CLSIDFromProgID(progId.c_str(), &clsApplication))) {
            Error(_W("Error CLSIDFromProgID."));
        }
    }
    if (machine != L"") {
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
        ServerInfo.pwszName = &machine[0];
        ServerInfo.pAuthInfo = &athn;
        ServerInfo.dwReserved1 = 0;
        ServerInfo.dwReserved2 = 0;
        MULTI_QI qi = { 0, 0, 0 };
        ZeroMemory(&qi, sizeof(MULTI_QI));
        qi.pIID = &IID_IDispatch;
        if (FAILED(CoCreateInstanceEx(
                clsApplication, NULL, CLSCTX_REMOTE_SERVER, &ServerInfo, 1, &qi))) {
            Error(_W("Error CoCreateInstanceEx."));
        }
        pdispApplication = (IDispatch*)qi.pItf;
    } else {
        if (FAILED(CoCreateInstance(
                clsApplication, NULL, CLSCTX_SERVER, IID_IDispatch, (void**)&pdispApplication))) {
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
    res = new ComHandleObject(pVariantApplication);
    return res;
}
//=============================================================================
ComHandleObject*
GetRunningActiveXServer(std::wstring progId)
{
    IUnknown* pUnknown;
    CLSID clsApplication;
    IDispatch* pdispApplication = nullptr;
    VARIANT* pVariantApplication = nullptr;
    HRESULT hRes = S_FALSE;
    LPOLESTR idName = W2OLE((wchar_t*)progId.c_str());
    if (progId[0] == L'{') {
        if (FAILED(CLSIDFromString(idName, &clsApplication))) {
            Error(_W("Invalid PROGID."));
        }
    } else {
        if (FAILED(CLSIDFromProgID(idName, &clsApplication))) {
            Error(_W("Invalid PROGID."));
        }
    }
    hRes = GetActiveObject(clsApplication, NULL, &pUnknown);
    if (FAILED(hRes)) {
        Error(_W("Server is not running on this system."));
    }
    hRes = pUnknown->QueryInterface(IID_IDispatch, (void**)&pdispApplication);
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
    ComHandleObject* res = new ComHandleObject(pVariantApplication);
    return res;
}
//=============================================================================
static std::wstring
reg_enum_key(HKEY hkey, DWORD i)
{
    wchar_t buf[4096];
    DWORD size_buf = sizeof(buf);
    FILETIME ft;
    LSTATUS err = RegEnumKeyExW(hkey, i, buf, &size_buf, NULL, NULL, NULL, &ft);
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
    nError = RegQueryValueExW(hKey, strValueName.c_str(), 0, NULL, (LPBYTE)szBuffer, &dwBufferSize);
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
    ArrayOf* cell = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount());
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
    ArrayOf* cell = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount());
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
}
//=============================================================================
