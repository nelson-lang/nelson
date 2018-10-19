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
#ifdef _MSC_VER
#include "ToCellString.hpp"
#include <Windows.h>
#include <boost/unordered_map.hpp>
#endif
#include "WindowsQueryRegistry.hpp"
//=============================================================================
namespace Nelson {
#ifdef _MSC_VER
#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383
static boost::unordered::unordered_map<std::wstring, HKEY> hkeyMap;
static bool hkeyMapInitialized = false;
static void
initializeHkeyMap();
static bool
isValidHkeyFromString(const std::wstring& hkeystring);
static HKEY
getHkeyFromString(const std::wstring& hkeystring);
static bool
openRegistryKey(HKEY rootKey, std::wstring subKey, HKEY* ouputHkey);
static bool
getRegistryKeyInfo(HKEY rootKey, std::wstring subKey, size_t& keysNumber, size_t& valuesNumber);
#endif
//=============================================================================
ArrayOf
windowsQueryRegistryAllValuesNames(
    const std::wstring& rootkey, const std::wstring& subkey, std::wstring& errorMessage)
{
    ArrayOf res;
#ifdef _MSC_VER
    errorMessage = L"";
    initializeHkeyMap();
    if (!isValidHkeyFromString(rootkey)) {
        errorMessage = _W("Invalid ROOTKEY value.");
    } else {
        HKEY hroot = getHkeyFromString(rootkey);
        size_t nbKeys = 0;
        size_t nbValues = 0;
        if (getRegistryKeyInfo(hroot, subkey, nbKeys, nbValues) == false) {
            errorMessage = _W("Cannot get KEY information.");
        } else {
            HKEY desiredHkey = NULL;
            bool isOpen = openRegistryKey(hroot, subkey, &desiredHkey);
            if (!isOpen) {
                errorMessage = _W("Invalid SUBKEY value.");
            } else {
                wstringVector names;
                names.reserve(nbKeys);
                for (size_t i = 0; i < nbKeys; i++) {
                    TCHAR achKey[MAX_KEY_LENGTH];
                    DWORD cbName = MAX_KEY_LENGTH;
                    DWORD retCode = RegEnumValue(
                        desiredHkey, (DWORD)i, achKey, &cbName, NULL, NULL, NULL, NULL);
                    if (retCode == ERROR_SUCCESS) {
                        names.push_back(achKey);
                    }
                }
                RegCloseKey(desiredHkey);
                res = ToCellStringAsColumn(names);
            }
        }
    }
#else
    errorMessage = _W("Not implemented on this platform.");
#endif
    return res;
}
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP ArrayOf
windowsQueryRegistryValueName(const std::wstring& rootkey, const std::wstring& subkey,
    const std::wstring& valname, std::wstring& errorMessage)
{
    ArrayOf res;
#ifdef _MSC_VER
    errorMessage = L"";
    initializeHkeyMap();
    if (!isValidHkeyFromString(rootkey)) {
        errorMessage = _W("Invalid ROOTKEY value.");
    } else {
        HKEY hroot = getHkeyFromString(rootkey);
        HKEY desiredHkey = NULL;
        bool isOpen = openRegistryKey(hroot, subkey, &desiredHkey);
        if (!isOpen) {
            errorMessage = _W("Invalid SUBKEY value.");
            return res;
        }
        DWORD ouputType = 0;
        if (RegQueryValueEx(desiredHkey, valname.c_str(), NULL, &ouputType, NULL, NULL)
            == ERROR_SUCCESS) {
            if ((ouputType == REG_EXPAND_SZ) || (ouputType == REG_SZ)) {
                DWORD Length = MAX_VALUE_NAME;
                wchar_t Line[MAX_VALUE_NAME];
                if (RegQueryValueEx(
                        desiredHkey, valname.c_str(), NULL, &ouputType, (LPBYTE)&Line, &Length)
                    == ERROR_SUCCESS) {
                    res = ArrayOf::characterArrayConstructor(Line);
                }
            } else {
                DWORD size = 4;
                int Num = 0;
                if (RegQueryValueEx(
                        desiredHkey, valname.c_str(), NULL, &ouputType, (LPBYTE)&Num, &size)
                    == ERROR_SUCCESS) {
                    res = ArrayOf::int32Constructor(Num);
                }
            }
        } else {
            errorMessage = _W("Cannot convert RegQueryValueEx.");
        }
        RegCloseKey(desiredHkey);
    }
#else
    errorMessage = _W("Not implemented on this platform.");
#endif
    return res;
}
//=============================================================================
#ifdef _MSC_VER
void
initializeHkeyMap()
{
    if (!hkeyMapInitialized) {
        hkeyMap[L"HKEY_CLASSES_ROOT"] = HKEY_CLASSES_ROOT;
        hkeyMap[L"HKCR"] = HKEY_CLASSES_ROOT;
        hkeyMap[L"HKEY_CURRENT_USER"] = HKEY_CURRENT_USER;
        hkeyMap[L"HKCU"] = HKEY_CURRENT_USER;
        hkeyMap[L"HKEY_LOCAL_MACHINE"] = HKEY_LOCAL_MACHINE;
        hkeyMap[L"HKLM"] = HKEY_LOCAL_MACHINE;
        hkeyMap[L"HKEY_USERS"] = HKEY_USERS;
        hkeyMap[L"HKU"] = HKEY_USERS;
        hkeyMap[L"HKEY_PERFORMANCE_DATA"] = HKEY_PERFORMANCE_DATA;
        hkeyMap[L"HKPD"] = HKEY_PERFORMANCE_DATA;
        hkeyMap[L"HKEY_PERFORMANCE_TEXT"] = HKEY_PERFORMANCE_TEXT;
        hkeyMap[L"HKPT"] = HKEY_PERFORMANCE_TEXT;
        hkeyMap[L"HKEY_PERFORMANCE_NLSTEXT"] = HKEY_PERFORMANCE_NLSTEXT;
        hkeyMap[L"HKPN"] = HKEY_PERFORMANCE_NLSTEXT;
        hkeyMap[L"HKEY_CURRENT_CONFIG"] = HKEY_CURRENT_CONFIG;
        hkeyMap[L"HKCC"] = HKEY_CURRENT_CONFIG;
        hkeyMap[L"HKEY_DYN_DATA"] = HKEY_DYN_DATA;
        hkeyMap[L"HKDD"] = HKEY_DYN_DATA;
        hkeyMap[L"HKEY_CURRENT_USER_LOCAL_SETTINGS"] = HKEY_CURRENT_USER_LOCAL_SETTINGS;
        hkeyMap[L"HKCLS"] = HKEY_CURRENT_USER_LOCAL_SETTINGS;
        hkeyMapInitialized = true;
    }
}
//=============================================================================
bool
isValidHkeyFromString(const std::wstring& hkeystring)
{
    return (hkeyMap.count(hkeystring) != 0);
}
//=============================================================================
HKEY
getHkeyFromString(const std::wstring& hkeystring)
{
    return hkeyMap[hkeystring];
}
//=============================================================================
bool
openRegistryKey(HKEY rootKey, std::wstring subKey, HKEY* ouputHkey)
{
    DWORD OpensKeyOptions = KEY_ENUMERATE_SUB_KEYS | KEY_QUERY_VALUE | KEY_WOW64_32KEY;
    if (RegOpenKeyEx(rootKey, subKey.c_str(), 0, OpensKeyOptions, ouputHkey) == ERROR_SUCCESS) {
        return true;
    }
    OpensKeyOptions = KEY_ENUMERATE_SUB_KEYS | KEY_QUERY_VALUE | KEY_WOW64_64KEY;
    if (RegOpenKeyEx(rootKey, subKey.c_str(), 0, OpensKeyOptions, ouputHkey) == ERROR_SUCCESS) {
        return true;
    }
    OpensKeyOptions = KEY_ENUMERATE_SUB_KEYS | KEY_QUERY_VALUE;
    if (RegOpenKeyEx(rootKey, subKey.c_str(), 0, OpensKeyOptions, ouputHkey) == ERROR_SUCCESS) {
        return true;
    }
    return false;
}
//=============================================================================
bool
getRegistryKeyInfo(HKEY rootKey, std::wstring subKey, size_t& keysNumber, size_t& valuesNumber)
{
    keysNumber = 0;
    valuesNumber = 0;
    HKEY desiredKey;
    TCHAR achClass[MAX_VALUE_NAME] = TEXT("");
    DWORD cchClassName = MAX_VALUE_NAME;
    DWORD cSubKeys = 0;
    DWORD cbMaxSubKey = 0;
    DWORD cchMaxClass = 0;
    DWORD cValues = 0;
    DWORD cchMaxValue = 0;
    DWORD cbMaxValueData = 0;
    DWORD cbSecurityDescriptor = 0;
    FILETIME ftLastWriteTime;
    if (openRegistryKey(rootKey, subKey, &desiredKey) == false) {
        return false;
    }
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms724902(v=vs.85).aspx
    DWORD retCode = RegQueryInfoKey(desiredKey, achClass, &cchClassName, NULL, &cSubKeys,
        &cbMaxSubKey, &cchMaxClass, &cValues, &cchMaxValue, &cbMaxValueData, &cbSecurityDescriptor,
        &ftLastWriteTime);
    RegCloseKey(desiredKey);
    if (retCode == ERROR_SUCCESS) {
        keysNumber = (size_t)cValues;
        valuesNumber = (size_t)cSubKeys;
        return true;
    }
    return false;
}
//=============================================================================
#endif
//=============================================================================
}
//=============================================================================
