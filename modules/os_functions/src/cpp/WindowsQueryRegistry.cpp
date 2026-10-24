//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <unordered_map>
#endif
#include "WindowsQueryRegistry.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
#ifdef _MSC_VER
#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383
static std::unordered_map<std::wstring, HKEY> hkeyMap;
static bool hkeyMapInitialized = false;
static void
initializeHkeyMap();
static bool
isValidHkeyFromString(const std::wstring& hkeystring);
static HKEY
getHkeyFromString(const std::wstring& hkeystring);
static bool
openRegistryKey(HKEY rootKey, const std::wstring& subKey, HKEY* outputHkey);
static bool
getRegistryKeyInfo(
    HKEY rootKey, const std::wstring& subKey, size_t& keysNumber, size_t& valuesNumber);
#endif
//=============================================================================
ArrayOf
windowsQueryRegistryAllValuesNames(
    const std::wstring& rootkey, const std::wstring& subkey, std::wstring& errorMessage)
{
    ArrayOf res;
#ifdef _MSC_VER
    errorMessage.clear();
    initializeHkeyMap();
    if (!isValidHkeyFromString(rootkey)) {
        errorMessage = resolveMessageTemplate(
            L"Nelson:os_functions:invalidRootkey", L"Invalid ROOTKEY value.");
    } else {
        HKEY hroot = getHkeyFromString(rootkey);
        size_t nbKeys = 0;
        size_t nbValues = 0;
        if (static_cast<int>(getRegistryKeyInfo(hroot, subkey, nbKeys, nbValues)) == false) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:os_functions:cannotGetKeyInformation", L"Cannot get KEY information.");
        } else {
            HKEY desiredHkey = nullptr;
            bool isOpen = openRegistryKey(hroot, subkey, &desiredHkey);
            if (!isOpen) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:os_functions:invalidSubkey", L"Invalid SUBKEY value.");
            } else {
                wstringVector names;
                names.reserve(nbKeys);
                for (size_t i = 0; i < nbKeys; i++) {
                    TCHAR achKey[MAX_KEY_LENGTH];
                    DWORD cbName = MAX_KEY_LENGTH;
                    DWORD retCode = RegEnumValue(desiredHkey, static_cast<DWORD>(i), achKey,
                        &cbName, nullptr, nullptr, nullptr, nullptr);
                    if (retCode == ERROR_SUCCESS) {
                        names.push_back(achKey);
                    }
                }
                RegCloseKey(desiredHkey);
                res = ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(names);
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
    errorMessage.clear();
    initializeHkeyMap();
    if (!isValidHkeyFromString(rootkey)) {
        errorMessage = resolveMessageTemplate(
            L"Nelson:os_functions:invalidRootkey", L"Invalid ROOTKEY value.");
    } else {
        HKEY hroot = getHkeyFromString(rootkey);
        HKEY desiredHkey = nullptr;
        bool isOpen = openRegistryKey(hroot, subkey, &desiredHkey);
        if (!isOpen) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:os_functions:invalidSubkey", L"Invalid SUBKEY value.");
            return res;
        }
        DWORD outputType = 0;
        if (RegQueryValueEx(desiredHkey, valname.c_str(), nullptr, &outputType, nullptr, nullptr)
            == ERROR_SUCCESS) {
            if ((outputType == REG_EXPAND_SZ) || (outputType == REG_SZ)) {
                DWORD Length = sizeof(wchar_t) * MAX_VALUE_NAME;
                wchar_t Line[MAX_VALUE_NAME] = { 0 };
                if (RegQueryValueEx(desiredHkey, valname.c_str(), nullptr, &outputType,
                        reinterpret_cast<LPBYTE>(&Line), &Length)
                    == ERROR_SUCCESS) {
                    // RegQueryValueEx does not guarantee NUL termination; terminate
                    // at the returned byte length before treating Line as a string.
                    size_t count = Length / sizeof(wchar_t);
                    if (count >= MAX_VALUE_NAME) {
                        count = MAX_VALUE_NAME - 1;
                    }
                    Line[count] = L'\0';
                    res = ArrayOf::characterArray(Line);
                }
            } else {
                DWORD size = 4;
                int Num = 0;
                if (RegQueryValueEx(desiredHkey, valname.c_str(), nullptr, &outputType,
                        reinterpret_cast<LPBYTE>(&Num), &size)
                    == ERROR_SUCCESS) {
                    res = ArrayOf::int32Scalar(Num);
                }
            }
        } else {
            errorMessage
                = resolveMessageTemplate(L"Nelson:os_functions:cannotConvertRegqueryvalueex",
                    L"Cannot convert RegQueryValueEx.");
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
openRegistryKey(HKEY rootKey, const std::wstring& subKey, HKEY* outputHkey)
{
    DWORD OpensKeyOptions = KEY_ENUMERATE_SUB_KEYS | KEY_QUERY_VALUE | KEY_WOW64_32KEY;
    if (RegOpenKeyEx(rootKey, subKey.c_str(), 0, OpensKeyOptions, outputHkey) == ERROR_SUCCESS) {
        return true;
    }
    OpensKeyOptions = KEY_ENUMERATE_SUB_KEYS | KEY_QUERY_VALUE | KEY_WOW64_64KEY;
    if (RegOpenKeyEx(rootKey, subKey.c_str(), 0, OpensKeyOptions, outputHkey) == ERROR_SUCCESS) {
        return true;
    }
    OpensKeyOptions = KEY_ENUMERATE_SUB_KEYS | KEY_QUERY_VALUE;
    if (RegOpenKeyEx(rootKey, subKey.c_str(), 0, OpensKeyOptions, outputHkey) == ERROR_SUCCESS) {
        return true;
    }
    return false;
}
//=============================================================================
bool
getRegistryKeyInfo(
    HKEY rootKey, const std::wstring& subKey, size_t& keysNumber, size_t& valuesNumber)
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
    if (static_cast<int>(openRegistryKey(rootKey, subKey, &desiredKey)) == false) {
        return false;
    }
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms724902(v=vs.85).aspx
    DWORD retCode = RegQueryInfoKey(desiredKey, achClass, &cchClassName, nullptr, &cSubKeys,
        &cbMaxSubKey, &cchMaxClass, &cValues, &cchMaxValue, &cbMaxValueData, &cbSecurityDescriptor,
        &ftLastWriteTime);
    RegCloseKey(desiredKey);
    if (retCode == ERROR_SUCCESS) {
        keysNumber = static_cast<size_t>(cValues);
        valuesNumber = static_cast<size_t>(cSubKeys);
        return true;
    }
    return false;
}
//=============================================================================
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
