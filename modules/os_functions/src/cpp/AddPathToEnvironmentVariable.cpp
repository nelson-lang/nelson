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
#endif
#include <vector>
#include <cstdlib>
#include "AddPathToEnvironmentVariable.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<std::wstring>
splitEnvironmentPath(const std::wstring& envPaths)
{
    std::vector<std::wstring> result;
    size_t previous = 0;
#ifdef _MSC_VER
    const wchar_t delimiter = L';';
#else
    const wchar_t delimiter = L':';
#endif
    size_t index = envPaths.find(delimiter);
    while (index != std::string::npos) {
        std::wstring s = envPaths.substr(previous, index - previous);
        if (s != L"") {
            result.push_back(s);
        }
        previous = index + 1;
        index = envPaths.find(delimiter, previous);
    }
    std::wstring s = envPaths.substr(previous);
    if (s != L"") {
        result.push_back(s);
    }
    return result;
}
//=============================================================================
std::wstring
concateEnvironmentPath(std::vector<std::wstring> listPath)
{
    std::wstring res = L"";
    for (size_t k = 0; k < listPath.size() - 1; k++) {
        res.append(listPath[k]);
#ifdef _MSC_VER
        res.append(L";");
#else
        res.append(L":");
#endif
    }
    if (listPath.size() >= 1) {
        res.append(listPath[listPath.size() - 1]);
    }
    return res;
}
//=============================================================================
template <class InputIterator, class T>
InputIterator
localfind(InputIterator first, InputIterator last, const T& val)
{
    while (first != last) {
        if (*first == val) {
            return first;
        }
        ++first;
    }
    return last;
}
//=============================================================================
bool
AddPathToEnvironmentVariable(const std::wstring& envVar, const std::wstring& pathToAdd)
{
    std::wstring env;
#ifdef _MSC_VER
    env = GetVariableEnvironment(envVar, std::wstring());
#else
    char const* tmp = std::getenv(wstring_to_utf8(envVar).c_str());
    if (tmp) {
        env = utf8_to_wstring(tmp);
    }
#endif
    std::vector<std::wstring> pathVector = splitEnvironmentPath(env);
    std::vector<std::wstring>::iterator it;
#ifdef __GNUC__
    /*bug with GCC 5 */
    it = localfind(pathVector.begin(), pathVector.end(), pathToAdd);
#else
    it = localfind(pathVector.begin(), pathVector.end(), pathToAdd);
#endif
    if (it != pathVector.end()) {
        // nothing to do
        return true;
    } else {
        pathVector.insert(pathVector.begin(), pathToAdd);
        std::wstring newEnv = concateEnvironmentPath(pathVector);
#ifdef _MSC_VER
        SetEnvironmentVariableW(envVar.c_str(), newEnv.c_str());
#else
        setenv(wstring_to_utf8(envVar).c_str(), wstring_to_utf8(newEnv).c_str(), 1);
#endif
    }
    return false;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
