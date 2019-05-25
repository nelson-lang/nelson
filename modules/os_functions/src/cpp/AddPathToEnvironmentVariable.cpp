//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "AddPathToEnvironmentVariable.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include <boost/container/vector.hpp>
#include <cstdlib>
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::container::vector<std::wstring>
splitEnvironmentPath(const std::wstring& envPaths)
{
    boost::container::vector<std::wstring> result;
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
concateEnvironmentPath(boost::container::vector<std::wstring> listPath)
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
    boost::container::vector<std::wstring> pathVector = splitEnvironmentPath(env);
    boost::container::vector<std::wstring>::iterator it;
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
