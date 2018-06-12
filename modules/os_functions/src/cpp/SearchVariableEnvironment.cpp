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
#include "SearchVariableEnvironment.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include <boost/container/vector.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
splitEnvironmentPath(const std::wstring& envPaths)
{
    wstringVector result;
    size_t previous = 0;
#ifdef _MSC_VER
    const wchar_t delimiter[2] = L";";
#else
    const wchar_t delimiter[2] = L":";
#endif
    size_t index = envPaths.find(delimiter);
    while (index != std::string::npos) {
        std::wstring s = envPaths.substr(previous, index - previous);
        if (s.compare(L"") != 0) {
            result.push_back(s);
        }
        previous = index + 1;
        index = envPaths.find(delimiter, previous);
    }
    std::wstring s = envPaths.substr(previous);
    if (s.compare(L"") != 0) {
        result.push_back(s);
    }
    return result;
}
//=============================================================================
wstringVector
SearchVariableEnvironmentW(const std::wstring& fileToSearch, const std::wstring& envVarName)
{
    wstringVector res;
    std::wstring envValue = GetVariableEnvironment(envVarName, L"");
    wstringVector envValuevector = splitEnvironmentPath(envValue);
    for (size_t k = 0; k < envValuevector.size(); k++) {
        boost::filesystem::path fullpath(envValuevector[k]);
        fullpath /= fileToSearch;
        if (boost::filesystem::exists(fullpath) && !boost::filesystem::is_directory(fullpath)) {
            res.push_back(fullpath.generic_wstring());
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
