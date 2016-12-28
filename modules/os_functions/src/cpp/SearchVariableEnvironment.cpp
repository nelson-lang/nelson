//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <boost/container/vector.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "SearchVariableEnvironment.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    wstringVector splitEnvironmentPath(std::wstring envPaths)
    {
        wstringVector result;
        size_t previous = 0;
#ifdef _MSC_VER
        const wchar_t delimiter[2] = L";";
#else
        const wchar_t delimiter[2] = L":";
#endif
        size_t index = envPaths.find(delimiter);
        while (index != std::string::npos)
        {
            std::wstring s = envPaths.substr(previous, index - previous);
            if (s.compare(L"") != 0)
            {
                result.push_back(s);
            }
            previous = index + 1;
            index = envPaths.find(delimiter, previous);
        }
        std::wstring s = envPaths.substr(previous);
        if (s.compare(L"") != 0)
        {
            result.push_back(s);
        }
        return result;
    }
    //=============================================================================
    std::wstring SearchVariableEnvironmentW(std::wstring fileToSearch, std::wstring envVarName)
    {
        // As searchenv function, we search in current directory first
        boost::filesystem::path pwd = boost::filesystem::current_path();
        pwd /= fileToSearch;
        if (boost::filesystem::exists(pwd) && !boost::filesystem::is_directory(pwd))
        {
            return pwd.generic_wstring();
        }
        std::wstring envValue = GetVariableEnvironment(envVarName, L"");
        wstringVector envValuevector = splitEnvironmentPath(envValue);
        for (size_t k = 0; k < envValuevector.size(); k++)
        {
            boost::filesystem::path fullpath(envValuevector[k]);
            fullpath /= fileToSearch;
            if (boost::filesystem::exists(fullpath) && !boost::filesystem::is_directory(fullpath))
            {
                return fullpath.generic_wstring();
            }
        }
        return std::wstring(L"");
    }
    //=============================================================================
    std::string SearchVariableEnvironmentU(std::string fileToSearch, std::string envVarName)
    {
        std::wstring res = SearchVariableEnvironmentW(utf8_to_wstring(fileToSearch), utf8_to_wstring(envVarName));
        return wstring_to_utf8(res);
    }
    //=============================================================================
}
//=============================================================================
