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
#pragma once
//=============================================================================
#include "FileFunc.hpp"
#include "FileWatcherManager.hpp"
#include "Types.hpp"
#include <boost/unordered_map.hpp>
#include <string>
//=============================================================================
namespace Nelson {
class PathFunc
{
public:
    PathFunc(const std::wstring path);
    ~PathFunc();
    wstringVector
    getFunctionsName(std::wstring prefix = L"");
    wstringVector
    getFunctionsFilename();
    std::wstring
    getPath();
    void
    rehash();
    bool
    findFuncName(const std::wstring functionName, std::wstring& filename);
    bool
    findFuncName(const std::wstring functionName, FileFunc** ff);
    bool
    findFuncByHash(size_t hashid, std::wstring& functionName);

private:
    boost::unordered_map<std::wstring, FileFunc*> mapAllFiles;
    boost::unordered_map<std::wstring, FileFunc*> mapRecentFiles;
    std::wstring _path;
    bool
    isSupportedFuncFilename(std::wstring name);
    std::wstring
    uniformizePathName(std::wstring pathname);
    bool
    comparePathname(std::wstring path1, std::wstring path2);
    bool
    isdir(std::wstring path);
};
} // namespace Nelson
//=============================================================================
