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
#pragma once
//=============================================================================
#include "FileFunction.hpp"
#include "FileWatcherManager.hpp"
#include "Types.hpp"
#include <boost/unordered_map.hpp>
#include <string>
//=============================================================================
namespace Nelson {
class PathFunc
{
public:
    PathFunc(const std::wstring& path, bool withWatcher = true);
    ~PathFunc();
    wstringVector
    getFunctionsName(const std::wstring& prefix = L"");
    wstringVector
    getFunctionsFilename();
    std::wstring
    getPath();
    void
    rehash();
    bool
    findFuncName(const std::wstring& functionName, std::wstring& filename);
    bool
    findFuncName(const std::wstring& functionName, FileFunction** ff);

private:
    boost::unordered_map<std::wstring, FileFunction*> mapAllFiles;
    boost::unordered_map<std::wstring, FileFunction*> mapRecentFiles;
    std::wstring _path;
    bool
    isSupportedFuncFilename(const std::wstring& name);
    std::wstring
    uniformizePathName(const std::wstring& pathname);
    bool
    comparePathname(const std::wstring& path1, const std::wstring& path2);
    bool
    isdir(const std::wstring& path);
    bool withWatcher;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
