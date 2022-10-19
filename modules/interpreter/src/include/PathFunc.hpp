//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <unordered_map>
#include <string>
#include "FileFunction.hpp"
#include "Types.hpp"
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
    std::unordered_map<std::wstring, FileFunction*> mapAllFiles;
    std::unordered_map<std::wstring, FileFunction*> mapRecentFiles;
    std::wstring _path;
    bool
    isSupportedFuncFilename(const std::wstring& name);
    std::wstring
    uniformizePathName(const std::wstring& pathname);
    bool
    comparePathname(const std::wstring& path1, const std::wstring& path2);
    bool withWatcher;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
