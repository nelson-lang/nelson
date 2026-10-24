//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "Which.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "GatewayInfo.hpp"
#include "ModulesManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// A name with a file extension (e.g. 'data.mat') is a literal file: the current
// directory is searched first, then the search path. Every match is returned.
// Names without an extension are function/variable names only.
static wstringVector
resolveFilesOnSearchPath(const std::wstring& filename)
{
    wstringVector matches;
    const FileSystemWrapper::Path asPath(filename);
    if (!asPath.has_extension()) {
        return matches;
    }
    if (asPath.is_absolute()) {
        if (FileSystemWrapper::Path::is_regular_file(asPath)) {
            matches.push_back(asPath.generic_wstring());
        }
        return matches;
    }
    const FileSystemWrapper::Path inCurrentDirectory
        = FileSystemWrapper::Path::current_path() / filename;
    if (FileSystemWrapper::Path::is_regular_file(inCurrentDirectory)) {
        matches.push_back(inCurrentDirectory.generic_wstring());
    }
    const wstringVector paths = PathFunctionIndexerManager::getInstance()->getPathNameVector();
    for (const std::wstring& directory : paths) {
        const FileSystemWrapper::Path candidate = FileSystemWrapper::Path(directory) / filename;
        if (FileSystemWrapper::Path::is_regular_file(candidate)) {
            const std::wstring resolved = candidate.generic_wstring();
            if (std::find(matches.begin(), matches.end(), resolved) == matches.end()) {
                matches.push_back(resolved);
            }
        }
    }
    return matches;
}
//=============================================================================
static bool
endsWithMacroExtension(const std::string& name)
{
    size_t length = name.length();
    return length > 2 && name[length - 2] == '.'
        && (name[length - 1] == 'm' || name[length - 1] == 'M');
}
//=============================================================================
static std::string
macroNameWithoutExtension(const std::string& name)
{
    if (endsWithMacroExtension(name)) {
        return name.substr(0, name.length() - 2);
    }
    return name;
}
//=============================================================================
static void
findAllMatchingNames(const std::string& functionname, wstringVector& result)
{
    PathFunctionIndexerManager::getInstance()->find(functionname, result);
    wstringVector builtinFunctions;
    BuiltInFunctionDefManager::getInstance()->find(functionname, builtinFunctions);
    result.insert(result.end(), builtinFunctions.begin(), builtinFunctions.end());
}
//=============================================================================
static bool
findMatchingName(const std::string& functionname, std::wstring& origin)
{
    if (!PathFunctionIndexerManager::getInstance()->find(functionname, origin)) {
        BuiltInFunctionDefManager::getInstance()->find(functionname, origin);
    }
    return !origin.empty();
}
//=============================================================================
wstringVector
WhichAll(const std::string& functionname)
{
    wstringVector result;
    findAllMatchingNames(functionname, result);
    if (result.empty() && endsWithMacroExtension(functionname)) {
        findAllMatchingNames(macroNameWithoutExtension(functionname), result);
    }
    if (result.empty()) {
        result = resolveFilesOnSearchPath(utf8_to_wstring(functionname));
    }
    return result;
}
//=============================================================================
std::wstring
Which(const std::string& functionname)
{
    std::wstring origin;
    if (!findMatchingName(functionname, origin) && endsWithMacroExtension(functionname)) {
        findMatchingName(macroNameWithoutExtension(functionname), origin);
    }
    if (origin.empty()) {
        const wstringVector matches = resolveFilesOnSearchPath(utf8_to_wstring(functionname));
        if (!matches.empty()) {
            origin = matches.front();
        }
    }
    return origin;
}
//=============================================================================
wstringVector
WhichModule(const std::string& functionname)
{
    wstringVector res;
    wstringVector paths = WhichAll(functionname);
    if (!paths.empty()) {
        for (const auto& path : paths) {
            std::wstring moduleName = ModulesManager::Instance().findModuleNameByPath(path);
            if (!moduleName.empty()) {
                res.push_back(moduleName);
            } else {
                stringVector functionsList;
                std::wstring errorMessage;
                bool bRes = GatewayInfo(path, moduleName, functionsList, errorMessage);
                if (bRes) {
                    res.push_back(moduleName);
                } else {
                    res.push_back(L"");
                }
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
