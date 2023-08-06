//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "StringHelpers.hpp"
#include "PathFunctionIndexer.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PathFunctionIndexer::PathFunctionIndexer(const std::wstring& path, bool withWatcher)
{
    this->withWatcher = withWatcher;
    if (FileSystemWrapper::Path::is_directory(path)) {
        _path = path;
    } else {
        _path.clear();
    }
    rehash();
}
//=============================================================================
std::unordered_map<std::string, FileFunction*>
PathFunctionIndexer::getAllFileFunctions()
{
    return mapAllFiles;
}
//=============================================================================
bool
PathFunctionIndexer::comparePathname(const std::wstring& path1, const std::wstring& path2)
{

    FileSystemWrapper::Path p1(path1);
    FileSystemWrapper::Path p2(path2);
    return FileSystemWrapper::Path::equivalent(p1, p2);
}
//=============================================================================
PathFunctionIndexer::~PathFunctionIndexer()
{
    for (auto& mapAllFile : mapAllFiles) {
        if (mapAllFile.second) {
            delete mapAllFile.second;
            mapAllFile.second = nullptr;
        }
    }
    mapRecentFiles.clear();
    mapAllFiles.clear();
    _path.clear();
}
//=============================================================================
wstringVector
PathFunctionIndexer::getFunctionsName(const std::wstring& prefix)
{
    wstringVector functionsName;
    for (auto& mapAllFile : mapAllFiles) {
        if (mapAllFile.second) {
            if (prefix.empty()) {
                functionsName.push_back(mapAllFile.second->getName());
            } else {
                std::wstring name = mapAllFile.second->getName();
                if (StringHelpers::starts_with(name, prefix)) {
                    functionsName.push_back(name);
                }
            }
        }
    }
    return functionsName;
}
//=============================================================================
wstringVector
PathFunctionIndexer::getFunctionsFilename()
{
    wstringVector functionsFilename;
    for (auto& mapAllFile : mapAllFiles) {
        if (mapAllFile.second) {
            functionsFilename.push_back(mapAllFile.second->getFilename());
        }
    }
    return functionsFilename;
}
//=============================================================================
std::wstring
PathFunctionIndexer::getPath()
{
    return _path;
}
//=============================================================================
bool
PathFunctionIndexer::isSupportedFuncFilename(const std::wstring& name)
{
    for (int c : name) {
        bool bSupportedChar
            = (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || (c == '_') || (c >= 48 && c <= 57);
        if (!bSupportedChar) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
PathFunctionIndexer::rehash()
{
    if (!_path.empty()) {
        mapRecentFiles.clear();
        try {
            nfs::directory_iterator end_iter;
            FileSystemWrapper::Path path(_path);
            for (nfs::directory_iterator dir_iter(path.native()); dir_iter != end_iter;
                 ++dir_iter) {
                FileSystemWrapper::Path current(dir_iter->path().native());
                std::wstring ext = current.extension().generic_wstring();
                bool isMacro = ext == L".m";
                bool isMex = ext == L"." + getMexExtension();
                if (isMacro || isMex) {
                    std::wstring name = current.stem().generic_wstring();
                    if (isSupportedFuncFilename(name)) {
                        FileFunction* ff = nullptr;
                        try {
                            ff = new FileFunction(_path, name, isMex, withWatcher);
                        } catch (const std::bad_alloc&) {
                            ff = nullptr;
                        }
                        if (ff) {
                            mapAllFiles.emplace(wstring_to_utf8(name), ff);
                        }
                    }
                }
            }
        } catch (const nfs::filesystem_error&) {
        }
    }
}
//=============================================================================
bool
PathFunctionIndexer::findFuncName(const std::string& functionName, std::wstring& filename)
{
    std::unordered_map<std::string, FileFunction*>::iterator found
        = mapAllFiles.find(functionName);
    if (found != mapAllFiles.end()) {
        filename = found->second->getFilename();
        if (FileSystemWrapper::Path::is_regular_file(filename)) {
            return true;
        }
    }
    if (withWatcher) {
        const std::wstring mexFullFilename
            = _path + L"/" + utf8_to_wstring(functionName) + L"." + getMexExtension();
        if (FileSystemWrapper::Path::is_regular_file(mexFullFilename)) {
            filename = mexFullFilename;
            return true;
        }
        const std::wstring macroFullFilename = _path + L"/" + utf8_to_wstring(functionName) + L".m";
        if (FileSystemWrapper::Path::is_regular_file(macroFullFilename)) {
            filename = macroFullFilename;
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
