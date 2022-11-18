//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "StringHelpers.hpp"
#include "PathFunc.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PathFunc::PathFunc(const std::wstring& path, bool withWatcher)
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
bool
PathFunc::comparePathname(const std::wstring& path1, const std::wstring& path2)
{

    FileSystemWrapper::Path p1(path1);
    FileSystemWrapper::Path p2(path2);
    return FileSystemWrapper::Path::equivalent(p1, p2);
}
//=============================================================================
PathFunc::~PathFunc()
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
PathFunc::getFunctionsName(const std::wstring& prefix)
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
PathFunc::getFunctionsFilename()
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
PathFunc::getPath()
{
    return _path;
}
//=============================================================================
bool
PathFunc::isSupportedFuncFilename(const std::wstring& name)
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
PathFunc::rehash()
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
                            mapAllFiles.emplace(name, ff);
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
PathFunc::findFuncName(const std::wstring& functionName, std::wstring& filename)
{
    std::unordered_map<std::wstring, FileFunction*>::iterator found
        = mapAllFiles.find(functionName);
    if (found != mapAllFiles.end()) {
        filename = found->second->getFilename();
        if (FileSystemWrapper::Path::is_regular_file(filename)) {
            return true;
        }
    }
    if (withWatcher) {
        const std::wstring mexFullFilename = _path + L"/" + functionName + L"." + getMexExtension();
        if (FileSystemWrapper::Path::is_regular_file(mexFullFilename)) {
            filename = mexFullFilename;
            return true;
        }
        const std::wstring macroFullFilename = _path + L"/" + functionName + L".m";
        if (FileSystemWrapper::Path::is_regular_file(macroFullFilename)) {
            filename = macroFullFilename;
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
PathFunc::findFuncName(const std::wstring& functionName, FileFunction** ff)
{
    std::unordered_map<std::wstring, FileFunction*>::const_iterator foundit
        = mapRecentFiles.find(functionName);
    if (foundit != mapRecentFiles.end()) {
        *ff = foundit->second;
        if (FileSystemWrapper::Path::is_regular_file(foundit->second->getFilename())) {
            return true;
        }
        if (!withWatcher) {
            std::wstring msg
                = fmt::sprintf(_W("Previously accessible file '%s' is now inaccessible."),
                    foundit->second->getFilename());
            Error(msg);
        }
    }
    std::unordered_map<std::wstring, FileFunction*>::iterator found
        = mapAllFiles.find(functionName);
    if (found != mapAllFiles.end()) {
        *ff = found->second;
        if (FileSystemWrapper::Path::is_regular_file(found->second->getFilename())) {
            mapRecentFiles.emplace(functionName, *ff);
            return true;
        }
        if (!withWatcher) {
            std::wstring msg
                = fmt::sprintf(_W("Previously accessible file '%s' is now inaccessible."),
                    found->second->getFilename());
            Error(msg);
        }
    }
    if (withWatcher) {
        const std::wstring mexFullFilename = _path + L"/" + functionName + L"." + getMexExtension();
        bool foundAsMacro = false;
        bool foundAsMex = false;
        foundAsMex = FileSystemWrapper::Path::is_regular_file(mexFullFilename);
        if (!foundAsMex) {
            const std::wstring macroFullFilename = _path + L"/" + functionName + L".m";
            foundAsMacro = FileSystemWrapper::Path::is_regular_file(macroFullFilename);
        }
        if (foundAsMex || foundAsMacro) {
            try {
                *ff = new FileFunction(_path, functionName, foundAsMex, withWatcher);
            } catch (const std::bad_alloc&) {
                *ff = nullptr;
            }
            if (ff != nullptr) {
                mapAllFiles.emplace(functionName, *ff);
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
