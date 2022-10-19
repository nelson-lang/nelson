//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/dll/library_info.hpp>
#include "DynamicLinkLibraryObject.hpp"
#include "dynamic_library.hpp"
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#undef GetCurrentDirectory
#include "GetCurrentDirectory.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
DynamicLinkLibraryObject::DynamicLinkLibraryObject(const std::wstring& libraryPath)
    : HandleGenericObject(std::wstring(DLLIB_CATEGORY_STR), this, false)
{
    _propertiesNames = { L"Path" };
    std::wstring fullLibraryPath;
    if (searchLibrary(libraryPath, fullLibraryPath)) {
        boost::system::error_code errorCode;
        boost::dll::shared_library lib(fullLibraryPath, errorCode);
        if (errorCode) {
            Error(_("Cannot load library: ") + errorCode.message());
        }
        _libraryPath = lib.location().generic_wstring();
        _shared_library = lib;
    } else {
        Error(_W("Cannot load library: ") + libraryPath);
    }
}
//=============================================================================
DynamicLinkLibraryObject::~DynamicLinkLibraryObject()
{
    _propertiesNames.clear();
    _shared_library.unload();
    _libraryPath.clear();
}
//=============================================================================
bool
DynamicLinkLibraryObject::disp(Interface* io)
{
    if (io != nullptr) {
        io->outputMessage(L"\n");
        io->outputMessage(L"\tPath: \t'" + _libraryPath + L"'\n");
        io->outputMessage(L"\n");
        return true;
    }
    return false;
}
//=============================================================================
stringVector
DynamicLinkLibraryObject::getAvailableSymbols(std::string& errorMessage)
{
    stringVector symbols;
    try {
        boost::dll::library_info libinfo(_libraryPath, false);
        std::vector<std::string> stdSymbols = libinfo.symbols();
        symbols.reserve(stdSymbols.size());
        for (const std::string& s : stdSymbols) {
            symbols.push_back(s);
        }
    } catch (const std::runtime_error& e) {
        errorMessage = e.what();
    }
    return symbols;
}
//=============================================================================
void*
DynamicLinkLibraryObject::getFunctionPointer(const std::string& symbolName)
{
    return get_function(_shared_library.native(), symbolName);
}
//=============================================================================
bool
DynamicLinkLibraryObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    if (propertyName == L"Path") {
        res = ArrayOf::characterArrayConstructor(_libraryPath);
        return true;
    }
    return false;
}
//=========================================================================
bool
DynamicLinkLibraryObject::isWriteableProperty(const std::wstring& propertyName)
{
    return false;
}
//=============================================================================
wstringVector
DynamicLinkLibraryObject::fieldnames()
{
    return _propertiesNames;
}
//=============================================================================
bool
DynamicLinkLibraryObject::isProperty(const std::wstring& propertyName)
{
    auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
    return (it != _propertiesNames.end());
}
//=============================================================================
bool
DynamicLinkLibraryObject::isMethod(const std::wstring& propertyName)
{
    return false;
}
//=============================================================================
bool
DynamicLinkLibraryObject::searchLibrary(
    const std::wstring& libraryPath, std::wstring& fullLibraryPath)
{
    fullLibraryPath.clear();
    boost::filesystem::path pathToSplit = libraryPath;
    std::wstring parentPath;
    if (pathToSplit.has_parent_path()) {
        parentPath = pathToSplit.parent_path().generic_wstring();
    }
    wstringVector paths;

    if (!parentPath.empty()) {
        paths.push_back(parentPath);
        std::wstring filename = pathToSplit.filename().generic_wstring();
        if (findLibrary(paths, filename, fullLibraryPath)) {
            return true;
        }
    }
    paths.clear();
    paths.push_back(Nelson::GetCurrentDirectory());
    if (findLibrary(paths, libraryPath, fullLibraryPath)) {
        return true;
    }
    paths = getEnvironmentPaths(L"NLS_LIBRARY_PATH");
    if (findLibrary(paths, libraryPath, fullLibraryPath)) {
        return true;
    }
#ifdef _MSC_VER
    paths = getEnvironmentPaths(L"PATH");
#else
#ifdef __APPLE__
    paths = getEnvironmentPaths(L"DYLD_LIBRARY_PATH");
#else
    paths = getEnvironmentPaths(L"LD_LIBRARY_PATH");
#endif
#endif
    if (findLibrary(paths, libraryPath, fullLibraryPath)) {
        return true;
    }
#ifndef _MSC_VER
    paths.clear();
    paths.push_back(L"/usr/lib");
    paths.push_back(L"/usr/local/lib");
    if (findLibrary(paths, libraryPath, fullLibraryPath)) {
        return true;
    }
#endif
    Nelson::FileSystemWrapper::Path asPath(libraryPath);
    fullLibraryPath = asPath.generic_wstring();
    return false;
}
//=============================================================================
bool
DynamicLinkLibraryObject::findLibrary(
    const wstringVector& paths, const std::wstring& libraryName, std::wstring& fullLibraryPath)
{
    fullLibraryPath.clear();
    if (!paths.empty()) {
        for (const std::wstring& path : paths) {
            boost::system::error_code errorCode;
            Nelson::FileSystemWrapper::Path dir(path);
            Nelson::FileSystemWrapper::Path file(libraryName);
            Nelson::FileSystemWrapper::Path full_path = dir / file;
            std::wstring fullpath = full_path.generic_wstring();
            boost::dll::shared_library lib(fullpath, errorCode);
            if (!errorCode) {
                fullLibraryPath = fullpath;
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
wstringVector
DynamicLinkLibraryObject::getEnvironmentPaths(const std::wstring& environPath)
{
    wstringVector result;
    std::wstring Path = GetVariableEnvironment(environPath);
#if _MSC_VER
    const wchar_t delimiter = L';';
#else
    const wchar_t delimiter = L':';
#endif
    if (Path.empty()) {
        return result;
    }
    size_t previous = 0;
    size_t index = Path.find(delimiter);
    std::wstring path;
    while (index != std::wstring::npos) {
        path = Path.substr(previous, index - previous);
        if (!path.empty()) {
            result.push_back(path);
        }
        previous = index + 1;
        index = Path.find(delimiter, previous);
    }
    path = Path.substr(previous);
    if (!path.empty()) {
        result.push_back(path);
    }
    return result;
}
//=============================================================================
std::wstring
DynamicLinkLibraryObject::getLibraryPath()
{
    return _libraryPath;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
