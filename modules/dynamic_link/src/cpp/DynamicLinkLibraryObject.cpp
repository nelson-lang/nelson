//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/dll/library_info.hpp>
#include <boost/filesystem/exception.hpp>
#include "FileSystemWrapper.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "DynamicLibrary.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GetCurrentDirectory.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
DynamicLinkLibraryObject::DynamicLinkLibraryObject(const std::wstring& libraryPath)
    : HandleGenericObject(NLS_HANDLE_DLLIB_CATEGORY_STR, this, false)
{
    _propertiesNames = { L"Path" };
    boost::filesystem::path filePath(libraryPath);
    boost::filesystem::path dirPath;
    std::wstring parentPath;
    std::wstring filename;

    if (filePath.has_parent_path()) {
        dirPath = filePath.parent_path();
        parentPath = dirPath.wstring();
    }
    filename = filePath.filename().wstring();

    if (parentPath.empty()) {
        // libNelson.so
        if (searchLibrary(libraryPath, _libraryPath)) {
            boost::system::error_code errorCode;
            _shared_library = boost::dll::shared_library(_libraryPath, errorCode);
            if (errorCode) {
                Error(_("Cannot load library: ") + errorCode.message());
            }
        } else {
            Error(_W("Cannot load library: ") + libraryPath);
        }
    } else {
        // ../path/ppp/libNelson.so
        try {
            dirPath = boost::filesystem::absolute(dirPath);
        } catch (const boost::filesystem::filesystem_error&) {
            Error(_W("Cannot load library: ") + libraryPath);
        }
        filePath = dirPath / boost::filesystem::path(filename);
        boost::system::error_code errorCode;
        _shared_library = boost::dll::shared_library(filePath, errorCode);
        if (errorCode) {
            Error(_("Cannot load library: ") + errorCode.message());
        }
        _libraryPath = filePath.generic_wstring();
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
        boost::dll::library_info lib_info(_libraryPath, true);
        std::vector<std::string> stdSymbols = lib_info.symbols();

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
wstringVector
DynamicLinkLibraryObject::getProperties()
{
    return _propertiesNames;
}
//=============================================================================
wstringVector
DynamicLinkLibraryObject::getMethods()
{
    return {};
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
    FileSystemWrapper::Path asPath(libraryPath);
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
            FileSystemWrapper::Path dir(path);
            FileSystemWrapper::Path file(libraryName);
            FileSystemWrapper::Path full_path = dir / file;
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
