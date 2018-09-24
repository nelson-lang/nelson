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
#include "DynamicLinkLibraryObject.hpp"
#include "dynamic_library.hpp"
#include <boost/dll/library_info.hpp>
#include <boost/filesystem.hpp>
#undef GetCurrentDirectory
#include "Error.hpp"
#include "GetCurrentDirectory.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
DynamicLinkLibraryObject::DynamicLinkLibraryObject(std::wstring libraryPath)
    : HandleGenericObject(std::wstring(DLLIB_CATEGORY_STR), this, false)
{
    _propertiesNames = { L"Path" };
    std::wstring fullLibraryPath = L"";
    if (searchLibrary(libraryPath, fullLibraryPath)) {
        boost::system::error_code errorCode;
        boost::dll::shared_library lib(fullLibraryPath, errorCode);
        if (errorCode) {
            Error(_("Cannot load library: ") + errorCode.message());
        }
        boost::filesystem::path full_path = lib.location();
        _libraryPath = full_path.generic_wstring();
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
    _libraryPath = L"";
}
//=============================================================================
bool
DynamicLinkLibraryObject::disp(Evaluator* eval)
{
    if (eval != nullptr) {
        Interface* io = eval->getInterface();
        if (io) {
            io->outputMessage(L"\n");
            io->outputMessage(L"\tPath: \t'" + _libraryPath + L"'\n");
            io->outputMessage(L"\n");
            return true;
        }
    }
    return false;
}
//=============================================================================
stringVector
DynamicLinkLibraryObject::getAvailableSymbols()
{
    stringVector symbols;
    boost::dll::library_info libinfo(_libraryPath, false);
    std::vector<std::string> stdSymbols = libinfo.symbols();
    symbols.reserve(stdSymbols.size());
    for (std::string s : stdSymbols) {
        symbols.push_back(s);
    }
    return symbols;
}
//=============================================================================
void*
DynamicLinkLibraryObject::getFunctionPointer(std::string symbolName)
{
    return get_function(_shared_library.native(), symbolName);
}
//=============================================================================
bool
DynamicLinkLibraryObject::get(std::wstring propertyName, ArrayOf& res)
{
    if (propertyName == L"Path") {
        res = ArrayOf::characterArrayConstructor(_libraryPath);
        return true;
    }
    return false;
}
//=========================================================================
bool
DynamicLinkLibraryObject::isWriteableProperty(std::wstring propertyName)
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
DynamicLinkLibraryObject::isProperty(std::wstring propertyName)
{
    auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
    return (it != _propertiesNames.end());
}
//=============================================================================
bool
DynamicLinkLibraryObject::isMethod(std::wstring propertyName)
{
    return false;
}
//=============================================================================
bool
DynamicLinkLibraryObject::searchLibrary(std::wstring libraryPath, std::wstring& fullLibraryPath)
{
    fullLibraryPath = L"";
    boost::filesystem::path pathToSplit = libraryPath;
    std::wstring parentPath = L"";
    wstringVector paths;
    if (pathToSplit.has_parent_path()) {
        parentPath = pathToSplit.parent_path().generic_wstring();
    }
    if (parentPath != L"") {
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
    boost::filesystem::path asPath(libraryPath);
    fullLibraryPath = asPath.generic_wstring();
    return false;
}
//=============================================================================
bool
DynamicLinkLibraryObject::findLibrary(
    wstringVector paths, const std::wstring& libraryName, std::wstring& fullLibraryPath)
{
    fullLibraryPath = L"";
    if (!paths.empty()) {
        for (std::wstring path : paths) {
            boost::system::error_code errorCode;
            boost::filesystem::path dir(path);
            boost::filesystem::path file(libraryName);
            boost::filesystem::path full_path = dir / file;
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
}
//=============================================================================
