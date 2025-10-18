//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <system_error>
#include <cstdio>
#include <memory>
#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <cstring>
#include "FileSystemWrapper.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "DynamicLibrary.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GetCurrentDirectory.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"

#ifdef _MSC_VER
#define NOMINMAX
#include <windows.h>
#endif
//=============================================================================
namespace fs = std::filesystem;

namespace Nelson {
//=============================================================================
// Helper: run command and capture stdout
static std::string
runCommandCaptureOutput(const std::string& cmd)
{
    std::string result;
#ifdef _MSC_VER
    FILE* pipe = _popen(cmd.c_str(), "r");
#else
    FILE* pipe = popen(cmd.c_str(), "r");
#endif
    if (!pipe)
        return result;
    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), pipe) != nullptr) {
        result.append(buffer);
    }
#ifdef _MSC_VER
    _pclose(pipe);
#else
    pclose(pipe);
#endif
    return result;
}

//=============================================================================
DynamicLinkLibraryObject::DynamicLinkLibraryObject(const std::wstring& libraryPath)
    : HandleGenericObject(NLS_HANDLE_DLLIB_CATEGORY_STR, this, false)
{
    _propertiesNames = { L"Path" };
    fs::path filePath(libraryPath);
    fs::path dirPath;
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
            std::string libpath = wstring_to_utf8(_libraryPath);
            library_handle h = load_dynamic_library(libpath);
            if (!h) {
                Error(utf8_to_wstring(get_dynamic_library_error()));
            } else {
                _shared_library = h;
            }
        } else {
            Error(_W("Cannot load library: ") + libraryPath);
        }
    } else {
        // ../path/ppp/libNelson.so
        try {
            dirPath = fs::absolute(dirPath);
        } catch (const fs::filesystem_error&) {
            Error(_W("Cannot load library: ") + libraryPath);
        }
        filePath = dirPath / fs::path(filename);
        std::string libpath = wstring_to_utf8(filePath.generic_wstring());
        library_handle h = load_dynamic_library(libpath);
        if (!h) {
            Error(utf8_to_wstring(get_dynamic_library_error()));
        } else {
            _shared_library = h;
            _libraryPath = filePath.generic_wstring();
        }
    }
}
//=============================================================================
DynamicLinkLibraryObject::~DynamicLinkLibraryObject()
{
    _propertiesNames.clear();
    if (_shared_library) {
        close_dynamic_library(_shared_library);
        _shared_library = {};
    }
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
    errorMessage.clear();
    std::string path = wstring_to_utf8(_libraryPath.empty() ? L"" : _libraryPath);
    if (path.empty()) {
        errorMessage = "Empty library path.";
        return symbols;
    }
#ifdef _MSC_VER
    // Parse PE export table from file
    std::ifstream ifs(path, std::ios::binary);
    if (!ifs) {
        errorMessage = "Cannot open file for reading.";
        return symbols;
    }
    std::vector<uint8_t> data(
        (std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
    if (data.size() < sizeof(IMAGE_DOS_HEADER)) {
        errorMessage = "File too small to be a PE file.";
        return symbols;
    }
    auto dos = reinterpret_cast<const IMAGE_DOS_HEADER*>(data.data());
    if (dos->e_magic != IMAGE_DOS_SIGNATURE) {
        errorMessage = "Not a valid PE (DOS header mismatch).";
        return symbols;
    }
    if (dos->e_lfanew <= 0
        || static_cast<size_t>(dos->e_lfanew) > data.size() - sizeof(IMAGE_NT_HEADERS)) {
        errorMessage = "Invalid NT headers offset.";
        return symbols;
    }
    auto nt = reinterpret_cast<const IMAGE_NT_HEADERS*>(data.data() + dos->e_lfanew);
    if (nt->Signature != IMAGE_NT_SIGNATURE) {
        errorMessage = "Invalid NT signature.";
        return symbols;
    }
    const IMAGE_OPTIONAL_HEADER& opt = nt->OptionalHeader;
    const DWORD exportRVA = opt.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
    const DWORD exportSize = opt.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
    if (exportRVA == 0) {
        // no exports
        return symbols;
    }
    // helper to convert rva to file offset
    auto rva_to_offset = [&](DWORD rva) -> size_t {
        auto fileHeader = &nt->FileHeader;
        IMAGE_SECTION_HEADER* firstSection = IMAGE_FIRST_SECTION(const_cast<IMAGE_NT_HEADERS*>(nt));
        for (unsigned i = 0; i < fileHeader->NumberOfSections; ++i) {
            const IMAGE_SECTION_HEADER& sec = firstSection[i];
            DWORD va = sec.VirtualAddress;
            DWORD sz = sec.Misc.VirtualSize ? sec.Misc.VirtualSize : sec.SizeOfRawData;
            if (rva >= va && rva < va + sz) {
                size_t off
                    = static_cast<size_t>(sec.PointerToRawData) + static_cast<size_t>(rva - va);
                if (off < data.size())
                    return off;
                return std::string::npos;
            }
        }
        return std::string::npos;
    };
    size_t expOff = rva_to_offset(exportRVA);
    if (expOff == std::string::npos || expOff + sizeof(IMAGE_EXPORT_DIRECTORY) > data.size()) {
        errorMessage = "Invalid export directory.";
        return symbols;
    }
    const IMAGE_EXPORT_DIRECTORY* exp
        = reinterpret_cast<const IMAGE_EXPORT_DIRECTORY*>(data.data() + expOff);
    DWORD numberOfNames = exp->NumberOfNames;
    DWORD addressOfNamesRVA = exp->AddressOfNames;
    size_t namesOff = rva_to_offset(addressOfNamesRVA);
    if (namesOff == std::string::npos
        || namesOff + static_cast<size_t>(numberOfNames) * sizeof(DWORD) > data.size()) {
        errorMessage = "Invalid AddressOfNames.";
        return symbols;
    }
    for (DWORD i = 0; i < numberOfNames; ++i) {
        DWORD nameRVA = *reinterpret_cast<const DWORD*>(data.data() + namesOff + i * sizeof(DWORD));
        size_t nameOff = rva_to_offset(nameRVA);
        if (nameOff == std::string::npos || nameOff >= data.size())
            continue;
        const char* s = reinterpret_cast<const char*>(data.data() + nameOff);
        if (s)
            symbols.push_back(std::string(s));
    }
    std::sort(symbols.begin(), symbols.end());
    symbols.erase(std::unique(symbols.begin(), symbols.end()), symbols.end());
    return symbols;
#else
    // POSIX: try to use nm to list dynamic/exported symbols
    std::string cmd = "nm -g \"" + path + "\" 2>/dev/null";
    std::string out = runCommandCaptureOutput(cmd);
    if (out.empty()) {
        // try without -g (macOS nm doesn't support some flags)
        cmd = "nm \"" + path + "\" 2>/dev/null";
        out = runCommandCaptureOutput(cmd);
    }
    if (out.empty()) {
        // try objdump -T (for shared objects)
        cmd = "objdump -T \"" + path + "\" 2>/dev/null";
        out = runCommandCaptureOutput(cmd);
    }
    if (out.empty()) {
        errorMessage = "Could not list symbols: external tools (nm/objdump) not available or file "
                       "has no symbols.";
        return symbols;
    }
    std::istringstream iss(out);
    std::string line;
    while (std::getline(iss, line)) {
        // attempt to extract last token which is symbol name for nm
        if (line.empty())
            continue;
        // trim
        while (!line.empty() && (line.back() == '\r' || line.back() == '\n'))
            line.pop_back();
        std::istringstream ls(line);
        std::vector<std::string> parts;
        std::string tok;
        while (ls >> tok)
            parts.push_back(tok);
        if (parts.empty())
            continue;
        std::string sym;
        // For objdump -T, symbol is last column too
        sym = parts.back();
        // skip common markers like '*' or version info
        if (sym.size() > 0 && sym[0] == '_') {
            // on some systems exported symbols have leading underscore; keep as-is
        }
        // basic validation: symbol should be alnum or punctuation
        if (!sym.empty())
            symbols.push_back(sym);
    }
    // unique and sort
    std::sort(symbols.begin(), symbols.end());
    symbols.erase(std::unique(symbols.begin(), symbols.end()), symbols.end());
    return symbols;
#endif
}
//=============================================================================
void*
DynamicLinkLibraryObject::getFunctionPointer(const std::string& symbolName)
{
    return get_function(_shared_library, symbolName);
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
//======================================================================== =
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
    fs::path pathToSplit = libraryPath;
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
            std::error_code ec;
            FileSystemWrapper::Path dir(path);
            FileSystemWrapper::Path file(libraryName);
            FileSystemWrapper::Path full_path = dir / file;
            std::wstring fullpath = full_path.generic_wstring();
            library_handle h = load_dynamic_libraryW(fullpath);
            if (h) {
                close_dynamic_library(h);
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
