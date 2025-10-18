//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include <utility>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "GatewaysManager.hpp"
#include "characters_encoding.hpp"
#include "FindDynamicLibraryName.hpp"
#include "DynamicLibrary.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "FileSystemWrapper.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define GATEWAY_ENTRY "AddGateway"
#define REMOVEGATEWAY_ENTRY "RemoveGateway"
//=============================================================================
static std::map<std::wstring, library_handle> libraryMap;
//=============================================================================
GatewaysManager* GatewaysManager::m_pInstance = nullptr;
//=============================================================================
GatewaysManager::GatewaysManager() = default;
//=============================================================================
GatewaysManager*
GatewaysManager::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new GatewaysManager();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
GatewaysManager::destroy(Evaluator* eval)
{
    if (m_pInstance != nullptr) {
        wstringVector names;
        for (auto it = libraryMap.rbegin(); it != libraryMap.rend(); ++it) {
            names.push_back(it->first);
        }
        for (const auto& n : names) {
            std::wstring errorMessage;
            removeGateway(eval, n, errorMessage);
        }
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
GatewaysManager::addGateway(
    Evaluator* eval, const std::wstring& libraryFullName, std::wstring& errorMessage)
{
    /* to simplify some dependencies resolution, we move in the directory and restore it after */
    FileSystemWrapper::Path p(libraryFullName);
    p = p.generic_wstring();
    std::wstring filename;
    FileSystemWrapper::Path dir = p.parent_path();
    if (dir.generic_wstring().compare(L"") == 0) {
        dir = FileSystemWrapper::Path::current_path();
    }
    const std::wstring dirname = dir.generic_wstring();
    filename = p.filename().generic_wstring();
    filename = FindDynamicLibraryName(dirname, filename, false);
    if (filename.empty()) {
        errorMessage = fmt::sprintf(_W("File not found: %s"), p.generic_wstring());
        return false;
    } else {
        FileSystemWrapper::Path currentdirbackup = FileSystemWrapper::Path::current_path();
        FileSystemWrapper::Path::current_path(dir);

        bool needToAdd = false;
        library_handle nlsModuleHandleDynamicLibrary = nullptr;
        std::map<std::wstring, library_handle>::iterator found = libraryMap.find(libraryFullName);
        if (found != libraryMap.end()) {
            nlsModuleHandleDynamicLibrary = found->second;
        } else {
#ifdef _MSC_VER
            nlsModuleHandleDynamicLibrary = load_dynamic_libraryW(filename);
#else
            nlsModuleHandleDynamicLibrary = load_dynamic_library(wstring_to_utf8(filename));
#endif
            needToAdd = true;
        }

        if (nlsModuleHandleDynamicLibrary != nullptr) {
            using PROC_AddGateway = bool (*)(const void*, const wchar_t*);
            PROC_AddGateway AddGatewayPtr = reinterpret_cast<PROC_AddGateway>(
                get_function(nlsModuleHandleDynamicLibrary, GATEWAY_ENTRY));
            if (needToAdd) {
                libraryMap.emplace(libraryFullName, nlsModuleHandleDynamicLibrary);
            }
            FileSystemWrapper::Path::current_path(currentdirbackup);
            if (needToAdd) {
                return AddGatewayPtr((void*)eval, libraryFullName.c_str());
            }
            return true;
        }
        std::string error_msg = get_dynamic_library_error();
        FileSystemWrapper::Path::current_path(currentdirbackup);
        errorMessage = _W("Module not loaded: library not loaded.\n") + libraryFullName + L"\n"
            + utf8_to_wstring(error_msg) + L"\n";
    }
    return false;
}
//=============================================================================
bool
GatewaysManager::removeGateway(
    Evaluator* eval, const std::wstring& libraryFullName, std::wstring& errorMessage)
{
    /* to simplify some dependencies resolution, we move in the directory and restore it after */
    FileSystemWrapper::Path p(libraryFullName);
    p = p.generic_wstring();
    std::wstring dirname;
    std::wstring filename;
    FileSystemWrapper::Path dir(p.parent_path());
    if (dir.generic_wstring().compare(L"") == 0) {
        dir = FileSystemWrapper::Path::current_path();
    }
    dirname = dir.generic_wstring();
    filename = p.filename().generic_wstring();
    filename = FindDynamicLibraryName(dirname, filename, false);
    if (filename.empty()) {
        errorMessage = _W("File not found.");
        return false;
    }
    FileSystemWrapper::Path currentdirbackup = FileSystemWrapper::Path::current_path();
    FileSystemWrapper::Path::current_path(dir);

    std::map<std::wstring, library_handle>::iterator found = libraryMap.find(libraryFullName);
    if (found != libraryMap.end()) {
        library_handle nlsModuleHandleDynamicLibrary = found->second;
        if (nlsModuleHandleDynamicLibrary != nullptr) {
            using PROC_RemoveGateway = bool (*)(void*, const wchar_t*);
            PROC_RemoveGateway RemoveGatewayPtr = reinterpret_cast<PROC_RemoveGateway>(
                get_function(nlsModuleHandleDynamicLibrary, REMOVEGATEWAY_ENTRY));
            FileSystemWrapper::Path::current_path(currentdirbackup);
            if (RemoveGatewayPtr != nullptr) {
                bool res = RemoveGatewayPtr((void*)eval, libraryFullName.c_str());
                libraryMap.erase(libraryFullName);
                close_dynamic_library(nlsModuleHandleDynamicLibrary);
                return res;
            }
            errorMessage = _W("Module not loaded: symbol not found.");

        } else {
            errorMessage = _W("Module not loaded: library handle not found.");
        }
    } else {
        errorMessage = _W("Module not loaded: library handle not found.");
    }
    return false;
}
//=============================================================================
wstringVector
GatewaysManager::getLibraryNames()
{
    wstringVector names;
    for (auto it = libraryMap.rbegin(); it != libraryMap.rend(); ++it) {
        names.push_back(it->first);
    }
    return names;
}
//=============================================================================
}
//=============================================================================
